use crate::mem_map::*;
use crate::utils::*;
use std::sync::atomic::{AtomicUsize, Ordering};
pub const BITS_PER_INTPTR: usize = core::mem::size_of::<usize>() * 8;

pub struct SpaceBitMap<A: Alignment = ObjectAlignment> {
    storage: &'static mut [usize],
    size: usize,
    heap_begin: Address,
    heap_limit: Address,
    mem_map: MemMap,
    _m: core::marker::PhantomData<A>,
}

pub trait Alignment {
    const ALIGN: usize = core::mem::align_of::<crate::GCHeader>();
}

pub struct ObjectAlignment;
impl Alignment for ObjectAlignment {}

pub struct LargeObjectAlignment;
impl Alignment for LargeObjectAlignment {
    const ALIGN: usize = 4096 * 2;
}

impl<A: Alignment> SpaceBitMap<A> {
    pub fn offset_to_index(offset: usize) -> usize {
        offset / A::ALIGN / BITS_PER_INTPTR
    }

    pub fn index_to_offset(index: usize) -> usize {
        index * A::ALIGN * BITS_PER_INTPTR
    }
    #[inline(always)]
    pub fn offset_bit_index(off: usize) -> usize {
        (off / A::ALIGN) % BITS_PER_INTPTR
    }

    pub fn offset_to_mask(off: usize) -> usize {
        1 << Self::offset_bit_index(off)
    }
    pub fn compute_size(cap: usize) -> usize {
        let b = A::ALIGN * BITS_PER_INTPTR;
        (round_up(cap, b) / b) * core::mem::size_of::<usize>()
    }

    pub fn compute_heap_size(b: usize) -> usize {
        b * 8 * A::ALIGN
    }
    pub fn clear_all(&self) {
        let mut this = Ref::new(self);
        for i in this.storage.iter_mut() {
            *i = 0;
        }

        self.mem_map.madvise_dontneed_and_zero();
    }
    pub fn new(heap_begin: Address, heap_cap: usize) -> Self {
        let size = Self::compute_size(heap_cap);
        let map = MemMap::map_anonymous(
            "bitmap",
            Address::null(),
            size,
            PROT_READ | PROT_WRITE,
            false,
            None,
        );

        Self {
            storage: unsafe {
                core::slice::from_raw_parts_mut(
                    map.begin().to_mut_ptr(),
                    heap_cap / (core::mem::size_of::<usize>() * 8) / A::ALIGN,
                )
            },
            heap_begin,
            mem_map: map,
            heap_limit: heap_begin.offset(heap_cap),

            size,
            _m: Default::default(),
        }
    }

    pub fn modify<SetBit: Bool>(&self, addr: Address) -> bool {
        let off = addr.to_usize() as isize - self.heap_begin.to_usize() as isize;
        let index = Self::offset_to_index(off as _);
        let mask = Self::offset_to_mask(off as _);
        if index >= self.storage.len() {
            panic!(
                "Address {:p} out of space bitmap bounds",
                addr.to_ptr::<()>()
            );
        }
        /*println!(
            "offset {:x}, index {},mask {} for {:p}",
            off,
            index,
            mask,
            addr.to_ptr::<u8>()
        );*/
        let entry = unsafe { as_atomic!(&self.storage[index], AtomicUsize) };
        let old_word = entry.load(Ordering::Relaxed);
        if SetBit::RES {
            if (old_word & mask) == 0 {
                entry.store(old_word | mask, Ordering::Relaxed);
            }
        } else {
            entry.store(old_word & !mask, Ordering::Relaxed);
        }
        (old_word & mask) != 0
    }
    #[inline]
    pub fn set(&self, addr: Address) -> bool {
        self.modify::<True>(addr)
    }
    #[inline]
    pub fn clear(&self, addr: Address) -> bool {
        if addr.is_null() {
            return false;
        }
        self.modify::<False>(addr)
    }
    #[inline]
    pub fn set_heap_size(&mut self, nbytes: usize) {
        self.heap_limit = self.heap_begin.offset(nbytes);
        self.size = Self::offset_to_index(nbytes) * core::mem::size_of::<usize>();
    }
    #[inline]
    pub fn heap_size(&self) -> usize {
        Self::index_to_offset(self.size / core::mem::size_of::<usize>())
    }
    #[inline]
    pub fn size(&self) -> usize {
        self.size
    }
    pub fn test(&self, addr: Address) -> bool {
        if addr.is_null() {
            return false;
        }
        debug_assert!(addr.is_non_null());

        let off = addr.to_usize() as isize - self.heap_begin.to_usize() as isize;
        let index = Self::offset_to_index(off as _);
        let mask = Self::offset_to_mask(off as _);
        if index >= self.storage.len() {
            return false;
        }
        unsafe {
            (as_atomic!(&self.storage[index], AtomicUsize).load(Ordering::Relaxed) & mask) != 0
        }
    }

    pub fn has_addr(&self, addr: Address) -> bool {
        let off = addr.to_usize() as isize - self.heap_begin.to_usize() as isize;
        let ix = Self::offset_to_index(off as _);
        ix <= self.size() / core::mem::size_of::<usize>()
    }

    pub fn visit_range(
        &self,
        mut visit_begin: Address,
        visit_end: Address,
        mut visitor: impl FnMut(Address),
    ) {
        while visit_begin < visit_end {
            visitor(visit_begin);
            visit_begin = visit_begin.offset(A::ALIGN);
        }
    }

    pub fn sweep_walk(
        live: &SpaceBitMap<A>,
        mark: &SpaceBitMap<A>,
        sweep_begin: Address,
        sweep_end: Address,
        cb: &mut dyn FnMut(usize, Ref<Ref<crate::GcBox<()>>>),
    ) {
        if sweep_end <= sweep_begin {
            return;
        }
        let buffer_size = core::mem::size_of::<isize>() * BITS_PER_INTPTR;
        let start = Self::offset_to_index(
            (sweep_begin.to_usize() as isize - live.heap_begin.to_usize() as isize) as usize,
        );
        let end = Self::offset_to_index(
            (sweep_end.to_usize() as isize - live.heap_begin.to_usize() as isize - 1) as usize,
        );

        let mut pointer_buf = Vec::with_capacity(buffer_size);

        unsafe {
            for i in start..=end {
                let mut garbage = as_atomic!(&live.storage[i], AtomicUsize).load(Ordering::Relaxed)
                    & (!as_atomic!(&mark.storage[i], AtomicUsize).load(Ordering::Relaxed));
                //println!("{:b}", garbage);
                if garbage != 0 {
                    let ptr_base = live.heap_begin.offset(Self::index_to_offset(i));

                    while {
                        //log!("garbage {:b}", garbage);
                        let shift = garbage.trailing_zeros() as usize;
                        garbage ^= 1 << shift;
                        //*cur_pointer = Ref::new(ptr_base.offset(shift * A::ALIGN).to_ptr());
                        pointer_buf.push(Ref::new(ptr_base.offset(shift * A::ALIGN).to_ptr()));
                        //cur_pointer = cur_pointer.offset(1);
                        garbage != 0
                    } {}

                    if !pointer_buf.is_empty() {
                        cb(pointer_buf.len(), Ref::new(&pointer_buf[0]));
                        pointer_buf.clear();
                        //cur_pointer = Ref::new(&mut pointer_buf[0]);
                    }
                    //if cur_pointer.ptr >= pointer_end.ptr {
                    //    cb(
                    //        cur_pointer.ptr.sub((&pointer_buf[0] as *const _) as usize) as usize,
                    //        Ref::new(&pointer_buf[0]),
                    //    );
                    //    cur_pointer = Ref::new(&pointer_buf[0]);
                    //}
                }
            }

            if !pointer_buf.is_empty() {
                cb(pointer_buf.len(), Ref::new(&pointer_buf[0]));
                //cb(pointer_buf.len(), Ref::new(&pointer_buf[0]));
                //pointer_buf.clear();
            }
        }
    }

    pub fn visit_marked_range(
        &self,
        mut visit_begin: Address,
        visit_end: Address,
        mut visitor: impl FnMut(Address),
    ) {
        const EASY_WAY: bool = false;
        if EASY_WAY {
            while visit_begin < visit_end {
                if self.test(visit_begin) {
                    visitor(visit_begin);
                }
                visit_begin = visit_begin.offset(A::ALIGN);
            }
        } else {
            let offset_start =
                (visit_begin.to_usize() as isize - self.heap_begin.to_usize() as isize) as usize;
            let offset_end =
                (visit_end.to_usize() as isize - self.heap_begin.to_usize() as isize) as usize;

            let index_start = Self::offset_to_index(offset_start);
            let index_end = Self::offset_to_index(offset_end);

            let bit_start = (offset_start / A::ALIGN) % core::mem::size_of::<usize>();
            let bit_end = (offset_end / A::ALIGN) % core::mem::size_of::<usize>();
            // Index(begin)  ...    Index(end)
            // [xxxxx???][........][????yyyy]
            //      ^                   ^
            //      |                   #---- Bit of visit_end
            //      #---- Bit of visit_begin
            //

            let mut left_edge = self.storage[index_start];
            left_edge &= !((1 << bit_start) - 1);
            let mut right_edge;
            if index_start < index_end {
                // Left edge != right edge.

                // Traverse left edge.
                if left_edge != 0 {
                    let ptr_base = self.heap_begin.add_ptr(Self::index_to_offset(index_start));
                    while {
                        let shift = left_edge.trailing_zeros();
                        let obj = ptr_base.offset(shift as usize * A::ALIGN);
                        visitor(obj);
                        left_edge ^= 1 << shift as usize;
                        left_edge != 0
                    } {}
                }
                // Traverse the middle, full part.
                for i in index_start + 1..index_end {
                    let mut w = unsafe {
                        as_atomic!(&self.storage[i], AtomicUsize).load(Ordering::Relaxed)
                    };
                    if w != 0 {
                        let ptr_base = self.heap_begin.add_ptr(Self::index_to_offset(i));

                        while {
                            let shift = w.trailing_zeros();
                            let obj = ptr_base.offset(shift as usize * A::ALIGN);
                            visitor(obj);
                            w ^= 1 << shift as usize;
                            w != 0
                        } {}
                    }
                }
                // Right edge is unique.
                // But maybe we don't have anything to do: visit_end starts in a new word...
                if bit_end == 0 {
                    // Do not read memory, as it could be after the end of the bitmap.
                    right_edge = 0;
                } else {
                    right_edge = self.storage[index_end];
                }
            } else {
                right_edge = left_edge;
            }
            // Right edge handling.
            right_edge &= (1 << bit_end) - 1;
            if right_edge != 0 {
                let ptr_base = self.heap_begin.add_ptr(Self::index_to_offset(index_end));
                // Iterate on the bits set in word `right_edge`, from the least to the most significant bit.
                while {
                    let shift = right_edge.trailing_zeros();
                    let obj = ptr_base.offset(shift as usize * A::ALIGN);
                    visitor(obj);
                    right_edge ^= 1 << shift as usize;
                    right_edge != 0
                } {}
            }
        }
    }
}

impl<A: Alignment> Drop for SpaceBitMap<A> {
    fn drop(&mut self) {
        MemMap::target_munmap(self.mem_map.begin, self.mem_map.size());
    }
}
