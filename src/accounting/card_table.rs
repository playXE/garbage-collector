use super::space_bitmap::*;
use crate::{mem_map::*, utils::*, *};
use core::sync::atomic::{AtomicU8, AtomicUsize, Ordering};
pub const CARD_SHIFT: usize = 10;
pub const CARD_SIZE: usize = 1 << CARD_SHIFT;
pub const CARD_CLEAN: u8 = 0x0;
pub const CARD_DIRTY: u8 = 0x70;
pub const CARD_AGED: u8 = CARD_DIRTY - 1;

const fn is_aligned(x: usize, n: usize) -> bool {
    (x & (n - 1)) == 0
}

pub struct CardTable {
    pub map: MemMap,
    pub biased_begin: Address,
    pub offset: usize,
}

fn byte_cas(old: u8, new: u8, addr: &u8) -> bool {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        let atomic = as_atomic!(addr, AtomicU8);
        atomic
            .compare_exchange_weak(old, new, Ordering::AcqRel, Ordering::Relaxed)
            .is_ok()
    }
    #[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
    unsafe {
        use core::sync::atomic::AtomicUsize;
        let shift_in_bytes = addr as *const u8 as usize % core::mem::size_of::<usize>();
        let addr = addr as *const u8;
        let addr = addr.offset(-(shift_in_bytes as isize));
        let shift_in_bits = shift_in_bytes * core::mem::size_of::<usize>() * 8;
        let word_atomic = as_atomic!(&*addr, AtomicUsize);
        let cur_word = word_atomic.load(Ordering::Relaxed) & !(0xff << shift_in_bits);
        let old_word = cur_word | ((old as usize) << shift_in_bits);
        let new_word = cur_word | ((new as usize) << shift_in_bits);
        word_atomic
            .compare_exchange_weak(old_word, new_word, Ordering::AcqRel, Ordering::Relaxed)
            .is_ok()
    }
}

impl CardTable {
    pub fn new(heap_begin: Address, capacity: usize) -> Self {
        let cap = capacity / CARD_SIZE;
        let map = MemMap::map_anonymous(
            "card table",
            Address::null(),
            cap + 256,
            PROT_READ | PROT_WRITE,
            false,
            None,
        );
        let cbegbin = map.begin();
        let mut offset = 0;
        let mut biased_begin =
            Address::from(cbegbin.to_usize() - (heap_begin.to_usize() >> CARD_SHIFT));
        let biased_byte = biased_begin.to_usize() & 0xff;
        if biased_byte != CARD_DIRTY as usize {
            let delta = CARD_DIRTY as isize - biased_byte as isize;
            offset = (delta + (if delta < 0 { 0x100 } else { 0 })) as usize;
            biased_begin = biased_begin.offset(offset);
        }
        Self {
            offset,
            map,
            biased_begin,
        }
    }
    pub fn addr_from_card(&self, addr: Address) -> Address {
        let offset = addr.to_usize() as isize - self.biased_begin.to_usize() as isize;
        Address::from((offset as usize) << CARD_SHIFT)
    }

    pub fn card_from_addr(&self, addr: Address) -> Address {
        let card_addr = self.biased_begin.offset(addr.to_usize() >> CARD_SHIFT);
        card_addr
    }
    pub fn is_valid_card(&self, addr: Address) -> bool {
        let begin = self.map.begin().offset(self.offset);
        let end = self.map.end();
        addr >= begin && addr < end
    }

    pub fn clear_card_table(&self) {
        unsafe {
            libc::madvise(
                self.map.begin().to_mut_ptr(),
                self.map.size(),
                libc::MADV_DONTNEED,
            );
        }
    }

    pub fn clear_card_range(&self, start: Address, end: Address) {
        let start_card = self.card_from_addr(start);
        let end_card = self.card_from_addr(end);
        unsafe {
            let size = end_card.to_usize() - start_card.to_usize();
            core::ptr::write_bytes(start_card.to_mut_ptr::<u8>(), 0, size);
            libc::madvise(start_card.to_mut_ptr(), size, libc::MADV_DONTNEED);
        }
    }

    pub fn addr_is_in_card_table(&self, addr: Address) -> bool {
        self.is_valid_card(self.biased_begin.offset(addr.to_usize() >> CARD_SHIFT))
    }

    pub unsafe fn scan<'a, ClearCard: Bool>(
        &self,
        bitmap: &SpaceBitMap<ObjectAlignment>,
        scan_begin: Address,
        scan_end: Address,
        stack: &super::stack::ObjectStack,
        min_age: u8,
    ) {
        let card_begin = self.card_from_addr(scan_begin);
        let card_end = self.card_from_addr(scan_end);
        let mut card_cur = card_begin;
        // Handle any unaligned cards at the start.
        while !is_aligned(card_cur.to_usize(), core::mem::size_of::<usize>()) && card_cur < card_end
        {
            if *card_cur.to_ptr::<u8>() >= min_age {
                let start = self.addr_from_card(card_cur);
                bitmap.visit_marked_range(start, start.offset(CARD_SIZE), |addr| {
                    stack.push(Ref::new(addr.to_ptr::<GcBox<()>>()));
                });
            }
            card_cur = card_cur.offset(1);
        }

        let aligned_end = Address::from(
            (card_end.to_usize() as isize
                - (card_end.to_usize() as isize & (core::mem::size_of::<usize>() as isize & 1)))
                as usize,
        );

        let word_end = aligned_end;
        let mut word_cur = card_cur;
        'exit: while word_cur < word_end {
            while *word_cur.to_ptr::<usize>() == 0 {
                word_cur = word_cur.offset(8);
                if word_cur > word_end {
                    break 'exit;
                }
            }
            let mut start_word = *word_cur.to_ptr::<usize>();
            let mut start = self.addr_from_card(word_cur);

            for _ in 0..core::mem::size_of::<usize>() {
                //let card = word_cur.offset(i);
                if start_word as u8 >= min_age {
                    bitmap.visit_marked_range(start, start.offset(CARD_SIZE), |addr| {
                        stack.push(Ref::new(addr.to_ptr::<GcBox<()>>()));
                    });
                }
                start_word = start_word >> 8;
                start = start.offset(CARD_SIZE);
            }
            word_cur = word_cur.offset(8);
        }
        // Handle any unaligned cards at the end.
        let mut card_cur = word_end;
        while card_cur < card_end {
            if *card_cur.to_ptr::<u8>() >= min_age {
                let start = self.addr_from_card(card_cur);
                bitmap.visit_marked_range(start, start.offset(CARD_SIZE), |addr| {
                    stack.push(Ref::new(addr.to_ptr::<GcBox<()>>()))
                });
            }
            card_cur = card_cur.offset(1);
        }

        if ClearCard::RES {
            self.clear_card_range(scan_begin, scan_end);
        }
    }
    pub fn modify_cards_atomic(
        &self,
        scan_begin: Address,
        scan_end: Address,
        mut visitor: impl FnMut(u8) -> u8,
        mut modified: impl FnMut(Address, u8, u8),
    ) {
        let mut card_cur = self.card_from_addr(scan_begin);
        let mut card_end = self.card_from_addr(scan_end.align_to(CARD_SIZE));
        unsafe {
            while !is_aligned(card_cur.to_usize(), core::mem::size_of::<usize>())
                && card_cur < card_end
            {
                let mut expected;
                let mut new_value;
                while {
                    expected = card_cur.to_ptr::<u8>().read();
                    new_value = visitor(expected);
                    expected != new_value
                        && !byte_cas(expected, new_value, &*card_cur.to_ptr::<u8>())
                } {}
                if expected != new_value {
                    modified(card_cur, expected, new_value);
                }
                card_cur = card_cur.offset(1);
            }
            // handle unaligned cards
            while !is_aligned(card_end.to_usize(), core::mem::size_of::<usize>())
                && card_end > card_cur
            {
                card_end = card_end.sub(1);
                let mut expected;
                let mut new_value;
                while {
                    expected = card_cur.to_ptr::<u8>().read();
                    new_value = visitor(expected);
                    expected != new_value
                        && !byte_cas(expected, new_value, &*card_end.to_ptr::<u8>())
                } {}
                if expected != new_value {
                    modified(card_end, expected, new_value);
                }
            }
            // Now we have the words, we can process words in parallel.
            let mut word_cur = card_cur.to_mut_ptr::<usize>();
            let mut word_end = card_end.to_mut_ptr::<usize>();

            union X {
                expected_word: usize,
                expected_bytes: [u8; core::mem::size_of::<usize>()],
            }

            let mut x = X { expected_word: 0 };
            let (mut expected_word, expected_bytes): (Ref<usize>, &mut [u8]) =
                (Ref::new(&mut x.expected_word), &mut x.expected_bytes);

            union Y {
                new_word: usize,
                new_bytes: [u8; core::mem::size_of::<usize>()],
            };
            let mut y = Y { new_word: 0 };
            let (mut new_word, new_bytes): (Ref<usize>, &mut [u8]) =
                (Ref::new(&mut y.new_word), &mut y.new_bytes);

            while word_cur < word_end {
                loop {
                    *expected_word = *word_cur;
                    if *expected_word == 0 {
                        break;
                    }
                    for i in 0..core::mem::size_of::<usize>() {
                        new_bytes[i] = visitor(expected_bytes[i]);
                    }

                    let atomic_word: *mut AtomicUsize = std::mem::transmute(word_cur);
                    if (&*atomic_word)
                        .compare_exchange_weak(
                            *expected_word as _,
                            *new_word as _,
                            Ordering::Relaxed,
                            Ordering::Relaxed,
                        )
                        .is_ok()
                    {
                        for i in 0..core::mem::size_of::<usize>() {
                            let expected_byte = expected_bytes[i];
                            let new_byte = new_bytes[i];
                            if expected_byte != new_byte {
                                modified(
                                    Address::from_ptr(word_cur.cast::<u8>().offset(i as _)),
                                    expected_byte,
                                    new_byte,
                                );
                            }
                        }
                        break;
                    }
                }
                word_cur = word_cur.offset(1);
            }
        }
    }
    pub fn mark_card(&self, addr: Address) {
        unsafe {
            *self.card_from_addr(addr).to_mut_ptr::<u8>() = CARD_DIRTY;
        }
    }

    pub fn is_dirty(&self, addr: Address) -> bool {
        self.get_card(addr) == CARD_DIRTY
    }

    pub fn get_card(&self, addr: Address) -> u8 {
        unsafe { *self.card_from_addr(addr).to_ptr::<u8>() }
    }

    pub fn is_clean(&self, addr: Address) -> bool {
        self.get_card(addr) == CARD_CLEAN
    }
    /// Visit and clear cards within memory range, only visits dirty cards.
    pub fn visit_clear(&self, start: Address, end: Address, mut visitor: impl FnMut(Address)) {
        let card_start = self.card_from_addr(start);
        let card_end = self.card_from_addr(end);
        let mut cur = card_start;
        while cur != card_end {
            unsafe {
                if *cur.to_mut_ptr::<u8>() == CARD_DIRTY {
                    *cur.to_mut_ptr::<u8>() = CARD_CLEAN;
                    visitor(cur);
                }
            }
            cur = cur.offset(1);
        }
    }
}
