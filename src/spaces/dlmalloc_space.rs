extern "C" {
    fn create_mspace_with_base(
        base: *mut u8,
        cap: usize,
        locked: u8,
        rt: *mut libc::c_void,
    ) -> usize;
    fn create_mspace(cap: usize, locked: i32) -> usize;
    fn mspace_malloc(msp: usize, bytes: usize) -> *mut u8;
    fn mspace_memalign(msp: usize, align: usize, bytes: usize) -> *mut u8;
    fn mspace_realloc(msp: usize, old: *mut u8, bytes: usize) -> *mut u8;
    fn mspace_free(msp: usize, mem: *mut u8);
    fn mspace_set_footprint_limit(msp: usize, limit: usize);
    fn mspace_footprint(msp: usize) -> usize;
    fn mspace_trim(msp: usize, pad: usize) -> i32;
    fn mspace_usable_size(ptr: usize) -> usize;
    fn mspace_bulk_free(msp: usize, ptrs: Address, num: usize);
    fn mspace_inspect_all(msp: usize, handler: usize, arg: Address);
}
use super::Space;
use crate::*;
use locks::*;
use mem_map::*;
use spaces::spaces::*;
use utils::*;
pub struct DLMallocSpace {
    pub mspace: usize,

    pub lock: Mutex<()>,
    pub c: ContinuousMemMapSpace,
}

impl DLMallocSpace {
    #[inline]
    pub fn alloc_without_growth_locked(&self, num_bytes: usize) -> Address {
        unsafe { Address::from_ptr(mspace_malloc(self.mspace, num_bytes)) }
    }

    pub fn alloc_with_growth(&self, num_bytes: usize) -> Address {
        unsafe {
            let result;
            let lock = self.lock.lock();
            let max_allowed = self.c.growth_limit();
            mspace_set_footprint_limit(self.mspace, max_allowed);
            result = self.alloc_without_growth_locked(num_bytes);
            let footprint = mspace_footprint(self.mspace);
            mspace_set_footprint_limit(self.mspace, footprint);
            drop(lock);
            if result.is_non_null() {
                core::ptr::write_bytes(result.to_mut_ptr::<u8>(), 0, num_bytes);
            }

            result
        }
    }
    pub fn create_mspace(
        rt: &mut crate::Heap,
        begin: Address,
        morecore_start: usize,
        initial_size: usize,
    ) -> usize {
        unsafe {
            let msp = create_mspace_with_base(
                begin.to_mut_ptr(),
                morecore_start,
                0,
                rt as *mut crate::Heap as *mut libc::c_void,
            );
            if msp != 0 {
                mspace_set_footprint_limit(msp, initial_size);
            } else {
                panic!("Failed to create mspace");
            }
            /*log!(
                "Initialized mspace with {} initial size from {:p} to {:p}",
                formatted_size(initial_size),
                begin.to_ptr::<u8>(),
                begin.offset(initial_size).to_ptr::<u8>()
            );*/
            msp
        }
    }
    pub fn free(&self, ptr: Address) -> usize {
        if ptr.is_null() {
            return 0;
        }
        let lock = self.lock.lock();
        unsafe {
            //valgrind_freelike!(ptr.to_ptr::<u8>());
            let freed = mspace_usable_size(ptr.to_usize());
            mspace_free(self.mspace, ptr.to_mut_ptr());
            drop(lock);
            return freed;
        }
    }
    pub fn trim(&self) -> usize {
        let l = self.lock.lock();
        let mut reclaimed = 0;
        unsafe {
            mspace_trim(self.mspace, 0);
            mspace_inspect_all(
                self.mspace,
                dlmalloc_madvise_cb as usize,
                Address::from_ptr(&mut reclaimed),
            );
        }
        drop(l);
        reclaimed
    }

    pub fn freelist(&self, ptrs: &[Address]) -> usize {
        let mut freed = 0;
        for i in 0..ptrs.len() {
            let ptr = ptrs[i];

            //            freed += unsafe { mspace_usable_size(ptr.to_usize()) };
            //valgrind_freelike!(ptr.to_ptr::<u8>());
        }
        let lock = self.lock.lock();
        unsafe {
            mspace_bulk_free(self.mspace, Address::from_ptr(ptrs.as_ptr()), ptrs.len());
            drop(lock);
        }
        freed
    }

    pub fn more_core(&self, inc: isize) -> *mut u8 {
        let orig_end = self.c.end();
        if inc != 0 {
            let new_end = Address::from((orig_end.to_usize() as isize + inc) as usize);

            if inc > 0 {
                #[cfg(feature = "logging")]
                {
                    log::info!("MoreCore {}", formatted_size(inc as _));
                }
                unsafe {
                    assert!(
                        new_end <= self.c.limit(),
                        "new end {:p} is bigger than limit {:p}",
                        new_end.to_ptr::<u8>(),
                        self.c.limit().to_ptr::<u8>()
                    );
                    libc::mprotect(orig_end.to_mut_ptr(), inc as _, PROT_READ | PROT_WRITE);
                }
            } else {
                let size = -(inc as isize);
                #[cfg(feature = "logging")]
                {
                    log::info!("LessCore {}", formatted_size(size as _));
                }
                unsafe {
                    libc::madvise(new_end.to_mut_ptr(), size as _, libc::MADV_DONTNEED);
                    libc::mprotect(new_end.to_mut_ptr(), size as _, PROT_NONE);
                };
            }

            self.c.set_end(new_end);
        }
        orig_end.to_mut_ptr()
    }
}

#[no_mangle]
pub unsafe extern "C" fn waffle_heap_morecore(
    _msp: usize,
    increment: isize,
    rt: *mut crate::Heap,
) -> *mut u8 {
    let rt = &mut *rt;
    let heap = &mut *rt;
    heap.space().more_core(increment)
}

pub unsafe extern "C" fn dlmalloc_madvise_cb(
    start: Address,
    end: Address,
    used: usize,
    arg: Address,
) {
    if used != 0 {
        return;
    }
    let start = round_up(start.to_usize(), crate::utils::os::page_size());
    let end = round_up(end.to_usize(), crate::utils::os::page_size());
    if end > start {
        let length = end - start;
        let rc = libc::madvise(start as *mut _, length, libc::MADV_DONTNEED);
        if rc != 0 {
            panic!("madvise failed during heap trim");
        }
        let reclaimed = arg.to_mut_ptr::<usize>();
        *reclaimed += length;
    }
}

impl super::Allocator for DLMallocSpace {
    fn alloc_with_growth(&self, num_bytes: usize) -> Address {
        unsafe {
            let result;
            let max_allowed = self.c.growth_limit();
            mspace_set_footprint_limit(self.mspace, max_allowed);
            result = self.alloc_without_growth_locked(num_bytes);
            let footprint = mspace_footprint(self.mspace);
            mspace_set_footprint_limit(self.mspace, footprint);
            if result.is_non_null() {
                core::ptr::write_bytes(result.to_mut_ptr::<u8>(), 0, num_bytes);
            }
            result
        }
    }
    fn create(
        rt: &mut crate::Heap,
        name: &str,
        mut initial_size: usize,
        mut growth_limit: usize,
        mut cap: usize,
        can_move: bool,
    ) -> Self {
        let starting = crate::utils::os::page_size();

        let mem_map = ContinuousMemMapSpace::create_memmap(
            name,
            starting,
            &mut initial_size,
            &mut growth_limit,
            &mut cap,
        );

        let begin = mem_map.begin;
        let end = begin.offset(initial_size);
        let c = ContinuousMemMapSpace::new(
            name,
            mem_map,
            begin,
            end,
            begin.offset(cap),
            growth_limit,
            starting,
            initial_size,
        );
        Self {
            lock: Mutex::new(()),
            mspace: Self::create_mspace(
                rt,
                c.mem_map().begin(),
                c.starting_size(),
                c.initial_size(),
            ),
            c,
        }
    }
}
