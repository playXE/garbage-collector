use crate::utils::Ref;
use crate::utils::*;
use crate::*;
use alloc::sync::Arc;
use core::sync::atomic::Ordering;
use core::sync::atomic::{AtomicBool, AtomicU32, AtomicU64};
use parking_lot::Mutex;
use semaphore::Semaphore;
use std::cell::UnsafeCell;
pub struct Thread {}

/// Stack that holds all allocated objects.
pub struct AllocStack {
    pub head: Ref<GcBox<()>>,
    pub sweeps: Ref<GcBox<()>>,
}

impl AllocStack {
    pub const fn new() -> Self {
        Self {
            head: Ref::null(),
            sweeps: Ref::null(),
        }
    }

    pub fn push_unsync(&self, value: *const GcBox<()>) {
        let node = unsafe { &mut *(value as *mut GcBox<()>) };
        let current = self.head.as_atomic().load(Ordering::Relaxed);
        node.header.next = current;
        self.head.as_atomic().store(&mut *node, Ordering::Relaxed);
    }

    pub fn push(&self, value: *const GcBox<()>) {
        let node = unsafe { &mut *(value as *mut GcBox<()>) };
        let mut current = self.head.as_atomic().load(Ordering::Relaxed);
        loop {
            node.header.next = current;
            match self.head.as_atomic().compare_exchange_weak(
                current,
                &mut *node,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    return;
                }
                Err(e) => {
                    current = e;
                }
            }
        }
    }

    pub fn sweep(&self) -> (Self, usize) {
        let new = Self::new();
        let mut freed = 0;
        let mut current = self.head;

        while current.is_non_null() {
            let next = current.header.next;
            if current.header.is_marked() {
                //current.header.unmark();
                current.header.vtable = TaggedPointer::new(current.header.vtable.untagged());
                //current.header.set_old();
                current.header.next = 0 as *mut _;
                new.push(current.ptr);
            } else {
                current.header.next = 0 as *mut _;

                freed += current.trait_object().size() + core::mem::size_of::<GcHeader>();
                unsafe {
                    core::ptr::drop_in_place(current.trait_object());
                }
                //println!("free {:p}", current.ptr);
                MiHeap::free(current.ptr);
            }
            current = Ref::new(next);
        }
        (new, freed)
    }

    pub fn sweep_sticky(&self) -> (Self, usize) {
        let mut new = Self::new();

        let mut current = self.head;
        if current.is_null() {
            return (Self::new(), 0);
        }
        let mut freed = 0;
        while current.is_non_null() && current.ptr != self.sweeps.ptr {
            let next = current.header.next;
            if self.sweeps.ptr == next {
                break;
            }
            assert!(current.header.is_new());
            if current.header.is_sticky_marked() {
                current.header.vtable = TaggedPointer::new(current.header.vtable.untagged());
                current.header.next = 0 as *mut _;
                current.header.set_old();
                new.push(current.ptr);
            } else {
                current.header.next = 0 as *mut _;
                freed += current.trait_object().size() + core::mem::size_of::<GcHeader>();
                unsafe {
                    core::ptr::drop_in_place(current.trait_object());
                }
                //println!("sticky free {:p}", current.ptr);
                MiHeap::free(current.ptr);
            }

            current = Ref::new(next);
        }
        new.sweeps = new.head;
        (new, freed)
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct MiHeapC {
    __pad: [u8; 0],
}
const fn round_up(a: usize, b: usize) -> usize {
    return (1 + (a - 1) / b) * b;
}
#[repr(C)]
pub struct MiHeap(*mut MiHeapC);

extern "C" {
    fn mi_heap_new() -> *mut MiHeapC;
    fn mi_heap_delete(heap: *mut MiHeapC);
    fn mi_heap_collect(heap: *mut MiHeapC);
    fn mi_heap_zalloc(heap: *mut MiHeapC, size: usize) -> *mut u8;
    fn mi_free(p: *mut u8);
}

impl MiHeap {
    pub fn new() -> Self {
        Self(unsafe { mi_heap_new() })
    }

    pub fn malloc(&self, size: usize) -> *mut () {
        unsafe { mi_heap_zalloc(self.0, size).cast() }
    }

    pub fn collect(&self) {
        unsafe { mi_heap_collect(self.0) }
    }

    pub fn free<T>(p: *mut T) {
        unsafe { mi_free(p.cast()) }
    }
}

impl Drop for MiHeap {
    fn drop(&mut self) {
        unsafe {
            // we do not want to *destroy* heap because it still may contain live objects
            // so with mi_heap_delete we just migrate them to default heap.
            mi_heap_delete(self.0);
        }
    }
}
use parking_lot::lock_api::RawMutex;
pub(crate) static THREAD_STATE_CHANGE_LOCK: Mutex<()> =
    Mutex::const_new(parking_lot::RawMutex::INIT, ());

pub struct LocalAllocator {
    pub(crate) id: usize,
    pub(crate) gc_free_zone: AtomicBool,
    ready_for_collect: Semaphore,
    collect_done: Semaphore,
    rootlist: UnsafeCell<RootList>,
    mi_heap: MiHeap,

    set: remembered_set::RememberedSet,
}

fn get_vtable<T: GcObject>(x: &T) -> *mut () {
    unsafe { std::mem::transmute::<_, TraitObject>(x as &dyn GcObject).vtable }
}

pub struct VirtualHandle<T: GcObject + ?Sized> {
    gc_box: *mut GcBox<T>,
}

impl<T: GcObject + ?Sized> VirtualHandle<T> {
    pub unsafe fn write(self, val: T)
    where
        T: Sized,
    {
        self.gc_box
            .offset(core::mem::size_of::<GcHeader>() as _)
            .cast::<T>()
            .write(val);
    }

    pub unsafe fn value_ptr(self) -> *mut T
    where
        T: Sized,
    {
        self.gc_box
            .cast::<GcBox<()>>()
            .offset(core::mem::size_of::<GcHeader>() as _) as *mut T
    }

    pub fn to_handle(self) -> Handle<T>
    where
        T: Sized,
    {
        Handle {
            ptr: unsafe { std::ptr::NonNull::new_unchecked(self.gc_box) },
        }
    }
}

impl LocalAllocator {
    pub fn write_barrier<T: GcObject, U: GcObject>(
        &self,
        mut holder: Handle<T>,
        mut new_value: Handle<U>,
    ) {
        if get_heap().generational {
            unsafe {
                if holder.ptr.as_mut().header.is_old() && new_value.ptr.as_mut().header.is_new() {
                    self.set.remember(holder.ptr.as_ptr().cast());
                }
            }
        }
    }
    pub unsafe fn allocate_virtual<T: GcObject + Sized>(
        &self,
        vtable: *mut (),
        size: usize,
    ) -> VirtualHandle<T> {
        if PAUSE_FOR_COLLECT.load(Ordering::Relaxed) != 0 {
            pause_for_collect();
        }
        let size = size + core::mem::size_of::<GcHeader>();
        let alloc = get_heap().allocated.load(Ordering::Relaxed) + size;
        if alloc >= get_heap().threshold.load(Ordering::Relaxed) {
            collect_from_this_thread();
        }
        let allocation = Ref::new(self.mi_heap.malloc(size).cast::<GcBox<u8>>());
        allocation.cast::<GcBox<u8>>().write(GcBox {
            val: 0,
            header: GcHeader {
                vtable: TaggedPointer::new(vtable),
                next: 0 as *mut _,
            },
        });
        allocation.header.unmark();
        allocation.header.sticky_unmark();
        //allocation.header.set_new();
        get_heap().alloc_stack().push(allocation.ptr.cast());
        get_heap().allocated.fetch_add(size, Ordering::Relaxed);
        debug_assert!(get_heap().alloc_stack().head.is_non_null());

        VirtualHandle {
            gc_box: allocation.ptr.cast(),
        }
    }

    pub fn allocate<T: GcObject + Sized>(&self, value: T) -> Root<T> {
        if PAUSE_FOR_COLLECT.load(Ordering::Relaxed) != 0 {
            pause_for_collect();
        }
        let size = round_up(value.size() + core::mem::size_of::<GcHeader>(), 8);
        let alloc = get_heap().allocated.load(Ordering::Relaxed) + size;
        if alloc >= get_heap().threshold.load(Ordering::Relaxed) {
            collect_from_this_thread();
        }
        let vtable = get_vtable(&value);

        let allocation = Ref::new(self.mi_heap.malloc(size).cast::<GcBox<T>>());
        allocation.write(GcBox {
            val: value,
            header: GcHeader {
                vtable: TaggedPointer::new(vtable),
                next: 0 as *mut _,
            },
        });

        allocation.header.unmark();
        allocation.header.sticky_unmark();
        //allocation.header.set_new();
        get_heap().allocated.fetch_add(size, Ordering::Relaxed);
        get_heap().alloc_stack().push(allocation.ptr.cast());
        debug_assert!(get_heap().alloc_stack().head.is_non_null());

        self.roots().root(allocation)
    }

    pub fn roots(&self) -> &mut RootList {
        unsafe { &mut *self.rootlist.get() }
    }

    pub fn new(id: usize) -> Self {
        Self {
            id,
            rootlist: UnsafeCell::new(RootList::new()),
            gc_free_zone: AtomicBool::new(false),
            ready_for_collect: Semaphore::new(),
            collect_done: Semaphore::new(),
            mi_heap: MiHeap::new(),
            set: remembered_set::RememberedSet::new(),
        }
    }

    pub fn wait_for_safe(&self) {
        if !self.gc_free_zone.load(Ordering::Relaxed) {
            self.ready_for_collect.wait();
        }
    }

    pub fn release_from_safe(&self) {
        if !self.gc_free_zone.load(Ordering::Relaxed) {
            self.collect_done.set();
        }
    }

    pub fn pause_for_collect(&self) {
        self.ready_for_collect.set();
        self.collect_done.wait();
    }

    pub fn enter_gc_free_zone(&self) {
        self.gc_free_zone.store(true, Ordering::Relaxed);
        self.ready_for_collect.set();
    }

    pub fn exit_gc_free_zone(&self) {
        if !self.gc_free_zone.load(Ordering::Relaxed) {
            panic!("GC Free zone mismatch");
        }
        let l = THREAD_STATE_CHANGE_LOCK.lock();
        self.ready_for_collect.reset();
        self.gc_free_zone.store(false, Ordering::Relaxed);
        drop(l);
    }

    pub fn try_gc_free_zone(&self) -> bool {
        if self.gc_free_zone.load(Ordering::Relaxed) {
            return false;
        }
        self.enter_gc_free_zone();
        true
    }
    pub fn try_exit_gc_free_zone(&self) -> bool {
        if !self.gc_free_zone.load(Ordering::Relaxed) {
            return false;
        }
        self.exit_gc_free_zone();
        true
    }

    pub fn exit_gc_free_zone_locked(&self) {
        self.ready_for_collect.reset();
        self.gc_free_zone.store(false, Ordering::Relaxed);
    }
}

use core::cell::RefCell;
thread_local! {
    pub(crate) static TLA: RefCell<Option<Arc<LocalAllocator>>> = RefCell::new(None);
}

pub(crate) fn get_tla() -> Option<Arc<LocalAllocator>> {
    TLA.with(|x| x.borrow().clone())
}

pub fn enter_gc_free_zone() {
    if let Some(tla) = get_tla() {
        tla.enter_gc_free_zone();
    }
}

pub fn exit_gc_free_zone() {
    if let Some(tla) = get_tla() {
        tla.exit_gc_free_zone();
    }
}

pub fn try_gc_free_zone() -> bool {
    if let Some(tla) = get_tla() {
        tla.try_gc_free_zone()
    } else {
        false
    }
}

pub fn try_exit_gc_free_zone() -> bool {
    if let Some(tla) = get_tla() {
        tla.try_exit_gc_free_zone()
    } else {
        false
    }
}

pub(crate) fn pause_for_collect() {
    if let Some(tla) = get_tla() {
        tla.pause_for_collect();
    }
}

static PAUSE_FOR_COLLECT: AtomicU32 = AtomicU32::new(0x00000000);
const LOCAL_POOL_SIZE: usize = 2;
pub struct GlobalAllocator {
    pub local_id: AtomicUsize,
    pub total_freed_ms: AtomicUsize,
    pub total_freed_sticky: AtomicUsize,
    pub total_ms_time: AtomicU64,
    pub total_sticky_time: AtomicU64,
    pub ms_iters: AtomicUsize,
    pub next_sticky: AtomicBool,
    stacks: [ProtectedBy<'static, AllocStack>; 2],
    stack: AtomicBool,
    print_timings: AtomicBool,
    threshold: AtomicUsize,
    allocated: AtomicUsize,
    generational: bool,
    global_rootlist: Mutex<RootList>,
    local_allocs: ProtectedBy<'static, Vec<Arc<LocalAllocator>>>,
    local_pool: ProtectedBy<'static, [Option<Arc<LocalAllocator>>; 2]>,
    mark_stack: ProtectedBy<'static, std::collections::VecDeque<Ref<GcBox<()>>>>,
}

impl GlobalAllocator {
    pub fn new() -> Self {
        Self {
            local_id: AtomicUsize::new(0),
            total_freed_ms: AtomicUsize::new(0),
            total_ms_time: AtomicU64::new(0),
            total_freed_sticky: AtomicUsize::new(0),
            total_sticky_time: AtomicU64::new(0),
            ms_iters: AtomicUsize::new(0),
            next_sticky: AtomicBool::new(false),
            threshold: AtomicUsize::new(16 * 1024),
            allocated: AtomicUsize::new(0),
            stacks: [
                ProtectedBy::new(&THREAD_STATE_CHANGE_LOCK, AllocStack::new()),
                ProtectedBy::new(&THREAD_STATE_CHANGE_LOCK, AllocStack::new()),
            ],
            stack: AtomicBool::new(false),
            print_timings: AtomicBool::new(false),
            global_rootlist: Mutex::new(RootList::new()),
            generational: false,
            local_allocs: ProtectedBy::new(&THREAD_STATE_CHANGE_LOCK, Default::default()),
            local_pool: ProtectedBy::new(&THREAD_STATE_CHANGE_LOCK, Default::default()),
            mark_stack: ProtectedBy::new(&THREAD_STATE_CHANGE_LOCK, Default::default()),
        }
    }
    pub fn get_ms_estimated_meant_throughput(&self) -> u64 {
        return (self.total_freed_ms.load(Ordering::Relaxed) as u64 * 1000)
            / ((self.total_ms_time.load(Ordering::Relaxed) / 1000 / 1000) + 1);
    }

    pub fn get_sticky_estimated_meant_throughput(&self) -> u64 {
        return (self.total_freed_sticky.load(Ordering::Relaxed) as u64 * 1000)
            / ((self.total_sticky_time.load(Ordering::Relaxed) / 1000 / 1000) + 1);
    }
    pub(crate) fn add_local(&self, alloc: Arc<LocalAllocator>) {
        let (lock, vec) = self.local_allocs.get();
        vec.push(alloc.clone());
        drop(lock);
    }
    pub(crate) fn return_to_pool_locked(&self, alloc: Arc<LocalAllocator>) -> bool {
        let pool = unsafe { self.local_pool.get_locked() };
        for i in 0..LOCAL_POOL_SIZE {
            if pool[i].is_none() {
                pool[i] = Some(alloc);
                return true;
            }
        }
        false
    }

    pub(crate) fn get_pooled_allocator(&self) -> Option<Arc<LocalAllocator>> {
        let (l, pool) = self.local_pool.get();
        for i in 0..LOCAL_POOL_SIZE {
            if let Some(alloc) = pool[i].take() {
                return Some(alloc);
            }
        }
        drop(l);
        None
    }

    pub(crate) fn remove_local_locked(&self, alloc: &LocalAllocator) {
        unsafe {
            self.local_allocs.get_locked().retain(|x| alloc.id != x.id);
        }
    }

    pub(crate) fn swap_stacks(&self) {
        self.stack.fetch_xor(true, Ordering::Relaxed);
    }

    pub(crate) fn alloc_stack(&self) -> &mut AllocStack {
        unsafe {
            if self.stack.load(Ordering::Relaxed) {
                self.stacks[1].get_locked()
            } else {
                self.stacks[0].get_locked()
            }
        }
    }
    pub(crate) fn live_stack(&self) -> &mut AllocStack {
        unsafe {
            if self.stack.load(Ordering::Relaxed) {
                self.stacks[0].get_locked()
            } else {
                self.stacks[1].get_locked()
            }
        }
    }

    /*pub(crate) fn alloc_stack(&self) -> &mut AllocStack {
        unsafe { self.alloc_stack.get_locked() }
    }*/
    pub fn collect(&self, mut in_locked: Option<parking_lot::MutexGuard<'_, ()>>) {
        // If we set the flag from 0 -> 0xffffffff then we are the collector
        // otherwise, someone else is collecting at the moment - so wait...
        if PAUSE_FOR_COLLECT
            .compare_exchange(0, 0xffffffff, Ordering::AcqRel, Ordering::Relaxed)
            .is_err()
        {
            if let Some(l) = in_locked {
                drop(l);
                pause_for_collect();
                enter_gc_free_zone();
                in_locked = Some(THREAD_STATE_CHANGE_LOCK.lock());
                exit_gc_free_zone();
            } else {
                pause_for_collect();
            }
        }
        if in_locked.is_none() {
            in_locked = Some(THREAD_STATE_CHANGE_LOCK.lock());
        }
        unsafe {
            let this_local = get_tla().unwrap();
            let locals = self.local_allocs.get_locked();
            for i in 0..locals.len() {
                if Arc::ptr_eq(&locals[i], &this_local) == false {
                    locals[i].wait_for_safe();
                }
            }

            let cur_sticky = self.next_sticky.load(Ordering::Relaxed) && self.generational;

            let (freed, duration) = if !cur_sticky {
                let start = std::time::Instant::now();
                self.mark();
                let mut freed = 0;
                if self.generational {
                    let new_stack = AllocStack::new();
                    let old_gen = self.live_stack();

                    let mut head = old_gen.head;
                    while head.is_non_null() {
                        let next = Ref::new(head.header.next);
                        if head.header.is_marked() {
                            head.header.unmark();
                            head.header.next = 0 as *mut _;
                            new_stack.push_unsync(head.ptr);
                        } else {
                            freed += head.trait_object().size() + core::mem::size_of::<GcHeader>();

                            core::ptr::drop_in_place(head.trait_object());

                            //println!("free {:p}", head.ptr);
                            MiHeap::free(head.ptr);
                        }
                        head = next;
                    }

                    let new_gen = self.alloc_stack();
                    let mut head = new_gen.head;
                    while head.is_non_null() {
                        let next = Ref::new(head.header.next);
                        if head.header.is_marked() {
                            head.header.unmark();
                            head.header.next = 0 as *mut _;
                            new_stack.push_unsync(head.ptr);
                        } else {
                            freed += head.trait_object().size() + core::mem::size_of::<GcHeader>();

                            core::ptr::drop_in_place(head.trait_object());

                            //println!("not promoted free {:p}", head.ptr);
                            MiHeap::free(head.ptr);
                        }
                        head = next;
                    }
                    *old_gen = new_stack;
                    *new_gen = AllocStack::new();
                } else {
                    let new_stack = AllocStack::new();
                    let stack = self.alloc_stack();
                    let mut head = stack.head;

                    while head.is_non_null() {
                        let next = head.header.next;
                        if head.header.is_marked() {
                            head.header.unmark();
                            head.header.next = 0 as *mut _;
                            new_stack.push_unsync(head.ptr);
                        } else {
                            freed += head.trait_object().size() + core::mem::size_of::<GcHeader>();

                            core::ptr::drop_in_place(head.trait_object());

                            //println!("not promoted free {:p}", head.ptr);
                            MiHeap::free(head.ptr);
                        }
                        head = Ref::new(next);
                    }

                    *stack = new_stack;
                }
                let end = start.elapsed();
                (freed, end.as_nanos() as u64)
            } else {
                let start = std::time::Instant::now();
                self.mark_sticky();
                let mut freed = 0;
                //let (stack, freed) = self.alloc_stack().sweep_sticky();
                let old_gen = self.live_stack();
                let new_gen = self.alloc_stack();
                let mut head = new_gen.head;
                while head.is_non_null() {
                    let next = Ref::new(head.header.next);
                    if head.header.is_sticky_marked() {
                        head.header.sticky_unmark();
                        head.header.next = 0 as *mut _;
                        old_gen.push_unsync(head.ptr);
                    } else {
                        freed += head.trait_object().size() + core::mem::size_of::<GcHeader>();

                        core::ptr::drop_in_place(head.trait_object());

                        //println!("new free {:p}", head.ptr);
                        MiHeap::free(head.ptr);
                    }
                    head = next;
                }
                *new_gen = AllocStack::new();
                let end = start.elapsed();
                (freed, end.as_nanos() as u64)
            };
            const STICKY_GC_THROUGHPUT_ADJ: f64 = 1.0;
            #[inline(always)]
            const fn estimated_throughput(freed: usize, duration: u64) -> u64 {
                (freed as u64 * 1000) / ((duration / 1000 / 1000) + 1)
            }
            if cur_sticky {
                self.total_freed_sticky.fetch_add(freed, Ordering::Relaxed);
                self.total_sticky_time
                    .fetch_add(duration, Ordering::Relaxed);
                if self.print_timings.load(Ordering::Relaxed) {
                    eprintln!(
                        "Minor GC cycle finished:\n Freed {} bytes in {}ns\n GC estimated throughput={}",
                        freed,
                        duration,
                        estimated_throughput(freed, duration)
                    );
                }
            } else {
                self.ms_iters.fetch_add(1, Ordering::Relaxed);
                self.total_freed_ms.fetch_add(freed, Ordering::Relaxed);
                self.total_ms_time.fetch_add(duration, Ordering::Relaxed);
                if self.print_timings.load(Ordering::Relaxed) {
                    eprintln!(
                        "Major GC cycle finished:\n Freed {} bytes in {}ns\n GC estimated throughput={}",
                        freed,
                        duration,
                        estimated_throughput(freed, duration)
                    );
                }
            }
            self.allocated.fetch_sub(freed, Ordering::Relaxed);
            if self.allocated.load(Ordering::Relaxed) >= self.threshold.load(Ordering::Relaxed) {
                self.threshold.store(
                    (self.allocated.load(Ordering::Relaxed) as f64 / 0.75) as usize,
                    Ordering::Relaxed,
                );
            }

            if !cur_sticky {
                self.next_sticky.store(true, Ordering::Relaxed);
            } else {
                let estimated_throughput = estimated_throughput(freed, duration) as f64;
                if estimated_throughput * STICKY_GC_THROUGHPUT_ADJ
                    >= self.get_ms_estimated_meant_throughput() as f64
                    && self.ms_iters.load(Ordering::Relaxed) > 0
                {
                    self.next_sticky.store(true, Ordering::Relaxed);
                } else {
                    self.next_sticky.store(false, Ordering::Relaxed);
                }
            }

            PAUSE_FOR_COLLECT.store(0, Ordering::Relaxed);
            for i in 0..locals.len() {
                if Arc::ptr_eq(&locals[i], &this_local) == false {
                    locals[i].release_from_safe();
                }
            }
        }

        drop(in_locked);
    }

    fn mark(&self) {
        // clear remembered sets since we're running full GC cycle now.
        self.process_remembred_sets(false);
        self.mark_roots();
        self.process_gray_stack();
    }

    fn mark_sticky(&self) {
        self.mark_sticky_roots();
        // All reachable objects must be referenced by a root or a dirty card, so we can clear the mark
        // stack here since all objects in the mark stack will get scanned by the card scanning anyways.
        self.mark_stack_locked().clear();
        self.process_remembred_sets(true);
        self.process_sticky_gray_stack();
    }
    fn process_gray_stack(&self) {
        let stack = self.mark_stack_locked();
        while let Some(item) = stack.pop_front() {
            self.scan_object(item);
        }
    }
    fn process_sticky_gray_stack(&self) {
        let stack = self.mark_stack_locked();
        while let Some(item) = stack.pop_front() {
            self.scan_sticky_object(item);
        }
    }

    fn mark_roots(&self) {
        let mut groots = self.global_rootlist.lock();

        groots.walk(&mut |root| {
            self.mark_object(root.obj);
        });

        let locals = unsafe { self.local_allocs.get_locked() };
        for local in locals.iter() {
            local.roots().walk(&mut |root| {
                //println!("root {:p} for {:p}", root.ptr, root.obj.ptr);
                self.mark_object(root.obj);
            })
        }
    }

    fn mark_sticky_roots(&self) {
        let mut groots = self.global_rootlist.lock();

        groots.walk(&mut |root| {
            self.mark_sticky_object(root.obj);
        });

        let locals = unsafe { self.local_allocs.get_locked() };
        for local in locals.iter() {
            local.roots().walk(&mut |root| {
                self.mark_sticky_object(root.obj);
            })
        }
    }

    fn process_remembred_sets(&self, sticky: bool) {
        let locals = unsafe { self.local_allocs.get_locked() };
        if sticky {
            for local in locals.iter() {
                local.set.iter().for_each(|x| {
                    self.mark_sticky_object(Ref::new(*x));
                })
            }
        } else {
            for local in locals.iter() {
                local.set.prune();
            }
        }
    }
    fn mark_stack_locked(&self) -> &mut std::collections::VecDeque<Ref<GcBox<()>>> {
        unsafe { self.mark_stack.get_locked() }
    }
    fn scan_object(&self, obj: Ref<GcBox<()>>) {
        if obj.is_non_null() {
            // println!("{:p}", obj.header.vtable.raw);
            // println!("obj {:p}", obj.ptr);
            obj.trait_object().visit_references(&mut |object| {
                self.mark_object(Ref::new(object));
            });
        }
    }
    fn scan_sticky_object(&self, obj: Ref<GcBox<()>>) {
        if obj.is_non_null() {
            obj.trait_object().visit_references(&mut |object| {
                self.mark_sticky_object(Ref::new(object));
            });
        }
    }
    fn mark_object(&self, obj: Ref<GcBox<()>>) {
        if obj.is_non_null() {
            self.mark_object_non_null(obj);
        }
    }

    fn mark_object_non_null(&self, obj: Ref<GcBox<()>>) {
        if !obj.header.is_marked() {
            obj.header.sync_mark();

            self.mark_stack_locked().push_back(obj);
        }
    }

    fn mark_sticky_object(&self, obj: Ref<GcBox<()>>) {
        if obj.is_non_null() {
            self.mark_sticky_object_non_null(obj);
        }
    }

    fn mark_sticky_object_non_null(&self, obj: Ref<GcBox<()>>) {
        if !obj.header.is_sticky_marked() {
            obj.header.sync_sticky_mark();
            self.mark_stack_locked().push_back(obj);
        }
    }
}

pub fn collect_from_this_thread() {
    get_heap().collect(None);
}

unsafe impl Sync for GlobalAllocator {}
unsafe impl Send for GlobalAllocator {}

use once_cell::sync::Lazy;
static HEAP: Lazy<GlobalAllocator> = Lazy::new(|| GlobalAllocator::new());

pub(crate) fn get_heap() -> &'static GlobalAllocator {
    &*HEAP
}

pub fn gc_print_timings(x: bool) {
    get_heap().print_timings.store(x, Ordering::Relaxed);
}
macro_rules! simple_gc_ty {
    ($($t: ty)*) => {
        $(
            impl GcObject for $t {}
        )*
    };
}

simple_gc_ty!(
    bool
    i8 u8
    i16 u16
    i32 u32
    i64 u64
    i128 u128
    f32 f64

);
impl<T: GcObject> Copy for VirtualHandle<T> {}
impl<T: GcObject> Clone for VirtualHandle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: GcObject> GcObject for VirtualHandle<T> {
    fn visit_references(&self, visit: &mut dyn FnMut(*const GcBox<()>)) {
        visit(self.gc_box.cast());
    }
}
