use crate::utils::Ref;
use crate::*;
use core::sync::atomic::Ordering;
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

    pub fn sweep(&self) -> Self {
        let new = Self::new();

        let mut current = self.sweeps;
        if current.is_null() {
            current = self.head;
            if current.is_null() {
                return Self::new();
            }
        }
        while current.is_non_null() {
            let next = current.header.next;
            if current.header.is_marked() {
                current.header.unmark();
                new.push(current.ptr);
            } else {
                unsafe {
                    core::ptr::drop_in_place(current.trait_object());
                }
                MiHeap::free(current.ptr);
            }
            current = Ref::new(next);
        }
        new
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct MiHeapC {
    __pad: [u8; 0],
}

#[repr(C)]
pub struct MiHeap(*mut MiHeapC);

extern "C" {
    fn mi_heap_new() -> *mut MiHeapC;
    fn mi_heap_delete(heap: *mut MiHeapC);
    fn mi_heap_collect(heap: *mut MiHeapC);
    fn mi_heap_malloc(heap: *mut MiHeapC, size: usize) -> *mut ();
    fn mi_free(p: *mut u8);
}

impl MiHeap {
    pub fn new() -> Self {
        Self(unsafe { mi_heap_new() })
    }

    pub fn malloc(&self, size: usize) -> *mut () {
        unsafe { mi_heap_malloc(self.0, size) }
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
static THREAD_STATE_CHANGE_LOCK: Mutex<()> = Mutex::const_new(parking_lot::RawMutex::INIT, ());

use crate::utils::*;
use alloc::sync::Arc;
use core::sync::atomic::{AtomicBool, AtomicU32};
use parking_lot::Mutex;
use semaphore::Semaphore;
pub struct LocalAllocator {
    gc_free_zone: AtomicBool,
    ready_for_collect: Semaphore,
    collect_done: Semaphore,
    mi_heap: MiHeap,
    set: remembered_set::RememberedSet,
}

impl LocalAllocator {
    pub fn new() -> Self {
        Self {
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
    static TLA: RefCell<Option<Arc<LocalAllocator>>> = RefCell::new(None);
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
    stacks: [AllocStack; 2],
    stack: AtomicBool,
    generational: bool,
    local_allocs: ProtectedBy<'static, Vec<Arc<LocalAllocator>>>,
    local_pool: ProtectedBy<'static, [Option<Arc<LocalAllocator>>; 2]>,
    mark_stack: ProtectedBy<'static, std::collections::VecDeque<Ref<GcBox<()>>>>,
}

impl GlobalAllocator {
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

    pub(crate) fn remove_local_locked(&self, alloc: &Arc<LocalAllocator>) {
        unsafe {
            self.local_allocs
                .get_locked()
                .retain(|x| !Arc::ptr_eq(alloc, x));
        }
    }

    pub(crate) fn swap_stacks(&self) {
        self.stack.fetch_xor(true, Ordering::Relaxed);
    }

    pub(crate) fn alloc_stack(&self) -> &AllocStack {
        if self.stack.load(Ordering::Relaxed) {
            &self.stacks[1]
        } else {
            &self.stacks[0]
        }
    }
    pub(crate) fn live_stack(&self) -> &AllocStack {
        if self.stack.load(Ordering::Relaxed) {
            &self.stacks[0]
        } else {
            &self.stacks[1]
        }
    }

    fn collect(&self, major: bool, mut in_locked: Option<parking_lot::MutexGuard<'_, ()>>) {
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
                locals[i].wait_for_safe();
            }
            /* TODO: GC cycle there */
            let mut stack = self.sweep();
            let next_gc_is_minor = major;
            // in minor GC we sweep all objects allocated after *last* GC cycle.
            if next_gc_is_minor {
                stack.sweeps = stack.head;
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

    fn sweep(&self) -> AllocStack {
        self.alloc_stack().sweep()
    }

    fn mark(&self) {
        // clear remembered sets since we're running full GC cycle now.
        self.process_remembred_sets(false);
        self.mark_roots();
        self.process_gray_stack();
    }

    fn mark_sticky(&self) {
        self.mark_roots();
        // All reachable objects must be referenced by a root or a dirty card, so we can clear the mark
        // stack here since all objects in the mark stack will get scanned by the card scanning anyways.
        self.mark_stack_locked().clear();
        self.process_remembred_sets(true);
        self.process_gray_stack();
    }
    fn process_gray_stack(&self) {
        let stack = self.mark_stack_locked();
        while let Some(item) = stack.pop_front() {
            self.scan_object(item);
        }
    }
    fn mark_roots(&self) {
        /* TBD */
    }

    fn process_remembred_sets(&self, sticky: bool) {
        let locals = unsafe { self.local_allocs.get_locked() };
        if sticky {
            for local in locals.iter() {
                local.set.iter().for_each(|x| {
                    self.mark_object(Ref::new(*x));
                })
            }
        } else {
            for local in locals.iter() {
                local.set.prune();
            }
        }
    }
    fn scan_object(&self, obj: Ref<GcBox<()>>) {}
    fn mark_object(&self, obj: Ref<GcBox<()>>) {}

    fn mark_stack_locked(&self) -> &mut std::collections::VecDeque<Ref<GcBox<()>>> {
        unsafe { self.mark_stack.get_locked() }
    }
    fn mark_object_non_null(&self, obj: Ref<GcBox<()>>) {
        if !obj.header.is_marked() {
            self.mark_stack_locked().push_back(obj);
        }
    }
}

unsafe impl Sync for GlobalAllocator {}
unsafe impl Send for GlobalAllocator {}

use once_cell::sync::Lazy;
static HEAP: Lazy<GlobalAllocator> = Lazy::new(|| unimplemented!());

pub(crate) fn get_heap() -> &'static GlobalAllocator {
    &*HEAP
}
