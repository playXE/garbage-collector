#[macro_use]
pub mod utils;
pub(crate) mod accounting;
pub(crate) mod collectors;
pub mod locks;
pub(crate) mod mem_map;
pub(crate) mod spaces;
use spaces::*;
pub trait GcObject {
    const VTABLE: VTable;
}

pub struct VTable {
    pub visit_fn:
        Option<unsafe fn(this: utils::Ref<GcBox<()>>, visit: &mut dyn FnMut(*const GcBox<()>))>,
    pub finalize_fn: Option<unsafe fn(this: utils::Ref<GcBox<()>>)>,

    pub instance_size: usize,
    pub determine_size: Option<unsafe fn(this: utils::Ref<GcBox<()>>) -> usize>,
}

pub struct GCHeader {
    /// Static reference to vtable of this object.
    vtable: &'static VTable,

    #[cfg(feature = "incremental")]
    next: utils::Ref<GcBox<()>>,
}

impl utils::Ref<GcBox<()>> {
    pub fn size(self) -> usize {
        if self.header.vtable.instance_size != 0 {
            return self.header.vtable.instance_size + core::mem::size_of::<GCHeader>();
        } else {
            if let Some(f) = self.header.vtable.determine_size {
                unsafe { f(self) + core::mem::size_of::<GCHeader>() }
            } else {
                core::mem::size_of::<GCHeader>()
            }
        }
    }
}

use accounting::stack::*;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
#[repr(C)]
pub struct Heap<'a> {
    space: Option<spaces::dlmalloc_space::DLMallocSpace>,

    mutator_running: AtomicBool,

    mutator_should_stop: AtomicBool,
    total_freed_ms: AtomicUsize,
    total_freed_sticky: AtomicUsize,
    total_ms_time: AtomicU64,
    total_sticky_time: AtomicU64,
    ms_iters: AtomicUsize,
    allocated: AtomicUsize,
    threshold: AtomicUsize,
    rootlist: RootList<'a>,
    alloc_stack: [ObjectStack; 2],
    stack: AtomicBool,
    print_timings: bool,
    card_table: Option<accounting::card_table::CardTable>,

    gc: Box<dyn collectors::GarbageCollector<'a>>,
}

use utils::*;

struct RootInner<'a> {
    next: Ref<RootInner<'a>>,
    prev: Ref<RootInner<'a>>,
    list: Ref<RootList<'a>>,
    pub obj: Ref<GcBox<()>>,
}

pub struct Root<'a, T: GcObject> {
    inner: Ref<RootInner<'a>>,
    _marker: std::marker::PhantomData<T>,
}

pub struct RootList<'a> {
    roots: Ref<RootInner<'a>>,
    lock: locks::Mutex<()>,
    _marker: core::marker::PhantomData<&'a Root<'a, ()>>,
}

impl GcObject for () {
    const VTABLE: VTable = VTable {
        visit_fn: None,
        finalize_fn: None,
        determine_size: None,
        instance_size: 0,
    };
}

impl<'a> RootList<'a> {
    pub fn new() -> Self {
        Self {
            roots: Ref::null(),
            lock: locks::Mutex::new(()),
            _marker: Default::default(),
        }
    }
    pub fn root<T: GcObject>(&mut self, o: Ref<GcBox<T>>) -> Root<'a, T> {
        let mut r = Ref::new(Box::into_raw(Box::new(RootInner {
            next: Ref::null(),
            prev: Ref::null(),
            list: Ref::new(self),
            obj: o.cast(),
        })));
        r.next = self.roots;
        if self.roots.is_non_null() {
            self.roots.prev = r;
        }
        self.roots = r;

        Root {
            inner: r,
            _marker: Default::default(),
        }
    }
    pub fn unroot<T: GcObject>(&mut self, r: Root<'a, T>) {
        drop(r)
    }

    pub(crate) fn walk(&self, walk: &mut dyn FnMut(Ref<RootInner<'a>>)) {
        let mut cur = self.roots;
        while cur.is_non_null() {
            walk(cur);
            cur = cur.next;
        }
    }
}
impl<'a, T: GcObject> Clone for Root<'a, T> {
    fn clone(&self) -> Self {
        let lock = self.inner.list.lock.lock();
        let root_inner = Ref::new(Box::into_raw(Box::new(RootInner {
            next: self.inner.next,
            prev: self.inner,
            obj: self.inner.obj,
            list: self.inner.list,
        })));
        let mut inner = self.inner;
        if inner.next.is_non_null() {
            inner.next.prev = root_inner;
        }
        inner.next = root_inner;
        drop(lock);
        Self {
            inner: root_inner,
            _marker: Default::default(),
        }
    }
}

impl<'a, T: GcObject> Drop for Root<'a, T> {
    fn drop(&mut self) {
        let inner = self.inner;
        let lock = inner.list.lock.lock();
        let mut inner = inner;
        if inner.prev.is_non_null() {
            inner.prev.next = inner.next;
        }
        if inner.next.is_non_null() {
            inner.next.prev = inner.prev;
        }
        if inner.list.roots.ptr == inner.ptr {
            inner.list.roots = inner.next;
        }
        drop(lock);
        unsafe {
            let _ = Box::from_raw(inner.ptr);
        }
    }
}
impl<'a> Heap<'a> {
    pub fn new(max_mem: usize, use_gen_gc: bool, print_timings: bool) -> Box<Self> {
        let mut this = Box::new(Self {
            stack: AtomicBool::new(false),
            mutator_running: AtomicBool::new(true),
            mutator_should_stop: AtomicBool::new(false),
            space: None,
            print_timings,
            alloc_stack: [ObjectStack::new(), ObjectStack::new()],
            total_freed_ms: AtomicUsize::new(0),
            total_freed_sticky: AtomicUsize::new(0),
            total_ms_time: AtomicU64::new(0),
            total_sticky_time: AtomicU64::new(0),
            allocated: AtomicUsize::new(0),
            threshold: AtomicUsize::new(16 * 1024),
            card_table: None,
            gc: Box::new(collectors::DummyGc {}),
            ms_iters: AtomicUsize::new(0),
            rootlist: RootList::new(),
        });
        this.gc = Box::new(collectors::mark_sweep::MarkAndSweep {
            heap: Ref::new(&*this).cast(),
            mark_stack: ObjectStack::new(),
            dur: 0,
            freed: 0,
            current_space_bitmap: None,
        });
        let space = spaces::dlmalloc_space::DLMallocSpace::create(
            &mut this,
            "alloc space",
            4096,
            max_mem,
            max_mem,
            false,
        );

        this.space = Some(space);
        this
    }

    pub fn allocate<T: GcObject + 'static>(&mut self, val: T) -> Root<'a, T> {
        let mem = self
            .space()
            .alloc_with_growth(core::mem::size_of::<GcBox<T>>());
        unsafe {
            if mem.is_non_null() {
                mem.to_mut_ptr::<GcBox<T>>().write(GcBox {
                    header: GCHeader {
                        #[cfg(feature = "incremental")]
                        next: Ref::null(),
                        vtable: &T::VTABLE,
                    },
                    val,
                });
                self.alloc_stack().push(Ref::new(mem.to_ptr()));

                return self.rootlist.root(Ref::new(mem.to_ptr::<GcBox<T>>()));
            }

            self.gc();
            let mem = self
                .space()
                .alloc_with_growth(core::mem::size_of::<GcBox<T>>());
            if mem.is_non_null() {
                self.alloc_stack().push(Ref::new(mem.to_ptr()));
                mem.to_mut_ptr::<GcBox<T>>().write(GcBox {
                    header: GCHeader {
                        #[cfg(feature = "incremental")]
                        next: Ref::null(),
                        vtable: &T::VTABLE,
                    },
                    val,
                });

                return self.rootlist.root(Ref::new(mem.to_ptr::<GcBox<T>>()));
            }
            panic!("Out of memory");
        }
    }

    pub fn gc(&mut self) {
        self.gc.run_phases();
        self.space().trim();
        if self.allocated.load(Ordering::Relaxed) >= self.threshold.load(Ordering::Relaxed) {
            self.threshold.store(
                (self.allocated.load(Ordering::Relaxed) as f64 / 0.75) as usize,
                Ordering::Relaxed,
            );
        }
    }

    pub(crate) fn space(&self) -> &dlmalloc_space::DLMallocSpace {
        match self.space {
            Some(ref s) => s,
            None => unsafe { std::hint::unreachable_unchecked() },
        }
    }
    pub fn alloc_stack(&self) -> &ObjectStack {
        if self.stack.load(Ordering::Relaxed) {
            &self.alloc_stack[1]
        } else {
            &self.alloc_stack[0]
        }
    }
    pub fn live_stack(&self) -> &ObjectStack {
        if self.stack.load(Ordering::Relaxed) {
            &self.alloc_stack[0]
        } else {
            &self.alloc_stack[1]
        }
    }
    pub fn mark_alloc_stack_as_live(
        &self,
        stack: &ObjectStack,
        bitmap: &accounting::space_bitmap::SpaceBitMap,
    ) {
        let mut item = stack.pop();
        while let Some(obj) = item {
            if bitmap.has_addr(utils::Address::from_ptr(obj.ptr)) {
                bitmap.set(utils::Address::from_ptr(obj.ptr));
            }
            item = stack.pop();
        }
        //debug_assert!(stack.is_empty());
    }
    pub(crate) fn swap_stacks(&self) {
        self.stack.fetch_xor(true, Ordering::Relaxed);
    }
}

#[repr(C)]
pub struct GcBox<T: GcObject> {
    header: GCHeader,
    val: T,
}

pub struct Handle<T: GcObject> {
    ptr: std::ptr::NonNull<GcBox<T>>,
}

impl<T: GcObject> Handle<T> {
    pub fn gc_ptr(&self) -> *const GcBox<()> {
        self.ptr.cast::<_>().as_ptr()
    }

    pub unsafe fn from_raw<U>(x: *const U) -> Self {
        Self {
            ptr: std::ptr::NonNull::new((x as *mut U).cast()).unwrap(),
        }
    }
}

impl<'a, T: GcObject> Root<'a, T> {
    pub fn to_heap(self) -> Handle<T> {
        Handle {
            ptr: unsafe {
                std::ptr::NonNull::new_unchecked(self.inner.obj.cast::<GcBox<T>>().ptr as *mut _)
            },
        }
    }
}

impl<T: GcObject> std::ops::Deref for Handle<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &(&*self.ptr.as_ptr()).val }
    }
}

impl<T: GcObject> std::ops::DerefMut for Handle<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (&mut *self.ptr.as_ptr()).val }
    }
}

impl<T: GcObject> Copy for Handle<T> {}
impl<T: GcObject> Clone for Handle<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'a, T: GcObject> std::ops::Deref for Root<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &(&*self.inner.obj.cast::<GcBox<T>>().ptr).val }
    }
}

impl<'a, T: GcObject> std::ops::DerefMut for Root<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (&mut *self.inner.obj.cast::<GcBox<T>>().ptr).val }
    }
}
