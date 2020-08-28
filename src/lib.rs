#![allow(dead_code)]
#![allow(unused_variables)]
#[macro_use]
pub mod utils;
pub(crate) mod accounting;
pub(crate) mod collectors;
pub mod locks;
pub(crate) mod mem_map;
pub(crate) mod spaces;
pub(crate) mod types;
use spaces::*;
/*pub trait GcObj {
    const VTABLE: VTable;
}

pub struct VTable {
    pub visit_fn:
        Option<unsafe fn(this: utils::Ref<GcBox<()>>, visit: &mut dyn FnMut(*const GcBox<()>))>,
    pub finalize_fn: Option<unsafe fn(this: utils::Ref<GcBox<()>>)>,

    pub instance_size: usize,
    pub determine_size: Option<unsafe fn(this: utils::Ref<GcBox<()>>) -> usize>,
}
*/
pub struct GCHeader {
    /// Static reference to vtable of this object.
    vtable: *mut (),

    #[cfg(feature = "incremental")]
    next: utils::Ref<GcBox<()>>,
}

impl<T: GcObj> Ref<GcBox<T>> {
    pub fn size(self) -> usize {
        std::mem::size_of_val(self.trait_object()) + std::mem::size_of::<GCHeader>()
    }
    pub fn trait_object<'a>(self) -> &'a mut dyn GcObj {
        unsafe {
            std::mem::transmute(TraitObject {
                vtable: self.header.vtable,
                data: &self.val as *const _ as *mut _,
            })
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

pub struct Root<'a, T: GcObj> {
    inner: Ref<RootInner<'a>>,
    _marker: std::marker::PhantomData<T>,
}

pub struct RootList<'a> {
    roots: Ref<RootInner<'a>>,
    lock: locks::Mutex<()>,
    _marker: core::marker::PhantomData<&'a Root<'a, ()>>,
}
/*
impl GcObj for () {
    const VTABLE: VTable = VTable {
        visit_fn: None,
        finalize_fn: None,
        determine_size: None,
        instance_size: 0,
    };
}*/

impl<'a> RootList<'a> {
    pub fn new() -> Self {
        Self {
            roots: Ref::null(),
            lock: locks::Mutex::new(()),
            _marker: Default::default(),
        }
    }
    pub fn root<T: GcObj>(&mut self, o: Ref<GcBox<T>>) -> Root<'a, T> {
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
    pub fn unroot<T: GcObj>(&mut self, r: Root<'a, T>) {
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

impl<'a, T: GcObj> Clone for Root<'a, T> {
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

impl<'a, T: GcObj> Drop for Root<'a, T> {
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
    pub fn new(
        max_mem: usize,
        use_gen_gc: bool,
        print_timings: bool,
        threshold: usize,
    ) -> Box<Self> {
        let mut this = Box::new(Self {
            stack: AtomicBool::new(false),
            mutator_running: AtomicBool::new(true),
            mutator_should_stop: AtomicBool::new(false),
            space: None,
            print_timings,
            alloc_stack: [ObjectStack::new(), ObjectStack::new()],

            allocated: AtomicUsize::new(0),
            threshold: AtomicUsize::new(threshold),
            card_table: None,
            gc: Box::new(collectors::DummyGc {}),

            rootlist: RootList::new(),
        });
        if use_gen_gc == false {
            this.gc = Box::new(collectors::mark_sweep::MarkAndSweep {
                heap: Ref::new(&*this).cast(),
                mark_stack: ObjectStack::new(),
                dur: 0,
                freed: 0,
                null_marks: 0,
                current_space_bitmap: None,
                marked: 0,
            });
        } else {
            this.gc = Box::new(collectors::sticky_mark_sweep::StickyHeap {
                last_dur: 0,
                last_freed: 0,
                total_freed_ms: AtomicUsize::new(0),
                total_ms_time: AtomicU64::new(0),
                total_freed_sticky: AtomicUsize::new(0),
                total_sticky_time: AtomicU64::new(0),
                ms: Box::new(collectors::mark_sweep::MarkAndSweep {
                    heap: Ref::new(&*this).cast(),
                    mark_stack: ObjectStack::new(),
                    dur: 0,
                    marked: 0,
                    null_marks: 0,
                    freed: 0,
                    current_space_bitmap: None,
                }),
                sticky: Box::new(collectors::sticky_mark_sweep::StickyMarkAndSweep {
                    heap: Ref::new(&*this).cast(),
                    mark_stack: ObjectStack::new(),
                    duration: 0,
                    freed: 0,
                    current_space_bitmap: None,
                }),
                next_sticky: AtomicBool::new(true),
                ms_iters: AtomicUsize::new(0),
            });
        }
        let space = spaces::dlmalloc_space::DLMallocSpace::create(
            &mut this,
            "alloc space",
            4096,
            max_mem,
            max_mem,
            false,
        );

        if use_gen_gc {
            this.card_table = Some(accounting::card_table::CardTable::new(
                space.c.begin(),
                space.c.limit().to_usize() - space.c.begin().to_usize(),
            ));
        }
        this.space = Some(space);

        this
    }
    /// This function allows to allocate dynamic memory for arrays or other dynamically sized objects
    /// It is unsafe because GC may not properly scan object that is not allocated by GC itself.
    pub unsafe fn unsafe_allocate_nogc(&mut self, vtable: *mut (), size: usize) -> Ref<GcBox<()>> {
        let mem = self
            .space()
            .alloc_with_growth(core::mem::size_of::<GcBox<()>>() + size);

        if mem.is_non_null() {
            mem.to_mut_ptr::<GcBox<()>>().write(GcBox {
                header: GCHeader {
                    #[cfg(feature = "incremental")]
                    next: Ref::null(),
                    vtable: vtable,
                },
                val: (),
            });
            self.alloc_stack().push(Ref::new(mem.to_ptr()));
            self.allocated
                .fetch_add(core::mem::size_of::<GcBox<()>>() + size, Ordering::Relaxed);
            return Ref::new(mem.to_ptr());
        }

        self.gc();
        if mem.is_non_null() {
            mem.to_mut_ptr::<GcBox<()>>().write(GcBox {
                header: GCHeader {
                    #[cfg(feature = "incremental")]
                    next: Ref::null(),
                    vtable,
                },
                val: (),
            });
            self.allocated
                .fetch_add(core::mem::size_of::<GcBox<()>>() + size, Ordering::Relaxed);
            self.alloc_stack().push(Ref::new(mem.to_ptr()));

            return Ref::new(mem.to_ptr());
        }
        panic!("Out of memory");
    }
    pub fn should_collect(&self) -> bool {
        let should_collect =
            self.allocated.load(Ordering::Relaxed) >= self.threshold.load(Ordering::Relaxed);
        should_collect
    }
    pub fn allocate<T: GcObj + 'static>(&mut self, val: T) -> Root<'a, T> {
        let should_collect = self.allocated.load(Ordering::Relaxed)
            + core::mem::size_of::<GcBox<T>>()
            >= self.threshold.load(Ordering::Relaxed);
        if should_collect {
            self.gc();
        }
        let vtable = gcvtbl_of(&val);
        let mem = self
            .space()
            .alloc_with_growth(core::mem::size_of::<GcBox<T>>());
        unsafe {
            if mem.is_non_null() {
                mem.to_mut_ptr::<GcBox<T>>().write(GcBox {
                    header: GCHeader {
                        #[cfg(feature = "incremental")]
                        next: Ref::null(),
                        vtable: vtable,
                    },
                    val,
                });
                self.alloc_stack().push(Ref::new(mem.to_ptr()));
                self.allocated
                    .fetch_add(core::mem::size_of::<GcBox<T>>(), Ordering::Relaxed);
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
                        vtable: vtable,
                    },
                    val,
                });
                self.allocated
                    .fetch_add(core::mem::size_of::<GcBox<T>>(), Ordering::Relaxed);
                return self.rootlist.root(Ref::new(mem.to_ptr::<GcBox<T>>()));
            }
            panic!("Out of memory");
        }
    }

    pub fn gc(&mut self) {
        self.gc.run_phases();
        //self.space().trim();
        let prev = self.threshold.load(Ordering::Relaxed);
        if self.allocated.load(Ordering::Relaxed) >= self.threshold.load(Ordering::Relaxed) {
            self.threshold.store(
                (self.allocated.load(Ordering::Relaxed) as f64 / 0.75) as usize,
                Ordering::Relaxed,
            );
        }
        if self.print_timings {
            if prev == self.threshold.load(Ordering::Relaxed) {
                println!(" GC threshold unchanged from {}", formatted_size(prev));
            } else {
                println!(
                    " GC threshold changed from {} to {}",
                    formatted_size(prev),
                    formatted_size(self.threshold.load(Ordering::Relaxed))
                );
            }
        }
    }

    pub fn root<T: GcObj>(&mut self, handle: Handle<T>) -> Root<'a, T> {
        self.rootlist.root(Ref::new(handle.ptr.as_ptr()))
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
    pub fn from_config(config: HeapConfig) -> Box<Self> {
        Heap::new(
            config.heap_size,
            config.use_gen_gc,
            config.print_timings,
            config.threshold,
        )
    }
}

#[repr(C)]
pub struct GcBox<T: GcObj> {
    header: GCHeader,
    val: T,
}

pub struct Handle<T: GcObj> {
    ptr: std::ptr::NonNull<GcBox<T>>,
}

impl<T: GcObj> Handle<T> {
    pub fn gc_ptr(&self) -> *const GcBox<()> {
        self.ptr.cast::<_>().as_ptr()
    }

    pub unsafe fn from_raw<U>(x: *const U) -> Self {
        Self {
            ptr: std::ptr::NonNull::new((x as *mut U).cast()).unwrap(),
        }
    }
}

impl<'a, T: GcObj> Root<'a, T> {
    pub fn to_heap(&self) -> Handle<T> {
        Handle {
            ptr: unsafe {
                std::ptr::NonNull::new_unchecked(self.inner.obj.cast::<GcBox<T>>().ptr as *mut _)
            },
        }
    }
}
impl<T: GcObj + 'static> Root<'static, T> {
    pub fn new(val: T) -> Self {
        allocate(val)
    }
}
impl<T: GcObj> std::ops::Deref for Handle<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &(&*self.ptr.as_ptr()).val }
    }
}

impl<T: GcObj> std::ops::DerefMut for Handle<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (&mut *self.ptr.as_ptr()).val }
    }
}

impl<T: GcObj> Copy for Handle<T> {}
impl<T: GcObj> Clone for Handle<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'a, T: GcObj> std::ops::Deref for Root<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &(&*self.inner.obj.cast::<GcBox<T>>().ptr).val }
    }
}

impl<'a, T: GcObj> std::ops::DerefMut for Root<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (&mut *self.inner.obj.cast::<GcBox<T>>().ptr).val }
    }
}

pub struct HeapConfig {
    incremental: bool,
    heap_size: usize,
    use_gen_gc: bool,
    print_timings: bool,
    use_conc_gc: bool,
    threshold: usize,
}

impl Default for HeapConfig {
    fn default() -> Self {
        Self {
            incremental: false,
            heap_size: 4 * 1024 * 1024,
            use_conc_gc: false,
            use_gen_gc: false,
            threshold: 16 * 1024,
            print_timings: false,
        }
    }
}

pub const MIN_HEAP_SIZE: usize = 64 * 1024;

impl HeapConfig {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn incremental(mut self, x: bool) -> Self {
        self.incremental = x;
        self
    }

    pub fn cocnurrent(mut self, x: bool) -> Self {
        self.use_conc_gc = x;
        self
    }

    pub fn generational(mut self, x: bool) -> Self {
        self.use_gen_gc = x;
        self
    }

    pub fn print_timings(mut self, x: bool) -> Self {
        self.print_timings = x;
        self
    }
    pub fn threshold(mut self, x: usize) -> Self {
        self.threshold = if x < 16 * 1024 { self.threshold } else { x };
        self
    }
    pub fn heap_size(mut self, size: usize) -> Self {
        self.heap_size = if size < MIN_HEAP_SIZE {
            MIN_HEAP_SIZE
        } else {
            size
        };
        self
    }
}

/// Initializes thread-local heap.
pub fn initialize_heap(config: HeapConfig) {
    HEAP.with(|heap| {
        heap.get_or_init(|| {
            Mutex::new(Heap::new(
                config.heap_size,
                config.use_gen_gc,
                config.print_timings,
                config.threshold,
            ))
        });
    })
}
/// Allocates object in thread-local heap.
pub fn allocate<T: GcObj + 'static>(value: T) -> Root<'static, T> {
    HEAP.with(|x| {
        let mut heap = x
            .get_or_init(|| Mutex::new(Heap::new(4 * 1024 * 1024, false, false, 16 * 1024)))
            .lock();

        heap.allocate(value)
    })
}
/// Puts `value` to thread-local heap root list.
pub fn root<T: GcObj + 'static>(value: Handle<T>) -> Root<'static, T> {
    HEAP.with(|x| {
        let mut heap = x
            .get_or_init(|| Mutex::new(Heap::new(4 * 1024 * 1024, false, false, 16 * 1024)))
            .lock();

        heap.rootlist.root(Ref::new(value.ptr.as_ptr()))
    })
}

use locks::Mutex;
use once_cell::unsync::OnceCell as UCell;
thread_local! {
    static HEAP: UCell<Mutex<Box<Heap<'static>>>> = UCell::new();
}
macro_rules! simple_gc_ty {
    ($($t: ty)*) => {
        $(
            impl Mark for $t {}
            impl Finalize for $t {}
            impl GcObj for $t {}
        )*
    };
}

macro_rules! simple_gc_ty_drop {
    ($($t: ty)*) => {
        $(
            impl Mark for $t {}
            impl Finalize for $t {}
            impl GcObj for $t {}


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

impl Mark for () {}
impl Finalize for () {}
impl GcObj for () {}

simple_gc_ty_drop!(
    String
    std::fs::File
);
/*
impl<T: GcObj> GcObj for Vec<Handle<T>> {
    const VTABLE: VTable = VTable {
        visit_fn: Some({
            unsafe fn visit(this: Ref<GcBox<()>>, visit: &mut dyn FnMut(*const GcBox<()>)) {
                for value in this.cast::<GcBox<Vec<Handle<()>>>>().val.iter() {
                    visit(value.gc_ptr());
                }
            }
            visit
        }),
        finalize_fn: Some({
            unsafe fn fin(this: Ref<GcBox<()>>) {
                std::ptr::drop_in_place(this.cast::<GcBox<Vec<Handle<()>>>>().ptr);
            }
            fin
        }),
        instance_size: core::mem::size_of::<Vec<Handle<T>>>(),
        determine_size: None,
    };
}*/
impl<T: GcObj> Finalize for Vec<T> {}
impl<T: GcObj> GcObj for Vec<T> {}
impl<T: GcObj> Mark for Vec<T> {
    fn mark(&self, visit: &mut dyn FnMut(*const GcBox<()>)) {
        for val in self.iter() {
            val.mark(visit);
        }
    }
}

/// Trait used internally to mark `Handle` and `Root`. Do not write your own code in `mark` function.
pub trait Mark {
    fn mark(&self, _visit: &mut dyn FnMut(*const GcBox<()>)) {}
}

impl<T: GcObj> Mark for Handle<T> {
    fn mark(&self, visit: &mut dyn FnMut(*const GcBox<()>)) {
        visit(self.gc_ptr());
    }
}

impl<T: GcObj> Mark for Root<'_, T> {
    fn mark(&self, visit: &mut dyn FnMut(*const GcBox<()>)) {
        visit(self.inner.obj.ptr);
    }
}

/// A trait used to invoke finalization code when object is deleted.
pub trait Finalize {
    fn finalize(&mut self) {}
}

pub trait GcObj: Finalize + Mark {}

fn gcvtbl_of(x: &dyn GcObj) -> *mut () {
    unsafe { std::mem::transmute::<_, TraitObject>(x).vtable }
}

use std::hash::{Hash, Hasher};

impl<T: Hash + GcObj> Hash for Handle<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T: PartialEq + GcObj> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(&**other)
    }
}

impl<T: Eq + GcObj> Eq for Handle<T> {}
impl<T: PartialOrd + GcObj> PartialOrd for Handle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}
impl<T: Ord + GcObj> Ord for Handle<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

use std::fmt::{self, Formatter};

impl<T: fmt::Display + GcObj> fmt::Display for Handle<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", **self)
    }
}
impl<T: fmt::Debug + GcObj> fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", **self)
    }
}
