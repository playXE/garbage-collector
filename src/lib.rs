#![allow(dead_code)]
extern crate alloc;

pub mod allocator;
pub mod utils;

use core::sync::atomic::AtomicUsize;
use utils::tagged_ptr::*;

pub trait GcObject {
    /// Returns size_of::<Self>()
    fn size(&self) -> usize {
        core::mem::size_of_val(self)
    }
    #[allow(unused_variables, dead_code)]
    fn visit_references(&self, visit: &mut dyn FnMut(*const GcBox<()>)) {}
}

/// # GC Object header.
/// This type stores information needed by GC.
pub struct GcHeader {
    /// Virtual table for GcObject
    pub(crate) vtable: TaggedPointer<()>,
    pub(crate) next: TaggedPointer<GcBox<()>>,
    #[cfg(feature = "concurrent")]
    pub(crate) color: AtomicU8,
}

impl GcObject for () {}

pub struct GcBox<T: GcObject + ?Sized> {
    header: GcHeader,
    val: T,
}

const GC_WHITE: u8 = 0b00;
const GC_GRAY: u8 = 0b01;
const GC_BLACK: u8 = 0b10;
impl GcHeader {
    pub fn is_new(&self) -> bool {
        !self.next.atomic_bit_is_set(0)
    }
    pub fn is_old(&self) -> bool {
        self.next.atomic_bit_is_set(0)
    }
    pub fn set_old(&self) {
        self.next.atomic_set_bit(0);
    }

    pub fn set_new(&self) {
        self.next.atomic_unset_bit(0);
    }

    pub fn sync_mark(&self) -> bool {
        self.vtable.atomic_set_bit(0)
    }

    pub fn unsync_mark(&self) -> bool {
        self.vtable.set_bit(0)
    }

    pub fn is_marked(&self) -> bool {
        self.vtable.atomic_bit_is_set(0)
    }

    pub fn unmark(&self) {
        self.vtable.atomic_unset_bit(0);
    }
    pub fn sync_sticky_mark(&self) -> bool {
        self.vtable.atomic_set_bit(1)
    }

    pub fn unsync_sticky_mark(&self) -> bool {
        self.vtable.set_bit(1)
    }

    pub fn is_sticky_marked(&self) -> bool {
        self.vtable.atomic_bit_is_set(1)
    }

    pub fn sticky_unmark(&self) {
        self.vtable.atomic_unset_bit(1);
    }
    #[cfg(feature = "concurrent")]
    pub fn color(&self) -> u8 {
        self.color.load(Ordering::Relaxed)
    }
    #[cfg(feature = "concurrent")]
    pub fn set_color(&self, c: u8) {
        self.color.store(c, Ordering::Relaxed);
    }
}

impl<T: GcObject + ?Sized> GcBox<T> {
    pub fn trait_object(&self) -> &'static mut dyn GcObject {
        unsafe {
            core::mem::transmute(utils::TraitObject {
                data: &self.val as *const T as *mut (),
                vtable: self.header.vtable.untagged(),
            })
        }
    }
}

struct RootInner {
    rc: u32,
    pub obj: Ref<GcBox<()>>,
}

pub struct Root<T: GcObject> {
    inner: Ref<RootInner>,
    _marker: std::marker::PhantomData<T>,
}
use crate::utils::*;
pub struct RootList {
    roots: Vec<Ref<RootInner>>,
}

impl RootList {
    pub fn new() -> Self {
        Self {
            roots: Vec::with_capacity(4),
        }
    }
    pub fn root<T: GcObject>(&mut self, o: Ref<GcBox<T>>) -> Root<T> {
        let root = Ref::new(Box::into_raw(Box::new(RootInner {
            rc: 1,
            obj: o.cast(),
        })));
        self.roots.push(root);
        Root {
            inner: root,
            _marker: Default::default(),
        }
    }
    pub fn unroot<T: GcObject>(&mut self, r: Root<T>) {
        drop(r)
    }

    pub(crate) fn walk(&mut self, walk: &mut dyn FnMut(Ref<RootInner>)) {
        /*let mut cur = self.roots;
        while cur.is_non_null() {
            walk(cur);
            cur = cur.next;
        }*/

        self.roots.retain(|x| unsafe {
            if x.rc == 0 {
                let _ = Box::from_raw(x.ptr);
                false
            } else {
                walk(*x);
                true
            }
        });
    }
}
use std::sync::Arc;

pub fn gc_thread<
    F: FnOnce(&Arc<allocator::LocalAllocator>) -> R + Send + 'static,
    R: Send + 'static,
>(
    f: F,
) -> std::thread::JoinHandle<R> {
    std::thread::spawn(move || {
        let candidate = allocator::get_heap().get_pooled_allocator();
        let local = if let Some(candidate) = candidate {
            candidate
        } else {
            let id = allocator::get_heap()
                .local_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            let local = std::sync::Arc::new(allocator::LocalAllocator::new(id));
            allocator::get_heap().add_local(local.clone());
            local
        };
        allocator::TLA.with(|x| {
            *x.borrow_mut() = Some(local.clone());
        });
        let res = f(&local);

        if !local
            .gc_free_zone
            .load(std::sync::atomic::Ordering::Relaxed)
        {
            allocator::enter_gc_free_zone();
        }
        let l = allocator::THREAD_STATE_CHANGE_LOCK.lock();
        allocator::get_heap().remove_local_locked(&local);
        allocator::get_heap().return_to_pool_locked(local);
        drop(l);
        res
    })
}

use std::hash::{Hash, Hasher};

impl<T: Hash + GcObject> Hash for Handle<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T: PartialEq + GcObject> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(&**other)
    }
}

impl<T: Eq + GcObject> Eq for Handle<T> {}
impl<T: PartialOrd + GcObject> PartialOrd for Handle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}
impl<T: Ord + GcObject> Ord for Handle<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

use std::fmt::{self, Formatter};

impl<T: fmt::Display + GcObject> fmt::Display for Handle<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", **self)
    }
}
impl<T: fmt::Debug + GcObject> fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", **self)
    }
}
pub struct Handle<T: GcObject> {
    ptr: std::ptr::NonNull<GcBox<T>>,
}

impl<T: GcObject> Handle<T> {
    pub fn gc_ptr(&self) -> *const GcBox<()> {
        self.ptr.cast::<_>().as_ptr()
    }
    pub fn ptr_eq(this: Self, other: Self) -> bool {
        this.ptr == other.ptr
    }
    pub unsafe fn from_raw<U>(x: *const U) -> Self {
        Self {
            ptr: std::ptr::NonNull::new((x as *mut U).cast()).unwrap(),
        }
    }

    pub unsafe fn to_virtual_handle(self) -> allocator::VirtualHandle<T> {
        std::mem::transmute(self)
    }
}
impl<T: GcObject> Root<T> {
    pub fn to_heap(&self) -> Handle<T> {
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
impl<'a, T: GcObject> std::ops::Deref for Root<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &(&*self.inner.obj.cast::<GcBox<T>>().ptr).val }
    }
}

impl<T: GcObject> std::ops::DerefMut for Root<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (&mut *self.inner.obj.cast::<GcBox<T>>().ptr).val }
    }
}
impl<T: GcObject> GcObject for Handle<T> {
    fn visit_references(&self, visit: &mut dyn FnMut(*const GcBox<()>)) {
        visit(self.gc_ptr());
    }
}
impl<T: GcObject> Drop for Root<T> {
    fn drop(&mut self) {
        self.inner.rc = self.inner.rc.wrapping_sub(1);
    }
}

impl<T: GcObject> Clone for Root<T> {
    fn clone(&self) -> Self {
        let mut inn = self.inner;
        inn.rc += 1;
        Self {
            inner: self.inner,
            _marker: Default::default(),
        }
    }
}

/// Get current local allocator or create new one. If you sure that you do not have access to local allocator
/// then use this function and at the end of thread execution detach this allocator manually.
pub fn local_allocator() -> Arc<allocator::LocalAllocator> {
    allocator::TLA.with(|x| {
        let xx: std::cell::Ref<'_, Option<Arc<allocator::LocalAllocator>>> = x.borrow();
        if let Some(ref tla) = &*xx {
            return tla.clone();
        }
        drop(xx);
        let candidate = allocator::get_heap().get_pooled_allocator();
        let local = if let Some(candidate) = candidate {
            candidate
        } else {
            let id = allocator::get_heap()
                .local_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            let local = std::sync::Arc::new(allocator::LocalAllocator::new(id));
            allocator::get_heap().add_local(local.clone());
            local
        };
        *x.borrow_mut() = Some(local.clone());

        local
    })
}

/// Detach local allocator created by user request. There is no way currently
/// to see if this allocator created in `gc_thread` or no so be carefull with using this function.
pub fn detach_local_allocator(local: Arc<allocator::LocalAllocator>) {
    if !local
        .gc_free_zone
        .load(std::sync::atomic::Ordering::Relaxed)
    {
        allocator::enter_gc_free_zone();
    }
    let l = allocator::THREAD_STATE_CHANGE_LOCK.lock();
    allocator::get_heap().remove_local_locked(&local);
    allocator::get_heap().return_to_pool_locked(local);
    drop(l);
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct GCConfig {
    generational: bool,
    print: bool,
}

impl GCConfig {
    pub fn new(generational: bool, print: bool) -> Self {
        Self {
            generational,
            print,
        }
    }
}

use once_cell::sync::OnceCell;

pub fn gc_config(c: Option<GCConfig>) -> GCConfig {
    static CONF: OnceCell<GCConfig> = OnceCell::new();
    if let Some(c) = c {
        *CONF.get_or_init(|| c)
    } else {
        *CONF.get_or_init(|| GCConfig::new(false, false))
    }
}
