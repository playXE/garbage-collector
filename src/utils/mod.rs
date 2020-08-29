pub mod remembered_set;
pub mod semaphore;
pub mod tagged_ptr;
use core::sync::atomic::AtomicPtr;
#[repr(C)]
pub struct Ref<T> {
    pub ptr: *mut T,
}

impl<T> Ref<T> {
    pub const fn null() -> Self {
        Self {
            ptr: core::ptr::null_mut(),
        }
    }
    pub fn uget(&self) -> &mut T {
        unsafe { &mut *self.ptr }
    }

    pub const fn new(val: *const T) -> Self {
        Self { ptr: val as *mut _ }
    }

    pub fn is_null(self) -> bool {
        self.ptr.is_null()
    }

    pub fn is_non_null(self) -> bool {
        !self.is_null()
    }

    pub fn cast<U>(self) -> Ref<U> {
        Ref {
            ptr: self.ptr.cast(),
        }
    }

    pub fn offset(&self, ix: isize) -> Self {
        Self {
            ptr: unsafe { self.ptr.offset(ix) },
        }
    }

    pub fn write(self, val: T) {
        unsafe { self.ptr.write(val) }
    }

    pub fn read(self) -> T {
        unsafe { self.ptr.read() }
    }

    pub fn as_atomic(&self) -> &AtomicPtr<T> {
        unsafe { &*(self as *const Ref<T> as *const AtomicPtr<T>) }
    }
}

impl<T> core::ops::Deref for Ref<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.uget()
    }
}

impl<T> core::ops::DerefMut for Ref<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.uget()
    }
}

impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Ref<T> {}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct TraitObject {
    pub data: *mut (),
    pub vtable: *mut (),
}

pub struct ProtectedBy<'a, T> {
    mutex: &'a parking_lot::Mutex<()>,
    value: std::cell::UnsafeCell<T>,
}

impl<'a, T> ProtectedBy<'a, T> {
    pub fn new(mutex: &'a parking_lot::Mutex<()>, value: T) -> Self {
        Self {
            mutex,
            value: std::cell::UnsafeCell::new(value),
        }
    }

    pub fn get(&self) -> (parking_lot::MutexGuard<'a, ()>, &mut T) {
        unsafe {
            let lock = self.mutex.lock();
            (lock, &mut *self.value.get())
        }
    }

    pub unsafe fn get_locked(&self) -> &mut T {
        &mut *self.value.get()
    }
}

unsafe impl<'a, T> Sync for ProtectedBy<'a, T> {}
