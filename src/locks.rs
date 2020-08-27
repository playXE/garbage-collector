//! Locks used in GC for synchronization if `concurrent` feature is enabled.
//!
//!
//!
//!

#[cfg(feature = "concurrent")]
pub mod __locks {
    pub use parking_lot::*;
}

/// Checked locks implementation for non-concurrent GC
#[cfg(all(not(feature = "concurrent"), feature = "rt-checks"))]
pub mod __locks {
    use std::cell::*;
    pub struct Mutex<T> {
        val: RefCell<T>,
    }

    pub struct MutexGuard<'a, T> {
        val: RefMut<'a, T>,
    }

    impl<T> Mutex<T> {
        pub fn lock(&self) -> MutexGuard<T> {
            MutexGuard {
                val: self.val.borrow_mut(),
            }
        }

        pub const fn new(val: T) -> Self {
            Self {
                val: RefCell::new(val),
            }
        }
    }

    impl<T> std::ops::Deref for MutexGuard<'_, T> {
        type Target = T;
        fn deref(&self) -> &Self::Target {
            &self.val
        }
    }
    impl<T> std::ops::DerefMut for MutexGuard<'_, T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.val
        }
    }
    pub struct RwLock<T> {
        val: RefCell<T>,
    }

    impl<T> RwLock<T> {
        pub const fn new(x: T) -> Self {
            Self {
                val: RefCell::new(x),
            }
        }

        pub fn read(&self) -> RwLockReadGuard<'_, T> {
            RwLockReadGuard {
                val: self.val.borrow(),
            }
        }

        pub fn write(&self) -> RwLockWriteGuard<'_, T> {
            RwLockWriteGuard {
                val: self.val.borrow_mut(),
            }
        }
    }

    pub struct RwLockReadGuard<'a, T> {
        val: Ref<'a, T>,
    }

    pub struct RwLockWriteGuard<'a, T> {
        val: RefMut<'a, T>,
    }

    impl<T> std::ops::Deref for RwLockReadGuard<'_, T> {
        type Target = T;
        fn deref(&self) -> &Self::Target {
            &self.val
        }
    }

    impl<T> std::ops::Deref for RwLockWriteGuard<'_, T> {
        type Target = T;
        fn deref(&self) -> &Self::Target {
            &self.val
        }
    }
    impl<T> std::ops::DerefMut for RwLockWriteGuard<'_, T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.val
        }
    }
}
#[cfg(all(not(feature = "concurrent"), not(feature = "rt-checks")))]
pub mod __locks {
    use std::cell::UnsafeCell;
    struct RefCell<T> {
        x: UnsafeCell<T>,
    }

    impl<T> RefCell<T> {
        pub const fn new(x: T) -> Self {
            Self {
                x: UnsafeCell::new(x),
            }
        }

        pub fn borrow(&self) -> &'_ T {
            unsafe { &*self.x.get() }
        }
        pub fn borrow_mut(&self) -> &'_ mut T {
            unsafe { &mut *self.x.get() }
        }
    }

    pub struct Mutex<T> {
        val: RefCell<T>,
    }

    pub struct MutexGuard<'a, T> {
        val: &'a mut T,
    }

    impl<T> Mutex<T> {
        pub fn lock(&self) -> MutexGuard<T> {
            MutexGuard {
                val: self.val.borrow_mut(),
            }
        }

        pub const fn new(val: T) -> Self {
            Self {
                val: RefCell::new(val),
            }
        }
    }

    impl<T> std::ops::Deref for MutexGuard<'_, T> {
        type Target = T;
        fn deref(&self) -> &Self::Target {
            &self.val
        }
    }
    impl<T> std::ops::DerefMut for MutexGuard<'_, T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.val
        }
    }
    pub struct RwLock<T> {
        val: RefCell<T>,
    }

    impl<T> RwLock<T> {
        pub const fn new(x: T) -> Self {
            Self {
                val: RefCell::new(x),
            }
        }

        pub fn read(&self) -> RwLockReadGuard<'_, T> {
            RwLockReadGuard {
                val: self.val.borrow(),
            }
        }

        pub fn write(&self) -> RwLockWriteGuard<'_, T> {
            RwLockWriteGuard {
                val: self.val.borrow_mut(),
            }
        }
    }

    pub struct RwLockReadGuard<'a, T> {
        val: &'a T,
    }

    pub struct RwLockWriteGuard<'a, T> {
        val: &'a mut T,
    }

    impl<T> std::ops::Deref for RwLockReadGuard<'_, T> {
        type Target = T;
        fn deref(&self) -> &Self::Target {
            &self.val
        }
    }

    impl<T> std::ops::Deref for RwLockWriteGuard<'_, T> {
        type Target = T;
        fn deref(&self) -> &Self::Target {
            &self.val
        }
    }
    impl<T> std::ops::DerefMut for RwLockWriteGuard<'_, T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.val
        }
    }
}

pub use __locks::*;
