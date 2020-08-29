use parking_lot::{Condvar, Mutex};
pub struct Semaphore {
    mutex: Mutex<bool>,
    cond: Condvar,
}

impl Semaphore {
    #[inline]
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(false),
            cond: Condvar::new(),
        }
    }

    pub fn set(&self) {
        let mut lock = self.mutex.lock();
        if *lock == false {
            *lock = true;
            self.cond.notify_one();
        }
        drop(lock);
    }

    pub fn reset(&self) {
        let mut lock = self.mutex.lock();
        *lock = false;
    }

    pub fn wait(&self) {
        let mut lock = self.mutex.lock();
        while *lock == false {
            self.cond.wait(&mut lock);
        }
        *lock = false;
    }
}
