use crate::*;
use crate::{utils::Ref, GCHeader};
#[cfg(all(feature = "incremental", feature = "concurrent"))]
pub mod __stack {
    use super::*;
    use flume::{unbounded, Receiver, Sender};

    pub struct Stack<T> {
        recv: Receiver<T>,
        snd: Sender<T>,
    }

    impl<T> Stack<T> {
        pub fn new() -> Self {
            let (s, r) = unbounded();
            Self { recv: r, snd: s }
        }

        pub fn pop(&self) -> Option<T> {
            self.recv.try_recv().ok()
        }

        pub fn push(&self, val: T) {
            if let Err(_) = self.snd.send(val) {
                panic!("Stack::push failed");
            }
        }

        pub fn drain(&self) -> flume::Drain<'_, T> {
            self.recv.drain()
        }
    }

    pub type ObjectStack = Stack<Ref<GcBox<()>>>;
}

#[cfg(all(feature = "incremental", not(feature = "concurrent")))]
mod __stack {
    use super::GcBox;
    use crate::locks::Mutex;
    use std::collections::VecDeque;
    pub struct Drain<'a, T> {
        data: VecDeque<T>,
        __m: std::marker::PhantomData<&'a ()>,
    }

    pub struct Stack<T> {
        data: Mutex<VecDeque<T>>,
    }

    impl<T> Stack<T> {
        pub fn new() -> Self {
            Self {
                data: Mutex::new(VecDeque::new()),
            }
        }

        pub fn push(&self, val: T) {
            self.data.lock().push_back(val);
        }

        pub fn pop(&self) -> Option<T> {
            self.data.lock().pop_back()
        }

        pub fn drain<'a>(&self) -> Drain<'a, T> {
            Drain {
                data: std::mem::replace(&mut *self.data.lock(), VecDeque::new()),
                __m: Default::default(),
            }
        }
    }
    use std::iter::{ExactSizeIterator, Iterator};
    impl<'a, T> Iterator for Drain<'a, T> {
        type Item = T;

        fn next(&mut self) -> Option<Self::Item> {
            self.data.pop_front()
        }
    }

    impl<'a, T> ExactSizeIterator for Drain<'a, T> {
        fn len(&self) -> usize {
            self.data.len()
        }
    }
    use super::*;
    pub struct ObjDrain {
        data: Ref<GcBox<()>>,
    }

    impl Iterator for ObjDrain {
        type Item = Ref<GcBox<()>>;
        fn next(&mut self) -> Option<Self::Item> {
            if self.data.is_non_null() {
                let prev = self.data;
                self.data = prev.header.next;
                return Some(prev);
            } else {
                None
            }
        }
    }
    use std::cell::UnsafeCell;
    pub struct ObjectStack {
        head: UnsafeCell<Ref<GcBox<()>>>,
    }

    impl ObjectStack {
        pub fn new() -> Self {
            Self {
                head: UnsafeCell::new(Ref::null()),
            }
        }
        fn head(&self) -> &mut Ref<GcBox<()>> {
            unsafe { &mut *self.head.get() }
        }
        pub fn push(&self, mut val: Ref<GcBox<()>>) {
            let mut head = self.head();
            if head.is_non_null() {
                let mut prev = *head;
                *head = val;
                head.header.next = prev;
            } else {
                *head = val;
            }
        }

        pub fn pop(&self) -> Option<Ref<GcBox<()>>> {
            let mut head = self.head();
            if head.is_non_null() {
                let prev = *head;
                *head = prev.header.next;
                Some(prev)
            } else {
                None
            }
        }

        pub fn drain(&self) -> ObjDrain {
            let head = self.head();
            let val = *head;
            *head = Ref::null();
            ObjDrain { data: val }
        }
    }
}

#[cfg(all(not(feature = "incremental"), not(feature = "concurrent")))]
pub mod __stack {
    use crate::locks::Mutex;
    use std::collections::VecDeque;
    pub struct Drain<'a, T> {
        data: VecDeque<T>,
        __m: std::marker::PhantomData<&'a ()>,
    }

    pub struct Stack<T> {
        data: Mutex<VecDeque<T>>,
    }

    impl<T> Stack<T> {
        pub fn new() -> Self {
            Self {
                data: Mutex::new(VecDeque::new()),
            }
        }

        pub fn push(&self, val: T) {
            self.data.lock().push_back(val);
        }

        pub fn pop(&self) -> Option<T> {
            self.data.lock().pop_back()
        }

        pub fn drain<'a>(&self) -> Drain<'a, T> {
            Drain {
                data: std::mem::replace(&mut *self.data.lock(), VecDeque::new()),
                __m: Default::default(),
            }
        }
    }
    use std::iter::{ExactSizeIterator, Iterator};
    impl<'a, T> Iterator for Drain<'a, T> {
        type Item = T;

        fn next(&mut self) -> Option<Self::Item> {
            self.data.pop_front()
        }
    }

    impl<'a, T> ExactSizeIterator for Drain<'a, T> {
        fn len(&self) -> usize {
            self.data.len()
        }
    }

    pub type ObjectStack = Stack<Ref<GcBox<()>>>;
}

pub use __stack::*;
