use crate::*;
use parking_lot::Mutex;
use std::mem;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

type ObjectPointer = *const GcBox<()>;
/// The number of values that can be put into a chunk.
const CHUNK_VALUES: usize = 4;

/// A single chunk of remembered pointers.
pub struct Chunk {
    /// The next chunk in the remembered set.
    next: Option<Box<Chunk>>,

    /// The index for the next value in the chunk.
    index: AtomicUsize,

    /// The values to store in this chunk.
    values: [ObjectPointer; CHUNK_VALUES],
}

impl Chunk {
    fn boxed() -> (Box<Self>, *mut Chunk) {
        let chunk = Chunk {
            next: None,
            index: AtomicUsize::new(0),
            values: [0 as *mut _; CHUNK_VALUES],
        };

        let boxed = Box::new(chunk);
        let ptr = &*boxed as *const _ as *mut _;

        (boxed, ptr)
    }

    /// Remembers a pointer in the current chunk.
    ///
    /// This method returns `true` if the pointer was remembered.
    fn remember(&mut self, value: ObjectPointer) -> bool {
        loop {
            let index = self.index.load(Ordering::Acquire);

            if index == CHUNK_VALUES {
                return false;
            }

            let next = index + 1;

            if self.index.compare_and_swap(index, next, Ordering::AcqRel) == index {
                self.values[index] = value;

                return true;
            }
        }
    }

    /// Remembers a pointer without any synchronisaton.
    unsafe fn remember_fast(&mut self, value: ObjectPointer) -> bool {
        let index = *self.index.get_mut();

        if index == CHUNK_VALUES {
            return false;
        }

        *self.index.get_mut() += 1;
        self.values[index] = value;

        true
    }
}

use std::cell::UnsafeCell;

/// A collection of pointers to mature objects that contain pointers to young
/// objects.
///
/// Values can be added to a remembered set, and an iterator can be obtained to
/// iterate over these values. Removing individual values is not supported,
/// instead one must prune the entire remembered set.
pub struct RememberedSet {
    /// The first chunk in the remembered set.
    head: UnsafeCell<Box<Chunk>>,

    /// A pointer to the last chunk in the remembered set. New values will be
    /// allocated into this chunk.
    tail: AtomicPtr<Chunk>,

    /// A lock used when allocating a new chunk.
    lock: Mutex<()>,
}

impl RememberedSet {
    /// Creates a new remembered set with a single chunk.
    pub fn new() -> RememberedSet {
        let (head, tail) = Chunk::boxed();

        RememberedSet {
            head: UnsafeCell::new(head),
            tail: AtomicPtr::new(tail),
            lock: Mutex::new(()),
        }
    }

    /// Remembers a pointer in the remembered set.
    ///
    /// This method supports concurrent operations and does not require you to
    /// use a lock of sorts.
    pub fn remember(&self, value: ObjectPointer) {
        loop {
            let tail_ptr = self.tail.load(Ordering::Acquire);
            let mut tail = unsafe { &mut *tail_ptr };

            if tail.remember(value) {
                return;
            }

            let _lock = self.lock.lock();

            if self.tail.load(Ordering::Acquire) != tail_ptr {
                continue;
            }

            let (chunk, new_tail_ptr) = Chunk::boxed();

            tail.next = Some(chunk);
            self.tail.store(new_tail_ptr, Ordering::Release);
        }
    }

    /// Remembers a pointer in the remembered set, without synchronisation.
    unsafe fn remember_fast(&self, value: ObjectPointer) {
        loop {
            let tail_ptr = *get_mut(&self.tail);
            let mut tail = &mut *tail_ptr;

            if tail.remember_fast(value) {
                return;
            }

            let (chunk, new_tail_ptr) = Chunk::boxed();

            tail.next = Some(chunk);
            *get_mut(&self.tail) = new_tail_ptr;
        }
    }

    /// Returns an iterator over the pointers in the remembered set.
    ///
    /// This method takes a mutable reference to `self` as iteration can not
    /// take place when the set is modified concurrently.
    pub fn iter(&self) -> RememberedSetIterator {
        RememberedSetIterator {
            chunk: unsafe { &*self.head.get() },
            index: 0,
        }
    }

    /// Prunes the remembered set by removing pointers to unmarked objects.
    pub fn prune(&self) {
        let (mut head, tail) = Chunk::boxed();

        // After this `head` is the old head, and `self.head` will be an
        // empty chunk.
        mem::swap(&mut head, unsafe { &mut *self.head.get() });
        *get_mut(&self.tail) = tail;

        let mut current = Some(head);

        while let Some(mut chunk) = current {
            for value in &chunk.values {
                if value.is_null() {
                    // Once we encounter a NULL value there can not be any
                    // non-NULL values that follow it.
                    break;
                }

                if unsafe { !(&**value).header.is_marked() } {
                    // Pointers that are not marked should no longer be
                    // remembered.
                    continue;
                }

                unsafe {
                    self.remember_fast(*value);
                }
            }

            current = chunk.next.take();
        }
    }

    /// Returns `true` if this RememberedSet is empty.
    pub fn is_empty(&self) -> bool {
        unsafe { (&*self.head.get()).values[0].is_null() }
    }
}

pub struct RememberedSetIterator<'a> {
    chunk: &'a Chunk,
    index: usize,
}

impl<'a> Iterator for RememberedSetIterator<'a> {
    type Item = &'a ObjectPointer;

    fn next(&mut self) -> Option<&'a ObjectPointer> {
        if self.index == CHUNK_VALUES {
            if let Some(chunk) = self.chunk.next.as_ref() {
                self.chunk = chunk;
                self.index = 0;
            } else {
                return None;
            }
        }

        let value = &self.chunk.values[self.index];

        if value.is_null() {
            None
        } else {
            self.index += 1;

            Some(value)
        }
    }
}
fn get_mut(c: &AtomicPtr<Chunk>) -> &mut *mut Chunk {
    unsafe { &mut *(c as *const AtomicPtr<Chunk> as *mut *mut Chunk) }
}
