//! Tagged Pointers
//!
//! The TaggedPointer struct can be used to wrap a `*mut T` with one (or both)
//! of the lower 2 bits set (or unset).

use core::hash::{Hash, Hasher};
use core::ptr;
use core::sync::atomic::{AtomicPtr, Ordering};

/// The mask to use for untagging a pointer.
const UNTAG_MASK: usize = (!0x7) as usize;

/// Returns true if the pointer has the given bit set to 1.
pub fn bit_is_set<T>(pointer: *mut T, bit: usize) -> bool {
    let shifted = 1 << bit;

    (pointer as usize & shifted) == shifted
}

/// Returns the pointer with the given bit set.
pub fn with_bit<T>(pointer: *mut T, bit: usize) -> *mut T {
    (pointer as usize | 1 << bit) as _
}

/// Returns the given pointer without any tags set.
pub fn untagged<T>(pointer: *mut T) -> *mut T {
    (pointer as usize & UNTAG_MASK) as _
}

/// Structure wrapping a raw, tagged pointer.
#[derive(Debug)]
pub struct TaggedPointer<T> {
    pub raw: *mut T,
}

impl<T> TaggedPointer<T> {
    /// Returns a new TaggedPointer without setting any bits.
    pub fn new(raw: *mut T) -> TaggedPointer<T> {
        TaggedPointer { raw }
    }

    /// Returns a new TaggedPointer with the given bit set.
    pub fn with_bit(raw: *mut T, bit: usize) -> TaggedPointer<T> {
        let mut pointer = Self::new(raw);

        pointer.set_bit(bit);

        pointer
    }

    /// Returns a null pointer.
    pub fn null() -> TaggedPointer<T> {
        TaggedPointer {
            raw: ptr::null::<T>() as *mut T,
        }
    }

    /// Returns the wrapped pointer without any tags.
    pub fn untagged(self) -> *mut T {
        self::untagged(self.raw)
    }

    /// Returns a new TaggedPointer using the current pointer but without any
    /// tags.
    pub fn without_tags(self) -> Self {
        Self::new(self.untagged())
    }

    /// Returns true if the given bit is set.
    pub fn bit_is_set(self, bit: usize) -> bool {
        self::bit_is_set(self.atomic_load(), bit)
    }

    /// Sets the given bit.
    pub fn set_bit(&self, bit: usize) -> bool {
        let atomic = self.as_atomic();
        let old_word = atomic.load(Ordering::Relaxed);
        let mask = 1 << bit;
        if (old_word as usize & mask) != 0 {
            atomic.store((old_word as usize | mask) as *mut _, Ordering::Relaxed);
        }
        (old_word as usize & mask) != 0
    }

    pub fn unset_bit(&self, bit: usize) -> bool {
        let atomic = self.as_atomic();
        let old_word = atomic.load(Ordering::Relaxed);
        let mask = 1 << bit;
        atomic.store((old_word as usize & !mask) as *mut _, Ordering::Relaxed);
        (old_word as usize & mask) != 0
    }

    /// Returns true if the current pointer is a null pointer.
    pub fn is_null(self) -> bool {
        self.untagged().is_null()
    }

    /// Returns an immutable to the pointer's value.
    pub fn as_ref<'a>(self) -> Option<&'a T> {
        unsafe { self.untagged().as_ref() }
    }

    /// Returns a mutable reference to the pointer's value.
    pub fn as_mut<'a>(self) -> Option<&'a mut T> {
        unsafe { self.untagged().as_mut() }
    }

    /// Atomically swaps the internal pointer with another one.
    ///
    /// This boolean returns true if the pointer was swapped, false otherwise.
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::trivially_copy_pass_by_ref))]
    pub fn compare_and_swap(&self, current: *mut T, other: *mut T) -> bool {
        self.as_atomic()
            .compare_and_swap(current, other, Ordering::AcqRel)
            == current
    }

    /// Atomically replaces the current pointer with the given one.
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::trivially_copy_pass_by_ref))]
    pub fn atomic_store(&self, other: *mut T) {
        self.as_atomic().store(other, Ordering::Release);
    }

    /// Atomically loads the pointer.
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::trivially_copy_pass_by_ref))]
    pub fn atomic_load(&self) -> *mut T {
        self.as_atomic().load(Ordering::Acquire)
    }

    /// Checks if a bit is set using an atomic load.
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::trivially_copy_pass_by_ref))]
    pub fn atomic_bit_is_set(&self, bit: usize) -> bool {
        Self::new(self.atomic_load()).bit_is_set(bit)
    }
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::trivially_copy_pass_by_ref))]
    pub fn atomic_set_bit(&self, bit: usize) -> bool {
        let atomic = self.as_atomic();
        let mask = 1 << bit;
        let mut old_word;
        while {
            old_word = atomic.load(Ordering::Relaxed) as usize;
            if old_word & mask != 0 {
                return true;
            }
            atomic
                .compare_exchange_weak(
                    old_word as *mut _,
                    (old_word | mask) as *mut T,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                )
                .is_err()
        } {}
        false
    }
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::trivially_copy_pass_by_ref))]
    pub fn atomic_unset_bit(&self, bit: usize) -> bool {
        let atomic = self.as_atomic();
        let mask = 1 << bit;
        let mut old_word;
        while {
            old_word = atomic.load(Ordering::Relaxed) as usize;
            if old_word & mask == 0 {
                return true;
            }
            atomic
                .compare_exchange_weak(
                    old_word as *mut _,
                    (old_word & !mask) as *mut T,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                )
                .is_err()
        } {}
        false
    }
    fn as_atomic(&self) -> &AtomicPtr<T> {
        unsafe { &*(self as *const TaggedPointer<T> as *const AtomicPtr<T>) }
    }
}

impl<T> PartialEq for TaggedPointer<T> {
    fn eq(&self, other: &TaggedPointer<T>) -> bool {
        self.raw == other.raw
    }
}

impl<T> Eq for TaggedPointer<T> {}

// These traits are implemented manually as "derive" doesn't handle the generic
// "T" argument very well.
impl<T> Clone for TaggedPointer<T> {
    fn clone(&self) -> TaggedPointer<T> {
        TaggedPointer::new(self.raw)
    }
}

impl<T> Copy for TaggedPointer<T> {}

impl<T> Hash for TaggedPointer<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}
