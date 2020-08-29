#![allow(dead_code)]
extern crate alloc;

pub mod allocator;
pub mod utils;

use core::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};
use utils::tagged_ptr::*;

pub trait GcObject {
    /// Returns size_of::<Self>() + size_of::<GcHeader>() by default
    fn size(&self) -> usize {
        core::mem::size_of_val(self)
    }
}

/// # GC Object header.
/// This type stores information needed by GC.
pub struct GcHeader {
    /// Virtual table for GcObject
    pub(crate) vtable: TaggedPointer<()>,
    pub(crate) next: *mut GcBox<()>,
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
        self.vtable.unset_bit(0);
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
