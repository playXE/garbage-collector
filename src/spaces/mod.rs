pub mod dlmalloc_space;
pub mod spaces;

use crate::accounting;
use crate::*;
use accounting::space_bitmap::*;
use mem_map::*;
use std::sync::Arc;
use utils::*;

pub trait Space<A: Alignment> {
    fn supports_cons_gc(&self) -> bool {
        false
    }

    fn can_move_objects(&self) -> bool {
        false
    }
    fn has_bound_bitmaps(&self) -> bool {
        false
    }

    fn bind_live_to_mark_bitmap(&self);
    fn unbind_bitmaps(&self);
    fn swap_bitmaps(&self);
    fn has_ptr(&self, _addr: Address) -> bool {
        false
    }

    fn live_bitmap(&self) -> &Arc<SpaceBitMap<A>>;
    fn mark_bitmap(&self) -> &Arc<SpaceBitMap<A>>;
    fn begin(&self) -> Address;
    fn limit(&self) -> Address;
    fn end(&self) -> Address;
    fn starting_size(&self) -> usize;
    fn initial_size(&self) -> usize;
    fn growth_limit(&self) -> usize;
    fn set_end(&self, addr: Address);
    fn mem_map(&self) -> &MemMap;
}

pub trait Allocator {
    fn create(
        heap: &mut crate::Heap,
        name: &str,
        initial_size: usize,
        growth_limit: usize,
        cap: usize,
        can_move: bool,
    ) -> Self;

    fn alloc_with_growth(&self, num_bytes: usize) -> Address;
}
