use crate::*;
use accounting::*;
use core::sync::atomic::{AtomicUsize, Ordering};
use mem_map::*;
use space_bitmap::*;
use std::sync::Arc;
use utils::*;
pub struct ContinuousMemMapSpace {
    mark_bitmap: Arc<SpaceBitMap<ObjectAlignment>>,
    live_bitmap: Arc<SpaceBitMap<ObjectAlignment>>,
    temp_bitmap: Option<Arc<SpaceBitMap<ObjectAlignment>>>,
    mem_map: MemMap,
    begin: Address,
    limit: AtomicUsize,
    end: AtomicUsize,
    starting_size: usize,
    initial_size: usize,
    growth_limit: usize,
}

use super::Space;

impl Space<ObjectAlignment> for ContinuousMemMapSpace {
    fn has_bound_bitmaps(&self) -> bool {
        self.temp_bitmap.is_some()
    }
    fn bind_live_to_mark_bitmap(&self) {
        let mut this = Ref::new(self);

        if !Arc::ptr_eq(&this.live_bitmap, &this.mark_bitmap) {
            let mark_bitmap = self.mark_bitmap.clone();
            this.temp_bitmap = Some(mark_bitmap);
            this.mark_bitmap = this.live_bitmap.clone();
        }
    }

    fn unbind_bitmaps(&self) {
        let mut this = Ref::new(self);

        let new_bitmap = this.temp_bitmap.take().unwrap();
        let _ = core::mem::replace(&mut this.mark_bitmap, new_bitmap);
    }
    /// Swap bitmaps so all marked objects now live objects.
    fn swap_bitmaps(&self) {
        let mut this = Ref::new(self);

        let mark = this.mark_bitmap.clone();
        let live = this.live_bitmap.clone();
        this.mark_bitmap = live;
        this.live_bitmap = mark;
    }
    fn has_ptr(&self, addr: Address) -> bool {
        if addr >= self.mem_map().begin() && addr < self.mem_map().end() {
            let res = if self.live_bitmap.has_addr(addr) {
                self.live_bitmap.test(addr)
            } else {
                false
            };
            res
        } else {
            false
        }
    }

    fn live_bitmap(&self) -> &Arc<SpaceBitMap> {
        &self.live_bitmap
    }

    fn mark_bitmap(&self) -> &Arc<SpaceBitMap> {
        &self.mark_bitmap
    }

    fn starting_size(&self) -> usize {
        self.starting_size
    }

    fn initial_size(&self) -> usize {
        self.initial_size
    }

    fn growth_limit(&self) -> usize {
        self.growth_limit
    }

    fn set_end(&self, addr: Address) {
        self.end.store(addr.to_usize(), Ordering::Relaxed);
    }

    fn limit(&self) -> Address {
        Address::from(self.limit.load(Ordering::Relaxed))
    }

    fn begin(&self) -> Address {
        self.begin
    }

    fn end(&self) -> Address {
        Address::from(self.end.load(Ordering::Relaxed))
    }

    fn mem_map(&self) -> &MemMap {
        &self.mem_map
    }
}

impl ContinuousMemMapSpace {
    pub fn create_memmap(
        name: &str,
        starting_size: usize,
        init: &mut usize,
        growth: &mut usize,
        cap: &mut usize,
    ) -> MemMap {
        if starting_size > *init {
            *init = starting_size;
        }
        if *init > *growth {
            panic!();
        }

        if *growth > *cap {
            panic!();
        }

        *growth = round_up(*growth, crate::utils::os::page_size());
        *cap = round_up(*cap, crate::utils::os::page_size());
        let mem_map = MemMap::map_anonymous(
            name,
            Address::null(),
            *cap,
            PROT_READ | PROT_WRITE,
            false,
            None,
        );

        mem_map
    }
    pub fn new(
        _name: &str,
        mem_map: MemMap,
        begin: Address,
        end: Address,
        limit: Address,
        growth_limit: usize,
        starting_size: usize,
        initial_size: usize,
    ) -> Self {
        let this = Self {
            growth_limit,
            limit: AtomicUsize::new(limit.to_usize()),
            end: AtomicUsize::new(end.to_usize()),
            begin,
            mark_bitmap: Arc::new(SpaceBitMap::new(begin, mem_map.size())),
            live_bitmap: Arc::new(SpaceBitMap::new(begin, mem_map.size())),
            temp_bitmap: None,
            mem_map,
            starting_size,
            initial_size,
        };
        this
    }
}
