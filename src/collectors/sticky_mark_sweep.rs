use super::mark_sweep::*;
use crate::accounting::card_table::*;
use crate::accounting::space_bitmap::*;
use crate::accounting::stack::ObjectStack;
use crate::spaces::*;
use crate::utils::*;
use crate::*;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
pub struct StickyHeap<'a> {
    total_freed_ms: AtomicUsize,
    total_freed_sticky: AtomicUsize,
    total_ms_time: AtomicU64,
    total_sticky_time: AtomicU64,
    ms_iters: AtomicUsize,
    next_sticky: AtomicBool,
    ms: Box<MarkAndSweep<'a>>,
    last_freed: usize,
    last_dur: u64,
    sticky: Box<StickyMarkAndSweep<'a>>,
}

impl<'a> StickyHeap<'a> {
    pub fn get_ms_estimated_meant_throughput(&self) -> u64 {
        return (self.total_freed_ms.load(Ordering::Relaxed) as u64 * 1000)
            / ((self.total_ms_time.load(Ordering::Relaxed) / 1000 / 1000) + 1);
    }

    pub fn get_sticky_estimated_meant_throughput(&self) -> u64 {
        return (self.total_freed_sticky.load(Ordering::Relaxed) as u64 * 1000)
            / ((self.total_sticky_time.load(Ordering::Relaxed) / 1000 / 1000) + 1);
    }
}

impl<'a, 'b: 'a> super::GarbageCollector<'a> for StickyHeap<'b> {
    fn run_phases(&mut self) {
        let next_gc_is_sticky = self.next_sticky.load(Ordering::Relaxed);

        let (freed, duration) = if !next_gc_is_sticky {
            self.ms.run();
            self.ms_iters.fetch_add(1, Ordering::Relaxed);
            (self.ms.freed, self.ms.dur)
        } else {
            self.sticky.run_phases();
            (self.sticky.freed, self.sticky.duration)
        };
        if next_gc_is_sticky {
            self.total_freed_sticky.fetch_add(freed, Ordering::Relaxed);
            self.total_sticky_time
                .fetch_add(duration, Ordering::Relaxed);
        } else {
            self.total_freed_ms.fetch_add(freed, Ordering::Relaxed);
            self.total_ms_time.fetch_add(duration, Ordering::Relaxed);
        }
        if !next_gc_is_sticky {
            self.next_sticky.store(true, Ordering::Relaxed);
        } else {
            const STICKY_GC_THROUGHPUT_ADJ: f64 = 1.0;
            #[inline(always)]
            const fn estimated_throughput(freed: usize, duration: u64) -> u64 {
                (freed as u64 / 1000) / ((duration / 1000 / 1000) + 1)
            }

            let estimated_throughput = estimated_throughput(freed, duration) as f64;
            if estimated_throughput * STICKY_GC_THROUGHPUT_ADJ
                >= self.get_ms_estimated_meant_throughput() as f64
                && self.ms_iters.load(Ordering::Relaxed) > 0
            {
                self.next_sticky.store(true, Ordering::Relaxed);
            } else {
                self.next_sticky.store(false, Ordering::Relaxed);
            }
        }
    }

    fn duration(&self) -> u64 {
        self.last_dur
    }

    fn freed(&self) -> usize {
        self.last_freed
    }

    fn mark_stack(&self) -> &crate::accounting::stack::ObjectStack {
        &self.ms.mark_stack
    }
}

pub struct StickyMarkAndSweep<'a> {
    pub current_space_bitmap: Option<Arc<SpaceBitMap<ObjectAlignment>>>,
    pub heap: Ref<Heap<'a>>,
    pub mark_stack: ObjectStack,
    pub freed: usize,
    pub duration: u64,
}

impl<'a> StickyMarkAndSweep<'a> {
    pub fn run_phases(&mut self) {
        self.current_space_bitmap = Some(self.heap.space().c.mark_bitmap().clone());
        let start = std::time::Instant::now();

        // For sticky GC, we want to bind the bitmaps of all spaces as the allocation stack lets us
        // know what was allocated since the last GC. A side-effect of binding the allocation space mark
        // and live bitmap is that marking the objects will place them in the live bitmap.
        self.heap.space().c.bind_live_to_mark_bitmap();
        self.heap.swap_stacks();
        let m = std::time::Instant::now();
        self.marking_phase();
        let mend = m.elapsed();
        let rs = std::time::Instant::now();
        self.reclaim_phase();
        let re = rs.elapsed();
        self.heap.space().c.swap_bitmaps();
        let c = std::time::Instant::now();
        self.heap.space().c.mark_bitmap().clear_all();
        let ce = c.elapsed();
        self.duration = start.elapsed().as_nanos() as u64;
        if self.heap.print_timings {
            println!(
            "Sticky GC cycle stats: \nFreed {} in {}ns\n\tMarking took {}ns\n\tReclaim took {}ns\n\tBitmap cleared in {}ns",
            formatted_size(self.freed),
            self.duration,
            mend.as_nanos(),
            re.as_nanos(),
            ce.as_nanos(),
        );
        }
    }

    fn reclaim_phase(&mut self) {
        let heap = self.heap;
        self.sweep_array(&*heap.live_stack(), false);
        if self.heap.space().c.has_bound_bitmaps() {
            self.heap.space().c.unbind_bitmaps();
        }
    }
    fn sweep_array(&mut self, live: &ObjectStack, swap_bitmaps: bool) {
        let mut live_bitmap = self.heap.space().c.live_bitmap();
        let mut mark_bitmap = self.heap.space().c.mark_bitmap();
        if swap_bitmaps {
            core::mem::swap(&mut live_bitmap, &mut mark_bitmap);
        }

        let mut object = live.pop();
        while let Some(obj) = object {
            if !mark_bitmap.test(Address::from_ptr(obj.ptr)) {
                self.freed += obj.size();
                self.heap.space().free(Address::from_ptr(obj.ptr));
                //self.heap.in_heap.clear(Address::from_ptr(object.ptr));
            }
            object = live.pop();
        }
        self.heap.allocated.fetch_sub(self.freed, Ordering::Relaxed);
    }
    fn marking_phase(&mut self) {
        // TODO: Marking can be done concurrently after marking roots.
        //log!("Marking root objects");
        // self.heap
        //    .mark_alloc_stack_as_live(self.heap.live_stack(), self.heap.space.c.live_bitmap());
        //self.heap.card_table.as_ref().unwrap().
        let heap = self.heap;
        heap.rootlist.walk(&mut |root| {
            self.mark_object(root.obj);
        });
        // All reachable objects must be referenced by a root or a dirty card, so we can clear the mark
        // stack here since all objects in the mark stack will get scanned by the card scanning anyways.
        // TODO: Not put these objects in the mark stack in the first place.
        self.mark_stack.clear();
        self.scan_grey_objects(CARD_DIRTY - 1);
        self.process_mark_stack();
    }
    fn scan_grey_objects(&mut self, min_age: u8) {
        let heap = self.heap;
        // TODO: Try to mark card tables in parallel?
        let card_table = self.heap.card_table.as_ref().unwrap();
        unsafe {
            card_table.scan::<True>(
                &*heap.space().c.mark_bitmap(),
                heap.space().c.begin(),
                heap.space().c.end(),
                &self.mark_stack,
                min_age,
            );
        }
    }

    fn process_mark_stack(&mut self) {
        while let Some(object) = self.mark_stack.pop() {
            //log!("Process {:p}", object.ptr);
            self.scan_object(object);
        }
    }
    fn scan_object(&mut self, obj: Ref<GcBox<()>>) {
        if obj.is_non_null() {
            /*if let Some(x) = obj.trait_object().visit_fn {
                unsafe {
                    x(obj, &mut |ptr| {
                        if ptr.is_null() == false {
                            self.mark_object_non_null(Ref::new(ptr), obj);
                        }
                    });
                }
            }*/
            obj.trait_object().mark(&mut |ptr| {
                if ptr.is_null() == false {
                    self.mark_object_non_null(Ref::new(ptr), obj);
                }
            });
        }
    }
    fn mark_object(&mut self, obj: Ref<GcBox<()>>) {
        if obj.is_non_null() {
            self.mark_object_non_null(obj, Ref::null());
        }
    }
    fn mark_object_non_null(&mut self, obj: Ref<GcBox<()>>, _holder: Ref<GcBox<()>>) {
        if self
            .current_space_bitmap
            .as_ref()
            .unwrap()
            .has_addr(Address::from_ptr(obj.ptr))
        {
            if !self
                .current_space_bitmap
                .as_ref()
                .unwrap()
                .set(Address::from_ptr(obj.ptr))
            {
                self.mark_stack.push(obj);
            }
        } else {
            unreachable!("Unknown pointer: {:p}", obj.ptr);
        }
    }
}
