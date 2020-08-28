use crate::accounting::space_bitmap::*;
use crate::accounting::stack::ObjectStack;
use crate::spaces::*;
use crate::utils::*;
use crate::*;
use std::sync::Arc;

pub struct MarkAndSweep<'a> {
    pub current_space_bitmap: Option<Arc<SpaceBitMap<ObjectAlignment>>>,
    pub heap: Ref<Heap<'a>>,
    pub mark_stack: ObjectStack,
    pub freed: usize,
    pub dur: u64,
    pub marked: u32,
    pub null_marks: u32,
}

impl<'a> MarkAndSweep<'a> {
    pub fn run(&mut self) {
        self.null_marks = 0;
        self.marked = 0;
        self.current_space_bitmap = Some(self.heap.space().c.mark_bitmap().clone());
        //let tasks = pause.get_tasks();
        let start = std::time::Instant::now();
        self.heap.swap_stacks();
        let m = std::time::Instant::now();
        self.marking_phase();
        let mend = m.elapsed();
        let rs = std::time::Instant::now();
        self.reclaim_phase();
        let re = rs.elapsed();

        self.dur = start.elapsed().as_nanos() as u64;
        if self.heap.print_timings {
            println!(
                "GC cycle stats:\nMarked: \n null={}\n objects={}\nTimings: \n Freed {} in {}ns\n Marking took {}ns\n Reclaim took {}ns",
                self.null_marks,
                self.marked,
                formatted_size(self.freed),
                self.dur,
                mend.as_nanos(),
                re.as_nanos(),
            );
        }
        //drop(pause);
    }
    fn marking_phase(&mut self) {
        if let Some(card_table) = &self.heap.card_table {
            card_table.clear_card_range(self.heap.space().c.begin(), self.heap.space().c.end());
        }
        #[cfg(feature = "logging")]
        {
            log::info!("Marking root objects...");
        }
        let heap = self.heap;
        heap.rootlist.walk(&mut |root| {
            self.mark_object(root.obj);
        });
        #[cfg(feature = "logging")]
        {
            log::info!("Processing gray stack...");
        }
        self.process_mark_stack();
    }
    fn reclaim_phase(&mut self) {
        self.sweep(false);
    }
    fn sweep(&mut self, swap_bitmaps: bool) {
        let mut live = self.heap.space().c.live_bitmap().clone();
        let mut mark = self.heap.space().c.mark_bitmap().clone();
        if Arc::ptr_eq(&live, &mark) {
            return;
        }
        //let writer = crate::locks::HEAP_BITMAP_LOCK.write();
        self.heap
            .mark_alloc_stack_as_live(self.heap.live_stack(), &live);
        if swap_bitmaps {
            core::mem::swap(&mut live, &mut mark);
        }
        let sbegin = self.heap.space.as_ref().unwrap().c.begin();
        let send = self.heap.space().c.end();
        SpaceBitMap::sweep_walk(
            &live,
            &mark,
            self.heap.space().c.begin(),
            self.heap.space().c.end(),
            &mut |count: usize, ptrs: Ref<Ref<GcBox<()>>>| {
                //if swap_bitmaps == false {

                let bitmap = self.heap.space().c.live_bitmap();
                for i in 0..count {
                    let ptr = ptrs.offset(i as _);
                    if ptr.is_null() {
                        panic!();
                    }
                    let p = *ptr;

                    if p.ptr >= sbegin.to_mut_ptr::<GcBox<()>>()
                        && p.ptr < send.to_mut_ptr::<GcBox<()>>()
                    {
                        #[cfg(feature = "logging")]
                        log::info!("Sweep ->{:p}", (*ptrs.offset(i as _)).ptr);
                        let ptr = ptrs.offset(i as _);
                        let p = *ptr;
                        let size = p.size();
                        ptr.trait_object().finalize();
                        unsafe {
                            std::ptr::drop_in_place(ptr.trait_object() as *mut dyn GcObj);
                        }
                        self.heap.allocated.fetch_sub(size, Ordering::Relaxed);
                        self.freed += size;
                        self.heap.space().free(Address::from_ptr(p.ptr));
                        //self.heap.in_heap.clear(Address::from_ptr(p.ptr));
                        bitmap.clear(Address::from_ptr(p.ptr));
                    }
                    //bitmap.clear(Address::from_ptr((*ptrs.offset(i as _)).ptr));
                }
                //}

                //for i in 0..count {}
                /*self.heap
                .space
                .freelist(unsafe { core::slice::from_raw_parts(ptrs.ptr.cast(), count) });*/
            },
        );
        self.heap.space().c.swap_bitmaps();

        if self.heap.space().c.has_bound_bitmaps() {
            self.heap.space().c.unbind_bitmaps();
        }
        if !Arc::ptr_eq(
            &self.heap.space().c.live_bitmap(),
            &self.heap.space().c.mark_bitmap(),
        ) {
            self.heap.space().c.mark_bitmap().clear_all();
        }

        //drop(writer);
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
                }in
            }*/
            println!("vt {:p}",obj.header.vtable);
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
        } else {
            self.null_marks += 1;
        }
    }
    fn mark_object_non_null(&mut self, obj: Ref<GcBox<()>>, _holder: Ref<GcBox<()>>) {
        if self
            .current_space_bitmap
            .as_ref()
            .unwrap()
            .has_addr(Address::from_ptr(obj.ptr))
        {
            self.marked += 1;
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

impl<'a, 'b: 'a> super::GarbageCollector<'a> for MarkAndSweep<'b> {
    fn run_phases(&mut self) {
        self.freed = 0;
        self.dur = 0;
        self.run();
    }

    fn duration(&self) -> u64 {
        self.dur
    }

    fn freed(&self) -> usize {
        self.freed
    }

    fn mark_stack(&self) -> &crate::accounting::stack::ObjectStack {
        &self.mark_stack
    }
}
