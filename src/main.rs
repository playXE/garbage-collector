use garbage_collector::*;
pub struct Foo {
    next: Option<Handle<Foo>>,
}

impl GcObj for Foo {}

impl Mark for Foo {
    fn mark(&self, visit: &mut dyn FnMut(*const GcBox<()>)) {
        if let Some(next) = self.next {
            next.mark(visit);
        }
    }
}
impl Finalize for Foo {
    fn finalize(&mut self) {
        //println!("ded");
    }
}

fn main() {
    let mut heap = Heap::from_config(
        HeapConfig::new()
            .print_timings(true)
            .generational(false)
            .heap_size(64 * 1024),
    );
    let val2 = heap.allocate(Foo { next: None });
    let mut val = heap.allocate(Foo { next: None });
    val.next = Some(heap.allocate(Foo { next: None }).to_heap());
    drop(val);
    let mut val = heap.allocate(Foo { next: None });
    val.next = Some(heap.allocate(Foo { next: None }).to_heap());
    drop(val);
    heap.gc();
    let v1 = heap.allocate(Foo { next: None });

    drop(val2);
    //drop(v1);
    heap.gc();
    println!("done {:p}", &*v1);
}
