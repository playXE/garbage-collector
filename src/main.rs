use vm::utils::Ref;
use vm::*;
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
        println!("ded");
    }
}

fn main() {
    //    simple_logger::init().unwrap();

    let mut heap = Heap::from_config(HeapConfig::new().print_timings(true).heap_size(64 * 1024));

    let mut val = heap.allocate(Foo { next: None });
    val.next = Some(heap.allocate(Foo { next: None }).to_heap());
    drop(val);
    heap.gc();
}
