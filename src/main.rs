use vm::utils::Ref;
use vm::*;
pub struct Foo {
    next: Option<Handle<Foo>>,
}

unsafe fn trace_foo(this: Ref<GcBox<()>>, trace: &mut dyn FnMut(*const GcBox<()>)) {
    let this = Handle::<Foo>::from_raw(this.ptr);
    if let Some(x) = this.next {
        trace(x.gc_ptr());
    }
}

impl GcObject for Foo {
    const VTABLE: VTable = VTable {
        visit_fn: Some(trace_foo),
        finalize_fn: None,
        instance_size: core::mem::size_of::<Foo>(),
        determine_size: None,
    };
}

fn main() {
    simple_logger::init().unwrap();

    let mut heap = Heap::new(128 * 1024, false, true);
    for _ in 0..1000 {
        let mut val = heap.allocate(Foo { next: None });
        val.next = Some(heap.allocate(Foo { next: None }).to_heap());
        drop(val);
        let mut val = heap.allocate(Foo { next: None });
        val.next = Some(heap.allocate(Foo { next: None }).to_heap());
        drop(val);
        let mut val = heap.allocate(Foo { next: None });
        val.next = Some(heap.allocate(Foo { next: None }).to_heap());
        drop(val);
        let mut val = heap.allocate(Foo { next: None });
        val.next = Some(heap.allocate(Foo { next: None }).to_heap());
        drop(val);
        let mut val = heap.allocate(Foo { next: None });
        val.next = Some(heap.allocate(Foo { next: None }).to_heap());
        drop(val);
        let mut val = heap.allocate(Foo { next: None });
        val.next = Some(heap.allocate(Foo { next: None }).to_heap());
        drop(val);
        let mut val = heap.allocate(Foo { next: None });
        val.next = Some(heap.allocate(Foo { next: None }).to_heap());
        drop(val);
        let mut val = heap.allocate(Foo { next: None });
        val.next = Some(heap.allocate(Foo { next: None }).to_heap());
        drop(val);
    }
    heap.gc();
}
