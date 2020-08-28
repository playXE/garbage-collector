use garbage_collector::*;

struct Foo {
    val: Handle<XX>,
}

impl GcObj for Foo {}
impl Finalize for Foo {}
impl Mark for Foo {
    fn mark(&self, _visit: &mut dyn FnMut(*const GcBox<()>)) {
        self.val.mark(_visit);
        println!("mm {:p}",self.val.gc_ptr());
    }
}

struct XX(i32);

impl GcObj for XX {}
impl Mark for XX {
    fn mark(&self, _visit: &mut dyn FnMut(*const GcBox<()>)) {
        println!("mark {}",self.0);
    }
}

impl Finalize for XX {
    fn finalize(&mut self) {
        println!("dead {}",self.0);
    }
}

fn main() {
    initialize_heap(HeapConfig::new().generational(true).print_timings(true));
    let val = Root::new(XX(42));
    let val2 = Root::new(XX(43));
    let mut val3 = Root::new(Foo {
        val: val.to_heap()
    });
    //drop(val2);
    drop(val);
    collect_garbage();
    collect_garbage();
    let _vv = Root::new(XX(45));
    let handle = _vv.to_heap();

    drop(_vv);
    write_barrier(val3.to_heap(), handle);
    val3.val = handle;
    collect_garbage();
    println!("{}",val3.val.0);
}
