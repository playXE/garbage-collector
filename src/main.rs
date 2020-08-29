use garbage_collector::utils::tagged_ptr::TaggedPointer;

fn main() {
    let mut ptr = TaggedPointer::new(Box::into_raw(Box::new(42)));
    println!("{:p}", ptr.raw);
    ptr.atomic_set_bit(1);
    println!("{:p}", ptr.raw);
    ptr.unset_bit(1);
    println!("{:p}", ptr.raw);
}
