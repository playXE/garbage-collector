use garbage_collector::utils::tagged_ptr::*;

fn main() {
    let mut a = TaggedPointer::new(&0 as *const i32 as *mut u8);
    let mut b = TaggedPointer::new(&2 as *const i32 as *mut u8);
    let z = TaggedPointer::with_tags_from(b, a);
    println!("{:p}={:p} + {:p}", z.raw, b.raw, a.raw);
    a.atomic_set_bit(0);
    println!("{:p}", b.raw);
    println!(
        "{:b} vs {:b}",
        z.raw as usize,
        TaggedPointer::with_tags_from(b, a).raw as usize
    );
}
