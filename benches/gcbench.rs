use garbage_collector::*;

pub struct Node {
    i: i32,
    j: i32,
    left: Option<Handle<Self>>,
    right: Option<Handle<Self>>,
}

impl GcObj for Node {}
impl Finalize for Node {}
impl Mark for Node {
    fn mark(&self, visit: &mut dyn FnMut(*const GcBox<()>)) {
        if let Some(left) = self.left {
            left.mark(visit);
        }
        if let Some(right) = self.right {
            right.mark(visit);
        }
    }
}

impl Node {
    pub fn leaf() -> Self {
        Self {
            i: 0,
            j: 0,
            left: None,
            right: None,
        }
    }
}

use std::sync::atomic::{AtomicI32, Ordering};

static STRETCH_TREE_DEPTH: AtomicI32 = AtomicI32::new(0);
static LONG_LIVED_TREE_DEPTH: AtomicI32 = AtomicI32::new(0);

pub const fn tree_size(i: i32) -> i32 {
    (1 << (i + 1) - 1)
}

pub fn num_iters(i: i32) -> i32 {
    4 * tree_size(STRETCH_TREE_DEPTH.load(Ordering::Relaxed)) / tree_size(i)
}

pub fn populate(depth: i32, mut this_node: &mut Node) {
    let mut depth = depth;
    if depth <= 0 {
        return;
    } else {
        depth = depth - 1;
        let mut left = Root::new(Node::leaf());
        let mut right = Root::new(Node::leaf());
        this_node.left = Some(left.to_heap());
        this_node.right = Some(right.to_heap());
        populate(depth, &mut *left);
        populate(depth, &mut *right);
        drop(left);
        drop(right);
    }
}

pub fn make_tree(idepth: i32) -> Root<'static, Node> {
    if idepth <= 0 {
        allocate(Node::leaf())
    } else {
        let mut node = allocate(Node {
            i: 0,
            j: 0,
            left: None,
            right: None,
        });
        node.left = Some(make_tree(idepth - 1).to_heap());
        node.right = Some(make_tree(idepth - 1).to_heap());
        node
    }
}

const DEPTH: i32 = 6;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn top_down_construction(depth: i32) {
    let mut inum_iters = num_iters(depth);

    let mut i = 0;

    for i in 0..inum_iters {
        let mut temp_tree = Root::new(Node::leaf());
        populate(depth, &mut temp_tree);
    }
}

pub fn bottom_up_construction(depth: i32) {
    let mut inum_iters = num_iters(depth);

    let mut i = 0;

    for i in 0..inum_iters {
        let mut temp_tree = make_tree(depth);
    }
}

fn bench_top_down(c: &mut Criterion) {
    initialize_heap(HeapConfig::new().print_timings(false));
    STRETCH_TREE_DEPTH.store(7, Ordering::Relaxed);
    LONG_LIVED_TREE_DEPTH.store(6, Ordering::Relaxed);
    c.bench_function("top down 6", |b| b.iter(|| top_down_construction(6)));
    c.bench_function("bottom_up 6", |b| b.iter(|| bottom_up_construction(6)));
}

criterion_group!(benches, bench_top_down);
criterion_main!(benches);
