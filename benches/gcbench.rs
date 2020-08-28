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
use criterion::BenchmarkId;
fn bench_top_down(c: &mut Criterion) {
    initialize_heap(HeapConfig::new().print_timings(false));
    STRETCH_TREE_DEPTH.store(7, Ordering::Relaxed);
    LONG_LIVED_TREE_DEPTH.store(6, Ordering::Relaxed);
    let mut group = c.benchmark_group("binary tree");
    for i in 6..8 {
        group.bench_with_input(BenchmarkId::new(&format!("gc btree top down depth={}",i),i), &i, |b,i| {
            b.iter(|| {
                top_down_construction(*i)
            });
        });
        group.bench_with_input(BenchmarkId::new(&format!("gc btree bottom up depth={}",i),i), &i, |b,i| {
            b.iter(|| {
                bottom_up_construction(*i);
            });
        });
        group.bench_with_input(BenchmarkId::new(&format!("rc btree top down depth={}",i),i), &i, |b,i| {
            b.iter(|| {
                rctop_down_construction(*i)
            });
        });
        group.bench_with_input(BenchmarkId::new(&format!("rc btree bottom up depth={}",i),i), &i, |b,i| {
            b.iter(|| {
                rcbottom_up_construction(*i);
            });
        });
        //group.bench_function("top down 6", |b| b.iter(|| top_down_construction(6)));
        //c.bench_function("bottom_up 6", |b| b.iter(|| bottom_up_construction(6)));
    }
    group.finish();
}

struct Inner<T> {
    rc: u32,
    val: T,
}

pub struct Rc<T> {
    inner: *mut Inner<T>,
}

impl<T> Rc<T> {
    pub fn new(val: T) -> Self {
        Self {
            inner: Box::into_raw(Box::new(Inner { val, rc: 1 })),
        }
    }
}

impl<T> Clone for Rc<T> {
    fn clone(&self) -> Self {
        unsafe {
            let inner = &mut *self.inner;
            inner.rc += 1;
            Self { inner: self.inner }
        }
    }
}

impl<T> Drop for Rc<T> {
    fn drop(&mut self) {
        unsafe {
            let inner = &mut *self.inner;
            if inner.rc == 1 {
                let _ = Box::from_raw(self.inner);
            } else {
                inner.rc -= 1;
            }
        }
    }
}

impl<T> std::ops::Deref for Rc<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &(&*self.inner).val }
    }
}

impl<T> std::ops::DerefMut for Rc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (&mut *self.inner).val }
    }
}

pub struct RCNode {
    left: Option<Rc<Self>>,
    right: Option<Rc<Self>>,
}

impl RCNode {
    pub fn leaf() -> Self {
        Self {
            left: None,
            right: None,
        }
    }
}

pub fn rcpopulate(depth: i32, mut this_node: &mut RCNode) {
    let mut depth = depth;
    if depth <= 0 {
        return;
    } else {
        depth = depth - 1;
        let mut left = Rc::new(RCNode::leaf());
        let mut right = Rc::new(RCNode::leaf());
        this_node.left = Some(left.clone());
        this_node.right = Some(right.clone());
        rcpopulate(depth, &mut *left);
        rcpopulate(depth, &mut *right);
        drop(left);
        drop(right);
    }
}

pub fn rcmake_tree(idepth: i32) -> Rc<RCNode> {
    if idepth <= 0 {
        Rc::new(RCNode::leaf())
    } else {
        let mut node = Rc::new(RCNode {
            left: None,
            right: None,
        });
        node.left = Some(rcmake_tree(idepth - 1));
        node.right = Some(rcmake_tree(idepth - 1));
        node
    }
}

pub fn rctop_down_construction(depth: i32) {
    let mut inum_iters = num_iters(depth);

    let mut i = 0;

    for i in 0..inum_iters {
        let mut temp_tree = Rc::new(Node::leaf());
        populate(depth, &mut temp_tree);
    }
}

pub fn rcbottom_up_construction(depth: i32) {
    let mut inum_iters = num_iters(depth);

    let mut i = 0;

    for i in 0..inum_iters {
        let mut temp_tree = rcmake_tree(depth);
    }
}

criterion_group!(benches, bench_top_down);
criterion_main!(benches);
