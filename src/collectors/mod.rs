#[cfg(feature = "incremental")]
pub mod incremental;
pub mod mark_sweep;
pub mod sticky_mark_sweep;
use super::*;
pub trait GarbageCollector<'a> {
    fn run_phases(&mut self);
    fn freed(&self) -> usize;
    fn duration(&self) -> u64;
    fn mark_stack(&self) -> &crate::accounting::stack::ObjectStack;
    fn write_barrier(&self, field: Ref<GcBox<()>>, holder: Ref<GcBox<()>>) {}
}

pub struct DummyGc {}
impl<'a> GarbageCollector<'a> for DummyGc {
    fn run_phases(&mut self) {
        panic!();
    }
    fn freed(&self) -> usize {
        panic!()
    }

    fn duration(&self) -> u64 {
        panic!()
    }
    fn mark_stack(&self) -> &crate::accounting::stack::ObjectStack {
        panic!()
    }
}
