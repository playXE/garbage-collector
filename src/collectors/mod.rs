pub mod mark_sweep;

#[cfg(feature = "incremental")]
pub mod incremental;

pub trait GarbageCollector<'a> {
    fn run_phases(&mut self);
    fn freed(&self) -> usize;
    fn duration(&self) -> u64;
    fn mark_stack(&self) -> &crate::accounting::stack::ObjectStack;
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
