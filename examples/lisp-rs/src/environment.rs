use super::interpreter;
use garbage_collector::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    stack: Vec<HashMap<String, Handle<interpreter::LispValue>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            stack: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.stack.push(HashMap::new())
    }

    pub fn exit_scope(&mut self) {
        self.stack.pop().expect("Popping from empty stack");
    }

    pub fn get(&mut self, key: String) -> Option<interpreter::HValue> {
        self.stack[self.stack.len() - 1]
            .get(&key)
            .cloned() // find_copy(&key)
            .or(self.stack[0].get(&key).cloned()) // find_copy(&key))
    }

    pub fn put(&mut self, key: String, value: interpreter::HValue) {
        let len = self.stack.len();
        self.stack[len - 1].insert(key, value);
    }

    pub fn put_global(&mut self, key: String, value: interpreter::HValue) {
        self.stack[0].insert(key, value);
    }
}
