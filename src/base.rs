use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use rustc_hash::FxHashMap;

pub(crate) type HashMap<K, V> = FxHashMap<K, V>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RcMut<T>(pub Rc<RefCell<T>>);

impl<T> RcMut<T> {
    pub fn new(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }

    pub fn get(&self) -> Ref<T> {
        self.0.borrow()
    }

    // Can use this if only use in single thread
    pub fn get_mut(&self) -> RefMut<T> {
        self.0.borrow_mut()
    }
}
