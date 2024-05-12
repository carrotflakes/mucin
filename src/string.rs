use std::sync::Arc;

pub type Str = Arc<String>;

#[derive(Debug)]
pub struct StrPool {
    pool: Vec<Arc<String>>,
}

impl StrPool {
    pub fn new() -> Self {
        Self { pool: Vec::new() }
    }

    pub fn from_strs(strs: &[&str]) -> (Self, Vec<Str>) {
        let strs: Vec<_> = strs.iter().map(|s| Arc::new(s.to_string())).collect();
        let pool = Self { pool: strs.clone() };
        (pool, strs)
    }

    pub fn intern(&mut self, s: &str) -> Str {
        if let Some(interned) = self.try_intern(s) {
            return interned;
        }

        let str = Arc::new(s.to_string());
        self.pool.push(str.clone());
        str
    }

    pub fn try_intern(&mut self, s: &str) -> Option<Str> {
        self.pool
            .iter()
            .find(|interned| interned.as_str() == s)
            .cloned()
    }

    pub fn size(&self) -> usize {
        self.pool.len()
    }

    pub fn compact(&mut self) {
        self.pool.retain(|str| Arc::strong_count(str) > 1);
    }
}

thread_local! {
    pub static STR_POOL: std::cell::RefCell<StrPool> = std::cell::RefCell::new(StrPool::new());
}

pub fn intern(s: &str) -> Str {
    STR_POOL.with(|pool| pool.borrow_mut().intern(s))
}

pub fn compact_str_pool() {
    STR_POOL.with(|pool| pool.borrow_mut().compact());
}

thread_local! {
    pub static UNIQUE_INT: std::cell::Cell<usize> = std::cell::Cell::new(0);
}

pub fn unique_str(prefix: &str) -> Str {
    let mut i = UNIQUE_INT.with(|int| {
        let i = int.get();
        int.set(i.wrapping_add(1));
        i
    });

    STR_POOL.with(|pool| {
        let mut pool = pool.borrow_mut();
        loop {
            let s = format!("{}{}", prefix, i);
            if pool.try_intern(&s).is_none() {
                return pool.intern(&s);
            }
            i += 1;
        }
    })
}
