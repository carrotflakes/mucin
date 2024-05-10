use std::sync::{Arc, Weak};

pub type Str = Arc<String>;

#[derive(Debug)]
pub struct StrPool {
    pool: Vec<Weak<String>>,
}

impl StrPool {
    pub fn new() -> Self {
        Self { pool: Vec::new() }
    }

    pub fn from_strs(strs: &[&str]) -> (Self, Vec<Str>) {
        let strs: Vec<_> = strs.iter().map(|s| Arc::new(s.to_string())).collect();
        let pool = strs.iter().map(|s| Arc::downgrade(s)).collect();
        let pool = Self { pool };
        (pool, strs)
    }

    pub fn intern(&mut self, s: &str) -> Str {
        if let Some(interned) = self.try_intern(s) {
            return interned;
        }

        let str = Arc::new(s.to_string());
        self.pool.push(Arc::downgrade(&str));
        str
    }

    pub fn try_intern(&mut self, s: &str) -> Option<Str> {
        for interned in &self.pool {
            if let Some(interned) = interned.upgrade() {
                if interned.as_str() == s {
                    return Some(interned.clone());
                }
            }
        }

        None
    }

    pub fn size(&self) -> usize {
        self.pool.len()
    }

    pub fn compact(&mut self) {
        self.pool.retain(|weak| weak.strong_count() > 0);
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
