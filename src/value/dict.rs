use gc_arena::Collect;

use crate::string::Str;

use super::Value;

#[derive(Collect, Debug, Clone, Default)]
#[collect(no_drop)]
pub struct Dict<'gc>(pub Vec<(Str, Value<'gc>)>);

impl<'gc> Dict<'gc> {
    pub fn get(&self, key: &Str) -> Option<&Value<'gc>> {
        self.0
            .iter()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    pub fn get_by_str(&self, key: &str) -> Option<&Value<'gc>> {
        self.0
            .iter()
            .find_map(|(k, v)| if k.as_str() == key { Some(v) } else { None })
    }

    pub fn set(&mut self, key: Str, value: Value<'gc>) {
        if let Some(e) = self.0.iter_mut().find(|(k, _)| k == &key) {
            e.1 = value;
        } else {
            self.0.push((key, value));
        }
    }

    pub fn entries(&self) -> impl Iterator<Item = (&Str, &Value<'gc>)> {
        self.0.iter().map(|(k, v)| (k, v))
    }
}

impl<'gc> std::fmt::Display for Dict<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}
