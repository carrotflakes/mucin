#[macro_export]
macro_rules! destruct_value {
    ($value:expr, $($pat:tt => $body:block),* $(,)?) => {
        'dv: {
            $($crate::destruct_value_1!(
                [($value, $pat),],
                { #[allow(unreachable_code)] break 'dv ($body); });)*
        }
    };
}

#[macro_export]
macro_rules! destruct_value_1 {
    ([], $body:block) => {
        $body
    };
    ([($value:expr, $pat:tt), $($rest:tt,)*], $body:block) => {
        $crate::destruct_value_2! {
            $value,
            $pat,
            {
                $crate::destruct_value_1! {
                    [$($rest,)*],
                    $body
                }
            }
        }
    };
}

#[macro_export]
macro_rules! destruct_value_2 {
    ($value:expr, (), $body:block) => {
        if let $crate::value::Value::Unit = $value {
            $body
        }
    };
    ($value:expr, (null), $body:block) => {
        if let $crate::value::Value::Null = $value {
            $body
        }
    };
    ($value:expr, (bool $pat:tt), $body:block) => {
        if let $crate::value::Value::Bool($pat) = $value {
            $body
        }
    };
    ($value:expr, (int $pat:tt), $body:block) => {
        if let $crate::value::Value::Int($pat) = $value {
            $body
        }
    };
    ($value:expr, (float $pat:tt), $body:block) => {
        if let $crate::value::Value::Float($pat) = $value {
            $body
        }
    };
    ($value:expr, (float* $pat:tt), $body:block) => {
        if let Some($pat) = $value.as_float() {
            $body
        }
    };
    ($value:expr, (str $lit:literal), $body:block) => {
        if let $crate::value::Value::String(str) = $value {
            if str.as_str() == $lit {
                $body
            }
        }
    };
    ($value:expr, (str $pat:tt), $body:block) => {
        if let $crate::value::Value::String($pat) = $value {
            $body
        }
    };
    ($value:expr, (any $pat:tt), $body:block) => {
        if let $crate::value::Value::Any($pat) = $value {
            $body
        }
    };
    ($value:expr, (any_mut $pat:tt), $body:block) => {
        if let $crate::value::Value::AnyMut($pat) = $value {
            $body
        }
    };
    ($value:expr, (vec [$($pat:tt),* $(,)?]), $body:block) => {
        if let $crate::value::Value::Vec(ref vec) = $value {
            let vec = $crate::gc_arena::lock::RefLock::borrow(vec);
            let mut it = vec.iter();
            $crate::destruct_value_1! {
                [$((it.next(), (Some($pat))),)*],
                $body
            }
        }
    };
    ($value:expr, (vec $pat:tt), $body:block) => {
        if let $crate::value::Value::Vec($pat) = $value {
            $body
        }
    };
    ($value:expr, (dict {$($k:tt : $v:tt),* $(,)?}), $body:block) => {
        if let $crate::value::Value::Dict(ref dict) = $value {
            let d = $crate::gc_arena::lock::RefLock::borrow(dict);
            $crate::destruct_value_1! {
                [$((d.get_by_str(stringify!($k)), (Some($v))),)*],
                $body
            }
        }
    };
    ($value:expr, (dict $pat:pat), $body:block) => {
        if let $crate::value::Value::Dict($pat) = $value {
            $body
        }
    };
    ($value:expr, (Some($pat:tt)), $body:block) => {
        if let Some(x) = $value {
            $crate::destruct_value_2! {
                x,
                $pat,
                $body
            }
        }
    };
    ($value:expr, _, $body:block) => {
        $body
    };
    ($value:expr, $x:pat, $body:block) => {
        #[allow(irrefutable_let_patterns)]
        if let $x = $value {
            $body
        }
    };
}

#[test]
fn test_destruct_value() {
    use gc_arena::{lock::RefLock, Arena, Gc, Rootable};
    use std::sync::Arc;

    use crate::value::{Dict, Value};

    Arena::<Rootable![()]>::new(|mc| {
        let value = Value::Int(42);
        destruct_value! {
            &value,
            (bool _b) => {
            },
            (int n) => {
                assert_eq!(*n, 42);
            },
            (vec [a, b]) => {
                assert_eq!(a, b);
            },
            (vec v) => {
                panic!("{:?}", v);
            },
            _ => {
                panic!("unexpected value");
            }
        }
        let value = Value::Vec(Gc::new(
            mc,
            RefLock::new(vec![Value::Int(42), Value::Int(43)]),
        ));
        destruct_value! {
            &value,
            (int n) => {
                assert_eq!(*n, 42);
            },
            (vec [(int 42), (int a)]) => {
                assert_eq!(*a, 43);
            },
            (vec v) => {
                panic!("{:?}", v);
            },
            _ => {
                panic!("unexpected value");
            }
        }
        let value = Value::Dict(Gc::new(
            mc,
            RefLock::new(Dict(vec![
                (Arc::new("a".to_string()), Value::Int(42)),
                (Arc::new("b".to_string()), Value::Int(43)),
            ])),
        ));
        destruct_value! {
            &value,
            (int n) => {
                assert_eq!(*n, 42);
            },
            (dict {a: (int a)}) => {
              assert_eq!(a, &42);
            },
            (dict d) => {
              panic!("{:?}", d);
            },
            _ => {
                panic!("unexpected value");
            }
        }
        let value = Value::String(crate::string::intern("hello"));
        destruct_value! {
            &value,
            (str s) => {
                assert_eq!(s.as_str(), "hello");
            },
            _ => {
                panic!("unexpected value");
            }
        }
        destruct_value! {
            &value,
            (str "hello") => {
            },
            _ => {
                panic!("unexpected value");
            }
        }

        ()
    });
}
