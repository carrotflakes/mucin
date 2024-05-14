use std::sync::Arc;

use crate::{
    value::{Value, VmFn},
    vm::Vm,
};

pub struct JumpPoint {
    pub frame_index: usize,
    pub value_index: usize,
}

fn vf_calljp<'gc>(vm: &mut Vm<'gc>) -> Result<(), String> {
    let callee = vm.values.pop().unwrap();
    let jp = Arc::new(JumpPoint {
        frame_index: vm.frames.len(),
        value_index: vm.values.len(),
    });
    vm.values.push(Value::Any(jp));
    vm.call(callee, 1)?;
    Ok(())
}

fn vf_jump<'gc>(vm: &mut Vm<'gc>) -> Result<(), String> {
    let jp = vm.values.pop().unwrap();
    if let Some(any) = jp.as_any() {
        if let Some(jp) = any.downcast_ref::<JumpPoint>() {
            vm.frames.truncate(jp.frame_index);
            vm.values.truncate(jp.value_index);
            vm.values.push(Value::Unit);
            return Ok(());
        }
    }
    Err(format!("Expected JumpPoint, got {:?}", jp))
}

pub static VF_CALLJP: VmFn = VmFn {
    arity: 1,
    function: vf_calljp,
};

pub static VF_JUMP: VmFn = VmFn {
    arity: 1,
    function: vf_jump,
};

#[test]
fn test() {
    let srcs = [r#"
let a = [];
fn main() {
    calljp(|jp| {
        a.push(1);
        1 + jump(jp);
        a.push(2);
    });
    calljp(|jump| {
        a.push(3);
        4
    });
    a
}"#];
    for (i, src) in srcs.iter().enumerate() {
        println!("test {}", i);

        let mut runtime = crate::runtime::Runtime::new();
        runtime.push_env_from_src(src).unwrap();
        runtime
            .call_fn(
                "main",
                |_| vec![],
                |_, value| {
                    gilder::assert_golden!(format!("{:?}", value));
                },
            )
            .unwrap();
    }
}
