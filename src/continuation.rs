use std::sync::Arc;

use gc_arena::Gc;

use crate::{
    value::{Value, VmFn},
    vm::Vm,
};

pub struct JumpPoint {
    pub frame_index: usize,
    pub value_index: usize,
    function: *const (),
    pc: usize,
}

fn vf_calljp<'gc>(vm: &mut Vm<'gc>) -> Result<(), String> {
    let callee = vm.values.pop().unwrap();
    let jp = Arc::new(JumpPoint {
        frame_index: vm.frames.len(),
        value_index: vm.values.len(),
        function: Gc::as_ptr(vm.frames.last().unwrap().function) as *const (),
        pc: vm.frames.last().unwrap().pc,
    });
    vm.values.push(Value::Any(jp));
    vm.call(callee, 1)?;
    Ok(())
}

fn vf_jump<'gc>(vm: &mut Vm<'gc>) -> Result<(), String> {
    let retval = vm.values.pop().unwrap();
    let jp = vm.values.pop().unwrap();
    if let Some(any) = jp.as_any() {
        if let Some(jp) = any.downcast_ref::<JumpPoint>() {
            let frame = vm
                .frames
                .get(jp.frame_index - 1)
                .ok_or("JumpPoint is out of date")?;
            if jp.function != Gc::as_ptr(frame.function) as *const () || jp.pc != frame.pc {
                return Err("JumpPoint is out of date".to_string());
            }

            vm.frames.truncate(jp.frame_index);
            vm.values.truncate(jp.value_index);
            vm.values.push(retval);
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
    arity: 2,
    function: vf_jump,
};

#[test]
fn test() {
    let srcs = [r#"
let a = [];
fn main() {
    a.push(calljp(|jp| {
        a.push(1);
        1 + jump(jp);
        a.push(2);
    }));
    a.push(calljp(|jp| {
        a.push(3);
        1 + jump(jp, 5);
        a.push(4);
    }));
    a.push(calljp(|jp| {
        a.push(6);
        7
    }));
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
