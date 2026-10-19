module local_fn_value_callee_mod

pub struct VM {
pub mut:
	calls int
}

// call has the same name as the fn values that callers name `call`; its first
// parameter is `mut`, which a call through such a fn value must not adopt.
pub fn call(mut vm VM, function int, arguments []int) !int {
	vm.calls++
	return function + arguments.len
}
