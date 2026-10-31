import local_fn_value_callee_mod

struct ArgumentsCallback {
	call_with_arguments ?fn ([]int) int
}

fn sum_arguments(arguments []int) int {
	mut total := 0
	for argument in arguments {
		total += argument
	}
	return total
}

fn run_callback(callback ArgumentsCallback, arguments []int) int {
	if call := callback.call_with_arguments {
		return call(arguments)
	}
	return -1
}

fn test_local_fn_value_named_like_an_imported_fn_is_called_by_value() {
	mut vm := local_fn_value_callee_mod.VM{}
	result := local_fn_value_callee_mod.call(mut vm, 1, [1, 2]) or { 0 }
	assert result == 3
	assert vm.calls == 1
	assert run_callback(ArgumentsCallback{ call_with_arguments: sum_arguments }, [1, 2, 3]) == 6
	assert run_callback(ArgumentsCallback{}, [1]) == -1
}
