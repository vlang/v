import v.util

@[noreturn]
fn exit_inner() {
	exit(1)
}

@[noreturn]
fn exit_outer() {
	exit_inner()
}

@[noreturn]
fn imported_exit_outer() {
	util.verror('test error', 'stop')
}

fn test_noreturn_function_can_delegate_to_another_noreturn_function() {
	assert true
}
