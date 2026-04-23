interface Depends {
	depends() []Depends
}

struct Signal[T] {
}

fn (x Signal[T]) depends() []Depends {
	return []
}

struct Add[T] {
	a Signal[T]
	b Signal[T]
}

fn (a Add[T]) depends() []Depends {
	return [a.a, a.b]
}

fn test_generics_interface_decl() {
	assert true
}

type Action[A, B] = fn (v1 A, v2 B)

interface PropWithListener[T] {
	listen_state(action Action[T, bool])
}

fn noop_string_state_action(_ string, _ bool) {}

fn test_generics_interface_decl_with_fn_type_concrete_param() {
	action := Action[string, bool](noop_string_state_action)
	action('ready', true)
	assert true
}
