interface IAbc {
	xyz()
}

struct Abc {}

fn (a Abc) xyz() {}

fn f(i &IAbc) string {
	return '${i}'
}

fn test_passing_voidptr_as_an_interface_reference() {
	i := IAbc(Abc{})
	assert f(&i) == '&IAbc(Abc{})'
	// a voidptr() cast is an escape hatch, that should be allowed
	// but perhaps it should be forced by the compiler to be in unsafe{}
	assert f(unsafe { voidptr(u64(0)) }) == '&nil'
}

interface IActionable24309 {
mut:
	count int
}

struct Stack24309 {
mut:
	count int = 4
}

fn (mut s Stack24309) inc() {
	s.count++
}

type ActionFn24309 = fn (item &IActionable24309)

struct Action24309 {
	actionable &IActionable24309
	action_fn  ActionFn24309 = unsafe { nil }
}

struct EventManager24309 {
mut:
	receivers map[string][]Action24309
}

fn stack_on_test_24309(item &IActionable24309) {
	mut stack := unsafe { &Stack24309(item) }
	stack.inc()
}

fn (mut em EventManager24309) on_test_24309() {
	unsafe {
		for receiver in em.receivers['test'] {
			receiver.action_fn(receiver.actionable)
		}
	}
}

fn test_pointer_to_interface_cast_uses_underlying_object_pointer() {
	stack := Stack24309{}
	mut em := EventManager24309{}
	action := Action24309{
		actionable: &stack
		action_fn:  stack_on_test_24309
	}
	unsafe { em.receivers['test'] << action }
	em.on_test_24309()
	em.on_test_24309()
	em.on_test_24309()
	assert stack.count == 7
}
