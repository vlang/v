@[has_globals]
module main

import v.debug

__global traced = []string{}

fn C.abs(x int) int

// abs shares its name with the C function, but returns a different type.
fn abs(x int) string {
	return 'abs:${C.abs(x)}'
}

struct Callbacks {
	double fn (int) int = unsafe { nil }
}

fn twice(x int) int {
	return x * 2
}

fn record_call(fn_name string) {
	traced << fn_name
}

fn test_traced_calls_keep_their_types() {
	traced = []string{}
	hook := debug.add_before_call(record_call)
	// A module function keeps its own return type next to a same-named C function.
	text := abs(-3)
	// A builtin intrinsic and an fn-valued field are not declared functions.
	mut values := ['a', 'b']
	last := values.pop()
	callbacks := Callbacks{
		double: twice
	}
	doubled := callbacks.double(21)
	debug.remove_before_call(hook)
	assert text == 'abs:3'
	assert last == 'b'
	assert values == ['a']
	assert doubled == 42
	$if trace ? {
		assert traced == ['abs']
	} $else {
		assert traced == []string{}
	}
}
