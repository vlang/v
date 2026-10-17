// An `if`/`match` expression whose branches are `unsafe { nil }` or fn values
// can be stored in a fn-typed field or variable.
struct Machine {
	base int
}

type NativeFn = fn (mut m Machine) !int

type Callback = fn (x int) int

struct NativeFunction {
	behaviour NativeFn = unsafe { nil }
	name      string
}

struct Holder {
mut:
	cb Callback = unsafe { nil }
}

fn make_native(raw bool, behaviour NativeFn) &NativeFunction {
	return &NativeFunction{
		name:      'f'
		behaviour: if raw { unsafe { nil } } else { behaviour }
	}
}

fn answer(mut m Machine) !int {
	return 42 + m.base
}

fn double(x int) int {
	return x * 2
}

fn pick(kind int) Callback {
	mut cb := Callback(double)
	cb = match kind {
		0 { unsafe { nil } }
		else { double }
	}
	return cb
}

fn test_if_expr_with_unsafe_nil_in_fn_field() {
	native := make_native(false, answer)
	mut m := Machine{
		base: 1
	}
	assert native.behaviour(mut m)! == 43
	raw := make_native(true, answer)
	assert raw.behaviour == unsafe { nil }
	off := false
	h := Holder{
		cb: if off { unsafe { nil } } else { double }
	}
	assert h.cb(5) == 10
}

fn test_if_and_match_expr_with_unsafe_nil_in_fn_assignment() {
	on := true
	mut h := Holder{}
	h.cb = if on { Callback(double) } else { unsafe { nil } }
	assert h.cb(4) == 8
	mut f := Callback(double)
	f = if on { unsafe { nil } } else { double }
	assert f == unsafe { nil }
	assert pick(0) == unsafe { nil }
	assert pick(1)(3) == 6
}
