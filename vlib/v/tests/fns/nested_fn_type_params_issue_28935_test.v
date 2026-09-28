// Function types that take (or return) other multi-parameter function types
// must keep their exact C signature, see issue #28935.
pub type Inner = fn (a int, b int) int

pub type Outer = fn (x int, cb Inner, p voidptr) int

type OuterInline = fn (x int, cb fn (int, int) int, p voidptr) int

type InnerFirst = fn (cb fn (int, int) int, x int) int

type InnerLast = fn (x int, cb fn (int, int) int) int

type Deep = fn (cb fn (x fn (int, int) int, y int) int, z int) int

type ResultCb = fn (cb fn (int, int) !int, x int) !int

type OptionRet = fn (a int, b int) ?fn (int, int) int

type ReturnsFn = fn (a int, b int) Inner

type MakerParam = fn (mk fn (int, int) Inner, x int) int

type CollectionCb = fn (cb fn ([]int, map[string]int) int, x int) int

struct Holder {
	outer  Outer @[required]
	inline fn (x int, cb fn (int, int) int, p voidptr) int @[required]
}

fn add(a int, b int) int {
	return a + b
}

fn mul(a int, b int) int {
	return a * b
}

fn real(x int, cb Inner, p voidptr) int {
	extra := if p == unsafe { nil } { 0 } else { 1000 }
	return x + cb(x, 10) + extra
}

fn real_inline(x int, cb fn (int, int) int, p voidptr) int {
	extra := if p == unsafe { nil } { 0 } else { 1000 }
	return x + cb(x, 10) + extra
}

fn inner_first(cb fn (int, int) int, x int) int {
	return cb(x, 3)
}

fn inner_last(x int, cb fn (int, int) int) int {
	return cb(4, x)
}

fn deep(cb fn (x fn (int, int) int, y int) int, z int) int {
	return cb(mul, z)
}

fn deep_cb(x fn (int, int) int, y int) int {
	return x(y, y)
}

fn checked_div(a int, b int) !int {
	if b == 0 {
		return error('division by zero')
	}
	return a / b
}

fn result_cb(cb fn (int, int) !int, x int) !int {
	return cb(100, x)!
}

fn option_ret(a int, b int) ?fn (int, int) int {
	if a == b {
		return none
	}
	return if a < b { add } else { mul }
}

fn returns_fn(a int, b int) fn (int, int) int {
	return if a < b { add } else { mul }
}

fn maker_param(mk fn (int, int) Inner, x int) int {
	f := mk(1, 2)
	return f(x, x)
}

fn collection_cb(cb fn ([]int, map[string]int) int, x int) int {
	return cb([x, x], {
		'x': x
	})
}

fn sum_all(a []int, m map[string]int) int {
	mut total := 0
	for v in a {
		total += v
	}
	for _, v in m {
		total += v
	}
	return total
}

fn apply_generic[T](cb fn (fn (T, T) T, T) T, inner fn (T, T) T, x T) T {
	return cb(inner, x)
}

fn twice_generic[T](f fn (T, T) T, x T) T {
	return f(x, x)
}

fn test_issue_repro_local() {
	f := Outer(real)
	assert f(1, add, unsafe { nil }) == 12
	assert f(2, mul, unsafe { nil }) == 22
	mut x := 7
	assert f(3, add, &x) == 1016
}

fn test_struct_fields() {
	h := Holder{
		outer:  real
		inline: real_inline
	}
	assert h.outer(1, add, unsafe { nil }) == 12
	assert h.inline(2, mul, unsafe { nil }) == 22
}

fn test_inline_inner_param_positions() {
	f := OuterInline(real_inline)
	assert f(5, add, unsafe { nil }) == 20
	first := InnerFirst(inner_first)
	assert first(add, 4) == 7
	assert first(mul, 4) == 12
	last := InnerLast(inner_last)
	assert last(5, add) == 9
	assert last(5, mul) == 20
}

fn test_two_levels_deep() {
	d := Deep(deep)
	assert d(deep_cb, 6) == 36
	anon := fn (cb fn (x fn (int, int) int, y int) int, z int) int {
		return cb(add, z)
	}
	assert anon(deep_cb, 6) == 12
}

fn test_result_param_and_return() {
	f := ResultCb(result_cb)
	assert f(checked_div, 4)! == 25
	if _ := f(checked_div, 0) {
		assert false
	} else {
		assert err.msg() == 'division by zero'
	}
}

fn test_fn_returning_option_and_plain_fn() {
	o := OptionRet(option_ret)
	if g := o(1, 2) {
		assert g(3, 4) == 7
	} else {
		assert false
	}
	if g := o(3, 2) {
		assert g(3, 4) == 12
	} else {
		assert false
	}
	if _ := o(2, 2) {
		assert false
	}
	r := ReturnsFn(returns_fn)
	assert r(1, 2)(5, 6) == 11
	assert r(2, 1)(5, 6) == 30
	m := MakerParam(maker_param)
	assert m(returns_fn, 9) == 18
}

fn test_collection_params() {
	f := CollectionCb(collection_cb)
	assert f(sum_all, 5) == 15
}

fn test_generic_nested_fn_params() {
	assert apply_generic[int](twice_generic[int], add, 8) == 16
	assert apply_generic[int](twice_generic[int], mul, 8) == 64
	assert apply_generic[f64](twice_generic[f64], fn (a f64, b f64) f64 {
		return a + b
	}, 1.5) == 3.0
}

fn test_closure_capturing_nested_fn_value() {
	base := 100
	f := Outer(real)
	wrapped := fn [f, base] (x int) int {
		return f(x, add, unsafe { nil }) + base
	}
	assert wrapped(1) == 112
}
