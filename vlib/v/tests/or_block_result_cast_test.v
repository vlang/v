type MyF64 = f64

struct SelectorCastHolder {
	x  ?f64
	ax ?MyF64
}

struct GenericSelectorCastHolder[T] {
	x ?T
}

fn result_f64() !f64 {
	return error('no f64')
}

fn result_f32() !f32 {
	return error('no f32')
}

fn option_f64() ?f64 {
	return none
}

fn result_my_f64() !MyF64 {
	return error('no MyF64')
}

fn option_my_f64() ?MyF64 {
	return none
}

fn result_generic[T]() !T {
	return error('no generic')
}

fn cast_or_block_return() int {
	return int(result_f64() or { return 7 })
}

fn keep_none() ?f64 {
	return option_f64() or { none }
}

fn keep_error() !f64 {
	return result_f64() or { error('fallback') }
}

fn selector_cast_return(h SelectorCastHolder) int {
	return int(h.x or { return 7 })
}

fn selector_keep_none(h SelectorCastHolder) ?f64 {
	return h.x or { none }
}

fn selector_keep_error(h SelectorCastHolder) !f64 {
	return h.x or { error('fallback') }
}

fn test_or_block_result_cast() {
	a := int(result_f64() or { 256.0 })
	b := int(result_f64() or { f64(256.0) })
	c := int(result_f32() or { f32(42.0) })
	d := int(option_f64() or { 128.0 })
	e := int(option_f64() or { f64(128.0) })

	assert a == 256
	assert b == 256
	assert c == 42
	assert d == 128
	assert e == 128
}

fn test_or_block_result_cast_alias_payload() {
	a := int(result_my_f64() or { MyF64(64.0) })
	b := int(option_my_f64() or { MyF64(32.0) })

	assert a == 64
	assert b == 32
}

fn test_or_block_result_cast_generic_call() {
	a := int(result_generic[f64]() or { f64(16.0) })

	assert a == 16
}

fn test_or_block_cast_preserves_parent_exits() {
	assert cast_or_block_return() == 7
	assert keep_none() == none

	if _ := keep_error() {
		assert false
	} else {
		assert err.msg() == 'fallback'
	}
}

fn test_or_block_selector_cast() {
	h := SelectorCastHolder{}
	gh := GenericSelectorCastHolder[f64]{}

	a := int(h.x or { 3.0 })
	b := int(h.x or { f64(4.0) })
	c := int(h.ax or { MyF64(5.0) })
	d := int(gh.x or { f64(6.0) })

	assert a == 3
	assert b == 4
	assert c == 5
	assert d == 6
}

fn test_or_block_selector_cast_preserves_parent_exits() {
	h := SelectorCastHolder{}
	assert selector_cast_return(h) == 7
	assert selector_keep_none(h) == none

	if _ := selector_keep_error(h) {
		assert false
	} else {
		assert err.msg() == 'fallback'
	}
}

fn test_or_block_result_void_context() {
	mut seen := false
	result_f64() or { seen = true }
	assert seen

	result_f64() or {}
}
