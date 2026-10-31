type GenericFn[T] = fn (s string) !T

type GenericOpt[T] = fn (s string) ?T

type GenericPlain[T] = fn (s string) T

type IntFn = fn (s string) !int

struct Item {
	name string
	size int
}

struct Holder[T] {
	cb GenericFn[T] = unsafe { nil }
}

fn call_propagate[T](s string, cb GenericFn[T]) !T {
	return cb[T](s)!
}

fn call_propagate_without_type_args[T](s string, cb GenericFn[T]) !T {
	return cb(s)!
}

fn call_propagate_to_local[T](s string, cb GenericFn[T]) !T {
	value := cb[T](s)!
	return value
}

fn call_or_default[T](s string, cb GenericFn[T]) T {
	return cb[T](s) or { T{} }
}

fn call_or_return_err[T](s string, cb GenericFn[T]) !T {
	value := cb[T](s) or { return err }
	return value
}

fn call_forward_result[T](s string, cb GenericFn[T]) !T {
	return cb[T](s)
}

fn call_option_or[T](s string, cb GenericOpt[T]) T {
	return cb[T](s) or { T{} }
}

fn call_option_propagate[T](s string, cb GenericOpt[T]) ?T {
	value := cb[T](s)?
	return value
}

fn call_plain[T](s string, cb GenericPlain[T]) T {
	return cb[T](s)
}

fn call_inline_fn_type[T](s string, cb fn (string) !T) !T {
	return cb[T](s)!
}

fn call_through_local[T](s string, cb GenericFn[T]) !T {
	f := cb
	return f[T](s)!
}

fn call_in_closure[T](s string, cb GenericFn[T]) !T {
	inner := fn [cb] [T](s string) !T {
		return cb[T](s)!
	}
	return inner(s)!
}

fn (i Item) call_method[T](cb GenericFn[T]) !T {
	return cb[T](i.name)!
}

fn call_field_propagate[T](s string, h Holder[T]) !T {
	return h.cb[T](s)!
}

fn call_field_to_local[T](s string, h &Holder[T]) !T {
	value := h.cb[T](s)!
	return value
}

fn call_field_or_default[T](s string, h Holder[T]) T {
	return h.cb[T](s) or { T{} }
}

fn (h Holder[T]) run(s string) !T {
	return h.cb[T](s)!
}

fn call_int_fn[T](s string, cb IntFn) !int {
	return cb(s)!
}

fn call_int_fn_non_generic(s string, cb IntFn) !int {
	return cb(s)!
}

fn str_len(s string) !int {
	if s == '' {
		return error('empty')
	}
	return s.len
}

fn make_item(s string) !Item {
	if s == '' {
		return error('empty')
	}
	return Item{
		name: s
		size: s.len
	}
}

fn maybe_upper(s string) ?string {
	if s == '' {
		return none
	}
	return s.to_upper()
}

fn test_result_fn_value_with_explicit_type_args() {
	cb := fn (s string) !int {
		return s.len
	}
	assert call_propagate[int]('hello', cb)! == 5
	assert call_propagate_without_type_args[int]('hello', cb)! == 5
	assert call_propagate_to_local[int]('hello', cb)! == 5
	assert call_or_default[int]('hello', cb) == 5
	assert call_or_return_err[int]('hello', cb)! == 5
	assert call_forward_result[int]('hello', cb)! == 5
	assert call_inline_fn_type[int]('hello', cb)! == 5
	assert call_through_local[int]('hello', cb)! == 5
	assert call_in_closure[int]('hello', cb)! == 5
	assert Item{
		name: 'hello'
	}.call_method[int](cb)! == 5
}

fn test_result_fn_value_errors_propagate() {
	assert call_or_default[int]('', str_len) == 0
	if _ := call_propagate[int]('', str_len) {
		assert false
	} else {
		assert err.msg() == 'empty'
	}
	if _ := call_propagate_to_local[int]('', str_len) {
		assert false
	} else {
		assert err.msg() == 'empty'
	}
	if _ := call_or_return_err[int]('', str_len) {
		assert false
	} else {
		assert err.msg() == 'empty'
	}
	if _ := call_forward_result[int]('', str_len) {
		assert false
	} else {
		assert err.msg() == 'empty'
	}
}

fn test_named_fn_as_generic_fn_value() {
	assert call_propagate[int]('hello', str_len)! == 5
	assert call_propagate_to_local[int]('hi', str_len)! == 2
	assert call_or_default[int]('abc', str_len) == 3
}

fn test_string_and_struct_type_args() {
	to_upper := fn (s string) !string {
		return s.to_upper()
	}
	assert call_propagate[string]('abc', to_upper)! == 'ABC'
	assert call_propagate_to_local[string]('abc', to_upper)! == 'ABC'
	assert call_or_default[string]('abc', to_upper) == 'ABC'
	item := call_propagate[Item]('abcd', make_item)!
	assert item.name == 'abcd'
	assert item.size == 4
	assert call_propagate_to_local[Item]('xy', make_item)!.size == 2
	assert call_or_default[Item]('', make_item) == Item{}
}

fn test_option_fn_value_with_explicit_type_args() {
	assert call_option_or[string]('abc', maybe_upper) == 'ABC'
	assert call_option_or[string]('', maybe_upper) == ''
	assert call_option_propagate[string]('abc', maybe_upper)? == 'ABC'
	if _ := call_option_propagate[string]('', maybe_upper) {
		assert false
	}
	half := fn (s string) ?int {
		if s.len % 2 != 0 {
			return none
		}
		return s.len / 2
	}
	assert call_option_or[int]('abcd', half) == 2
	assert call_option_or[int]('abc', half) == 0
}

fn test_plain_fn_value_with_explicit_type_args() {
	double := fn (s string) string {
		return s + s
	}
	assert call_plain[string]('ab', double) == 'abab'
	assert call_plain[int]('abc', fn (s string) int {
		return s.len * 2
	}) == 6
}

fn test_fn_value_field_with_explicit_type_args() {
	h := Holder[int]{
		cb: str_len
	}
	assert call_field_propagate[int]('hello', h)! == 5
	assert call_field_to_local[int]('abc', &h)! == 3
	assert call_field_or_default[int]('abcd', h) == 4
	assert call_field_or_default[int]('', h) == 0
	assert h.run('ab')! == 2
	if _ := call_field_propagate[int]('', h) {
		assert false
	} else {
		assert err.msg() == 'empty'
	}
	items := Holder[Item]{
		cb: make_item
	}
	assert call_field_propagate[Item]('xyz', items)!.size == 3
	assert items.run('ab')!.name == 'ab'
}

fn test_non_generic_fn_alias() {
	cb := fn (s string) !int {
		return s.len
	}
	assert call_int_fn[string]('hello', cb)! == 5
	assert call_int_fn_non_generic('hello', cb)! == 5
	assert call_int_fn_non_generic('abc', str_len)! == 3
}
