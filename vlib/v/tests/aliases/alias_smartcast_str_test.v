import os

type Value = int | i64 | string

type AliasValue = Value

type AliasOfAliasValue = AliasValue

fn captured_stdout(action fn ()) string {
	mut capture := os.stdio_capture() or { panic(err) }
	action()
	out, err := capture.finish()
	assert err.join('') == ''
	return out.join('')
}

fn narrowed_param_str(a AliasValue) string {
	if a is i64 {
		println(a)
		return '${a}'
	}
	return 'not an i64'
}

fn test_alias_sumtype_smartcast_str() {
	assert captured_stdout(fn () {
		a := AliasValue(Value(int(5)))
		if a is int {
			println(a)
			assert '${a}' == '5'
			assert '[${a:3}]' == '[  5]'
			assert a.str() == '5'
		} else {
			assert false
		}
		s := AliasValue(Value('abc'))
		if s is string {
			println(s)
			assert '${s}' == 'abc'
		} else {
			assert false
		}
		assert narrowed_param_str(AliasValue(Value(i64(6)))) == '6'
	}) == '5\nabc\n6\n'
}

fn test_alias_sumtype_smartcast_str_in_match() {
	assert captured_stdout(fn () {
		a := AliasValue(Value(i64(7)))
		match a {
			i64 {
				println(a)
				assert '${a}' == '7'
			}
			else {
				assert false
			}
		}
	}) == '7\n'
}

fn test_alias_of_alias_sumtype_smartcast_str() {
	assert captured_stdout(fn () {
		a := AliasOfAliasValue(AliasValue(Value(i64(8))))
		if a is i64 {
			println(a)
			assert '${a}' == '8'
		} else {
			assert false
		}
	}) == '8\n'
}

type NamedSum = int | string

fn (v NamedSum) str() string {
	return 'sum'
}

type NamedAlias = NamedSum

type GenericSum[T] = string | T

fn (v GenericSum[T]) str[T]() string {
	return 'generic sum'
}

type GenericSumAlias = GenericSum[int]

type GenericValue[T] = string | T

type GenericAlias[T] = GenericValue[T]

fn (v GenericAlias[T]) str[T]() string {
	return 'generic alias'
}

fn test_sum_str_methods_are_preserved() {
	assert captured_stdout(fn () {
		a := NamedAlias(NamedSum(5))
		if a is int {
			println(a)
			assert '${a}' == 'NamedAlias(sum)'
		} else {
			assert false
		}
		b := GenericSumAlias(GenericSum[int](6))
		if b is int {
			println(b)
			assert '${b}' == 'GenericSumAlias(generic sum)'
		} else {
			assert false
		}
		c := GenericAlias[int](GenericValue[int](7))
		if c is int {
			println(c)
			assert '${c}' == 'generic alias'
		} else {
			assert false
		}
	}) == 'NamedAlias(sum)\nGenericSumAlias(generic sum)\ngeneric alias\n'
}

type Tagged = Value

fn (t Tagged) str() string {
	return 'tagged'
}

struct TaggedHolder {
	a Tagged
}

fn test_alias_str_method_and_compound_operands_are_preserved() {
	assert captured_stdout(fn () {
		a := Tagged(Value(int(5)))
		if a is int {
			println(a)
			assert '${a}' == 'tagged'
		} else {
			assert false
		}
		h := TaggedHolder{Tagged(Value(int(5)))}
		if h.a is int {
			println(h.a)
			assert '${h.a}' == 'tagged'
		} else {
			assert false
		}
		arr := [Tagged(Value(int(5)))]
		if arr[0] is int {
			println(arr[0])
			assert '${arr[0]}' == 'tagged'
		} else {
			assert false
		}
	}) == 'tagged\ntagged\ntagged\n'
}

// Keep the old output on excluded pointer-backed paths. In particular, &nil is
// the current (incorrect) rendering of a non-null pointer holding integer zero,
// not an assertion that the pointer is actually null.
fn test_pointer_and_mutable_loop_printing_are_preserved() {
	assert captured_stdout(fn () {
		a := &AliasValue(Value(int(0)))
		if a is int {
			println(a)
		} else {
			assert false
		}
		mut values := [&AliasValue(Value(int(5)))]
		for mut item in values {
			if mut item is int {
				println(item)
			} else {
				assert false
			}
		}
	}) == '&nil\nAliasValue(Value(5))\n'
}
