struct FixedStruct1 {
	a int
	b string
	c ?int
	d ?string
}

struct Encoder {}

struct OptionalPointerStringer {
	value string
}

fn (value &OptionalPointerStringer) str() string {
	return 'custom: ${value.value}'
}

fn optional_pointer_stringer() ?&OptionalPointerStringer {
	return &OptionalPointerStringer{
		value: 'value'
	}
}

fn test_main() {
	fixed := FixedStruct1{123, '456', 789, '321'}
	// this work well
	println(fixed.a.str())
	println(fixed.c?.str())

	println(fixed.b.int())
	println(fixed.d?.int())

	e := Encoder{}
	// this not work
	e.encode_struct(fixed)
}

fn test_custom_str_on_optional_pointer() {
	assert optional_pointer_stringer()?.str() == 'custom: value'
}

fn (e &Encoder) encode_struct[T](val T) {
	mut count := 0
	$for field in T.fields {
		mut value := val.$(field.name)
		$if field.is_option {
			if field.name in ['c', 'd'] {
				assert true
			}
			println('>> ${value?.str()}')
			println(val.$(field.name)?.str())
			count += 1
		}
	}
	assert count == 2
}
