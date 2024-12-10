struct FixedStruct1 {
	a int
	b string
	c ?int
	d ?string
}

struct Encoder {}

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

fn (e &Encoder) encode_struct[T](val T) {
	mut count := 0
	$for field in T.fields {
		mut value := val.$(field.name)
		$if field.is_option {
			if field.name in ['c', 'd'] {
				assert true
			}
			println('>> ${value ?.str()}')
			println(val.$(field.name) ?.str())
			count += 1
		}
	}
	assert count == 2
}
