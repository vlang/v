struct StructType {
mut:
	a string
	b ?int
	//	c ?int
	//	d ?f64
	//	e ?[]string
}

struct Decoder {}

fn (d &Decoder) decode[T](typ T) T {
	$if T is $Struct {
		dump('is Struct')
		$for field in T.fields {
			$if field.is_option {
				dump(typ.$(field.name) ?.str())
				typ.$(field.name) = none
				dump(typ.$(field.name) ?.str())
			}
		}
	}
	return typ
}

fn test_comptime() {
	d := Decoder{}
	result := d.decode(StructType{
		a: 'foo'
		b: 3
	})
	println(result)
}

fn test_cast_option() {
	mut x := ?int(123)
	dump(x)
	assert x != none
	x = none
	dump(x)
}

fn test_assign_from_option() {
	mut x := ?int(123)
	mut y := x
	println(y)
	assert x != none
	assert y != none
}

fn test_blank_assign() {
	_ := ?bool(false)
}

fn test_optional_value_assign() {
	x := ?int(0)
	assert x != none
	assert x == 0
}

fn test_assert_initialized() {
	mut x := ?int(1)
	mut y := ?int(1)
	assert x == x
	assert x == y
	assert x == 1
	assert y == 1
}

fn test_comptime_checks() {
	val := StructType{
		a: 'foo'
		b: 3
	}
	$for field in StructType.fields {
		value := val.$(field.name)
		$if field.is_option {
			var := val.$(field.name)
			var2 := var
			assert var == var2
			assert value == var
		}
	}
}
