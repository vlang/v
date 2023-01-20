import time

type MyAlias = []int
type MySumType = []f64 | []int

struct StructType2 {
	a ?time.Time = none
}

struct StructType {
mut:
	a string
	b ?int
	c ?f64
	d ?u8
	e ?[]string
	f ?MyAlias
	g ?MySumType
}

struct Decoder {}

fn (d &Decoder) decode[T](typ T) T {
	$for field in T.fields {
		$if field.is_option {
			dump(typ.$(field.name) ?.str())
			typ.$(field.name) = none
			dump(typ.$(field.name) ?.str())
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
	assert x == int(0)
}

fn test_assert_initialized() {
	mut x := ?int(1)
	mut y := ?int(1)
	assert x == int(1)
	assert y == int(1)
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
			dump(var)
			dump(var2)
		}
	}
}

fn test_none_initialization() {
	mut var := ?int(none)
	mut var2 := ?int(none)
	mut var3 := ?int(none)

	assert var == none
	assert var2 == none
	assert var3 == none

	var = 1
	var2 = 2
	var3 = 3

	assert var == int(1)
	assert var2 == int(2)
	assert var3 == int(3)
}

fn test_as_cast() {
	var := StructType2{}
	b := var.a as ?time.Time
}
