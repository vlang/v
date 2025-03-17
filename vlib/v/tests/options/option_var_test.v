import time

type MyAlias = []int
type MySumType = []f64 | []int

struct Test {
}

struct StructType2 {
	a ?time.Time
}

struct StructType {
mut:
	a string
	b ?int
	c ?f64
	d ?[]string
	e ?MyAlias
	f ?MySumType
}

struct Decoder {}

fn (d &Decoder) decode[T](mut typ T) T {
	$for field in T.fields {
		$if field.is_option {
			if typ.$(field.name) != none {
				dump(typ.$(field.name) ?.str())
			}
			typ.$(field.name) = none
		}
	}
	return typ
}

fn test_comptime() {
	d := Decoder{}
	result := d.decode(mut StructType{
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
	assert x == none
	dump(x)
}

fn test_assign_from_option() {
	mut x := ?int(123)
	mut y := x
	println(y)
	assert x != none
	assert y != none
	assert x? == 123
	assert y? == 123
}

fn test_blank_assign() {
	_ := ?bool(false)
}

fn test_optional_value_assign() {
	x := ?int(0)
	assert x != none
	assert x? == 0
}

fn test_assert_initialized() {
	mut x := ?int(1)
	mut y := ?int(1)
	assert x != none
	assert y != none
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

	assert var? == 1
	assert var2? == 2
	assert var3? == 3

	assert var != none
	assert var2 != none
	assert var3 != none
}

fn test_as_cast() {
	var := StructType2{}
	b := var.a as ?time.Time
}

fn test_unwrap() {
	var := ?int(1)
	println(var)
	assert var != none
	assert var? == 1

	var2 := var? + 1
	println(var2)

	assert var2 == 2
}

fn test_or_block() {
	var1 := ?int(none)
	var2 := var1 or { 0 }
	var3 := var1 or { 1 }
	assert var2 + var3 == 1
	var4 := var1 or {
		t := 1 + var3
		t
	}

	assert var4 == 2
}

fn test_default_values() {
	var_none := ?f64(none)
	var_none2 := ?string(none)

	var2 := var_none or { 1.0 }
	println(var2)
	assert var2 == 1.0

	var3 := var_none2 or { 'foo' }
	println(var3)
	assert var3 == 'foo'
}

fn test_assert_option() {
	var1 := ?int(none)
	varz := ?f64(none)
	assert var1 == none

	var2 := var1 or { 1 }

	assert var1 == none
	assert var2 == 1

	println(var2)

	var3 := varz or { 0.0 }
	assert var3 == 0

	var4 := varz
	assert var4 == none
}

fn test_opt_assign() {
	mut var1 := ?int(none)
	assert var1 == none
	var1 = 1
	assert var1 != none
	var2 := var1
	assert var2? == var1?
}

fn test_opt_none() {
	mut t1 := ?int(none)
	t2 := ?int(none)

	t1 = t2
	assert t1 == none
	assert t2 == none

	mut t3 := ?Test{}
	t4 := t3
	assert t4 == none
	assert t3 == none

	mut t5 := ?map[string]string{}
	t7 := {
		'foo': 'bar'
	}
	t6 := if t5 != none { t5.clone() } else { t7 }
	assert t5 == none
	assert t6.len == 1
}
