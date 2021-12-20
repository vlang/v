fn returns_sumtype() int | string {
	return 1
}

fn test_stringification() {
	x := returns_sumtype()
	assert '$x' == '(int | string)(1)'
}

struct Milk {
	egg string | int
}

fn test_struct_with_inline_sumtype() {
	m := Milk{
		egg: 1
	}
	assert m.egg is int
}

interface IMilk {
	egg string | int
}

fn receive_imilk(milk IMilk) {}

fn test_interface_with_inline_sumtype() {
	m := Milk{
		egg: 1
	}
	receive_imilk(m)
}

fn returns_sumtype_in_multireturn() (int | string, string) {
	return 1, ''
}

fn test_multireturn() {
	x, y := returns_sumtype_in_multireturn()
	assert '$x' == '(int | string)(1)'
	assert '$y' == ''
}

fn returns_optional_inline_sumtype() ?string | int {
	return none
}

fn test_optional_inline_sumtype() {
	x := returns_optional_inline_sumtype() or { 100 }
	assert x is int
	assert (x as int) == 100
}
