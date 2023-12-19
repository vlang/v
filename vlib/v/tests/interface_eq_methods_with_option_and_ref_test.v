// for issue 19441, 20212.
// The issue 19441 manifests itself in the following way:
// when the Mixin struct is present in the code, it causes a cgen error,
// and when the Mixin struct is not included, the eq method results causes the final assertion to fail
pub interface Iface {}

pub struct Derived {}

pub struct Struct {
	field ?&Iface
}

pub struct Mixin {
	Derived
	Struct
}

fn test_main() {
	mut arr := []&Iface{}
	arr << &Derived{}
	arr << &Derived{}
	assert arr[0] == arr[1]

	s1 := Iface(Struct{})
	s2 := Iface(Struct{})
	assert s1 == s2

	i1 := Iface(Mixin{})
	i2 := Iface(Mixin{})
	assert i1 == i2
}
