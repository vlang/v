// for issue 19441, 20212.
// The issue 19441 manifests itself in the following way:
// when the Mixin struct is present in the code, it causes a cgen error,
// and when the Mixin struct is not included, the eq method results causes the final assertion to fail
pub interface Iface {}

// test ref
pub struct Derived {
	field1 &Iface = unsafe { nil }
}

// test option and ref
pub struct Struct {
	field2 ?&Iface
}

// test non-ref and embedded
pub struct Mixin {
	Derived
	Struct
	field3 Iface = Iface(1)
	field4 ?Iface
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
