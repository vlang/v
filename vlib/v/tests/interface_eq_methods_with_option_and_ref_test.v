// for issue 19441
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
}
