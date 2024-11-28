struct ObjectDesc {
	typ u32
	ptr voidptr
}

struct ABCD {
	name string
}

pub fn cast_object_desc[H](desc &ObjectDesc) ?H {
	$if H is &ABCD {
		/*
		if desc.typ == 12 { // desc == ABCD
			return &ABCD(desc.ptr)
		}*/
		if desc.typ == 12 { // desc == ABCD
			return ?&ABCD(unsafe { &ABCD(desc.ptr) })
		}
	}
	return none
}

fn test_option_ptr() {
	obj := ABCD{
		name: 'Foo'
	}
	desc := ObjectDesc{
		typ: 12
		ptr: voidptr(&obj)
	}
	obj2 := cast_object_desc[&ABCD](desc) or { panic('wwww') }
	// obj2 := &ABCD(desc.ptr)
	assert obj2.name == 'Foo'
}
