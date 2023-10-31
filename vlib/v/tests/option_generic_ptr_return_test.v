struct ObjectDesc {
	typ u32
	ptr voidptr
}

struct ABCD {}

pub fn cast_object_desc[H](desc &ObjectDesc) ?H {
	$if H is &ABCD {
		if desc.typ == 12 { // desc == ABCD
			return unsafe { &ABCD(desc.ptr) }
		}
	}
	return none
}

fn test_main() {
	obj := ABCD{}
	a := cast_object_desc[&ABCD](ObjectDesc{ typ: 12, ptr: voidptr(&obj) })
	dump(a)
	assert *a? == obj
}
