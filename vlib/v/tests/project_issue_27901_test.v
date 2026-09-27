struct Issue27901Inner {
	value int = 3
}

type Issue27901InnerPtr = &Issue27901Inner

struct Issue27901Outer {
	Issue27901InnerPtr
}

fn test_embedded_pointer_alias_field_access() {
	a := Issue27901Outer{
		value: 7
	}
	assert a.value == 7
}
