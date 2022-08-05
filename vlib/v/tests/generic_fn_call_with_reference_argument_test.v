struct Type<T> {
	value T
}

struct ContainerType<T> {
	typ &Type<T>
}

fn (instance &ContainerType<T>) contains(typ &Type<T>) bool {
	println(typ)
	if instance.typ == typ {
		return true
	} else {
		return false
	}
}

fn test_generic_fn_call_with_reference_argument() {
	con := ContainerType<int>{
		typ: &Type<int>{0}
	}
	ret := con.contains(con.typ)
	println(con)
	assert ret
}
