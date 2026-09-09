fn type_name_of[U]() string {
	return typeof[U]().name
}

fn pointee_name[T](val T) string {
	_ = val
	$if T is &V {
		return type_name_of[V]()
	} $else {
		return typeof[T]().name
	}
}

fn test_comptime_if_pointer_binding() {
	x := 123
	px := &x
	ppx := &px
	assert pointee_name(px) == 'int'
	assert pointee_name(ppx) == '&int'
}
