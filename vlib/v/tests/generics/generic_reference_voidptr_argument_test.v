struct VoidptrNode {
	value int
}

fn voidptr_identity[T](x T) T {
	return x
}

fn voidptr_nil_ref[T]() &T {
	return unsafe { nil }
}

fn test_generic_reference_result_passes_its_value_to_voidptr_parameter() {
	nil_node := unsafe { &VoidptrNode(nil) }
	assert isnil(voidptr_identity[&VoidptrNode](unsafe { nil }))
	assert isnil(voidptr_identity(nil_node))
	assert isnil(voidptr_nil_ref[VoidptrNode]())
	node := &VoidptrNode{ value: 7 }
	assert !isnil(voidptr_identity(node))
	assert voidptr(voidptr_identity(node)) == voidptr(node)
}
