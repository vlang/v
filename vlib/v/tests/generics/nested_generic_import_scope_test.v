import nestedgenericbox

fn default_number() int {
	return -1
}

fn test_nested_generic_default_keeps_import_scope() {
	outer := nestedgenericbox.Outer{}
	assert outer.box.value == 42
}

fn test_runtime_array_generic_defaults_keep_import_scope() {
	count := 2
	boxes := []nestedgenericbox.Box[int]{len: count}
	assert boxes[0].value == 42
	assert boxes[1].value == 42
	outers := []nestedgenericbox.Outer{len: count}
	assert outers[0].box.value == 42
	assert outers[1].box.value == 42
}
