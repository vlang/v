import nestedgenericbox

fn default_number() int {
	return -1
}

fn test_nested_generic_default_keeps_import_scope() {
	outer := nestedgenericbox.Outer{}
	assert outer.box.value == 42
}
