struct BranchReferenceValue {
mut:
	value int
}

fn read_branch_reference(value &BranchReferenceValue, extra int) int {
	return value.value + extra
}

fn read_branch_copy(value BranchReferenceValue, extra int) int {
	return value.value + extra
}

fn change_branch_value(mut value BranchReferenceValue) int {
	value.value = 42
	return 0
}

fn test_implicit_reference_preserves_caller_storage_before_later_branch() {
	mut value := BranchReferenceValue{ value: 1 }
	condition := true
	assert read_branch_reference(value, if condition { change_branch_value(mut value) } else { 0 }) == 42
	assert value.value == 42
}

fn test_value_argument_keeps_source_order_copy_before_later_branch() {
	mut value := BranchReferenceValue{ value: 1 }
	condition := true
	assert read_branch_copy(value, if condition { change_branch_value(mut value) } else { 0 }) == 1
	assert value.value == 42
}

fn read_branch_reference_with_prefix(prefix int, value &BranchReferenceValue, extra int) int {
	return prefix + value.value + extra
}

fn test_implicit_reference_block_preserves_earlier_argument_snapshot() {
	mut prefix := 1
	condition := true
	result := read_branch_reference_with_prefix(prefix, unsafe { BranchReferenceValue{ value: 2 } }, if condition {
		prefix = 10
		0
	} else {
		1
	})
	assert result == 3
	assert prefix == 10
}
