type BlockValue = int | map[string]BlockValue

struct BlockNumber {
mut:
	value int
}

fn block_map_leaf(mut source map[string]BlockValue) !map[string]BlockValue {
	mut current := unsafe { source }
	for part in ['first', 'second'] {
		value := current[part] or { panic(part) }
		if value !is map[string]BlockValue {
			return error(part)
		}
		current = unsafe { value }
	}
	return current
}

fn block_scalar_copy(mut value BlockNumber) BlockNumber {
	mut copy := unsafe { value }
	copy.value += 2
	return copy
}

fn block_pointer_copy(mut value &BlockNumber) &BlockNumber {
	copy := unsafe { value }
	return copy
}

fn test_unsafe_block_preserves_value_storage_and_pointer_depth() {
	mut number := BlockNumber{ value: 40 }
	assert block_scalar_copy(mut number).value == 42
	assert number.value == 40
	mut pointer := &number
	assert block_pointer_copy(mut pointer) == &number
	leaf := {
		'answer': BlockValue(42)
	}
	middle := {
		'second': BlockValue(leaf)
	}
	mut source := {
		'first': BlockValue(middle)
	}
	result := block_map_leaf(mut source)!
	answer := result['answer'] or { panic('missing answer') }
	assert answer == BlockValue(42)
	assert 'first' in source
}
