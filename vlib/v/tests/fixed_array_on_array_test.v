fn separate_wires(coo_adj_wires [][2]u32, id u64) [][2]u32 {
	return [][2]u32{len: coo_adj_wires.len, init: coo_adj_wires[index]}
}

fn fixed_array_len(mut arr []int) int {
	return arr.len
}

fn fixed_array_generic_len[T](mut arr []T) int {
	return arr.len
}

fn fixed_array_generic_dispatch[T](mut val T) int {
	$if T is $array_fixed {
		return fixed_array_generic_len(mut val)
	}
	return -1
}

fn test_main() {
	t := separate_wires([[u32(1), 2]!], 0)
	assert t.str() == '[[1, 2]]'
}

fn test_fixed_array_can_be_passed_to_array_param() {
	mut fixed := [3]int{}
	assert fixed_array_len(mut fixed) == 3
}

fn test_fixed_array_can_be_passed_to_generic_array_param() {
	mut fixed := [3]int{}
	assert fixed_array_generic_dispatch(mut fixed) == 3
}
