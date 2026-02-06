fn separate_wires(coo_adj_wires [][2]u32, id u64) [][2]u32 {
	return [][2]u32{len: coo_adj_wires.len, init: coo_adj_wires[index]}
}

fn test_main() {
	t := separate_wires([[u32(1), 2]!], 0)
	assert t.str() == '[[1, 2]]'
}
