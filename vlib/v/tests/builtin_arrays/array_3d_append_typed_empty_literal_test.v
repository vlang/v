struct AppendNode {
	n int
}

fn test_3d_append_typed_empty_array_literal() {
	mut runs := [][][]AppendNode{len: 1, init: [][]AppendNode{len: 1}}
	runs << [[]AppendNode{}]
	assert runs.len == 2
	assert runs[1].len == 1
	assert runs[1][0].len == 0
	// the `<< [][]T{len: 1}` workaround shape keeps working
	runs << [][]AppendNode{len: 1}
	assert runs.len == 3
	assert runs[2].len == 1
}
