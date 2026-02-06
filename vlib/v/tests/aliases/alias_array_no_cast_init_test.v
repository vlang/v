pub type Labels = [][]int

pub fn new_labels(width int, height int) Labels {
	mut labels := Labels{len: height, init: []int{len: width}}
	return labels
}

fn test_alias_array_no_cast_init() {
	mut labels := new_labels(2, 2)
	println(labels)
	assert labels == [[0, 0], [0, 0]]
}
