module ablank

fn blank(src string, ranges [][]int) string {
	return '${src}:${ranges.len}'
}

pub fn use_blank() string {
	return blank('x', [[1], [2, 3]])
}
