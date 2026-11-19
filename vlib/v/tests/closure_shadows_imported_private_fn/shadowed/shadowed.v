module shadowed

fn blank(src string, ranges [][]int) string {
	return src + ranges.len.str()
}

pub fn use_shadowed() string {
	return blank('x', [][]int{})
}
