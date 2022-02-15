struct Info {
	name  string
	notes shared []string
	maps  map[int]int
	info  []SubInfo
}

struct SubInfo {
	path  string
	files []string
}

fn test_autogen_free() {
	info := &Info{}
	info.free()
	assert true
}

fn test_multiple_autogen_free() {
	info := &Info{}
	info.free()
	assert true
}
