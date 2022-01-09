[flag]
pub enum Fill {
	crash
}

fn font_path() string {
	$if dragonfly {
		fonts := ['test', 'test/2']
		if true {
			return fonts[0]
		}
	}
	return ''
}

fn test_compilation() {
	a := Fill.crash
	println(a)
	assert true
}
