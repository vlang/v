module main

[heap]
pub struct Grid {
pub mut:
	header []string
}

fn test_string_index_in_for_mut_in() {
	h := ['yore', 'yaya']

	mut grid := Grid{
		header: h
	}
	wrap_text(mut grid)
}

fn wrap_text(mut gv Grid) {
	for mut ch in gv.header {
		ch = ch[1..2]
	}

	println(gv)
	assert gv.header == ['o', 'a']
}
