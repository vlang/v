module main

[heap]
pub struct Grid {
pub mut:
	header string
}

fn test_for_in_mut_string() {
	h := 'yo'

	mut grid := Grid{
		header: h
	}
	wrap_text(mut grid)
}

fn wrap_text(mut gv Grid) {
	mut results := []byte{}
	for mut ch in gv.header {
		println(ch)
		results << ch
	}
	println(results)
	assert results.len == 2
	assert results[0] == `y`
	assert results[1] == `o`
}
