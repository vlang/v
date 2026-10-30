module strings

fn test_tabwriter_basic() {
	mut w := TabWriter{}
	w.write_row(['name', 'age'])
	w.write_row(['alice', '30'])
	w.write_row(['bob', '5'])
	out := w.render()
	lines := out.split('\n')
	assert lines.len == 3
	assert lines[0] == 'name  age'
	assert lines[1] == 'alice 30'
	assert lines[2] == 'bob   5'
}

fn test_tabwriter_reset() {
	mut w := TabWriter{}
	w.write_row(['a', 'b'])
	w.reset()
	assert w.render() == ''
}

fn test_tabwriter_empty() {
	w := TabWriter{}
	assert w.render() == ''
}

fn test_tabwriter_min_width() {
	mut w := TabWriter{ min_width: 4 }
	w.write_row(['x', 'y'])
	out := w.render()
	assert out == 'x    y'
}
