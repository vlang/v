module strings

import os

const tabwriter_overflow_probe_arg = '--vtest-tabwriter-overflow'

fn testsuite_begin() {
	if os.args.len != 2 || os.args[1] != tabwriter_overflow_probe_arg {
		return
	}
	mut w := TabWriter{ min_width: max_int, padding: 1 }
	w.write_row(['', 'x'])
	println(w.render())
	exit(0)
}

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

fn test_tabwriter_unicode_widths() {
	mut w := TabWriter{}
	w.write_row(['é', 'first'])
	w.write_row(['界😀', 'second'])
	assert w.render() == 'é  first\n界😀 second'
}

fn test_tabwriter_custom_and_zero_padding() {
	mut w := TabWriter{
		min_width: 4
		padding:   2
	}
	w.write_row(['a', 'x'])
	w.write_row(['abc', 'y'])
	assert w.render() == 'a     x\nabc   y'
	w.padding = 0
	assert w.render() == 'a   x\nabc y'
}

fn test_tabwriter_negative_sizes_behave_as_zero() {
	mut w := TabWriter{
		min_width: -5
		padding:   -1
	}
	w.write_row(['a', 'x'])
	w.write_row(['abc', 'y'])
	assert w.render() == 'a  x\nabcy'
}

fn test_tabwriter_clones_rows_and_render_keeps_them() {
	mut w := TabWriter{}
	mut row := ['a', 'b']
	w.write_row(row)
	row[0] = 'changed'
	row << 'extra'
	assert w.render() == 'a b'
	assert w.render() == 'a b'
	w.write_row(['long', 'c'])
	assert w.render() == 'a    b\nlong c'
}

fn test_tabwriter_missing_cells_are_empty() {
	mut w := TabWriter{}
	w.write_row(['a', 'bb', 'c'])
	w.write_row(['ddd'])
	w.write_row([]string{})
	assert w.render() == 'a   bb c\nddd    \n       '
}

fn test_tabwriter_empty_rows_and_single_column() {
	mut w := TabWriter{
		min_width: 4
		padding:   2
	}
	w.write_row([]string{})
	w.write_row([]string{})
	assert w.render() == '\n'
	w.write_row(['é'])
	assert w.render() == '\n\né'
	w.reset()
	w.write_row(['x', 'y'])
	assert w.render() == 'x     y'
}

fn test_tabwriter_column_width_overflow_panics_before_allocation() {
	result := os.exec([os.executable(), tabwriter_overflow_probe_arg])
	assert result.exit_code != 0
	assert result.output.contains('strings.TabWriter: column width overflow'), result.output
}
