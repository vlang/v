// vtest vflags: -autofree -gc none
// `a << b[i]`, `a.insert(j, b[i])` and `a.prepend(b[i])` must store a copy of
// the element: `b.delete(i)` frees the element that `b` still owns.

struct Table {
	name string
	cols []string
}

fn move_tables(n int) ([]Table, []Table) {
	mut remaining := [Table{'a'.repeat(n), ['x'.repeat(n)]}, Table{'b'.repeat(n), ['y'.repeat(n)]}]
	mut ordered := []Table{}
	ordered << remaining[0]
	remaining.delete(0)
	return ordered, remaining
}

fn take_all(mut src []Table) []Table {
	mut out := []Table{}
	for src.len > 0 {
		i := src.len - 1
		out << src[i]
		src.delete(i)
	}
	return out
}

fn insert_strings(n int) []string {
	mut src := ['a'.repeat(n), 'b'.repeat(n), 'c'.repeat(n)]
	mut dst := []string{}
	dst << src[1]
	dst.insert(0, src[0])
	dst.prepend(src[2])
	src.delete(1)
	src.delete(0)
	src.delete(0)
	return dst
}

fn insert_tables(n int) []Table {
	mut src := [Table{'a'.repeat(n), ['x'.repeat(n)]}, Table{'b'.repeat(n), ['y'.repeat(n)]}]
	mut dst := []Table{}
	dst.insert(0, src[0])
	dst.prepend(src[1])
	src.delete(1)
	src.delete(0)
	return dst
}

fn nested(n int) [][]string {
	mut src := [['x'.repeat(n)], ['y'.repeat(n)]]
	mut out := [][]string{}
	out << src[0]
	src.delete(0)
	return out
}

fn test_append_of_deleted_element_keeps_value() {
	ordered, remaining := move_tables(1)
	assert ordered == [Table{'a', ['x']}]
	assert remaining == [Table{'b', ['y']}]
	assert nested(2) == [['xx']]
}

fn test_append_in_loop_then_delete() {
	mut src := [Table{'a', ['x'.repeat(2)]}, Table{'b', ['y'.repeat(2)]}]
	assert take_all(mut src) == [Table{'b', ['yy']}, Table{'a', ['xx']}]
	assert src.len == 0
}

fn test_insert_and_prepend_of_deleted_element_keep_value() {
	assert insert_strings(2) == ['cc', 'aa', 'bb']
	assert insert_tables(1) == [Table{'b', ['y']}, Table{'a', ['x']}]
}
