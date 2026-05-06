module main

pub interface Iterator {
mut:
	next() ?string
}

pub struct LineIterator {
	lines []string
mut:
	idx int
}

type AliasLineIterator = LineIterator

pub fn (mut line_iterator LineIterator) next() ?string {
	if line_iterator.idx >= line_iterator.lines.len {
		return none
	}
	defer { line_iterator.idx += 1 }
	return line_iterator.lines[line_iterator.idx]
}

fn iterator_as_interface() Iterator {
	return LineIterator{
		lines: ['interface']
	}
}

fn iterator_as_concrete_type() LineIterator {
	return LineIterator{
		lines: ['concrete']
	}
}

fn iterator_as_alias_concrete_type() AliasLineIterator {
	return AliasLineIterator(LineIterator{
		lines: ['alias', 'iterator', 'next']
	})
}

pub fn (mut line_iterator AliasLineIterator) next() ?string {
	if line_iterator.idx >= line_iterator.lines.len {
		return none
	}
	defer { line_iterator.idx += 2 }
	return line_iterator.lines[line_iterator.idx]
}

struct SquareIterator {
	arr []int
mut:
	idx int
}

interface Iterable[T] {
mut:
	idx int
	next() ?T
}

fn (mut iter SquareIterator) next() ?string {
	if iter.idx >= iter.arr.len {
		return none
	}
	defer { iter.idx++ }
	return (iter.arr[iter.idx] * iter.arr[iter.idx]).str()
}

fn collect_iterable(iter Iterable[string]) []string {
	mut out := []string{}
	for item in iter {
		out << item
	}
	return out
}

fn test_main() {
	mut out := []string{}
	for line in iterator_as_interface() {
		out << 'LINE: ${line}'
	}
	for idx, line in iterator_as_interface() {
		out << 'LINE [${idx}]: ${line}'
	}

	assert out[0] == 'LINE: interface'
	assert out[1] == 'LINE [0]: interface'

	for line in iterator_as_concrete_type() {
		out << 'LINE: ${line}'
	}
	for idx, line in iterator_as_concrete_type() {
		out << 'LINE [${idx}]: ${line}'
	}

	assert out[2] == 'LINE: concrete'
	assert out[3] == 'LINE [0]: concrete'

	mut iter := iterator_as_interface()
	for {
		line := iter.next() or { break }
		out << 'LINE: ${line}'
	}
	for _, line in iterator_as_concrete_type() {
		out << 'LINE: ${line}'
	}

	assert out[4] == 'LINE: interface'
	assert out[5] == 'LINE: concrete'

	for _, line in iterator_as_alias_concrete_type() {
		out << 'LINE: ${line}'
	}

	assert out[6] == 'LINE: alias'
	assert out[7] == 'LINE: next'
}

fn test_for_in_generic_iterator_interface_param() {
	iter := SquareIterator{
		arr: [1, 2, 3, 4, 5]
	}
	assert collect_iterable(iter) == ['1', '4', '9', '16', '25']
}
