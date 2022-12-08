struct Struct {
mut:
	array [][]int
}

fn (s Struct) rows() StructsRowIterator {
	return StructsRowIterator{
		array: s.array
		position: 0
	}
}

struct StructsRowIterator {
	Struct
mut:
	position int
}

fn (mut s StructsRowIterator) next() ?[]int {
	if s.position >= s.array.len {
		return error('out of range')
	}
	defer {
		s.position++
	}
	return s.array[s.position]
}

fn test_for_in_mut_struct_val() {
	mut s := Struct{
		array: [[1, 2, 3], [4, 5, 6]]
	}
	println(s)
	mut si := s.rows()
	println(si)

	mut rets := []string{}
	for mut row in si {
		println(row)
		rets << '${row}'
	}
	assert rets.len == 2
	assert rets[0] == '[1, 2, 3]'
	assert rets[1] == '[4, 5, 6]'
}
