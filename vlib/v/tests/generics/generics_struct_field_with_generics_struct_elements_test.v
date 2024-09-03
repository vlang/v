struct Root[T] {
mut:
	edge1 Edge[T]
	edge2 map[int]Edge[T]
	edge3 map[int][]Edge[T]
	edge4 []Edge[T]
	edge5 [][]Edge[T]
	edge6 [1]Edge[T]
	edge7 []T
	edge8 [][]T
	edge9 [1]T
}

struct Edge[T] {
}

fn new[T]() Root[T] {
	return Root[T]{}
}

// for issue 18852
fn test_generics_struct_field_with_generics_struct_elements() {
	mut root := new[int]()

	assert root.edge1 == Edge[int]{}

	root.edge2[1] = Edge[int]{}
	assert root.edge2[1] == Edge[int]{}

	root.edge3[1] = []Edge[int]{len: 1}
	assert root.edge3[1] == [Edge[int]{}]

	root.edge4 << Edge[int]{}
	assert root.edge4 == [Edge[int]{}]

	root.edge5 << root.edge4
	assert root.edge5 == [[Edge[int]{}]]

	assert root.edge6 == [Edge[int]{}]!

	root.edge7 << 1
	assert root.edge7 == [1]

	root.edge8 << root.edge7
	assert root.edge8 == [[1]]

	assert root.edge9 == [0]!
}
