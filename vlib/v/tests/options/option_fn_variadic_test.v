struct Empty {
}

type Elem = int | Empty

fn new_elems(elems ...??int) []Elem {
	mut out := []Elem{}
	for elem in elems {
		if elem == none {
			out << Empty{}
		} else {
			out << elem
		}
	}
	return out
}

fn test_main() {
	elems := new_elems(0, none, 2)
	assert '${elems}' == '[Elem(0), Elem(Empty{}), Elem(2)]'
}
