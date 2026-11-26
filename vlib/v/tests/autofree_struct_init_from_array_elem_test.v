// vtest vflags: -autofree -gc none
struct Column {
	name string
	n    int
}

struct Holder {
	s string
}

fn test_struct_update_from_array_element_keeps_element() {
	columns := [Column{'v'.repeat(2), 1}]
	c := Column{
		...columns[0]
		n: 2
	}
	assert c.name == 'vv'
	assert c.n == 2
	assert columns[0].name == 'vv'
	assert columns[0].n == 1
}

fn test_struct_field_from_array_element_keeps_element() {
	names := ['n'.repeat(2)]
	h := Holder{
		s: names[0]
	}
	assert h.s == 'nn'
	assert names[0] == 'nn'
}
