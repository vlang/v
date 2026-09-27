@[translated]
module main

struct Cell {
	link &int
}

struct Outer {
	cell Cell
}

type TranslatedValue = Cell | int

fn test_translated_reference_defaults() {
	c := Cell{}
	assert c.link == unsafe { nil }
	p := Outer{}
	assert p.cell.link == unsafe { nil }
	value := TranslatedValue{}
	if value is Cell {
		assert value.link == unsafe { nil }
	} else {
		assert false
	}
}
