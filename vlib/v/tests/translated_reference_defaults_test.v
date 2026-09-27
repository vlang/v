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

enum TranslatedMode {
	first
	second
}

enum NonzeroTranslatedMode {
	first = 7
	second
}

fn test_translated_empty_enum_initializers() {
	mode := TranslatedMode{}
	assert mode == .first
	nonzero := NonzeroTranslatedMode{}
	assert int(nonzero) == 0
}
