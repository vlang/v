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
	assert isnil(c.link)
	p := Outer{}
	assert isnil(p.cell.link)
	value := TranslatedValue{}
	if value is Cell {
		assert isnil(value.link)
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
	optional_mode := ?TranslatedMode{}
	assert (optional_mode or { TranslatedMode.second }) == .second
	nonzero := NonzeroTranslatedMode{}
	assert int(nonzero) == 0
}
