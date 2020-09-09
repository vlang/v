enum Months { january february march april may june july august september october november december }

struct Consts { 
	str string = 'the quick brown fox jumps over the lazy dog.'
	itg int = -1234567890
	flt64 f64 = 3.141592653589793238462643
	usg32 u32 = 4294967296
	boo bool = true
	month Months = .october
	anyi any_int = -111222333
	anyf any_float = 2.718281828459045235360287
}

const ( 
	constants = Consts{}
)

struct CellTree {
	content string
	up voidptr
	down voidptr
	left voidptr
	right voidptr
}

type Cell = string | int | f64 | u32 | bool | any_int | any_float | Months | CellTree

struct CellKey {
	key Cell
	mut: cell Cell
}

struct Aaa { mut: b Bbb }
struct Bbb { mut: c Ccc }
struct Ccc { mut: d Cell }


fn check(cell Cell) {
	match cell {
		string { assert cell == constants.str }
		int { assert cell == constants.itg }
		f64 { assert cell == constants.flt64 }
		u32 { assert cell == constants.usg32 }
		bool { assert cell == constants.boo }
		any_int { assert cell == constants.anyi }
		any_float { assert cell == constants.anyf }
		Months { assert cell == constants.month }
		CellTree { assert cell.content == constants.str }
	}
}

fn test_sumtype_of_string() {
	mut cell := Cell{}
	cell = constants.str
	check(cell)
	str := cell as string
	assert str == constants.str
}

fn test_sumtype_of_int() {
	mut cell := Cell{}
	cell = constants.itg
	check(cell)
	itg := cell as int
	assert itg == constants.itg
}

fn test_sumtype_of_f64() {
	mut cell := Cell{}
	cell = constants.flt64
	check(cell)
	flt64 := cell as f64
	assert flt64 == constants.flt64
}

fn test_sumtype_of_u32() {
	mut cell := Cell{}
	cell = constants.usg32
	check(cell)
	usg32 := cell as u32
	assert usg32 == constants.usg32
}

fn test_sumtype_of_boolean() {
	mut cell := Cell{}
	cell = constants.boo
	check(cell)
	boo := cell as bool
	assert boo == constants.boo
}

fn test_sumtype_of_any_int() {
	mut cell := Cell{}
	cell = constants.anyi
	check(cell)
	anyi := cell as any_int
	assert anyi == constants.anyi
}

fn test_sumtype_of_any_float() {
	mut cell := Cell{}
	cell = constants.anyf
	check(cell)
	anyf := cell as any_float
	assert anyf == constants.anyf
}

fn test_sumtype_of_enum() {
	mut cell := Cell{}
	cell = constants.month
	check(cell)
	month := cell as Months
	assert month == constants.month
}

fn test_sumtype_of_struct_with_string_field() {
	mut cell := Cell{}
	cell = CellTree{ content: constants.str }
	check(cell)
	mut cell_tree := cell as CellTree
	assert cell_tree.content == constants.str
}

fn test_sumtype_of_struct_with_ident_and_infix_as() {
	mut cell := Cell{}
	cell = CellTree{ content: constants.str }
	match mut cell as abc {
		string { assert abc == constants.str }
		int { assert abc == constants.itg }
		f64 { assert abc == constants.flt64 }
		u32 { assert abc == constants.usg32 }
		bool { assert abc == constants.boo }
		any_int { assert abc == constants.anyi }
		any_float { assert abc == constants.anyf }
		Months { assert abc == constants.month }
		CellTree { assert abc.content == constants.str }
	}
}

fn test_sumtype_of_struct_with_selector_condition() {
	mut cell := Cell{}
	cell = CellTree{ content: constants.str }
	mut cell_key := CellKey{ key: constants.usg32, cell: cell }
	match cell_key.key {
		string { assert it == constants.str }
		int { assert it == constants.itg }
		f64 { assert it == constants.flt64 }
		u32 { assert it == constants.usg32 }
		bool { assert it == constants.boo }
		any_int { assert it == constants.anyi }
		any_float { assert it == constants.anyf }
		Months { assert it == constants.month }
		CellTree { assert it.content == constants.str }
	}
	mut cell_tree := cell_key.cell as CellTree
	assert cell_tree.content == constants.str
	// testing sumtype mutability after match
	cell_key.cell = constants.boo
	match cell_key.cell {
		string { assert it == constants.str }
		int { assert it == constants.itg }
		f64 { assert it == constants.flt64 }
		u32 { assert it == constants.usg32 }
		bool { assert it == constants.boo }
		any_int { assert it == constants.anyi }
		any_float { assert it == constants.anyf }
		Months { assert it == constants.month }
		CellTree { it.content == constants.str }
	}
	check(cell_key.key)
	check(cell_key.cell)
	a_bool := cell_key.cell as bool
	assert a_bool == constants.boo
	// testing sumtype with a long selector
	a := Aaa { b: Bbb{ c: Ccc{ d: constants.flt64 } } }
	match a.b.c.d {
		string { assert it == constants.str }
		int { assert it == constants.itg }
		f64 { assert it == constants.flt64 }
		u32 { assert it == constants.usg32 }
		bool { assert it == constants.boo }
		any_int { assert it == constants.anyi }
		any_float { assert it == constants.anyf }
		Months { assert it == constants.month }
		CellTree { it.content == constants.str }
	}
	check(a.b.c.d)
	abcd := a.b.c.d as f64 
	assert abcd == constants.flt64
}

fn test_sumtype_of_struct_with_selector_condition_and_infix_as() {
	mut cell := Cell{}
	cell = CellTree{ content: constants.str }
	mut cell_key := CellKey{ key: constants.usg32, cell: cell }
	match cell_key.key as key {
		string { assert key == constants.str }
		int { assert key == constants.itg }
		f64 { assert key == constants.flt64 }
		u32 { assert key == constants.usg32 }
		bool { assert key == constants.boo }
		any_int { assert key == constants.anyi }
		any_float { assert key == constants.anyf }
		Months { assert key == constants.month }
		CellTree { assert key.content == constants.str }
	}
	mut cell_tree := cell_key.cell as CellTree
	assert cell_tree.content == constants.str
	// testing sumtype mutability after match
	cell_key.cell = constants.anyf
	match cell_key.cell as thing {
		string { assert thing == constants.str }
		int { assert thing == constants.itg }
		f64 { assert thing == constants.flt64 }
		u32 { assert thing == constants.usg32 }
		bool { assert thing == constants.boo }
		any_int { assert thing == constants.anyi }
		any_float { assert thing == constants.anyf }
		Months { assert thing == constants.month }
		CellTree { thing.content == constants.str }
	}
	check(cell_key.key)
	check(cell_key.cell)
	anyf := cell_key.cell as any_float
	assert anyf == constants.anyf
	// testing sumtype with a long selector
	a := Aaa { b: Bbb{ c: Ccc{ d: constants.month } } }
	match a.b.c.d as abcd {
		string { assert abcd == constants.str }
		int { assert abcd == constants.itg }
		f64 { assert abcd == constants.flt64 }
		u32 { assert abcd == constants.usg32 }
		bool { assert abcd == constants.boo }
		any_int { assert abcd == constants.anyi }
		any_float { assert abcd == constants.anyf }
		Months { assert abcd == constants.month }
		CellTree { abcd.content == constants.str }
	}
	check(a.b.c.d)
	abcd := a.b.c.d as Months 
	assert abcd == constants.month
}

