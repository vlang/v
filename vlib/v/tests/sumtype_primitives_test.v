struct Consts { 
	str string = 'the quick brown fox jumps over the lazy dog.'
	itg int = -1234567890
	flt64 f64 = 3.1415926535897932384
	usg32 u32 = 4294967296
	boo bool = true
	arr []string = []string{ len: 7, init: 'in the beginning ...' }
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

type Cell = string | int | f64 | u32 | bool | CellTree

// arg is not pointer
fn check(cell Cell) {
	match cell {
		string { assert cell == constants.str }
		int { assert cell == constants.itg }
		f64 { assert cell == constants.flt64 }
		u32 { assert cell == constants.usg32 }
		bool { assert cell == constants.boo }
		CellTree { assert cell.content == constants.str }
	}
}

// direct assginments, direct asserts and asserts through fn with match
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

fn test_sumtype_of_struct_with_string_field() {
	mut cell := Cell{}
	cell = CellTree{ content: constants.str }
	check(cell)
	mut cell_tree := cell as CellTree
	assert cell_tree.content == constants.str
}
