struct Abc {
mut:
	val string
}
struct Xyz {
	name string
}
type Alphabet = Abc | Xyz

fn test_if_smartcast() {
	x := Alphabet(Abc{'test'})
	if x is Abc {
		assert x.val == 'test'
	}
}

fn test_mutable() {
	mut x := Alphabet(Abc{'test'})
	if x is Abc {
		y := Abc{}
		mut mx := x
		mx = &y
		assert mx == &y
		assert u64(mx) == u64(&y)
		assert u64(x) != u64(&y)
	}
}

fn test_nested_if_smartcast() {
	x := Alphabet(Abc{'test'})
	y := Alphabet(Xyz{'foo'})
	if x is Abc {
		if y is Xyz {
			assert y.name == 'foo'
		}
	}
}

fn test_as_cast() {
	x := Alphabet(Abc{'test'})
	if x is Abc as test {
		assert test.val == 'test'
	}
}

struct Test {
	abc Alphabet
}

fn test_mutable_with_struct() {
	mut x := Test{Abc{'test'}}
	if x.abc is Abc as test {
		mut ttt := test
		assert u64(ttt) == u64(ttt)
		ttt.val = 'test'
		assert ttt.val == 'test'
	}
}

fn test_as_cast_with_struct() {
	x := Test{Abc{'test'}}
	if x.abc is Abc as test {
		assert test.val == 'test'
	}
}

struct CellStr { str string }
struct CellInt { itg i64 }
struct CellFloat { flt f64 }
type Cell = CellStr | CellInt | CellFloat
fn test_mutability() {
	my_str := 'the quick brown fox jumps over the lazy dog.'
	my_itg := -1234567890
	my_flt := 3.14159265358979323846
	my_u32 := u32(4294967296)
	cell_str := CellStr{ str: my_str }
	cell_itg := CellInt{ itg: my_itg }
	cell_flt := CellFloat{ flt: my_flt }
	mut cell := Cell{}
	cell = cell_str
	if cell is CellStr {
		println('$cell.str')
	}
	cell = cell_itg
	if cell is CellInt {
		println('$cell.itg')
	}
}
