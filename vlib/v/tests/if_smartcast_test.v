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
	mut x := Alphabet(Abc{'original'})
	if x is Abc {
		assert x.val == 'original'
		x.val = 'changed'
		assert x.val == 'changed'
	}
	if x is Abc {
		assert x.val == 'changed'
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

struct Container {
	abc Alphabet
}

fn test_mutable_with_struct() {
	mut c := Container{Abc{'original'}}
	if c.abc is Abc as abc {
		assert abc.val == 'original'
		mut mabc := abc
		// NB: since `abc` is a pointer,
		// `mabc` points to the same data:
		assert mabc.val == 'original'
		// Modifying `mabc`, modifies the data of abc too.
		mabc.val = 'xyz'
		assert abc.val == 'xyz'
	}
	if c.abc is Abc as another {
		// NB: in this second smart cast, `another` is
		// the same wrapped value, that was changed in
		// the first smart cast:
		assert another.val == 'xyz'
	}
}

fn test_as_cast_with_struct() {
	x := Container{Abc{'test'}}
	if x.abc is Abc as test {
		assert test.val == 'test'
	}
}

struct CellStr {
	str string
}

struct CellInt {
	itg i64
}

struct CellFloat {
	flt f64
}

struct CellU32 {
	u u32
}

type Cell = CellFloat | CellInt | CellStr | CellU32

fn test_mutability() {
	my_str := 'the quick brown fox jumps over the lazy dog.'
	my_itg := -1234567890
	my_flt := 3.14159265358979323846
	my_u32 := u32(4294967295)
	cell_str := CellStr{
		str: my_str
	}
	cell_itg := CellInt{
		itg: my_itg
	}
	cell_flt := CellFloat{
		flt: my_flt
	}
	cell_u32 := CellU32{
		u: my_u32
	}
	mut cell := Cell{}
	cell = cell_str
	if cell is CellStr {
		println('$cell.str')
	}
	cell = cell_itg
	if cell is CellInt {
		println('$cell.itg')
	}
	cell = cell_flt
	if cell is CellFloat {
		println('$cell.flt')
	}
	cell = cell_u32
	if cell is CellU32 {
		println('$cell.u')
	}
}
