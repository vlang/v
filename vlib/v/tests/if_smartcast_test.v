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
	if mut x is Abc {
		assert x.val == 'original'
		x.val = 'changed'
		assert x.val == 'changed'
	}
	if mut x is Abc {
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

struct MutContainer {
mut:
	abc Alphabet
}

struct Container {
	abc Alphabet
}

fn test_mutable_with_struct() {
	mut c := MutContainer{Abc{'original'}}
	if mut c.abc is Abc {
		assert c.abc.val == 'original'
		c.abc.val = 'xyz'
		assert c.abc.val == 'xyz'
	}
	if mut c.abc is Abc {
		// NB: in this second smart cast, `another` is
		// the same wrapped value, that was changed in
		// the first smart cast:
		assert c.abc.val == 'xyz'
	}
}

fn test_as_cast_with_struct() {
	x := Container{Abc{'test'}}
	if x.abc is Abc {
		assert x.abc.val == 'test'
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
	if mut cell is CellStr {
		println('$cell.str')
	}
	cell = cell_itg
	if mut cell is CellInt {
		println('$cell.itg')
	}
	cell = cell_flt
	if mut cell is CellFloat {
		println('$cell.flt')
	}
	cell = cell_u32
	if mut cell is CellU32 {
		println('$cell.u')
	}
}
