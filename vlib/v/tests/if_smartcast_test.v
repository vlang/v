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

type Bar = string | Test
type Xya = int | string

struct Test {
	x string
	xya Xya
}

struct BarWrapper {
	y Bar
}

fn test_nested_selector_smartcast() {
	f := BarWrapper{
		y: Bar(Test{
			x: 'Hi'
			xya: Xya(int(5))
		})
	}

	if f.y is Test {
		z := f.y.x
		assert f.y.x == 'Hi'
		assert z == 'Hi'
		if f.y.xya is int {
			assert f.y.xya == 5
		}
	}
}

type Inner = int | string
struct InnerStruct {
	x Inner
}
type Outer = string | InnerStruct

fn test_nested_if_is() {
	b := Outer(InnerStruct{Inner(0)})
	if b is InnerStruct {
		if b.x is int {
			assert b.x == 0
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

type Expr = CallExpr | CTempVarExpr
struct ExprWrapper {
mut:
	expr Expr
}
struct CallExpr {
	y int
	x string
}

struct CTempVarExpr {
	x string
}

fn gen(_ Expr) CTempVarExpr {
	return CTempVarExpr{}
}

fn test_reassign_from_function_with_parameter_selector() {
	mut f := ExprWrapper{Expr(CallExpr{})}
	if f.expr is CallExpr {
		f.expr = gen(f.expr)
	}
}

type Node = Expr | string

fn test_nested_sumtype() {
	c := Node(Expr(CallExpr{y: 1}))
	if c is Expr {
		if c is CallExpr {
			assert c.y == 1
		}
		else {
			assert false
		}
	}
	else {
		assert false
	}
}

type Food = Milk | Eggs

struct FoodWrapper {
mut:
	food Food
}

struct Milk {
mut:
	name string
}

struct Eggs {
mut:
	name string
}

fn test_if_mut_selector() {
	mut f := FoodWrapper{Food(Milk{'test'})}
	if mut f.food is Milk {
		f.food.name = 'milk'
		assert f.food.name == 'milk'
	}
}

struct NodeWrapper {
	node Node
}

fn test_nested_sumtype_selector() {
	c := NodeWrapper{Node(Expr(CallExpr{y: 1}))}
	if c.node is Expr {
		if c.node is CallExpr {
			assert c.node.y == 1
		}
		else {
			assert false
		}
	}
	else {
		assert false
	}
}

struct Foo1 {
	a int
}

struct Foo2 {
	a int
}

struct Bar1 {
	a int
}

struct Bar2 {
	a int
}

type Sum1 = Foo1 | Foo2

type Sum2 = Bar1 | Bar2

type SumAll = Sum1 | Sum2

struct All_in_one {
pub mut:
	ptrs []&SumAll
	ptr &SumAll
}

fn test_nested_pointer_smartcast() {
	mut s := All_in_one{
		ptr: &Sum1(Foo1{a: 1})
		ptrs: [&SumAll(Sum2(Bar1{a: 3}))]
	}

	if mut s.ptr is Sum1 {
		if mut s.ptr is Foo1 {
			assert s.ptr.a == 1
		}
	}

	a := s.ptrs[0]
	if a is Sum1 {
		if a is Foo1{
			assert a.a == 3
		}
	}
}
