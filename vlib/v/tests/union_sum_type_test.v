__type Expr = IfExpr | IntegerLiteral
__type Stmt = FnDecl | StructDecl
__type Node = Expr | Stmt

struct FnDecl {
	pos int
}

struct StructDecl {
	pos int
}


struct IfExpr {
	pos int
}

struct IntegerLiteral {
	val string
}

fn handle(e Expr) string {
	is_literal := e is IntegerLiteral
	assert is_literal
	assert !(e !is IntegerLiteral)
	if e is IntegerLiteral {
		println('int')
	}
	match union e {
		IntegerLiteral {
			assert e.val == '12'
			// assert e.val == '12' // TODO
			return 'int'
		}
		IfExpr {
			return 'if'
		}
	}
	return ''
}

fn test_expr() {
	expr := IntegerLiteral{
		val: '12'
	}
	assert handle(expr) == 'int'
	// assert expr is IntegerLiteral // TODO
}

fn test_assignment_and_push() {
	mut expr1 := Expr{}
	mut arr1 := []Expr{}
	expr := IntegerLiteral{
		val: '111'
	}
	arr1 << expr
	match union arr1[0] {
		IntegerLiteral {
			arr1 << arr1[0]
			// should ref/dereference on assignent be made automatic?
			// currently it is done for return stmt and fn args
			expr1 = arr1[0]
		}
		else {}
	}
}

// Test moving structs between master/sub arrays
__type Master = Sub1 | Sub2

struct Sub1 {
mut:
	val  int
	name string
}

struct Sub2 {
	name string
	val  int
}

fn test_converting_down() {
	mut out := []Master{}
	out << Sub1{
		val: 1
		name: 'one'
	}
	out << Sub2{
		val: 2
		name: 'two'
	}
	out << Sub2{
		val: 3
		name: 'three'
	}
	mut res := []Sub2{cap: out.len}
	for d in out {
		match union d {
			Sub2 { res << d }
			else {}
		}
	}
	assert res[0].val == 2
	assert res[0].name == 'two'
	assert res[1].val == 3
	assert res[1].name == 'three'
}

fn test_nested_sumtype() {
	mut a := Node{}
	mut b := Node{}
	a = StructDecl{pos: 1}
	b = IfExpr{pos: 1}
	match union a {
		StructDecl {
			assert true
		}
		else {
			assert false
		}
	}
	// TODO: not working
	// assert b is IfExpr
	if b is IfExpr {
		assert true
	}
	else {
		assert false
	}
	c := Node(Expr(IfExpr{pos:1}))
	if c is Expr {
		if c is IfExpr {
			assert true
		}
		else {
			assert false
		}
	}
	else {
		assert false
	}
}

__type Abc = int | string

fn test_string_cast_to_sumtype() {
	a := Abc('test')
	match union a {
		int {
			assert false
		}
		string {
			assert true
		}
	}
}

fn test_int_cast_to_sumtype() {
	// literal
	a := Abc(111)
	match union a {
		int {
			assert true
		}
		string {
			assert false
		}
	}
	// var
	i := 111
	b := Abc(i)
	match union b {
		int {
			assert true
		}
		string {
			assert false
		}
	}
}

// TODO: change definition once types other than any_int and any_float (int, f64, etc) are supported in sumtype
__type Number = any_int | any_float

fn is_gt_simple(val string, dst Number) bool {
	match union dst {
		any_int {
			return val.int() > dst
		}
		any_float {
			return dst < val.f64()
		}
	}
}

fn is_gt_nested(val string, dst Number) bool {
	dst2 := dst
	match union dst {
		any_int {
			match union dst2 {
				any_int {
					return val.int() > dst
				}
				// this branch should never been hit
				else {
					return val.int() < dst
				}
			}
		}
		any_float {
			match union dst2 {
				any_float {
					return dst < val.f64()
				}
				// this branch should never been hit
				else {
					return dst > val.f64()
				}
			}
		}
	}
}

fn concat(val string, dst Number) string {
	match union dst {
		any_int {
			mut res := val + '(int)'
			res += dst.str()
			return res
		}
		any_float {
			mut res := val + '(float)'
			res += dst.str()
			return res
		}
	}
}

fn get_sum(val string, dst Number) f64 {
	match union dst {
		any_int {
			mut res := val.int()
			res += dst
			return res
		}
		any_float {
			mut res := val.f64()
			res += dst
			return res
		}
	}
}

__type Bar = string | Test
__type Xyz = int | string

struct Test {
	x string
	xyz Xyz
}

struct Foo {
	y Bar
}

fn test_nested_selector_smartcast() {
	f := Foo{
		y: Bar(Test{
			x: 'Hi'
			xyz: Xyz(5)
		})
	}

	if f.y is Test {
		z := f.y.x
		assert f.y.x == 'Hi'
		assert z == 'Hi'
		if f.y.xyz is int {
			assert f.y.xyz == 5
		}
	}
}

fn test_as_cast() {
	f := Foo{
		y: Bar('test')
	}
	y := f.y as string
	assert y == 'test'
}

fn test_sum_type_match() {
	assert is_gt_simple('3', 2)
	assert !is_gt_simple('3', 5)
	assert is_gt_simple('3', 1.2)
	assert !is_gt_simple('3', 3.5)
	assert is_gt_nested('3', 2)
	assert !is_gt_nested('3', 5)
	assert is_gt_nested('3', 1.2)
	assert !is_gt_nested('3', 3.5)
	assert concat('3', 2) == '3(int)2'
	assert concat('3', 5) == '3(int)5'
	assert concat('3', 1.2) == '3(float)1.2'
	assert concat('3', 3.5) == '3(float)3.5'
	assert get_sum('3', 2) == 5.0
	assert get_sum('3', 5) == 8.0
	assert get_sum('3', 1.2) == 4.2
	assert get_sum('3', 3.5) == 6.5
}
