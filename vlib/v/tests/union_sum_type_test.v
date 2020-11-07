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

// TODO: change definition once types other than int and f64 (int, f64, etc) are supported in sumtype
__type Number = int | f64

fn is_gt_simple(val string, dst Number) bool {
	match union dst {
		int {
			return val.int() > dst
		}
		f64 {
			return dst < val.f64()
		}
	}
}

fn is_gt_nested(val string, dst Number) bool {
	dst2 := dst
	match union dst {
		int {
			match union dst2 {
				int {
					return val.int() > dst
				}
				// this branch should never been hit
				else {
					return val.int() < dst
				}
			}
		}
		f64 {
			match union dst2 {
				f64 {
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
		int {
			mut res := val + '(int)'
			res += dst.str()
			return res
		}
		f64 {
			mut res := val + '(float)'
			res += dst.str()
			return res
		}
	}
}

fn get_sum(val string, dst Number) f64 {
	match union dst {
		int {
			mut res := val.int()
			res += dst
			return res
		}
		f64 {
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

fn test_assignment() {
	y := 5
	mut x := Xyz(y)
	x = 'test'

	if x is string {
		assert x == 'test'
	}
}

__type Inner = int | string
struct InnerStruct {
mut:
	x Inner
}
__type Outer = string | InnerStruct

fn test_nested_if_is() {
	mut b := Outer(InnerStruct{Inner(0)})
	if b is InnerStruct {
		if b.x is int {
			println(b.x)
		}
	}
}

fn test_casted_sum_type_selector_reassign() {
	mut b := InnerStruct{Inner(0)}
	if b.x is int {
		assert typeof(b.x) == 'int'
		b.x = 'test'
		assert typeof(b.x) == 'string'
	}
	assert typeof(b.x) == 'Inner'
}

fn test_casted_sum_type_ident_reassign() {
	mut x := Inner(0)
	if x is int {
		assert typeof(x) == 'int'
		x = 'test'
		assert typeof(x) == 'string'
	}
	assert typeof(x) == 'Inner'
}

__type Expr2 = int | string

fn test_match_with_reassign_casted_type() {
	mut e := Expr2(0)
	match union mut e {
		int {
			e = int(5)
			assert e == 5
		}
		else {}
	}
}

fn test_if_is_with_reassign_casted_type() {
	mut e := Expr2(0)
	if e is int {
		e = int(5)
		assert e == 5
	}
}

fn test_change_type_if_is() {
	mut e := Expr2(0)
	if e is int {
		e = 'str'
		assert e.len == 3
	}
	assert e is string
}

fn test_change_type_match() {
	mut e := Expr2(0)
	match union mut e {
		int {
			e = 'str'
			assert e.len == 3
		}
		else {}
	}
	assert e is string
}

__type Expr3 = CallExpr | string

struct CallExpr {
mut:
	is_expr bool
}

fn test_assign_sum_type_casted_field() {
	mut e := Expr3(CallExpr{})
	if e is CallExpr {
		e.is_expr = true
		assert e.is_expr
	}
}

fn test_sum_type_match() {
	// TODO: Remove these casts
	assert is_gt_simple('3', int(2))
	assert !is_gt_simple('3', int(5))
	assert is_gt_simple('3', f64(1.2))
	assert !is_gt_simple('3', f64(3.5))
	assert is_gt_nested('3', int(2))
	assert !is_gt_nested('3', int(5))
	assert is_gt_nested('3', f64(1.2))
	assert !is_gt_nested('3', f64(3.5))
	assert concat('3', int(2)) == '3(int)2'
	assert concat('3', int(5)) == '3(int)5'
	assert concat('3', f64(1.2)) == '3(float)1.2'
	assert concat('3', f64(3.5)) == '3(float)3.5'
	assert get_sum('3', int(2)) == 5.0
	assert get_sum('3', int(5)) == 8.0
	assert get_sum('3', f64(1.2)) == 4.2
	assert get_sum('3', f64(3.5)) == 6.5
}
