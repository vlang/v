type Expr = IfExpr | IntegerLiteral
type Stmt = FnDecl | StructDecl
type Node = Expr | Stmt

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
	match e {
		IntegerLiteral {
			assert it.val == '12'
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
	match arr1[0] {
		IntegerLiteral {
			arr1 << it
			// should ref/dereference on assignent be made automatic?
			// currently it is done for return stmt and fn args
			expr1 = *it
		}
		else {}
	}
}

// Test moving structs between master/sub arrays
type Master = Sub1 | Sub2

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
		match d {
			Sub2 { res << it }
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
	match a {
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

type Abc = int | string

fn test_string_cast_to_sumtype() {
	a := Abc('test')
	match a {
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
	match a {
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
	match b {
		int {
			assert true
		}
		string {
			assert false
		}
	}
}

type Number = int | f64

fn is_gt_simple(val string, dst Number) bool {
	match dst {
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
	match dst {
		int {
			match dst2 {
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
			match dst2 {
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
	match dst {
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
	match dst {
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

fn calc_expr(dst Number, get_final bool) bool {
	if !get_final {
		match dst {
			int {
				dst2 := dst
				dst3 := dst2
				dst4 := (dst2 + dst3).str()
				temp := 2 * dst3 + 1
				res := temp - 3
				foo := 1 + dst - dst2
				dst5 := foo.str().int().str()
				return (foo + res) * res - res == 0 && dst4.len == 1 && dst5.len == 1
			}
			f64 {
				dst2 := dst
				dst3, foo := dst2, 2
				mut dst4 := dst3 + 1
				dst4 = dst / 1
				dst4 -= dst2
				mut temp := foo - 4
				temp += foo * (foo - 1)
				bar := !(dst2 < 1) && dst3 - foo - temp > 0 && dst4 == 0
				return bar
			}
		}
	}
	foo := 10
	temp := foo
	return temp + 10 == 20
}

struct IntAndStr {
	foo int
	bar string
	baz &IntAndStr
}

type CommonType = int | f64 | string | IntAndStr

fn (c CommonType) str() string {
	match c {
		string {
			return c
		}
		int {
			return c.str()
		}
		f64 {
			return c.str()
		}
		IntAndStr {
			return (c.foo + c.baz.foo).str() + '_' + c.bar + '_' + c.baz.bar
		}
	}
}

fn as_string(val CommonType) string {
	return 'This is the string representation of "' + val.str() + '"'
}

fn test_sum_type_match() {
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
	assert calc_expr(int(1), false)
	assert !calc_expr(int(2), false)
	assert calc_expr(f64(2.5), false)
	assert !calc_expr(f64(1.5), false)
	assert calc_expr(int(0), true)
	assert as_string(int(1)) == 'This is the string representation of "1"'
	assert as_string(f64(3.14)) == 'This is the string representation of "3.14"'
	assert as_string('String') == 'This is the string representation of "String"'
	assert as_string(IntAndStr{foo: 2, bar: 'hi', baz: &IntAndStr{foo: 3, bar: 'hello', baz: 0}}) == 'This is the string representation of "5_hi_hello"'
}
