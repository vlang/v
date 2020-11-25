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
		assert typeof(e.val) == 'string'
	}
	match e {
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
	match arr1[0] {
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
			Sub2 { res << d }
			else {}
		}
	}
	assert res[0].val == 2
	assert res[0].name == 'two'
	assert res[1].val == 3
	assert res[1].name == 'three'
}

struct NodeWrapper {
	node Node
}

fn test_nested_sumtype_selector() {
	c := NodeWrapper{Node(Expr(IfExpr{pos: 1}))}
	if c.node is Expr {
		if c.node is IfExpr {
			assert c.node.pos == 1
		}
		else {
			assert false
		}
	}
	else {
		assert false
	}
}

fn test_nested_sumtype_match_selector() {
	c := NodeWrapper{Node(Expr(IfExpr{pos: 1}))}
	match c.node {
		Expr {
			match c.node {
				IfExpr {
					assert c.node.pos == 1
				}
				else {
					assert false
				}
			}
		}
		else {
			assert false
		}
	}
}

fn test_nested_sumtype() {
	c := Node(Expr(IfExpr{pos:1}))
	if c is Expr {
		if c is IfExpr {
			assert c.pos == 1
		}
		else {
			assert false
		}
	}
	else {
		assert false
	}
}

fn test_nested_sumtype_match() {
	c := Node(Expr(IfExpr{pos: 1}))
	match c {
		Expr {
			match c {
				IfExpr {
					assert c.pos == 1
				}
				else {
					assert false
				}
			}
		}
		else {
			assert false
		}
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

// TODO: change definition once types other than int and f64 (int, f64, etc) are supported in sumtype
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

type Bar = string | Test
type Xyz = int | string

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
		x2 := x as string
		assert x2 == 'test'
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

type Expr3 = CallExpr | CTempVarExpr
struct Expr3Wrapper {
mut:
	expr Expr3
}
struct CallExpr {
	y int
	x string
}

struct CTempVarExpr {
	x string
}

fn gen(_ Expr3) CTempVarExpr {
	return CTempVarExpr{}
}

fn test_reassign_from_function_with_parameter() {
	mut f := Expr3(CallExpr{})
	if f is CallExpr {
		f = gen(f)
	}
}

fn test_reassign_from_function_with_parameter_selector() {
	mut f := Expr3Wrapper{Expr3(CallExpr{})}
	if f.expr is CallExpr {
		f.expr = gen(f.expr)
	}
}

fn test_typeof() {
    x := Expr3(CTempVarExpr{})
	assert typeof(x) == 'CTempVarExpr'
}

struct Outer2 {
	e Expr3
}

fn test_zero_value_init() {
	// no c compiler error then it's successful
	_ := Outer2{}
}

struct Milk {
mut:
	name string
}

struct Eggs {
mut:
	name string
}

type Food = Milk | Eggs

struct FoodWrapper {
mut:
	food Food
}

fn test_match_aggregate() {
	f := Food(Milk{'test'})
	match f {
		Milk, Eggs {
			assert f.name == 'test'
		}
	}
}

fn test_match_mut() {
	mut f := Food(Milk{'test'})
	match mut f {
		Eggs {
			f.name = 'eggs'
			assert f.name == 'eggs'
		}
		Milk {
			f.name = 'milk'
			assert f.name == 'milk'
		}
	}
}

fn test_match_not_mut() {
	mut f := Food(Milk{'test'})
	match f {
		Eggs {
			// only works without smartcast
			assert f is Eggs
		}
		Milk {
			// only works without smartcast
			assert f is Milk
		}
	}
}

fn test_if_mut() {
	mut f := Food(Milk{'test'})
	if mut f is Milk {
		f.name = 'milk'
		assert f.name == 'milk'
	}
}

fn test_if_not_mut() {
	mut f := Food(Milk{'test'})
	if f is Milk {
		// only works without smartcast
		assert f is Milk
	}
}

fn test_match_mut_selector() {
	mut f := FoodWrapper{Food(Milk{'test'})}
	match mut f.food {
		Eggs {
			f.food.name = 'eggs'
			assert f.food.name == 'eggs'
		}
		Milk {
			f.food.name = 'milk'
			assert f.food.name == 'milk'
		}
	}
}

fn test_if_mut_selector() {
	mut f := FoodWrapper{Food(Milk{'test'})}
	if mut f.food is Milk {
		f.food.name = 'milk'
		assert f.food.name == 'milk'
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
