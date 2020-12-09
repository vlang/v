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
	assert verify_complex_expr(int(1), false)
	assert !verify_complex_expr(int(2), false)
	assert verify_complex_expr(f64(2.5), false)
	assert !verify_complex_expr(f64(1.5), false)
	assert verify_complex_expr(int(0), true)
	assert as_string(int(1)) == 'This is the string representation of "1"'
	assert as_string(f64(3.14)) == 'This is the string representation of "3.14"'
	assert as_string('String') == 'This is the string representation of "String"'
	assert as_string(IntAndStr{foo: 2, bar: 'hi', baz: &IntAndStr{foo: 3, bar: 'hello', baz: 0}}) == 'This is the string representation of "5_hi_hello"'
	assert as_string(true) == 'This is the string representation of "true"'
	assert as_string(CommonType(Color.red)) == 'This is the string representation of "enum1_red"'
	assert as_string(CommonType(Color.green)) == 'This is the string representation of "enum2_green"'
	assert as_string(CommonType(Color.blue)) == 'This is the string representation of "enum3_blue"'
	assert sumtype_match_with_string_interpolation(1) == "it's an int: 5"
	assert sumtype_match_with_string_interpolation(2) == "it's a string: hello"
	assert sumtype_match_with_string_interpolation(3) == "green_green"
	assert sumtype_match_with_string_interpolation(4) == "it's a f64: 1.5"
	assert sumtype_match_with_string_interpolation(5) == "it's a bool: false" 
	assert sumtype_match_with_string_interpolation(6) == "it's an IntAndStr: 2_hi_3_hello"
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
			expr1 = arr1[0]
		}
		else {}
	}
}

fn test_expr() {
	expr := IntegerLiteral{
		val: '12'
	}
	assert handle(expr) == 'int'
	// assert expr is IntegerLiteral // TODO
}

struct Outer2 {
	e Expr
}

fn test_zero_value_init() {
	// no c compiler error then it's successful
	_ := Outer2{}
}

type Bar = string | Test
type Xyz = int | string

struct Test {
	x string
	xyz Xyz
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

struct Foo {
	y Bar
}

fn test_as_cast() {
	f := Foo{
		y: Bar('test')
	}
	y := f.y as string
	assert y == 'test'
}

fn test_typeof() {
    x := Expr(IfExpr{})
	assert typeof(x) == 'IfExpr'
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

fn test_match_aggregate() {
	f := Food(Milk{'test'})
	match f {
		Milk, Eggs {
			assert f.name == 'test'
		}
	}
}

fn test_if() {
	mut f := Food(Milk{'test'})
	if f is Milk {
		// only works without smartcast
		assert f is Milk
	}
}

fn test_match() {
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

type WrapperType = FoodWrapper | string

fn test_non_mut_ident_mut_selector_cast_match() {
	w := WrapperType(FoodWrapper{Food(Eggs{'test'})})
	match w {
		FoodWrapper {
			match w.food {
				Eggs {
					assert w.food.name + '2' == 'test2'
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

fn test_non_mut_ident_mut_selector_cast_if() {
	w := WrapperType(FoodWrapper{Food(Eggs{'test'})})
	if w is FoodWrapper {
		if w.food is Eggs {
			assert w.food.name + '2' == 'test2'
			
		} else {
			assert false
		}
	} else {
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

fn verify_complex_expr(dst Number, get_final bool) bool {
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

enum Color { red green blue }

type CommonType = int | f64 | string | IntAndStr | bool | Color

fn as_string(val CommonType) string {
	return 'This is the string representation of "' + val.str() + '"'
}

fn (c CommonType) str() string {
	match c {		
		string {
			d := c.int()
			e := d
			return c
		}
		int {
			d := c
			e := c + d - d
			return e.str()
		}
		f64 {
			return c.str()
		}
		IntAndStr {
			return (c.foo + c.baz.foo).str() + '_' + c.bar + '_' + c.baz.bar
		}
		bool {
			d := c
			return if d { 'true' } else { 'false' }
		}
		Color {
			d := c
			match d {
				.red { return 'enum1_' + d.str() }
				.green { return 'enum2_' + d.str() }
				.blue { return 'enum3_' + d.str() }
			}
		}
	}
}

fn sumtype_match_with_string_interpolation(code int) string {
	match code {
		1 {
		    bar := CommonType(5)
			match bar {
				f64 { return "shouldn't happen" }
				bool { return "shouldn't happen" }
				IntAndStr { return "shouldn't happen" }
				int { return "it's an int: $bar" }
				string { return "shouldn't happen" }
				Color { return "shouldn't happen" }
			}
		}
		2 {
			bar := CommonType('hello')
			match bar {
				string { return "it's a string: $bar" }
				int { return "shouldn't happen" }
				Color { return "shouldn't happen" }
				f64 { return "shouldn't happen" }
				bool { return "shouldn't happen" }
				IntAndStr { return "shouldn't happen" }
			}
		}
		3 {
			bar := CommonType(Color.green)
			match bar {
				string { return "shouldn't happen" }
				int { return "shouldn't happen" }
				f64 { return "shouldn't happen" }
				bool { return "shouldn't happen" }
				IntAndStr { return "shouldn't happen" }
				Color {
					match bar {
						.red { return 'red_$bar'}
						.green { return 'green_$bar' }
						.blue { return 'blue_$bar' }
					}
				}
			}
		}
		4 {
			bar := CommonType(1.5)
			match bar {
				string { return "shouldn't happen" }
				int { return "shouldn't happen" }
				Color { return "shouldn't happen" }
				f64 { return "it's a f64: $bar" }
				bool { return "shouldn't happen" }
				IntAndStr { return "shouldn't happen" }
			}
		}
		5 {
			mut bar := CommonType('hello')
			bar = CommonType(false)
			match bar {
				string { return "shouldn't happen" }
				int { return "shouldn't happen" }
				Color { return "shouldn't happen" }
				f64 { return "shouldn't happen" }
				bool { return "it's a bool: $bar" }
				IntAndStr { return "shouldn't happen" }
			}
		}
		6 {
			// TODO: this should work
			// mut bar := CommonType(100)
			// bar = CommonType(IntAndStr{foo: 2, bar: 'hi', baz: &IntAndStr{foo: 3, bar: 'hello', baz: 0}})
			bar := CommonType(IntAndStr{foo: 2, bar: 'hi', baz: &IntAndStr{foo: 3, bar: 'hello', baz: 0}})
			match bar {
				string { return "shouldn't happen" }
				int { return "shouldn't happen" }
				Color { return "shouldn't happen" }
				f64 { return "shouldn't happen" }
				bool { return "shouldn't happen" }
				IntAndStr { return "it's an IntAndStr: ${bar.foo}_${bar.bar}_${bar.baz.foo}_${bar.baz.bar}" }
			}
		}
		else { return 'wrong' }
	}
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
