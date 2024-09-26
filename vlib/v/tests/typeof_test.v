fn test_typeof_on_simple_expressions() {
	a := int(123)
	assert unsafe { typeof(int(42)) } == 'int'
	assert unsafe { typeof(f64(3.14)) } == 'f64'
	assert unsafe { typeof(int(2) + 2 * 10) } == 'int'
	assert unsafe { typeof(f64(1.0) * 12.2) } == 'f64'
	// assert typeof(1.0 * f32(12.2)) == 'f32'
	assert unsafe { typeof(a) } == 'int'
	assert typeof(a).name == 'int'
	// a2 := 123
	// assert typeof(a2) == 'int_literal'
	// assert typeof(42) == 'int_literal'
	// assert typeof(3.14) == 'float_literal'
	// assert typeof(2+2*10) == 'int_literal'
	// assert typeof(1.0 * 12.2) == 'float_literal'
}

fn test_arrays() {
	aint := []int{}
	astring := []string{}
	assert unsafe { typeof(aint) } == '[]int'
	assert unsafe { typeof(astring) } == '[]string'
	assert typeof(aint).name == '[]int'
	assert typeof(astring).name == '[]string'
}

fn test_type_constructors() {
	v := `c`
	assert typeof(&v).name == '&rune'
	assert typeof(&[v]).name == '&[]rune'
	assert typeof([v]!).name == '[1]rune'
	assert typeof(&[v]!).name == '&[1]rune'
	assert typeof(&FooBar{}).name == '&FooBar'
}

struct FooBar {
	x int
}

fn test_typeof_on_structs() {
	assert unsafe { typeof(FooBar{}) } == 'FooBar'
	astruct_static := [2]FooBar{}
	astruct_dynamic := [FooBar{}, FooBar{}]
	assert unsafe { typeof(astruct_static) } == '[2]FooBar'
	assert typeof(astruct_static).name == '[2]FooBar'
	assert unsafe { typeof(astruct_dynamic) } == '[]FooBar'
	assert typeof(astruct_dynamic).name == '[]FooBar'
}

type MySumType = FooBar | f32 | int

pub fn (ms MySumType) str() string {
	match ms {
		int { return ms.str() }
		f32 { return ms.str() }
		// FooBar { return it.x.str() }
		else { return ms.type_name() }
	}
}

fn test_typeof_on_sumtypes() {
	a := MySumType(int(32))
	b := MySumType(f32(123.0))
	c := MySumType(FooBar{
		x: 43
	})
	assert unsafe { typeof(a) } == 'int'
	assert unsafe { typeof(b) } == 'f32'
	assert unsafe { typeof(c) } == 'FooBar'
	assert a.type_name() == 'int'
	assert b.type_name() == 'f32'
	assert c.type_name() == 'FooBar'
	// typeof should be known at compile-time for all types
	assert typeof(a).name == 'MySumType'
	assert typeof(b).name == 'MySumType'
	assert typeof(c).name == 'MySumType'

	assert a.str() == '32'
	assert b.str() == '123.0'
	assert c.str() == 'FooBar'
}

//
struct UnaryExpr {
	a string
}

struct BinExpr {
	a string
	b string
}

struct BoolExpr {
	z int
}

type ExprType = BinExpr | BoolExpr | UnaryExpr

fn fexpr(k int) ExprType {
	match k {
		1 { return UnaryExpr{} }
		2 { return BinExpr{} }
		3 { return BoolExpr{} }
		else { return UnaryExpr{} }
	}
}

fn test_typeof_on_sumtypes_of_structs() {
	a := fexpr(1)
	b := fexpr(2)
	c := fexpr(3)
	d := ExprType(UnaryExpr{})
	assert unsafe { typeof(a) } == 'UnaryExpr'
	assert unsafe { typeof(b) } == 'BinExpr'
	assert unsafe { typeof(c) } == 'BoolExpr'
	assert unsafe { typeof(d) } == 'UnaryExpr'
	assert a.type_name() == 'UnaryExpr'
	assert b.type_name() == 'BinExpr'
	assert c.type_name() == 'BoolExpr'
	assert d.type_name() == 'UnaryExpr'
}

fn myfn(i int) int {
	return i
}

fn myfn2() {
}

fn myfn3(i int, s string) u8 {
	return u8(0)
}

fn myfn4() i8 {
	return -1
}

fn test_typeof_on_fn() {
	assert unsafe { typeof(myfn) } == 'fn (int) int'
	assert unsafe { typeof(myfn2) } == 'fn ()'
	assert unsafe { typeof(myfn3) } == 'fn (int, string) u8'
	assert unsafe { typeof(myfn4) } == 'fn () i8'
	assert typeof(myfn).name == typeof(myfn)
	assert typeof(&myfn).name == '&fn (int) int'
	assert typeof(myfn2).name == typeof(myfn2)
	assert typeof(myfn3).name == typeof(myfn3)
	assert typeof(myfn4).name == typeof(myfn4)
}

fn type_name[T](v T) string {
	return typeof(v).name
}

fn array_item_type[T](v []T) string {
	return typeof(v[0]).name
}

fn test_generic_type() {
	v := 5
	assert type_name(v) == 'int'
	// assert type_name(&v) == '&int'
	// assert type_name([v]!) == '[1]int'
	assert type_name([v]) == '[]int'
	assert type_name([[v]]) == '[][]int'
	assert type_name(FooBar{}) == 'FooBar'
	assert array_item_type([v]) == 'int'
	assert array_item_type([[v]]) == '[]int'
	// assert array_item_type([&v]) == '&int'
}

fn variadic_int(x ...int) string {
	return typeof(x)
}

fn variadic_bool(x ...bool) string {
	return typeof(x)
}

fn variadic_f64(x ...f64) string {
	return typeof(x)
}

fn test_variadic_type() {
	assert variadic_int(1, 2, 3) == '...int'
	assert variadic_bool(true, false) == '...bool'
	assert variadic_f64(3.1, 3.2) == '...f64'
}
