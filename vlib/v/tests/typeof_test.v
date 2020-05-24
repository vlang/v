fn test_typeof_on_simple_expressions() {
	a := int(123)
	assert typeof(int(42)) == 'int'
	assert typeof(f64(3.14)) == 'f64'
	assert typeof(int(2)+2*10) == 'int'
	assert typeof(f64(1.0) * 12.2) == 'f64'
	// assert typeof(1.0 * f32(12.2)) == 'f32'
	assert typeof(a) == 'int'
	// a2 := 123
	// assert typeof(a2) == 'any_int'
	// assert typeof(42) == 'any_int'
	// assert typeof(3.14) == 'any_float'
	// assert typeof(2+2*10) == 'any_int'
	// assert typeof(1.0 * 12.2) == 'any_float'
}

fn test_typeof_on_atypes() {
	aint := []int{}
	astring := []string{}
	assert typeof(aint) == 'array_int'
	assert typeof(astring) == 'array_string'
}

struct FooBar {
	x int
}

fn test_typeof_on_structs() {
	assert typeof(FooBar{}) == 'FooBar'
	astruct_static := [2]FooBar
	astruct_dynamic := [FooBar{}, FooBar{}]
	assert typeof(astruct_static) == '[2]FooBar'
	assert typeof(astruct_dynamic) == 'array_FooBar'
}

type MySumType = int | f32 | FooBar

pub fn (ms MySumType) str() string {
	match ms {
		int { return it.str() }
		f32 { return it.str() }
		//FooBar { return it.x.str() }
		else { return 'unknown: ' + typeof(ms) }
	}
}

fn test_typeof_on_sumtypes() {
	a := MySumType(int(32))
	b := MySumType(f32(123.0))
	c := MySumType(FooBar{x:43})
	assert typeof(a) == 'int'
	assert typeof(b) == 'f32'
	assert typeof(c) == 'FooBar'
}

//

struct UnaryExpr { a string }
struct BinExpr { a string b string }
struct BoolExpr { z int }
type ExprType = BoolExpr | BinExpr | UnaryExpr

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
	assert typeof(a) == 'UnaryExpr'
	assert typeof(b) == 'BinExpr'
	assert typeof(c) == 'BoolExpr'
	assert typeof(d) == 'UnaryExpr'
}

fn myfn(i int) int {
	return i
}
fn myfn2() {}
fn myfn3(i int, s string) byte {
	return byte(0)
}
fn myfn4() i8 {
	return -1
}

fn test_typeof_on_fn() {
	assert typeof(myfn) == 'fn (int) int'
	assert typeof(myfn2) == 'fn ()'
	assert typeof(myfn3) == 'fn (int, string) byte'
	assert typeof(myfn4) == 'fn () i8'
}
