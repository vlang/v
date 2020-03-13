
fn test_typeof_on_simple_expressions() {
	a := 123
	assert typeof(42) == 'int'
	assert typeof(3.14) == 'f32'
	assert typeof(2+2*10) == 'int'
	assert typeof(1.0 * 12.2) == 'f32'
	assert typeof(a) == 'int'
}

fn test_typeof_on_atypes(){
	aint := []int
	astring := []string
	assert typeof(aint) == 'array_int'
	assert typeof(astring) == 'array_string'
}

struct FooBar {
	x int
}

fn test_typeof_on_structs(){
	assert typeof(FooBar{}) == "FooBar"
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

fn test_typeof_on_sumtypes(){
	a := MySumType(32)
	b := MySumType(123.0)
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
	d := ExprType( UnaryExpr{} )
	assert typeof(a) == 'UnaryExpr'
	assert typeof(b) == 'BinExpr'
	assert typeof(c) == 'BoolExpr'
	assert typeof(d) == 'UnaryExpr'
}

type MyFn fn(int) int
type MyFn2 fn()

fn myfn(i int) int {
	return i
}
fn myfn2() {}

fn test_typeof_on_fn() {
	assert typeof(myfn) == 'fn (int) int'
	assert typeof(myfn2) == 'fn ()'
}