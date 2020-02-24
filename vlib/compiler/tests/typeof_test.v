
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
	svalues := [MySumType( 32 ), MySumType(123.0), MySumType(FooBar{x:43})]
	a := svalues[0]
	b := svalues[1]
	c := svalues[2]
	assert typeof(a) == 'int'
	assert typeof(b) == 'f32'
	assert typeof(c) == 'FooBar'
	for s in svalues {		
		println(s)
	}
}
