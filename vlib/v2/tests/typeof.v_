module main

fn test_typeof_fn() {
	assert typeof(fn (s string, x u32) (int, f32)).name == 'fn (string, u32) (int, f32)'
}

fn test_typeof_fn_literal() {
	assert typeof(fn (i int) int {return i+1}).name == 'TODO' 
}

fn test_typeof_int() {
	assert typeof(int).idx == 7
	assert typeof(int).name == 'int'
}

fn test_typeof_u32() {
	assert typeof(u32).idx == 12
	assert typeof(u32).name == 'u32'
}

fn test_typeof_string() {
	assert typeof(string).idx == 20
	assert typeof(string).name == 'string'

	assert typeof[string]().idx == 20
}

fn test_typeof_optional_type() {
	assert typeof(?string).name == '?string'
}

fn test_typeof_result_type() {
	assert typeof(!string).name == '!string'
}

fn test_typeof_array_type() {
	assert typeof([]string).name == '[]string'
	assert typeof(['a', 'b']).name == '[]string'
}

fn test_typeof_array_index() {
	assert typeof(['a', 'b'][0]).name == 'string'
}

fn test_typeof_map_type() {
	assert typeof(map[string]int).name == 'map[string]int'
}

fn test_typeof_nil() {
	assert typeof(nil).name == 'nil'
}

fn test_typeof_none() {
	assert typeof(none).name == 'none'
}

fn test_typeof_pointer() {
	assert typeof(&string).name == '&string'
}

//

struct MyStruct {}

struct MyGenericStruct[T] {}

struct MyGenericStruct2[T, U] {}

fn test_typeof_struct_type() {
	assert typeof(MyStruct).name == 'MyStruct'
	assert typeof(MyGenericStruct).name == 'MyGenericStruct'
	assert typeof(MyGenericStruct[int]).name == 'MyGenericStruct<int>'
	assert typeof(MyGenericStruct[string]).name == 'MyGenericStruct<string>'
	assert typeof(MyGenericStruct2).name == 'MyGenericStruct2'
	assert typeof(MyGenericStruct2[string, int]).name == 'MyGenericStruct2<string, int>'
}

//

union MyUnion {
	x int
	s string
}

fn test_typeof_union_type() {
	assert typeof(MyUnion).name == 'MyUnion'
}

//

type Abc = int | string

fn test_typeof_sumtype() {
	assert typeof(Abc).name == 'Abc'
}

//

enum EFoo {
	a
	b
	c
}

fn test_typeof_enum() {
	assert typeof(EFoo).name == 'EFoo'
}

//

type AnAlias = int

fn test_typeof_alias() {
	assert typeof(AnAlias).name == 'AnAlias'
}

//

fn abc[T](x T) string {
	return typeof(T).name
}

fn test_typeof_generic_type() {
	assert abc<int>(123) == 'int'
	assert abc<string>('xyz') == 'string'
}

//

fn test_typeof_idx_comparison() {
	i := 123
	u := u32(5)
	assert typeof(int).idx == typeof(i).idx
	assert typeof(u32).idx == typeof(u).idx
}
