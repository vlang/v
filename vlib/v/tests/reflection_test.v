import v.reflection

type MyInt = int

type MySumType = f64 | int

enum TestEnum {
	foo
	bar
}

struct User {
	name string
}

fn (u User) get_name() string {
	return u.name
}

fn test2(arg []string) {}

@[noreturn]
fn test3(a reflection.Function) {
	panic('foo')
}

fn test_module_existing() {
	assert 'v.reflection' in reflection.get_modules().map(it.name)
}

fn test_func_attribute() {
	assert reflection.get_funcs().filter(it.name == 'test3')[0].is_variadic == false
}

fn test_func_name() {
	assert reflection.get_funcs().filter(it.name == 'test2')[0].name == 'test2'
}

fn test_type_name() {
	ret_typ := reflection.get_funcs().filter(it.name == 'test3')[0].return_typ
	assert reflection.type_name(int(ret_typ)) == 'void'
	assert reflection.get_type(int(ret_typ))?.name == 'void'
	assert reflection.get_type_symbol(int(ret_typ))?.name == 'void'
	assert reflection.type_name(int(reflection.get_funcs().filter(it.name == 'test3')[0].args[0].typ)) == 'Function'
}

fn test_type_symbol() {
	ret_typ := reflection.get_funcs().filter(it.name == 'test3')[0].return_typ
	assert reflection.get_type_symbol(int(ret_typ))?.language == .v
}

fn test_method() {
	method := reflection.get_funcs().filter(it.name == 'get_name')[0]
	assert reflection.type_name(int(method.return_typ)) == 'string'
	println(reflection.get_type(int(method.receiver_typ))?.name)
	assert reflection.get_type(int(method.receiver_typ))?.name == 'User'
}

fn test_enum() {
	assert reflection.get_enums().filter(it.name == 'TestEnum')[0].name == 'TestEnum'
}

fn test_get_aliases() {
	assert reflection.get_aliases().filter(it.name == 'MyInt')[0].name == 'MyInt'
}

fn test_get_sum_types() {
	assert reflection.get_sum_types().filter(it.name == 'MySumType')[0].name == 'MySumType'
}

fn test_get_interfaces() {
	assert reflection.get_interfaces().filter(it.name == 'IError')[0].name == 'IError'
}

fn test_interfaces() {
	assert reflection.get_interfaces().filter(it.name == 'IError')[0].methods.len == 2
}

fn test_enum_fields() {
	assert (reflection.get_enums().filter(it.name == 'TestEnum')[0].sym.info as reflection.Enum).vals == [
		'foo',
		'bar',
	]
}

fn test_get_string_by_idx() {
	file_idx := reflection.get_funcs().filter(it.name == 'all_after_last')[0].file_idx
	assert reflection.get_string_by_idx(file_idx).ends_with('string.v')
}

fn test_ref() {
	cstr := c'abc' // &u8
	assert reflection.type_of(cstr).str().contains("name: 'u8'")
	ptr_user := &User{}
	assert reflection.type_of(ptr_user).str().contains("name: 'User'")
}
