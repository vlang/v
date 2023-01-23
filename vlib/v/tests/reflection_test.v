import v.reflection

struct User {
	name string
}

fn (u User) get_name() string {
	return u.name
}

fn test2(arg []string) {}

[noreturn]
fn test3(a reflection.Function) {
}

fn test_module_existing() {
	assert 'v.reflection' in reflection.get_modules().map(it.name)
}

fn test_func_attribute() {
	assert reflection.get_funcs().filter(it.name == 'test3')[0].is_noreturn == true
	assert reflection.get_funcs().filter(it.name == 'test3')[0].is_variadic == false
}

fn test_func_name() {
	assert reflection.get_funcs().filter(it.name == 'test2')[0].name == 'test2'
}

fn test_type_name() {
	ret_typ := reflection.get_funcs().filter(it.name == 'test3')[0].return_typ
	assert reflection.type_name(ret_typ) == 'void'
	assert reflection.get_type(ret_typ)?.name == 'void'
	assert reflection.get_type_symbol(ret_typ)?.name == '&void'
	assert reflection.type_name(reflection.get_funcs().filter(it.name == 'test3')[0].args[0].typ) == 'Function'
}

fn test_method() {
	method := reflection.get_funcs().filter(it.name == 'get_name')[0]
	assert method.is_method == true
	assert method.is_test == false
	assert reflection.type_name(method.return_typ) == 'string'
	println(reflection.get_type(method.receiver_typ)?.name)
	assert reflection.get_type(method.receiver_typ)?.name == 'User'
	assert reflection.get_type(method.receiver_typ)?.full_name == 'main.User'
}
