import v.reflection

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
}
