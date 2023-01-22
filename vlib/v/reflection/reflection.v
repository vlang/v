[has_globals]
module reflection

__global g_reflection = Reflection{}

[heap]
pub struct Reflection {
pub mut:
	modules []ReflectionModule
	funcs   []ReflectionFunction
}

pub struct ReflectionModule {
pub:
	name string // module name
}

pub struct ReflectionFunctionArg {
pub:
	name string // argument name
	typ  int    // argument type
}

pub struct ReflectionFunction {
pub:
	mod_name  string // module name
	name      string // function/method name
	full_name string // fully name
	is_method bool   // is a method?
	args      []ReflectionFunctionArg
}

// API module

// get_modules returns the module name built with V source
pub fn get_modules() []ReflectionModule {
	return g_reflection.modules
}

// get_functions returns the functions built with V source
pub fn get_funcs() []ReflectionFunction {
	return g_reflection.funcs
}

// V metadata info - called from backend to fill metadata info

fn add_module(mod_name string) {
	g_reflection.modules << ReflectionModule{mod_name}
}

fn add_func(mod_name string, func_name string, is_method bool) {
	g_reflection.funcs << ReflectionFunction{mod_name, func_name.all_after_last('.'), func_name, is_method, []}
}

fn add_func_with_args(mod_name string, func_name string, is_method bool, args []ReflectionFunctionArg) {
	g_reflection.funcs << ReflectionFunction{mod_name, func_name.all_after_last('.'), func_name, is_method, args}
}
