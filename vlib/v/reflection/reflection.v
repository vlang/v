[has_globals]
module reflection

__global g_reflection = Reflection{}

[heap]
pub struct Reflection {
pub mut:
	modules []Module
	funcs   []Function
	types   []Type
}

pub struct Type {
pub:
	name string // type name
	idx  int    // type idx
}

pub struct Module {
pub:
	name string // module name
}

pub struct FunctionArg {
pub:
	name string // argument name
	typ  int    // argument type idx
}

pub struct Function {
pub:
	mod_name     string        // module name
	name         string        // function/method name
	full_name    string        // fully name
	is_method    bool          // is a method?
	args         []FunctionArg // function/method args
	file         string        // source file name
	line_start   int  // declaration start line
	line_end     int  // declaration end line
	is_test      bool // is test?
	is_variadic  bool // is variadic?
	is_noreturn  bool // is [noreturn] ?
	return_typ   int  // return type idx
	receiver_typ int  // receiver type idx (is_method true)
}

// API module

// get_modules returns the module name built with V source
pub fn get_modules() []Module {
	return g_reflection.modules
}

// get_functions returns the functions built with V source
pub fn get_funcs() []Function {
	return g_reflection.funcs
}

pub fn get_types() []Type {
	return g_reflection.types
}

pub fn type_name(idx int) string {
	t := g_reflection.types.filter(it.idx == idx)
	return if t.len != 0 { t[0].name } else { '' }
}

// V metadata info - called from backend to fill metadata info

fn add_module(mod_name string) {
	g_reflection.modules << Module{mod_name}
}

fn add_func(func Function) {
	g_reflection.funcs << func
}

fn add_type(type_ Type) {
	g_reflection.types << type_
}
