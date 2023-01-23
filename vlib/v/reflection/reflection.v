[has_globals]
module reflection

import v.ast

__global g_reflection = Reflection{}

[heap]
pub struct Reflection {
pub mut:
	modules      []Module
	funcs        []Function
	types        []Type
	type_symbols []TypeSymbol
	// interfaces 	[]Interface
	enums []Enum
}

pub struct Enum {
pub:
	name       string // enum name
	full_name  string // full name
	is_pub     bool   // is pub?
	is_flag    bool   // is flag?
	typ        int    // type idx
	line_start int    // decl start line
	line_end   int    // decl end line
}

pub struct TypeSymbol {
pub:
	name       string   // symbol name
	idx        int      // symbol idx
	parent_idx int      // symbol parent idx
	language   string   // language
	kind       ast.Kind // kind
}

pub struct Type {
pub:
	name      string // type name
	full_name string // full name
	idx       int    // type idx
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
	line_start   int  // decl start line
	line_end     int  // decl end line
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

// get_types returns the registered types
pub fn get_types() []Type {
	return g_reflection.types
}

// get_enums returns the registered enums
pub fn get_enums() []Enum {
	return g_reflection.enums
}

// get_aliases returns the registered aliases
pub fn get_aliases() []Type {
	alias_idxs := g_reflection.type_symbols.filter(it.kind == .alias).map(it.idx)
	return g_reflection.types.filter(it.idx in alias_idxs)
}

// get_interfaces returns the registered aliases
pub fn get_interfaces() []Type {
	interface_idxs := g_reflection.type_symbols.filter(it.kind == .interface_).map(it.idx)
	return g_reflection.types.filter(it.idx in interface_idxs)
}

// get_sum_types returns the registered sum types
pub fn get_sum_types() []Type {
	sumtype_idxs := g_reflection.type_symbols.filter(it.kind == .sum_type).map(it.idx)
	return g_reflection.types.filter(it.idx in sumtype_idxs)
}

// get_type_symbol returns the registered type symbols
pub fn get_type_symbols() []TypeSymbol {
	return g_reflection.type_symbols
}

// Type API
pub fn type_name(idx int) string {
	t := g_reflection.types.filter(it.idx == idx)
	return if t.len != 0 { t[0].name } else { '' }
}

pub fn get_type(idx int) ?Type {
	t := g_reflection.types.filter(it.idx == idx)
	return if t.len != 0 { t[0] } else { none }
}

// Type Symbol API
pub fn type_symbol_name(idx int) string {
	t := g_reflection.type_symbols.filter(it.idx == idx)
	return if t.len != 0 { t[0].name } else { '' }
}

pub fn get_type_symbol(idx int) ?TypeSymbol {
	t := g_reflection.type_symbols.filter(it.idx == idx)
	return if t.len != 0 { t[0] } else { none }
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

fn add_type_symbol(typesymbol TypeSymbol) {
	g_reflection.type_symbols << typesymbol
}

fn add_enum(enum_ Enum) {
	g_reflection.enums << enum_
}
