// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

enum Section as u8 {
	custom_section
	type_section
	import_section
	function_section
	table_section
	memory_section
	global_section
	export_section
	start_section
	element_section
	code_section
	data_section
	data_count_section
}

enum Subsection as u8 {
	name_module
	name_function
	name_local
	// see: https://github.com/WebAssembly/extended-name-section
	name_label
	name_type
	name_table
	name_memory
	name_global
	name_elem
	name_data
}

pub enum NumType as u8 {
	i32_t = 0x7f
	i64_t = 0x7e
	f32_t = 0x7d
	f64_t = 0x7c
}

pub enum ValType as u8 {
	i32_t       = 0x7f
	i64_t       = 0x7e
	f32_t       = 0x7d
	f64_t       = 0x7c
	v128_t      = 0x7b
	funcref_t   = 0x70
	externref_t = 0x6f
}

pub enum RefType as u8 {
	funcref_t   = 0x70
	externref_t = 0x6f
}

// Module contains the WebAssembly module.
// Use the `compile` method to compile the module into a pure byte array.
@[heap]
pub struct Module {
mut:
	buf            []u8
	functypes      []FuncType
	functions      map[string]Function
	globals        []Global
	memory         ?Memory
	start          ?string
	fn_imports     []FunctionImport
	global_imports []GlobalImport
	segments       []DataSegment
	debug          bool
	mod_name       ?string
}

struct Global {
	typ    ValType
	is_mut bool
	name   string
	export bool
mut:
	init ConstExpression
}

struct GlobalImport {
	mod    string
	name   string
	typ    ValType
	is_mut bool
}

struct FunctionImport {
	mod  string
	name string
	tidx int
}

struct Memory {
	name   string
	export bool
	min    u32
	max    ?u32
}

struct DataSegment {
	idx  ?int
	data []u8
	name ?string
}

pub type LocalIndex = int
pub type GlobalIndex = int
pub type GlobalImportIndex = int
pub type DataSegmentIndex = int

pub struct FuncType {
pub:
	parameters []ValType
	results    []ValType
	name       ?string
}

fn (mut mod Module) new_functype(ft FuncType) int {
	// interns existing types
	mut idx := mod.functypes.index(ft)

	if idx == -1 {
		idx = mod.functypes.len
		mod.functypes << ft
	}

	return idx
}

// new_function creates a function struct.
pub fn (mut mod Module) new_function(name string, parameters []ValType, results []ValType) Function {
	assert name !in mod.functions.keys()

	idx := mod.functions.len
	tidx := mod.new_functype(FuncType{parameters, results, none})

	return Function{
		name:   name
		tidx:   tidx
		idx:    idx
		mod:    mod
		locals: parameters.map(FunctionLocal{}) // specifying it's ValType doesn't matter
	}
}

// new_debug_function creates a function struct with extra debug information.
// `argument_names` must be the same length as the parameters in the function type `typ`.
pub fn (mut mod Module) new_debug_function(name string, typ FuncType, argument_names []?string) Function {
	assert name !in mod.functions.keys()
	assert typ.parameters.len == argument_names.len

	idx := mod.functions.len
	tidx := mod.new_functype(typ)

	return Function{
		name:   name
		tidx:   tidx
		idx:    idx
		mod:    mod
		locals: argument_names.map(FunctionLocal{ name: it }) // specifying it's ValType doesn't matter
	}
}

// enable_debug sets whether to emit debug information for not.
pub fn (mut mod Module) enable_debug(mod_name ?string) {
	mod.debug = true
	mod.mod_name = mod_name
}

// assign_memory assigns memory to the current module.
pub fn (mut mod Module) assign_memory(name string, export bool, min u32, max ?u32) {
	mod.memory = Memory{
		name:   name
		export: export
		min:    min
		max:    max
	}
}

// assign_start assigns the start function to the current module.
pub fn (mut mod Module) assign_start(name string) {
	mod.start = name
}

// new_function_import imports a new function into the current module.
pub fn (mut mod Module) new_function_import(modn string, name string, parameters []ValType, results []ValType) {
	assert !mod.fn_imports.any(it.mod == modn && it.name == name)

	tidx := mod.new_functype(FuncType{parameters, results, none})

	mod.fn_imports << FunctionImport{
		mod:  modn
		name: name
		tidx: tidx
	}
}

// new_function_import_debug imports a new function into the current module with extra debug information.
pub fn (mut mod Module) new_function_import_debug(modn string, name string, typ FuncType) {
	assert !mod.fn_imports.any(it.mod == modn && it.name == name)

	tidx := mod.new_functype(typ)

	mod.fn_imports << FunctionImport{
		mod:  modn
		name: name
		tidx: tidx
	}
}

// commit commits a function to the module, use `export` to export the function.
pub fn (mut mod Module) commit(func Function, export bool) {
	assert func.name !in mod.functions.keys()

	mod.functions[func.name] = Function{
		...func
		export: export
	}
}

// new_data_segment inserts a new data segment at the memory index `pos`.
// `name` is optional, it is used for debug info.
pub fn (mut mod Module) new_data_segment(name ?string, pos int, data []u8) DataSegmentIndex {
	len := mod.segments.len
	mod.segments << DataSegment{
		idx:  pos
		data: data
		name: name
	}
	return len
}

// new_passive_data_segment inserts a new passive data segment.
// `name` is optional, it is used for debug info.
pub fn (mut mod Module) new_passive_data_segment(name ?string, data []u8) {
	mod.segments << DataSegment{
		data: data
		name: name
	}
}

// new_global creates a global and returns it's index.
// See `global_get`, `global_set`.
pub fn (mut mod Module) new_global(name string, export bool, typ ValType, is_mut bool, init ConstExpression) GlobalIndex {
	len := mod.globals.len
	mod.globals << Global{
		typ:    typ
		is_mut: is_mut
		name:   name
		export: export
		init:   init
	}
	return len
}

// new_global_import imports a new global into the current module and returns it's index.
// See `global_get`, `global_set`.
pub fn (mut mod Module) new_global_import(modn string, name string, typ ValType, is_mut bool) GlobalImportIndex {
	assert !mod.fn_imports.any(it.mod == modn && it.name == name)

	len := mod.global_imports.len
	mod.global_imports << GlobalImport{
		mod:    modn
		name:   name
		typ:    typ
		is_mut: is_mut
	}
	return len
}

// assign_global_init assigns a global with the constant expression `init`.
// See `new_global`.
pub fn (mut mod Module) assign_global_init(global GlobalIndex, init ConstExpression) {
	mod.globals[global].init = init
}
