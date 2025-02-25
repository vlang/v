// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// wasm module provides a builder for creating WebAssembly modules in V.
// This file defines the core Module struct and its components, aiming to support the full WebAssembly spec.
module wasm

// Section represents WebAssembly module section types.
// See: https://webassembly.github.io/spec/core/binary/modules.html#sections
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

// Subsection defines subtypes for the custom section, used for debug info.
// See: https://github.com/WebAssembly/extended-name-section
enum Subsection as u8 {
	name_module
	name_function
	name_local
	name_label
	name_type
	name_table
	name_memory
	name_global
	name_elem
	name_data
}

// NumType represents numeric value types in WebAssembly.
pub enum NumType as u8 {
	i32_t = 0x7f
	i64_t = 0x7e
	f32_t = 0x7d
	f64_t = 0x7c
}

// ValType represents all value types, including numeric and reference types.
pub enum ValType as u8 {
	i32_t       = 0x7f
	i64_t       = 0x7e
	f32_t       = 0x7d
	f64_t       = 0x7c
	v128_t      = 0x7b // 128-bit vector (SIMD)
	funcref_t   = 0x70 // Function reference
	externref_t = 0x6f // External reference
}

// RefType represents reference types in WebAssembly.
pub enum RefType as u8 {
	funcref_t   = 0x70 // Function reference
	externref_t = 0x6f // External reference
}

// Module is the central structure for building a WebAssembly module.
// Use the `compile` method to compile the module into a pure byte array.
@[heap]
pub struct Module {
mut:
	buf            []u8                // Buffer for compiled binary output
	functypes      []FuncType          // Unique function type signatures
	functions      map[string]Function // Named functions defined in the module
	globals        []Global            // Global variables
	memory         ?Memory             // Single memory instances (Make it a array for multiple memories)
	start          ?string             // Optional start function name
	fn_imports     []FunctionImport    // Imported functions
	global_imports []GlobalImport      // Imported globals
	segments       []DataSegment       // Data segments (active or passive)
	debug          bool                // Whether to include debug info
	mod_name       ?string             // Optional module name for debug info
}

// Global defines a global variable in the module.
struct Global {
	typ    ValType // Type of the global
	is_mut bool    // Whether the global is mutable
	name   string  // Name for debugging or export
	export bool    // Whether to export the global
mut:
	init ConstExpression // Initial value as a constant expression
}

// GlobalImport defines an imported global variable.
struct GlobalImport {
	mod    string  // Module name of the import
	name   string  // Name of the imported global
	typ    ValType // Type of the global
	is_mut bool    // Whether the global is mutable
}

// FunctionImport defines an imported function.
struct FunctionImport {
	mod  string // Module name of the import
	name string // Name of the imported function
	tidx int    // Index into functypes array
}

// Memory defines a memory instance in the module.
struct Memory {
	name   string // Name for debugging or export
	export bool   // Whether to export the memory
	min    u32    // Minimum size in pages (64KiB each)
	max    ?u32   // Optional maximum size in pages
}

// DataSegment defines a data segment for initializing memory.
struct DataSegment {
	idx  ?int    // Optional memory index (if active)
	data []u8    // Data bytes
	name ?string // Optional name for debugging
}

// Index aliases for better readability and type safety.
pub type LocalIndex = int
pub type GlobalIndex = int
pub type GlobalImportIndex = int
pub type DataSegmentIndex = int

// FuncType represents a function signature with parameters and results.
pub struct FuncType {
pub:
	parameters []ValType // Input parameter types
	results    []ValType // Return value types
	name       ?string   // Optional name for debugging
}

// new_functype interns a function type, returning its index. Reuses existing types if identical.
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
	assert name !in mod.functions.keys(), 'Function name "${name}" already exists'

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
	assert name !in mod.functions.keys(), 'Function name "${name}" already exists'
	assert typ.parameters.len == argument_names.len, 'Argument names length must match parameters'
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

// assign_start sets the module’s start function by name.
pub fn (mut mod Module) assign_start(name string) {
	mod.start = name
}

// new_function_import imports a new function into the current module.
// Panics if the import already exists.
pub fn (mut mod Module) new_function_import(modn string, name string, parameters []ValType, results []ValType) {
	assert !mod.fn_imports.any(it.mod == modn && it.name == name), 'Import "${modn}.${name}" already exists'
	tidx := mod.new_functype(FuncType{parameters, results, none})

	mod.fn_imports << FunctionImport{
		mod:  modn
		name: name
		tidx: tidx
	}
}

// new_function_import_debug imports a new function into the current module with extra debug information.
pub fn (mut mod Module) new_function_import_debug(modn string, name string, typ FuncType) {
	assert !mod.fn_imports.any(it.mod == modn && it.name == name), 'Import "${modn}.${name}" already exists'
	tidx := mod.new_functype(typ)

	mod.fn_imports << FunctionImport{
		mod:  modn
		name: name
		tidx: tidx
	}
}

// commit commits a function to the module, use `export` to export the function.
pub fn (mut mod Module) commit(func Function, export bool) {
	assert func.name !in mod.functions.keys(), 'Function "${func.name}" already committed'

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

// new_passive_data_segment inserts a new passive data segment (no initial offset).
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
	assert !mod.fn_imports.any(it.mod == modn && it.name == name), 'Import "${modn}.${name}" already exists'
	assert !mod.global_imports.any(it.mod == modn && it.name == name), 'Import "${modn}.${name}" already exists'
	len := mod.global_imports.len
	mod.global_imports << GlobalImport{
		mod:    modn
		name:   name
		typ:    typ
		is_mut: is_mut
	}
	return len
}

// assign_global_init sets the initial value of a global variable.
// See `new_global`.
pub fn (mut mod Module) assign_global_init(global GlobalIndex, init ConstExpression) {
	assert global >= 0 && global < mod.globals.len, 'Invalid global index: ${global}'
	mod.globals[global].init = init
}
