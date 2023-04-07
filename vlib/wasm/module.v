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

pub enum NumType as u8 {
	i32_t = 0x7f
	i64_t = 0x7e
	f32_t = 0x7d
	f64_t = 0x7c
}

pub enum ValType as u8 {
	i32_t = 0x7f
	i64_t = 0x7e
	f32_t = 0x7d
	f64_t = 0x7c
	v128_t = 0x7b
	funcref_t = 0x70
	externref_t = 0x6f
}

// Module contains the WebAssembly module.
// Use the `compile` method to compile the module into a pure byte array.
[heap]
pub struct Module {
mut:
	buf        []u8
	functypes  []FuncType
	functions  map[string]Function
	memory     ?Memory
	start      ?string
	fn_imports []FunctionImport
	segments   []DataSegment
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
}

[params]
pub struct FuncType {
pub:
	parameters []ValType
	results    []ValType
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
pub fn (mut mod Module) new_function(name string, typ FuncType) Function {
	assert name !in mod.functions

	idx := mod.functions.len
	tidx := mod.new_functype(typ)

	return Function{
		name: name
		tidx: tidx
		idx: idx
		mod: mod
	}
}

// assign_memory assigns memory to the current module.
pub fn (mut mod Module) assign_memory(name string, export bool, min u32, max ?u32) {
	mod.memory = Memory{
		name: name
		export: export
		min: min
		max: max
	}
}

// assign_start assigns the start function to the current module.
pub fn (mut mod Module) assign_start(name string) {
	mod.start = name
}

// new_function_import imports a new function into the current module.
pub fn (mut mod Module) new_function_import(modn string, name string, typ FuncType) {
	assert !mod.fn_imports.any(it.mod == modn && it.name == name)

	tidx := mod.new_functype(typ)

	mod.fn_imports << FunctionImport{
		mod: modn
		name: name
		tidx: tidx
	}
}

// commit commits a function to the module, use `export` to export the function.
pub fn (mut mod Module) commit(func Function, export bool) {
	assert func.name !in mod.functions

	mod.functions[func.name] = Function{
		...func
		export: export
	}
}

// new_data_segment inserts a new data segment at the memory index `pos`.
pub fn (mut mod Module) new_data_segment(pos int, data []u8) int {
	len := mod.segments.len
	mod.segments << DataSegment{
		idx: pos
		data: data
	}
	return len
}

// new_data_segment inserts a new passive data segment.
pub fn (mut mod Module) new_passive_data_segment(data []u8) {
	mod.segments << DataSegment{
		data: data
	}
}
