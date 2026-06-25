module wasm

// encode.v is the self-contained WebAssembly binary encoder used by the v3
// wasm backend. It mirrors the role of the arm64 backend's asm.v + macho.v:
// gen.v walks the flat AST and drives this module, which knows nothing about V
// and only emits raw WASM bytecode (LEB128 + sections). No external module is
// imported, so v3 stays self-contained and self-hostable.

// WASM value types.
pub const valtype_i32 = u8(0x7f)
pub const valtype_i64 = u8(0x7e)
pub const valtype_f32 = u8(0x7d)
pub const valtype_f64 = u8(0x7c)

// Export kinds.
pub const export_func = u8(0x00)
pub const export_mem = u8(0x02)

// FuncType is a single `(params) -> (results)` signature.
struct FuncType {
	params  []u8
	results []u8
}

// ImportFunc is an imported function (only the WASI import is used for now).
struct ImportFunc {
	module   string
	name     string
	type_idx int
}

// Func is a locally defined function. `code` already ends with the `end` (0x0b)
// opcode; `locals` lists the value type of each local declared beyond params.
struct Func {
	type_idx int
	locals   []u8
	code     []u8
}

struct Export {
	name  string
	kind  u8
	index int
}

// DataSeg is an active data segment placed at a fixed linear-memory offset.
struct DataSeg {
	offset int
	bytes  []u8
}

// Global is a module-level mutable global variable.
struct Global {
	valtype u8
	init    []u8 // constant init expression, ending with `end` (0x0b)
}

@[heap]
pub struct Module {
mut:
	types    []FuncType
	imports  []ImportFunc
	funcs    []Func
	exports  []Export
	datas    []DataSeg
	globals  []Global
	mem_min  int = 2
	n_import int
}

pub fn Module.new() &Module {
	return &Module{}
}

// add_type registers a signature, deduplicating identical ones.
pub fn (mut m Module) add_type(params []u8, results []u8) int {
	for i, t in m.types {
		if t.params == params && t.results == results {
			return i
		}
	}
	m.types << FuncType{
		params:  params.clone()
		results: results.clone()
	}
	return m.types.len - 1
}

// add_import_func registers an imported function and returns its function index.
// Imports must be added before any defined function so indices stay stable.
pub fn (mut m Module) add_import_func(module_ string, name string, type_idx int) int {
	m.imports << ImportFunc{
		module:   module_
		name:     name
		type_idx: type_idx
	}
	m.n_import = m.imports.len
	return m.imports.len - 1
}

// add_global appends a mutable global and returns its index.
pub fn (mut m Module) add_global(valtype u8, init []u8) int {
	m.globals << Global{
		valtype: valtype
		init:    init.clone()
	}
	return m.globals.len - 1
}

// reserve_func_index returns the function index a later add_func will occupy.
pub fn (m &Module) reserve_func_index(defined_so_far int) int {
	return m.n_import + defined_so_far
}

// add_func appends a defined function and returns its global function index.
pub fn (mut m Module) add_func(type_idx int, locals []u8, code []u8) int {
	m.funcs << Func{
		type_idx: type_idx
		locals:   locals.clone()
		code:     code.clone()
	}
	return m.n_import + m.funcs.len - 1
}

pub fn (mut m Module) add_export(name string, kind u8, index int) {
	m.exports << Export{
		name:  name
		kind:  kind
		index: index
	}
}

pub fn (mut m Module) add_data(offset int, bytes []u8) {
	m.datas << DataSeg{
		offset: offset
		bytes:  bytes.clone()
	}
}

pub fn (mut m Module) set_mem_min(pages int) {
	m.mem_min = pages
}

// ---- LEB128 ----

fn leb_u(mut out []u8, val_ u64) {
	mut val := val_
	for {
		mut b := u8(val & 0x7f)
		val >>= 7
		if val != 0 {
			b |= 0x80
		}
		out << b
		if val == 0 {
			break
		}
	}
}

fn leb_i(mut out []u8, val_ i64) {
	mut val := val_
	for {
		mut b := u8(val & 0x7f)
		val >>= 7
		done := (val == 0 && (b & 0x40) == 0) || (val == -1 && (b & 0x40) != 0)
		if !done {
			b |= 0x80
		}
		out << b
		if done {
			break
		}
	}
}

fn write_name(mut out []u8, name string) {
	leb_u(mut out, u64(name.len))
	for c in name {
		out << c
	}
}

// section wraps a section body with its id byte and LEB-encoded length.
fn section(mut out []u8, id u8, body []u8) {
	if body.len == 0 {
		return
	}
	out << id
	leb_u(mut out, u64(body.len))
	out << body
}

// compress_locals run-length-encodes the per-local value types into the wasm
// locals vector: count-of-groups, then (count, valtype) groups.
fn compress_locals(locals []u8) []u8 {
	mut groups := [][]int{}
	for vt in locals {
		if groups.len > 0 && groups[groups.len - 1][1] == int(vt) {
			groups[groups.len - 1][0]++
		} else {
			groups << [1, int(vt)]
		}
	}
	mut out := []u8{}
	leb_u(mut out, u64(groups.len))
	for g in groups {
		leb_u(mut out, u64(g[0]))
		out << u8(g[1])
	}
	return out
}

// compile serializes the whole module into a `.wasm` byte buffer.
pub fn (m &Module) compile() []u8 {
	mut out := []u8{}
	// magic + version
	out << [u8(0x00), 0x61, 0x73, 0x6d]
	out << [u8(0x01), 0x00, 0x00, 0x00]

	// type section (1)
	mut tsec := []u8{}
	leb_u(mut tsec, u64(m.types.len))
	for t in m.types {
		tsec << 0x60
		leb_u(mut tsec, u64(t.params.len))
		tsec << t.params
		leb_u(mut tsec, u64(t.results.len))
		tsec << t.results
	}
	section(mut out, 0x01, tsec)

	// import section (2)
	if m.imports.len > 0 {
		mut isec := []u8{}
		leb_u(mut isec, u64(m.imports.len))
		for imp in m.imports {
			write_name(mut isec, imp.module)
			write_name(mut isec, imp.name)
			isec << 0x00 // import kind: func
			leb_u(mut isec, u64(imp.type_idx))
		}
		section(mut out, 0x02, isec)
	}

	// function section (3)
	mut fsec := []u8{}
	leb_u(mut fsec, u64(m.funcs.len))
	for f in m.funcs {
		leb_u(mut fsec, u64(f.type_idx))
	}
	section(mut out, 0x03, fsec)

	// memory section (5)
	mut msec := []u8{}
	leb_u(mut msec, 1) // one memory
	msec << 0x00 // limits: min only
	leb_u(mut msec, u64(m.mem_min))
	section(mut out, 0x05, msec)

	// global section (6)
	if m.globals.len > 0 {
		mut gsec := []u8{}
		leb_u(mut gsec, u64(m.globals.len))
		for gl in m.globals {
			gsec << gl.valtype
			gsec << 0x01 // mutable
			gsec << gl.init
		}
		section(mut out, 0x06, gsec)
	}

	// export section (7)
	mut esec := []u8{}
	leb_u(mut esec, u64(m.exports.len))
	for e in m.exports {
		write_name(mut esec, e.name)
		esec << e.kind
		leb_u(mut esec, u64(e.index))
	}
	section(mut out, 0x07, esec)

	// code section (10)
	mut csec := []u8{}
	leb_u(mut csec, u64(m.funcs.len))
	for f in m.funcs {
		mut body := compress_locals(f.locals)
		body << f.code
		leb_u(mut csec, u64(body.len))
		csec << body
	}
	section(mut out, 0x0a, csec)

	// data section (11)
	if m.datas.len > 0 {
		mut dsec := []u8{}
		leb_u(mut dsec, u64(m.datas.len))
		for d in m.datas {
			dsec << 0x00 // active segment, memory 0
			dsec << 0x41 // i32.const offset
			leb_i(mut dsec, i64(d.offset))
			dsec << 0x0b // end of offset expr
			leb_u(mut dsec, u64(d.bytes.len))
			dsec << d.bytes
		}
		section(mut out, 0x0b, dsec)
	}

	return out
}
