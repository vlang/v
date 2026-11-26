// Copyright (c) 2026 the V contributors. All rights reserved.
// Use of this source code is governed by an MIT license in the LICENSE file.
module runtime

import math.bits

pub enum ValueType as u8 {
	i32     = 0x7f
	i64     = 0x7e
	f32     = 0x7d
	f64     = 0x7c
	funcref = 0x70
}

// Value holds a WebAssembly scalar without losing integer or floating-point bits.
pub struct Value {
pub:
	typ  ValueType
	bits u64
}

// i32 creates an i32 value.
pub fn Value.i32(value int) Value { return Value{.i32, u64(u32(value))} }

// i64 creates an i64 value.
pub fn Value.i64(value i64) Value { return Value{.i64, u64(value)} }

// f32 creates an f32 value.
pub fn Value.f32(value f32) Value { return Value{.f32, u64(bits.f32_bits(value))} }

// f64 creates an f64 value.
pub fn Value.f64(value f64) Value { return Value{.f64, bits.f64_bits(value)} }

// as_i32 returns the signed i32 representation.
pub fn (v Value) as_i32() int { return i32(u32(v.bits)) }

// as_i64 returns the signed i64 representation.
pub fn (v Value) as_i64() i64 { return i64(v.bits) }

// as_f32 returns the f32 representation.
pub fn (v Value) as_f32() f32 { return bits.f32_from_bits(u32(v.bits)) }

// as_f64 returns the f64 representation.
pub fn (v Value) as_f64() f64 { return bits.f64_from_bits(v.bits) }

pub struct Signature {
pub:
	parameters []ValueType
	results    []ValueType
}

pub type Callback = fn (mut Instance, []Value) ![]Value

// HostFunction supplies a typed function import. Names are matched separately.
pub struct HostFunction {
pub:
	module    string
	name      string
	signature Signature
	callback  Callback = missing_callback
}

fn missing_callback(mut instance Instance, args []Value) ![]Value {
	_ = instance
	_ = args
	return error('missing host callback')
}

// Config bounds guest resources. Fuel is renewed for each exported call; zero disables it.
@[params]
pub struct Config {
pub:
	max_memory_pages   int = 4096
	max_table_elements int = 1000000
	max_stack_values   int = 1000000
	max_call_depth     int = 1024
	fuel               u64 = 100000000
}

struct Instruction {
	op    u16
	a     int
	b     int
	value u64
}

struct Branch {
mut:
	pc     int
	height int
	arity  int
}

struct Function {
	typ    int
	module string
	name   string
mut:
	locals       []ValueType
	code         []Instruction
	branches     []Branch
	branch_table []int
	stack_size   int
}

struct Limits {
	min int
	max int
}

struct Global {
	typ     ValueType
	mutable bool
mut:
	value u64
}

pub struct Export {
pub:
	name  string
	kind  u8
	index int
}

struct Segment {
	passive  bool
	offset   u32
	data     []u8
	elements []u64
}

// Module is validated, decoded bytecode and can be instantiated repeatedly.
@[heap]
pub struct Module {
pub:
	exports []Export
mut:
	types                []Signature
	functions            []Function
	import_count         int
	globals              []Global
	memory               ?Limits
	table                ?Limits
	data                 []Segment
	elements             []Segment
	start                int = -1
	data_count           int = -1
	decoded_instructions int
}

// Instance owns the mutable state of one module. Calls on one instance are sequential.
@[heap]
pub struct Instance {
	mod    &Module
	config Config
mut:
	hosts          []Callback
	globals        []Global
	memory         []u8
	table          []u64
	data           [][]u8
	elements       [][]u64
	remaining_fuel u64
	running        bool
}

// read_memory returns a copy of a checked range of guest memory.
pub fn (instance &Instance) read_memory(offset u32, size u32) ![]u8 {
	end := u64(offset) + size
	if end > u64(instance.memory.len) { return error('out of bounds memory access') }
	return instance.memory[int(offset)..int(end)].clone()
}

// write_memory copies bytes into a checked range of guest memory.
pub fn (mut instance Instance) write_memory(offset u32, data []u8) ! {
	end := u64(offset) + u64(data.len)
	if end > u64(instance.memory.len) { return error('out of bounds memory access') }
	copy(mut instance.memory[int(offset)..int(end)], data)
}

// signature returns the type of an exported function.
pub fn (mod &Module) signature(name string) !Signature {
	for e in mod.exports {
		if e.name == name && e.kind == 0 { return mod.types[mod.functions[e.index].typ] }
	}
	return error('unknown function export `${name}`')
}

// imports lists the function imports and their required signatures, without callbacks.
pub fn (mod &Module) imports() []HostFunction {
	mut result := []HostFunction{cap: mod.import_count}
	for f in mod.functions[..mod.import_count] {
		result << HostFunction{ module: f.module, name: f.name, signature: mod.types[f.typ] }
	}
	return result
}

// get_global reads an exported global.
pub fn (instance &Instance) get_global(name string) !Value {
	for e in instance.mod.exports {
		if e.name == name && e.kind == 3 {
			g := instance.globals[e.index]
			return Value{g.typ, g.value}
		}
	}
	return error('unknown global export `${name}`')
}

// set_global writes a mutable exported global after checking its value type.
pub fn (mut instance Instance) set_global(name string, value Value) ! {
	for e in instance.mod.exports {
		if e.name == name && e.kind == 3 {
			g := instance.globals[e.index]
			if !g.mutable { return error('write to immutable global') }
			check_values([value], [g.typ], instance.mod.functions.len)!
			instance.globals[e.index].value = raw_value(value)
			return
		}
	}
	return error('unknown global export `${name}`')
}

// instantiate links host functions, initializes memory and tables, and runs the start section.
pub fn instantiate(mod &Module, imports []HostFunction, config Config) !Instance {
	if config.max_memory_pages < 0 || config.max_memory_pages > 32767
		|| config.max_table_elements < 0 || config.max_stack_values <= 0 || config.max_call_depth <= 0 {
		return error('invalid runtime resource limits')
	}
	mut instance := Instance{ mod: mod, config: config, globals: mod.globals.clone() }
	for f in mod.functions[..mod.import_count] {
		mut found := false
		for host in imports {
			if host.module == f.module && host.name == f.name {
				if host.signature != mod.types[f.typ] {
					return error('incompatible import `${f.module}.${f.name}`')
				}
				instance.hosts << host.callback
				found = true
				break
			}
		}
		if !found { return error('missing import `${f.module}.${f.name}`') }
	}
	if limits := mod.memory {
		if limits.min > config.max_memory_pages { return error('memory limit exceeded') }
		instance.memory = []u8{len: limits.min * 65536}
	}
	if limits := mod.table {
		if limits.min > config.max_table_elements { return error('table limit exceeded') }
		instance.table = []u64{len: limits.min}
	}
	for segment in mod.data {
		if segment.passive {
			instance.data << segment.data.clone()
		} else {
			instance.write_memory(segment.offset, segment.data)!
			instance.data << []u8{}
		}
	}
	for segment in mod.elements {
		if segment.passive {
			instance.elements << segment.elements.clone()
		} else {
			end := u64(segment.offset) + u64(segment.elements.len)
			if end > u64(instance.table.len) { return error('out of bounds table initialization') }
			copy(mut instance.table[int(segment.offset)..int(end)], segment.elements)
			instance.elements << []u64{}
		}
	}
	if mod.start >= 0 { instance.invoke(mod.start, [])! }
	return instance
}

// call invokes an exported function, checking argument and result types at the host boundary.
pub fn (mut instance Instance) call(name string, args []Value) ![]Value {
	for e in instance.mod.exports {
		if e.name == name && e.kind == 0 { return instance.invoke(e.index, args) }
	}
	return error('unknown function export `${name}`')
}
