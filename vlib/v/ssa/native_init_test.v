module ssa

import os
import v.parser
import v.pref

fn test_native_initialization_runs_before_main() {
	path := os.join_path(os.vtmp_dir(), 'native_startup_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, '__global message = "initialized"
__global count = 41
fn init() { count++ }
fn main() { println(message) }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build(a)
	main_fn := m.funcs.filter(it.name == 'main')[0]
	first := m.values[m.blocks[main_fn.blocks[0]].instrs[0]]
	call := m.instrs[first.index]
	assert call.op == .call
	assert m.values[call.operands[0]].name == '__ssa_init'
	mut stores := 0
	mut called_init := false
	for instruction in native_startup_instructions(m) {
		if instruction.op == .store {
			stores++
		} else if instruction.op == .call {
			called_init = called_init || m.values[instruction.operands[0]].name == 'init'
		}
	}
	assert stores == 2
	assert called_init
	message := m.values.filter(it.kind == .global && it.name == 'message')[0]
	assert m.type_store.types[message.typ].elem_type == m.funcs.filter(it.name == 'tos3')[0].typ
}

fn test_native_module_initialization_orders_dependencies_once() {
	imports := {
		'main':   ['parent', 'child']
		'parent': ['child']
	}
	mut visited := map[string]bool{}
	mut order := []string{}
	native_module_init_order('main', imports, mut visited, mut order)
	assert order == ['child', 'parent', 'main']
}

fn test_native_static_mutex_initialization_calls_pthread_init() {
	path := os.join_path(os.vtmp_dir(), 'native_mutex_init_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'module main
struct C.pthread_mutex_t {}
__global mutex C.pthread_mutex_t = C.PTHREAD_MUTEX_INITIALIZER
fn main() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build(a)
	mut found := false
	for instruction in native_startup_instructions(m) {
		if instruction.op == .call && m.values[instruction.operands[0]].name == 'pthread_mutex_init' {
			found = true
		}
	}
	assert found
}

fn test_native_global_arrays_have_static_storage_and_integer_stores_match_the_declared_width() {
	path := os.join_path(os.vtmp_dir(), 'native_global_storage_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, '__global bytes = [128]u8{}
__global number = 7
fn main() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build(a)
	bytes := m.globals.filter(it.name == 'bytes')[0]
	assert m.type_store.types[bytes.typ].kind == .array_t
	assert m.type_size(bytes.typ) == 128
	number := m.values.filter(it.kind == .global && it.name == 'number')[0]
	mut found := false
	for instruction in native_startup_instructions(m) {
		if instruction.op == .store && instruction.operands[1] == number.id {
			value := m.values[instruction.operands[0]]
			assert value.typ == m.type_store.types[number.typ].elem_type
			found = true
		}
	}
	assert found
}

fn test_native_process_argument_globals_keep_the_entry_abi_and_are_not_reinitialized() {
	path := os.join_path(os.vtmp_dir(), 'native_argument_globals_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, '__global g_main_argc = int(0)
__global g_main_argv = unsafe { nil }
fn main() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build(a)
	argc := m.globals.filter(it.name == 'g_main_argc')[0]
	argv := m.globals.filter(it.name == 'g_main_argv')[0]
	assert m.type_size(argc.typ) == 8
	assert m.type_store.types[argv.typ].kind == .ptr_t
	assert m.type_store.types[m.type_store.types[argv.typ].elem_type].kind == .ptr_t
	startup := m.funcs.filter(it.name == '__ssa_init')[0]
	for block in startup.blocks {
		for id in m.blocks[block].instrs {
			instruction := m.instrs[m.values[id].index]
			assert instruction.op != .store
		}
	}
}

fn native_startup_instructions(m &Module) []Instruction {
	mut instructions := []Instruction{}
	for function in m.funcs {
		if !function.name.starts_with('__ssa_init') {
			continue
		}
		for block in function.blocks {
			for id in m.blocks[block].instrs {
				instructions << m.instrs[m.values[id].index]
			}
		}
	}
	return instructions
}
