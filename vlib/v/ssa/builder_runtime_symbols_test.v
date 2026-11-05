module ssa

import os
import v.parser
import v.pref
import v.types

fn test_source_c_declarations_refine_predeclared_runtime_return_types() {
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_text('extern_return_types.v', 'module main
fn C.open(&char, int, ...int) int
fn C.v_os_exec_capture_start(voidptr, voidptr, voidptr) int
fn C.custom_unsigned() u32
fn main() {
	if C.open(c"missing", 0) != -1 { C.custom_unsigned() }
	if C.v_os_exec_capture_start(unsafe { nil }, unsafe { nil }, unsafe { nil }) != -1 {
		C.custom_unsigned()
	}
}
')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := build_with_options(a, map[string]bool{}, &tc, BuildOptions{
		use_native_c_sources: true
	})
	mut found := map[string]bool{}
	main_fns := m.funcs.filter(it.name == 'main')
	assert main_fns.len == 1
	mut source_calls := []Instruction{}
	for block_id in main_fns[0].blocks {
		for value_id in m.blocks[block_id].instrs {
			source_calls << m.instrs[m.values[value_id].index]
		}
	}
	for instr in source_calls {
		if instr.op != .call || instr.operands.len == 0 {
			continue
		}
		callee := m.values[instr.operands[0]]
		if callee.name !in ['open', 'C.open', 'v_os_exec_capture_start', 'C.v_os_exec_capture_start',
			'custom_unsigned', 'C.custom_unsigned'] {
			continue
		}
		f := m.funcs[callee.index]
		assert f.is_c_extern
		assert m.type_store.types[f.typ].width == 32, f.name
		assert m.type_store.types[instr.typ].width == 32, f.name
		assert m.type_store.types[instr.typ].is_unsigned == f.name.ends_with('custom_unsigned')
		found[f.name.trim_string_left('C.')] = true
	}
	assert found.len == 3, found.str()
}

fn test_retained_c_symbols_do_not_select_unreachable_v_library_functions() ! {
	dir := os.join_path(os.vtmp_dir(), 'ssa_c_symbols_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	math_path := os.join_path(dir, 'math.v')
	os_path := os.join_path(dir, 'os.v')
	main_path := os.join_path(dir, 'main.v')
	os.write_file(math_path, 'module math
fn C.floor(f64) f64
fn C.ceil(f64) f64
fn floor(x f64) f64 { return x }
fn ceil(x f64) f64 { return x }
fn copysign(x f64, y f64) f64 { return x }
')!
	os.write_file(os_path, 'module os
fn C.getenv(&char) &char
fn getenv(key string) string { return key }
')!
	os.write_file(main_path, 'module main
fn floor() int { return 17 }
fn main() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([math_path, os_path, main_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	for explicit in [false, true] {
		mut used := {
			'floor':         true
			'ceil':          true
			'getenv':        true
			'math.copysign': true
		}
		if explicit {
			used['math.floor'] = true
			used['math.ceil'] = true
			used['os.getenv'] = true
		}
		m := build_with_used(a, used, unsafe { nil })
		for name in ['floor', 'ceil', 'getenv'] {
			assert m.funcs.any(it.name == 'C.${name}' && it.is_c_extern), name
		}
		assert m.funcs.any(it.name == 'floor' && !it.is_c_extern && it.blocks.len > 0)
		assert m.funcs.any(it.name == 'math.copysign' && it.blocks.len > 0)
		for name in ['math.floor', 'math.ceil', 'os.getenv'] {
			assert m.funcs.any(it.name == name && it.blocks.len > 0) == explicit, name
		}
	}
}

fn assert_runtime_symbol_source_body(m &Module, name string, expected string) {
	mut found := 0
	for f in m.funcs {
		if f.name != name {
			continue
		}
		assert !f.is_c_extern, name
		assert f.params.len == 0, name
		assert f.blocks.len == 1, name
		last := m.blocks[f.blocks[0]].instrs.last()
		instruction := m.instrs[m.values[last].index]
		assert instruction.op == .ret, name
		returned := m.values[instruction.operands[0]]
		assert m.type_store.types[returned.typ].width == 32, name
		assert returned.kind == .instruction, name
		conversion := m.instrs[returned.index]
		assert conversion.op == .trunc && conversion.operands.len == 1, name
		literal := m.values[conversion.operands[0]]
		assert literal.kind == .constant && literal.name == expected, name
		found++
	}
	assert found == 1, name
}

fn test_runtime_replacements_preserve_selected_main_declarations() {
	names := ['wyhash64', 'array_new', 'new_map', 'prealloc_malloc', 'check_fwrite', 'is_dir',
		'bytestr', 'f32_to_str_l', 'normalize_path_in_builder', 'cpu_relax', 'tos2', 'int_str',
		'v3_pthread_create', 'join_path', 'name_list']
	path := os.join_path(os.vtmp_dir(), 'ssa_runtime_user_symbols_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	mut source := 'module main\n'
	mut used := map[string]bool{}
	for i, name in names {
		source += 'fn ${name}() int { return ${i + 17} }\n'
		used[name] = true
	}
	source += "
fn runtime_control() int {
	values := [1, 2]
	mut lookup := map[string]int{}
	lookup['a'] = 3
	lookup['b'] = 4
	lookup['c'] = values[1]
	return lookup['a'] + lookup['b'] + lookup['c']
}
fn source_control() int { return new_map() + array_new() + join_path() + name_list() }
"
	used['runtime_control'] = true
	used['source_control'] = true
	os.write_file(path, source)!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	m := build_with_options(a, used, &tc, BuildOptions{
		target:         TargetData{ ptr_size: 4 }
		exact_used_fns: true
	})
	for i, name in names {
		assert_runtime_symbol_source_body(m, name, '${i + 17}')
	}
	// C helpers retain their own implementations and signatures alongside V names.
	mut c_helpers := 0
	for f in m.funcs {
		if f.name == 'C.wyhash64' {
			assert !f.is_c_extern
			assert f.params.len == 2
			assert f.blocks.len > 0
			c_helpers++
		} else if f.name == 'C.cpu_relax' {
			assert !f.is_c_extern
			assert f.params.len == 0
			assert f.typ == TypeID(0)
			c_helpers++
		} else if f.name == 'C.v3_pthread_create' {
			assert !f.is_c_extern
			assert f.params.len == 4
			assert f.blocks.len > 0
			c_helpers++
		}
	}
	assert c_helpers == 3
	mut runtime_targets := map[string]bool{}
	mut source_targets := map[string]bool{}
	for f in m.funcs {
		if f.name !in ['runtime_control', 'source_control'] {
			continue
		}
		for block_id in f.blocks {
			for value_id in m.blocks[block_id].instrs {
				instruction := m.instrs[m.values[value_id].index]
				if instruction.op != .call {
					continue
				}
				callee := m.funcs[m.values[instruction.operands[0]].index]
				assert m.values[instruction.operands[0]].name == callee.name
				if f.name == 'runtime_control' {
					runtime_targets[callee.name] = true
				} else {
					assert instruction.operands.len == 1
					source_targets[callee.name] = true
				}
			}
		}
	}
	assert runtime_targets['__ssa_runtime_new_map'], runtime_targets.str()
	assert runtime_targets['__ssa_runtime_array_new'], runtime_targets.str()
	assert source_targets == {
		'new_map':   true
		'array_new': true
		'join_path': true
		'name_list': true
	}
}

fn test_runtime_replacement_filters_respect_module_ownership() {
	b := Builder{}
	for name in ['wyhash64', 'panic', 'new_map', 'prealloc_malloc', 'bytestr', '__v3_array_eq_raw'] {
		assert b.skip_source_fn_in_module(name, 'builtin'), name
		for module_name in ['', 'main', 'other', 'example.builtin'] {
			assert !b.skip_source_fn_in_module(name, module_name), '${module_name}.${name}'
		}
	}
	for name in ['new_builder', 'Builder.write_string', 'Builder.push_many'] {
		assert b.skip_source_fn_in_module(name, 'strings'), name
		assert !b.skip_source_fn_in_module(name, 'example.strings'), name
	}
	for name in ['is_dir', 'check_fwrite', 'join_path_single'] {
		assert b.skip_source_fn_in_module(name, 'os'), name
		assert !b.skip_source_fn_in_module(name, 'example.os'), name
	}
	for name in ['f32_to_str_l', 'f64_to_str_l_with_dot'] {
		assert b.skip_source_fn_in_module(name, 'strconv'), name
		assert !b.skip_source_fn_in_module(name, 'other'), name
	}
	// Decimal-string parsing retains its V implementation; only float entry points are replaced.
	for name in ['fxx_to_str_l_parse', 'fxx_to_str_l_parse_with_dot'] {
		assert !b.skip_source_fn_in_module(name, 'strconv'), name
		assert !b.skip_source_fn_in_module(name, 'other'), name
	}
	assert b.skip_source_fn_in_module('PRNG.u64', 'rand')
	assert !b.skip_source_fn_in_module('PRNG.u64', 'other')
	assert b.skip_source_fn_in_module('Decoder.decompress', 'embed_file')
	assert !b.skip_source_fn_in_module('Decoder.decompress', 'other')
}

fn test_runtime_replacements_preserve_nested_module_declarations() {
	dir := os.join_path(os.vtmp_dir(), 'ssa_runtime_nested_symbols_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	path := os.join_path(dir, 'strings.v')
	os.write_file(path, 'module strings
pub fn new_builder() int { return 41 }
pub fn wyhash64() int { return 43 }
pub fn check_fread() int { return 47 }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build_with_options(a, map[string]bool{}, unsafe { nil }, BuildOptions{
		target:         TargetData{ ptr_size: 4 }
		source_modules: {
			path: 'example.strings'
		}
	})
	assert_runtime_symbol_source_body(m, 'example.strings.new_builder', '41')
	assert_runtime_symbol_source_body(m, 'example.strings.wyhash64', '43')
	assert_runtime_symbol_source_body(m, 'example.strings.check_fread', '47')
	mut runtime_builder := false
	for f in m.funcs {
		if f.name == 'strings.new_builder' {
			assert f.params.len == 1
			assert f.blocks.len > 0
			runtime_builder = true
		}
	}
	assert runtime_builder
}

fn test_empty_source_module_override_preserves_builtin_ownership() {
	dir := os.join_path(os.vtmp_dir(), 'ssa_runtime_builtin_owner_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	builtin_source := os.join_path(dir, 'builtin.v')
	main_source := os.join_path(dir, 'main.v')
	os.write_file(builtin_source, 'module builtin
fn new_map_data() int { return 99 }
fn new_map() int { return new_map_data() }
')!
	os.write_file(main_source, 'fn new_map() int { return 31 }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([builtin_source, main_source])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	source_modules := {
		builtin_source: ''
		main_source:    ''
	}
	b := Builder{
		a:              a
		source_modules: source_modules
	}
	for node in a.nodes {
		if node.kind == .module_decl {
			assert b.source_module_name(node) == node.value
		}
	}
	m := build_with_options(a, {
		'new_map': true
	}, unsafe { nil }, BuildOptions{
		target:         TargetData{ ptr_size: 4 }
		exact_used_fns: true
		source_modules: source_modules
	})
	assert_runtime_symbol_source_body(m, 'new_map', '31')
	mut runtime_map := false
	for f in m.funcs {
		assert f.name != 'new_map_data'
		if f.name == '__ssa_runtime_new_map' {
			assert f.params.len == 6
			assert f.blocks.len > 0
			runtime_map = true
		}
	}
	assert runtime_map
}
