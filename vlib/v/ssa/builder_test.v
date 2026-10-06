module ssa

import os
import v.flat
import v.parser
import v.pref
import v.types

// test_bench_runtime_stubs_include_macos_rss_helper validates this v3 regression case.
fn test_bench_runtime_stubs_include_macos_rss_helper() {
	assert 'macos_rss_kb' in bench_runtime_stub_names
	assert 'bench.macos_rss_kb' in bench_runtime_stub_names
	assert 'macos_peak_rss_kb' !in bench_runtime_stub_names
	assert 'bench.macos_peak_rss_kb' !in bench_runtime_stub_names
	b := Builder{}
	assert b.skip_source_fn('macos_rss_kb')
	assert b.skip_source_fn('bench.macos_rss_kb')
}

// test_runtime_helpers_remain_used_when_module_qualified validates this v3 regression case.
fn test_runtime_helpers_remain_used_when_module_qualified() {
	b := Builder{}
	for name in ['os.vpopen', 'os.vpclose', 'os__vpopen', 'os__vpclose', 'os.fileno', 'os.Process.close',
		'os__Process__close', 'os.fd_close', 'os__fd_close', 'os.error_file_not_opened',
		'os.error_size_of_type_0', 'os.posix_wait4_to_exit_status', 'os.posix_wait_status_exited',
		'os.posix_wait_status_exit_code', 'os.posix_wait_status_signaled', 'os.posix_wait_status_signal'] {
		assert b.fn_is_used(name)
	}
}

// test_native_c_tm_uses_the_platform_abi_layout validates this v3 regression case.
fn test_native_c_tm_uses_the_platform_abi_layout() {
	assert native_c_struct_field_type('C.tm', 'tm_gmtoff') or { '' } == 'i64'
	assert native_c_struct_field_type('C.tm', 'tm_sec') == none
}

// test_native_task_basic_info_uses_the_platform_abi_layout validates this v3 regression case.
fn test_native_task_basic_info_uses_the_platform_abi_layout() {
	abi := native_c_struct_abi('C.task_basic_info') or { panic('missing task info ABI') }
	assert abi.field_names[1] == 'resident_size'
	assert abi.field_types == ['u64', 'u64', 'u64', 'i32', 'i32', 'i32', 'i32', 'i32', 'i32']
	assert native_c_struct_abi('C.other') == none
}

// test_native_rusage_uses_the_platform_abi_layout validates this v3 regression case.
fn test_native_rusage_uses_the_platform_abi_layout() {
	abi := native_c_struct_abi('C.rusage') or { panic('missing rusage ABI') }
	assert abi.field_names[4] == 'ru_maxrss'
	assert abi.field_types.len == 18
	assert abi.field_types.all(it == 'i64')
	assert '_proc_pid_rusage' in arm64_force_external_syms
}

fn test_native_darwin_pthread_types_use_the_platform_abi_layout() {
	expected := {
		'C.pthread_mutex_t':      '[8]u64'
		'C.pthread_rwlock_t':     '[25]u64'
		'C.pthread_rwlockattr_t': '[3]u64'
		'C.pthread_cond_t':       '[6]u64'
		'C.pthread_condattr_t':   '[2]u64'
	}
	for name, field_type in expected {
		abi := native_c_struct_abi(name) or { panic('missing ${name} ABI') }
		assert abi.field_names == ['opaque']
		assert abi.field_types == [field_type]
	}
}

// test_builtin_ownership_drop_names_are_ssa_intrinsics validates this v3 regression case.
fn test_builtin_ownership_drop_names_are_ssa_intrinsics() {
	tc := &types.TypeChecker{
		fn_type_modules: {
			'drop_owned': 'builtin'
		}
	}
	b := Builder{
		tc: tc
	}
	for name in ['drop_owned', 'drop_owned_T_string', 'builtin.drop_owned',
		'builtin__drop_owned_T_array', 'drop_owned_v3_interface',
		'builtin.drop_owned_v3_interface_T_Foo'] {
		assert b.ownership_drop_intrinsic_name(name)
	}
}

// test_v3_pthread_create_uses_the_sync_fallback validates this v3 regression case.
fn test_v3_pthread_create_uses_the_sync_fallback() {
	a := &flat.FlatAst{}
	m := build(a)
	mut found := false
	for f in m.funcs {
		if f.name != 'v3_pthread_create' {
			continue
		}
		assert f.blocks.len > 0
		for block_id in f.blocks {
			for value_id in m.blocks[block_id].instrs {
				value := m.values[value_id]
				if value.kind != .instruction {
					continue
				}
				instruction := m.instrs[value.index]
				if instruction.op == .ret && instruction.operands.len == 1 {
					result := m.values[instruction.operands[0]]
					assert result.kind == .constant
					assert result.name == '11'
					found = true
				}
			}
		}
	}
	assert found
}

// test_prealloc_allocator_stubs_use_native_safe_fallbacks validates this v3 regression case.
fn test_prealloc_allocator_stubs_use_native_safe_fallbacks() {
	a := &flat.FlatAst{}
	m := build(a)
	for name in ['prealloc_malloc', 'prealloc_calloc', 'prealloc_malloc_align', 'prealloc_realloc',
		'prealloc_scope_begin', 'v_realloc'] {
		mut found := false
		for f in m.funcs {
			if f.name == name {
				assert !f.is_c_extern
				assert f.blocks.len > 0
				found = true
				break
			}
		}
		assert found, 'missing native allocator fallback `${name}`'
	}

	mut scope_returns_nil := false
	for f in m.funcs {
		if f.name != 'prealloc_scope_begin' {
			continue
		}
		for block_id in f.blocks {
			for value_id in m.blocks[block_id].instrs {
				instruction := m.instrs[m.values[value_id].index]
				if instruction.op == .ret && instruction.operands.len == 1 {
					result := m.values[instruction.operands[0]]
					scope_returns_nil = result.kind == .constant && result.name == '0'
				}
			}
		}
	}
	assert scope_returns_nil
}

// test_map_clone_uses_the_runtime_pointer_abi validates this v3 regression case.
fn test_map_clone_uses_the_runtime_pointer_abi() {
	a := &flat.FlatAst{}
	m := build(a)
	for f in m.funcs {
		if f.name != 'map__clone' {
			continue
		}
		assert f.params.len == 1
		param_type := m.type_store.types[m.values[f.params[0]].typ]
		assert param_type.kind == .ptr_t
		assert m.type_store.types[param_type.elem_type].kind == .struct_t
		return
	}
	assert false, 'missing native map clone helper'
}

fn test_native_modulecache_metadata_helper_uses_hash_fallback() {
	a := &flat.FlatAst{}
	m := build(a)
	for f in m.funcs {
		if f.name == 'v3_modulecache_file_metadata' {
			assert !f.is_c_extern
			assert f.params.len == 8
			assert f.blocks.len == 1
			return
		}
	}
	assert false, 'missing native module-cache metadata fallback'
}

// test_release_codegen_analysis_metadata_keeps_codegen_data validates this v3 regression case.
fn test_release_codegen_analysis_metadata_keeps_codegen_data() {
	a := &flat.FlatAst{}
	mut m := build(a)
	values_len := m.values.len
	blocks_len := m.blocks.len
	funcs_len := m.funcs.len
	m.release_codegen_analysis_metadata()
	assert m.values.len == values_len
	assert m.blocks.len == blocks_len
	assert m.funcs.len == funcs_len
	assert m.values.all(it.uses.len == 0)
	assert m.blocks.all(it.preds.len == 0 && it.succs.len == 0 && it.dom_tree.len == 0)
}

// test_build_can_skip_optimizer_use_lists validates this v3 regression case.
fn test_build_can_skip_optimizer_use_lists() {
	a := &flat.FlatAst{}
	m := build_with_options(a, map[string]bool{}, unsafe { nil }, BuildOptions{
		track_uses: false
	})
	assert !m.track_uses
	assert m.values.all(it.uses.len == 0)
}

fn test_wasm32_and_native_string_layouts() {
	a := &flat.FlatAst{}
	for pointer_size in [4, 8] {
		m := build_with_options(a, map[string]bool{}, unsafe { nil }, BuildOptions{
			target: TargetData{ ptr_size: pointer_size }
		})
		mut found := false
		for f in m.funcs {
			if f.name != 'println' {
				continue
			}
			string_type := m.values[f.params[0]].typ
			pointer_type := m.type_store.types[string_type].fields[0]
			assert m.type_size(pointer_type) == pointer_size
			assert m.type_align(pointer_type) == pointer_size
			assert m.struct_field_offset(string_type, 1) == pointer_size
			assert m.struct_field_offset(string_type, 2) == pointer_size + 4
			assert m.type_size(string_type) == pointer_size + 8
			found = true
		}
		assert found
	}
}

fn test_wasm32_global_constant_initializers() {
	path := os.join_path(os.vtmp_dir(), 'ssa_wasm_global_initializers_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'const base = 10\n__global nested = int(u8(u16(300)))\n__global signed = int(i8(128))\n__global wrapped = int(u8(250) + u8(10))\n__global folded = base * 4 + 1\n__global half = u64(18446744073709551615) / u64(2)\n__global fractional = 1.5 + 2.0\n__global oversized64 = u64(1) << u64(64)\n__global oversized32 = u32(1) << u64(32)\n__global narrow_signed = int(i8(-1) >> u64(8))\n__global narrow_logical = int(i8(-5) >>> 1)\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	m := build_with_options(a, map[string]bool{}, &tc, BuildOptions{
		target: TargetData{ ptr_size: 4 }
	})
	expected := {
		'nested':         i64(44)
		'signed':         i64(-128)
		'wrapped':        i64(4)
		'folded':         i64(41)
		'half':           i64(9223372036854775807)
		'oversized64':    i64(0)
		'oversized32':    i64(0)
		'narrow_signed':  i64(-1)
		'narrow_logical': i64(125)
	}
	mut found := 0
	for global in m.globals {
		if value := expected[global.name] {
			assert global.initial_value == value, global.name
			found++
		} else if global.name == 'fractional' {
			assert global.initial_data == [u8(0), 0, 0, 0, 0, 0, 12, 64]
			found++
		}
	}
	assert found == expected.len + 1
}

fn test_wasm32_scalar_str_methods_without_native_runtime_bodies() {
	dir := os.join_path(os.vtmp_dir(), 'ssa_wasm_scalar_str_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	builtin_source := os.join_path(dir, 'builtin.v')
	main_source := os.join_path(dir, 'main.v')
	os.write_file(builtin_source, 'module builtin\nfn (value int) str() string { return "native signed" }\nfn (value u64) str() string { return "native unsigned" }\nfn (value bool) str() string { return "native boolean" }\n')!
	os.write_file(main_source, 'module main\nstruct Named {}\nfn (value Named) str() string { return "user method" }\nfn main() {\n signed := int(-1).str()\n unsigned := u64(18446744073709551615).str()\n boolean := true.str()\n user := Named{}.str()\n wide := (-9223372036854775807).str()\n}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_files([builtin_source, main_source])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	for transformed in [false, true] {
		if transformed {
			// The transformer moves method receivers into explicit call arguments.
			for id in 0 .. a.nodes.len {
				node := a.nodes[id]
				if node.kind != .call || node.children_count != 1 {
					continue
				}
				callee_id := a.child(&node, 0)
				callee := a.nodes[int(callee_id)]
				if callee.kind != .selector || callee.value != 'str' {
					continue
				}
				base_id := a.child(&callee, 0)
				base := a.nodes[int(base_id)]
				primitive := if base.kind == .bool_literal {
					'bool'
				} else if base.kind == .paren {
					'int'
				} else {
					base.value
				}
				if primitive !in ['int', 'u64', 'bool'] {
					continue
				}
				a.nodes[int(callee_id)].kind = .ident
				a.nodes[int(callee_id)].value = '${primitive}.str'
				a.nodes[int(callee_id)].children_count = 0
				a.nodes[id].children_start = i32(a.begin_children())
				a.add_child(callee_id)
				a.add_child(base_id)
				a.nodes[id].children_count = 2
			}
		}
		m := build_with_options(a, {
			'main':      true
			'Named.str': true
		}, &tc, BuildOptions{
			target:         TargetData{ ptr_size: 4 }
			exact_used_fns: true
		})
		mut targets := []string{}
		mut signed_widths := []int{}
		for f in m.funcs {
			assert f.name !in ['int.str', 'u64.str', 'bool.str']
			if f.name != 'main' {
				continue
			}
			for block in f.blocks {
				for value in m.blocks[block].instrs {
					instruction := m.instrs[m.values[value].index]
					if instruction.op == .call {
						target := m.values[instruction.operands[0]].name
						targets << target
						if target == 'int_str' {
							signed_widths << m.type_store.types[m.values[instruction.operands[1]].typ].width
						}
					}
				}
			}
		}
		assert targets == ['int_str', 'strconv__format_uint', 'bool_str', 'Named.str', 'int_str']
		assert signed_widths == [32, 64]
	}
}

fn test_type_size_reuses_module_layout_cache() {
	mut m := Module.new()
	i32_type := m.type_store.get_int(32)
	array_type := m.type_store.get_array(i32_type, 5)
	struct_type := m.type_store.register(Type{
		kind:        .struct_t
		fields:      [i32_type, array_type]
		field_names: ['number', 'items']
	})
	assert m.type_size(array_type) == 20
	assert m.struct_field_offset(struct_type, 1) == 4
	assert m.struct_field_size(struct_type, 1) == 20
	assert m.type_size_cache.len == m.type_store.types.len
	cache_capacity := m.type_size_cache.cap
	for _ in 0 .. 100 {
		assert m.type_size(array_type) == 20
		assert m.struct_field_offset(struct_type, 1) == 4
		assert m.struct_field_size(struct_type, 1) == 20
	}
	assert m.type_size_cache.cap == cache_capacity
	m.freeze_type_layouts()
	assert m.type_layout_frozen
	assert m.struct_field_size(struct_type, 0) == 4
	frozen_sizes := m.type_size_cache.clone()
	frozen_alignments := m.type_align_cache.clone()
	frozen_offsets := m.field_offset_cache.clone()
	frozen_field_sizes := m.field_size_cache.clone()
	for _ in 0 .. 100 {
		assert m.type_size(array_type) == 20
		assert m.type_align(struct_type) == 4
		assert m.struct_field_offset(struct_type, 1) == 4
		assert m.struct_field_size(struct_type, 1) == 20
	}
	assert m.type_size_cache == frozen_sizes
	assert m.type_align_cache == frozen_alignments
	assert m.field_offset_cache == frozen_offsets
	assert m.field_size_cache == frozen_field_sizes
	assert m.type_size_visiting.all(!it)
}

fn test_packed_and_aligned_struct_layout() {
	mut m := Module.new()
	u8_type := m.type_store.get_uint(8)
	u64_type := m.type_store.get_uint(64)
	packed_type := m.type_store.register(Type{
		kind:      .struct_t
		fields:    [u8_type, u64_type]
		is_packed: true
	})
	aligned_type := m.type_store.register(Type{
		kind:      .struct_t
		fields:    [u8_type, u64_type]
		alignment: 32
	})
	small_type := m.type_store.register(Type{
		kind:   .struct_t
		fields: [u8_type, u8_type]
	})
	outer_type := m.type_store.register(Type{
		kind:   .struct_t
		fields: [u8_type, small_type, u8_type]
	})
	assert m.struct_field_offset(packed_type, 1) == 1
	assert m.type_size(packed_type) == 9
	assert m.type_align(packed_type) == 1
	assert m.struct_field_offset(aligned_type, 1) == 8
	assert m.type_size(aligned_type) == 32
	assert m.type_align(aligned_type) == 32
	assert m.type_align(small_type) == 1
	assert m.struct_field_offset(outer_type, 1) == 1
	assert m.struct_field_offset(outer_type, 2) == 3
	assert m.type_size(outer_type) == 4
}

fn test_used_function_alias_lookups_are_precomputed() {
	mut b := Builder{
		used_fns:           {
			'alpha.beta.gamma':      true
			'delta__Thing__run':     true
			'leaf':                  true
			'outer.inner.operation': true
		}
		used_fn_normalized: map[string]bool{}
		used_fn_suffixes:   map[string]bool{}
	}
	b.prepare_used_fn_lookups()
	for name in ['gamma', 'beta.gamma', 'Thing.run', 'run', 'pkg.leaf', 'scope.outer.inner.operation'] {
		assert b.fn_is_used(name), 'expected `${name}` to match a used function alias'
	}
	for name in ['missing', 'beta.missing', 'scope.outer.other'] {
		assert !b.fn_is_used(name), 'did not expect `${name}` to match a used function alias'
	}
}

fn test_enum_lookup_keeps_exact_keyword_members_before_fallback() {
	b := Builder{
		enum_values: {
			'Keyword.@none':  -10
			'Plain.struct':   7
			'Distinct.none':  2
			'Distinct.@none': 4
		}
	}
	for member in ['none', '@none', 'Keyword.none', 'Keyword.@none'] {
		assert b.enum_value_for_type('Keyword', member) or { 0 } == -10
	}
	assert b.enum_value_for_type('Plain', '@struct') or { 0 } == 7
	assert b.enum_value_for_type('Distinct', 'none') or { 0 } == 2
	assert b.enum_value_for_type('Distinct', '@none') or { 0 } == 4
}

fn test_native_enum_registration_evaluates_escaped_initializer_references() {
	path := os.join_path(os.vtmp_dir(), 'v3_ssa_escaped_enum_initializers_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'module models\nenum Kind { struct = 4 next = int(Kind.@struct) + 6 @none = 11 reverse = int(Kind.none) + 2 type = 17 @type = 23 plain_exact = int(Kind.type) + 3 escaped_exact = int(Kind.@type) + 3 }\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut b := Builder{ a: p.a }
	for node in p.a.nodes {
		if node.kind == .enum_decl {
			b.register_enum_values(node, 'models')
		}
	}
	for field, expected in {
		'next':          10
		'reverse':       13
		'plain_exact':   20
		'escaped_exact': 26
	} {
		assert b.enum_value_for_type('models.Kind', field) or { -1 } == expected, field
	}
}
