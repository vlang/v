module ssa

import v3.flat
import v3.types

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
	for name in ['os.vpopen', 'os.vpclose', 'os__vpopen', 'os__vpclose', 'os.fileno',
		'os.Process.close', 'os__Process__close', 'os.fd_close', 'os__fd_close',
		'os.error_file_not_opened', 'os.error_size_of_type_0', 'os.posix_wait4_to_exit_status',
		'os.posix_wait_status_exited', 'os.posix_wait_status_exit_code',
		'os.posix_wait_status_signaled', 'os.posix_wait_status_signal'] {
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

fn test_type_size_reuses_module_layout_cache() {
	mut m := Module.new()
	i32_type := m.type_store.get_int(32)
	array_type := m.type_store.get_array(i32_type, 5)
	struct_type := m.type_store.register(Type{
		kind: .struct_t
		fields: [i32_type, array_type]
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
}

fn test_packed_and_aligned_struct_layout() {
	mut m := Module.new()
	u8_type := m.type_store.get_uint(8)
	u64_type := m.type_store.get_uint(64)
	packed_type := m.type_store.register(Type{
		kind: .struct_t
		fields: [u8_type, u64_type]
		is_packed: true
	})
	aligned_type := m.type_store.register(Type{
		kind: .struct_t
		fields: [u8_type, u64_type]
		alignment: 32
	})
	small_type := m.type_store.register(Type{
		kind: .struct_t
		fields: [u8_type, u8_type]
	})
	outer_type := m.type_store.register(Type{
		kind: .struct_t
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
		used_fns: {
			'alpha.beta.gamma':      true
			'delta__Thing__run':     true
			'leaf':                  true
			'outer.inner.operation': true
		}
		used_fn_normalized: map[string]bool{}
		used_fn_suffixes: map[string]bool{}
	}
	b.prepare_used_fn_lookups()
	for name in ['gamma', 'beta.gamma', 'Thing.run', 'run', 'pkg.leaf', 'scope.outer.inner.operation'] {
		assert b.fn_is_used(name), 'expected `${name}` to match a used function alias'
	}
	for name in ['missing', 'beta.missing', 'scope.outer.other'] {
		assert !b.fn_is_used(name), 'did not expect `${name}` to match a used function alias'
	}
}
