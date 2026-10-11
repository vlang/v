module ssa

import os
import v.flat
import v.parser
import v.pref
import v.transform
import v.types

fn test_native_noreturn_calls_terminate_only_their_control_flow_edges() {
	m := native_thread_test_module('noreturn', 'module main
fn C.exit(int)
@[noreturn]
fn C.abort()
@[noreturn]
fn stop() { C.exit(1) }
fn noop() {}
fn builtin_branch(take bool) int {
    if take { panic("stop") }
    return 17
}
fn attributed_branch(take bool) int {
    if take { stop() }
    return 19
}
fn interop_branch(take bool) int {
    if take { C.abort() }
    return 23
}
fn returning_branch(take bool) int {
    if take { noop() }
    return 29
}
fn main() {}
', false)
	for name, callee in {
		'builtin_branch':    'panic'
		'attributed_branch': 'stop'
		'interop_branch':    'abort'
	} {
		f := m.funcs.filter(it.name == name)[0]
		mut checked_call := false
		mut has_success_return := false
		for block_id in f.blocks {
			instrs := m.blocks[block_id].instrs
			for index, value_id in instrs {
				instr := m.instrs[m.values[value_id].index]
				if instr.op == .call && m.values[instr.operands[0]].name == callee {
					assert index + 1 < instrs.len, name
					assert m.instrs[m.values[instrs[index + 1]].index].op == .unreachable, name
					checked_call = true
				}
				if instr.op == .ret {
					has_success_return = true
				}
			}
		}
		assert checked_call, name
		assert has_success_return, name
	}
	ordinary := m.funcs.filter(it.name == 'returning_branch')[0]
	for block_id in ordinary.blocks {
		for value_id in m.blocks[block_id].instrs {
			assert m.instrs[m.values[value_id].index].op != .unreachable
		}
	}
}

fn test_native_field_type_lookup_preserves_containers_through_pointer_receivers() {
	b := Builder{
		struct_field_types: {
			'Checker.ids': 'map[string][]int'
		}
	}
	for receiver in ['Checker', '&Checker', '&&Checker', 'types.Checker', '&types.Checker'] {
		assert b.field_type_name(receiver, 'ids') == 'map[string][]int'
	}
	assert b.field_type_name('&Checker', 'missing') == ''
}

// test_bench_runtime_stubs_include_macos_rss_helper validates this v3 regression case.
fn test_bench_runtime_stubs_include_macos_rss_helper() {
	assert 'bench.macos_rss_kb' in bench_runtime_stub_names
	assert 'v.bench.macos_rss_kb' in bench_runtime_stub_names
	assert 'macos_rss_kb' in bench_runtime_stub_names
	assert 'macos_rss_kb' !in wasm_bench_runtime_stub_names
	assert wasm_bench_runtime_stub_names.all(it.contains('.'))
	assert 'macos_peak_rss_kb' !in bench_runtime_stub_names
	assert 'bench.macos_peak_rss_kb' !in bench_runtime_stub_names
	b := Builder{}
	for name in ['current_rss_kb', 'macos_rss_kb', 'linux_rss_kb'] {
		assert b.skip_source_fn_in_module(name, 'bench')
		assert b.skip_source_fn_in_module(name, 'v.bench')
		assert !b.skip_source_fn_in_module(name, 'main')
		assert !b.skip_source_fn_in_module(name, 'other')
	}
}

fn test_bench_runtime_stubs_preserve_user_functions() {
	path := os.join_path(os.vtmp_dir(), 'ssa_user_rss_helpers_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'module main\nfn current_rss_kb() i64 { return 17 }\nfn macos_rss_kb() i64 { return 23 }\nfn linux_rss_kb() i64 { return 31 }\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build_with_options(a, map[string]bool{}, unsafe { nil }, BuildOptions{
		target: TargetData{ ptr_size: 4 }
	})
	expected := {
		'current_rss_kb': '17'
		'macos_rss_kb':   '23'
		'linux_rss_kb':   '31'
	}
	mut found := 0
	for f in m.funcs {
		if want := expected[f.name] {
			assert f.blocks.len == 1
			last := m.blocks[f.blocks[0]].instrs.last()
			instruction := m.instrs[m.values[last].index]
			assert instruction.op == .ret
			assert m.values[instruction.operands[0]].name == want
			found++
		}
	}
	assert found == expected.len
}

fn test_char_literal_value_decodes_unicode_and_escapes() {
	expected := {
		'A':           65
		'é':           233
		'★':           9733
		'😀':          128512
		r'\x41':       65
		r'\u0041':     65
		r'\U0001F600': 128512
		r'\101':       65
		r'\0':         0
		r'\a':         7
		r'\b':         8
		r'\e':         27
		r'\f':         12
		r'\n':         10
		r'\r':         13
		r'\t':         9
		r'\v':         11
		r'\\':         92
	}
	for value, want in expected {
		assert char_literal_value(value) == want, value
	}
}

fn test_rune_literals_use_rune_width() {
	path := os.join_path(os.vtmp_dir(), 'ssa_rune_literals_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn unicode() rune { return `😀` }\nfn escaped_ascii() rune { return `\\u0041` }\nfn escaped_unicode() rune { return `\\xe2\\x98\\x85` }\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build_with_options(a, map[string]bool{}, unsafe { nil }, BuildOptions{
		target: TargetData{ ptr_size: 4 }
	})
	expected := {
		'unicode':         '128512'
		'escaped_ascii':   '65'
		'escaped_unicode': '9733'
	}
	mut found := 0
	for f in m.funcs {
		if want := expected[f.name] {
			last := m.blocks[f.blocks[0]].instrs.last()
			instruction := m.instrs[m.values[last].index]
			assert instruction.op == .ret
			value := m.values[instruction.operands[0]]
			assert value.name == want
			assert m.type_store.types[value.typ].width == 32
			found++
		}
	}
	assert found == expected.len
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
	abi := native_c_struct_abi('C.task_basic_info', 8) or { panic('missing task info ABI') }
	assert abi.field_names[1] == 'resident_size'
	assert abi.field_types == ['u64', 'u64', 'u64', 'i32', 'i32', 'i32', 'i32', 'i32', 'i32']
	assert native_c_struct_abi('C.other', 8) == none
}

// test_native_rusage_uses_the_platform_abi_layout validates this v3 regression case.
fn test_native_rusage_uses_the_platform_abi_layout() {
	abi := native_c_struct_abi('C.rusage', 8) or { panic('missing rusage ABI') }
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
		abi := native_c_struct_abi(name, 8) or { panic('missing ${name} ABI') }
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

fn test_native_map_stubs_replace_only_builtin_map_data_methods() ! {
	root := os.join_path(os.vtmp_dir(), 'ssa_map_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	builtin_path := os.join_path(root, 'builtin.v')
	user_path := os.join_path(root, 'models.v')
	// C-only runtime dependencies are absent from the native runtime's used functions.
	os.write_file(builtin_path, 'module builtin\nstruct VMapData {}\nfn (mut data VMapData) free() { missing_c_map_cleanup() }\n')!
	os.write_file(user_path, 'module models\nstruct VMapData {}\nfn (mut data VMapData) free() {}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(builtin_path)
	p.parse_file(user_path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build_with_used(p.a, {
		'free':                 true
		'models.VMapData.free': true
	}, unsafe { nil })
	assert !m.funcs.any(it.name == 'VMapData.free')
	assert m.funcs.any(it.name == 'models.VMapData.free' && it.blocks.len > 0)
	assert m.funcs.any(it.name == 'map__free' && it.blocks.len > 0)
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

fn test_wasm32_wide_literals_compared_with_narrow_integers() {
	path := os.join_path(os.vtmp_dir(), 'ssa_narrow_comparison_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn equal(value int) bool { return value == 4294967296 }\nfn lower(value int) bool { return value < 4294967296 }\nfn unsigned_byte(value u8) bool { return value == 256 }\nfn unsigned_max(value u64) bool { return value == 18446744073709551615 }\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	m := build_with_options(a, map[string]bool{}, &tc, BuildOptions{
		target: TargetData{ ptr_size: 4 }
	})
	expected_unsigned := {
		'equal':         false
		'lower':         false
		'unsigned_byte': true
		'unsigned_max':  true
	}
	mut found := 0
	for f in m.funcs {
		if unsigned := expected_unsigned[f.name] {
			for block in f.blocks {
				for value in m.blocks[block].instrs {
					instruction := m.instrs[m.values[value].index]
					if instruction.op !in [.eq, .lt] {
						continue
					}
					for operand in instruction.operands {
						typ := m.type_store.types[m.values[operand].typ]
						assert typ.kind == .int_t, f.name
						assert typ.width == 64, f.name
						assert typ.is_unsigned == unsigned, f.name
					}
					found++
				}
			}
		}
	}
	assert found == expected_unsigned.len
}

fn test_wasm32_indirect_call_numeric_parameter_types() {
	path := os.join_path(os.vtmp_dir(), 'ssa_indirect_numeric_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn scale(value f64) f64 { return value * 2.0 }\nfn scale32(value f32) f32 { return value * f32(2) }\nfn integer_argument() f64 { callback := scale\nreturn callback(3) }\nfn f32_argument() f64 { callback := scale\nreturn callback(f32(3.5)) }\nfn integer_to_f32_argument() f32 { callback := scale32\nreturn callback(3) }\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	// The CLI's non-generic path lowers after the local checker scopes are gone.
	tc.trust_checked_expr_types = false
	for node in a.nodes {
		if node.kind != .call || node.children_count == 0 {
			continue
		}
		callee_id := a.child(&node, 0)
		if a.nodes[int(callee_id)].value == 'callback' {
			// Transformed local callees recover their signature from the binding.
			a.nodes[int(callee_id)].typ = ''
			tc.expr_type_set[int(callee_id)] = false
		}
	}
	m := build_with_options(a, map[string]bool{}, &tc, BuildOptions{
		target: TargetData{ ptr_size: 4 }
	})
	expected := {
		'integer_argument':        64
		'f32_argument':            64
		'integer_to_f32_argument': 32
	}
	mut found := 0
	for f in m.funcs {
		if width := expected[f.name] {
			for block in f.blocks {
				for value in m.blocks[block].instrs {
					instruction := m.instrs[m.values[value].index]
					if instruction.op != .call_indirect {
						continue
					}
					argument := m.values[instruction.operands[1]]
					argument_type := m.type_store.types[argument.typ]
					assert argument_type.kind == .float_t, f.name
					assert argument_type.width == width, f.name
					found++
				}
			}
		}
	}
	assert found == expected.len
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

fn test_used_function_names_preserve_method_receivers() ! {
	mut b := Builder{
		used_fns: {
			'C.close':         true
			'close':           true
			'os.File.close':   true
			'copy_file':       true
			'Box__copy_T_int': true
		}
	}
	b.prepare_used_fn_lookups()
	assert b.source_fn_is_used('File.close', 'os')
	assert !b.source_fn_is_used('CommandArgs.close', 'os')
	assert !b.source_fn_is_used('Other.close', 'another')
	assert b.source_fn_is_used('copy_file', 'os')
	assert b.source_fn_is_used('Box.copy_T_int', 'models')
	path := os.join_path(os.vtmp_dir(), 'ssa_method_used_names_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'module os\nfn C.close(int) int\nstruct File {}\nstruct CommandArgs {}\nfn (mut f File) close() { C.close(-1) }\nfn (mut c CommandArgs) close() { unmarked_process_wait() }\nfn copy_file() {}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build_with_used(p.a, b.used_fns, unsafe { nil })
	assert m.funcs.any(it.name == 'os.File.close' && it.blocks.len > 0)
	assert m.funcs.any(it.name == 'os.copy_file' && it.blocks.len > 0)
	assert !m.funcs.any(it.name == 'os.CommandArgs.close')
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

fn test_wasm32_script_constants_reset_module_scope_after_imported_file() {
	root := os.join_path(os.vtmp_dir(), 'ssa_script_consts_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	library := os.join_path(root, 'dependency.v')
	script := os.join_path(root, 'main.v')
	os.write_file(library, 'module dependency\nconst base = 99\n__global imported_result = base\n')!
	os.write_file(script, 'const base = 10\nconst answer = base * 4 + 2\n__global result = answer - 1\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(library)
	p.parse_file(script)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(p.a)
	tc.enable_globals = true
	tc.collect(p.a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := build_with_options(p.a, map[string]bool{}, &tc, BuildOptions{
		target:         TargetData{ ptr_size: 4 }
		source_modules: {
			library: 'dependency'
			script:  'main'
		}
	})
	imported := m.globals.filter(it.name == 'dependency.imported_result')
	assert imported.len == 1
	assert imported[0].initial_value == 99
	for global in m.globals {
		if global.name == 'result' {
			assert global.initial_value == 41
			return
		}
	}
	assert false, 'missing script result global'
}

fn native_thread_test_module(name string, source string, detached bool) &Module {
	path := os.join_path(os.vtmp_dir(), 'v3_ssa_thread_${name}_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	if detached {
		for i, node in a.nodes {
			if node.kind == .spawn_expr {
				a.nodes[i].flags |= flat.node_flag_detached_spawn
			}
		}
	}
	return build_with_used(a, map[string]bool{}, tc)
}

fn native_thread_function_calls(m &Module, f Function) []string {
	mut calls := []string{}
	for block_id in f.blocks {
		for value_id in m.blocks[block_id].instrs {
			instr := m.instrs[m.values[value_id].index]
			if instr.op == .call {
				calls << m.values[instr.operands[0]].name
			} else if instr.op == .call_indirect {
				calls << '<indirect>'
			}
		}
	}
	return calls
}

fn test_native_spawn_noreturn_call_terminates_the_worker_only() {
	m := native_thread_test_module('noreturn_spawn', 'module main
fn C.exit(int)
@[noreturn]
fn stop() { C.exit(1) }
fn main() { job := spawn stop(); job.wait() }
', false)
	main_function := m.funcs.filter(it.name == 'main')[0]
	assert 'pthread_create' in native_thread_function_calls(m, main_function)
	for block_id in main_function.blocks {
		block := m.blocks[block_id]
		if block.name.starts_with('thread_failed_') {
			continue
		}
		for value_id in block.instrs {
			assert m.instrs[m.values[value_id].index].op != .unreachable
		}
	}
	worker := m.funcs.filter(it.name.starts_with('__ssa_spawn_'))[0]
	instrs := m.blocks[worker.blocks[0]].instrs
	mut checked_call := false
	for index, value_id in instrs {
		instruction := m.instrs[m.values[value_id].index]
		if instruction.op == .call_indirect {
			assert index + 1 == instrs.len - 1
			assert m.instrs[m.values[instrs[index + 1]].index].op == .unreachable
			checked_call = true
		}
	}
	assert checked_call
}

fn test_native_c_opaque_pointer_local_supplies_its_value() {
	m := native_thread_test_module('opaque_pointer', "module main
fn C.strtol(&char, &&char, int) i64
fn main() {
    mut ends := [&char(unsafe { nil })]
    target := ends.data
    _ = C.strtol(c'123x', target, 10)
}
", false)
	main_function := m.funcs.filter(it.name == 'main')[0]
	mut checked_call := false
	for block_id in main_function.blocks {
		for value_id in m.blocks[block_id].instrs {
			instruction := m.instrs[m.values[value_id].index]
			if instruction.op != .call || m.values[instruction.operands[0]].name != 'strtol' {
				continue
			}
			argument := m.values[instruction.operands[2]]
			assert argument.kind == .instruction
			cast := m.instrs[argument.index]
			assert cast.op == .bitcast
			loaded := m.values[cast.operands[0]]
			assert loaded.kind == .instruction
			assert m.instrs[loaded.index].op == .load
			checked_call = true
		}
	}
	assert checked_call
}

fn test_native_spawn_runs_aggregate_call_in_worker_and_joins_result() {
	m := native_thread_test_module('aggregate', '
struct Payload { first i64 second i64 third i64 }
fn work(value i64) Payload { return Payload{first: value, second: 8, third: 9} }
fn main() { job := spawn work(7); payload := job.wait(); _ = payload }
', false)
	mut found_main := false
	mut found_worker := false
	for f in m.funcs {
		if f.name == 'main' {
			calls := native_thread_function_calls(m, f)
			assert 'pthread_create' in calls
			assert 'pthread_join' in calls
			assert 'free' in calls
			assert 'work' !in calls
			mut loaded_payload := false
			for block_id in f.blocks {
				for value_id in m.blocks[block_id].instrs {
					instr := m.instrs[m.values[value_id].index]
					if instr.op == .load && m.type_size(instr.typ) == 24 {
						loaded_payload = true
					}
				}
			}
			assert loaded_payload
			found_main = true
		} else if f.name.starts_with('__ssa_spawn_') {
			calls := native_thread_function_calls(m, f)
			assert calls == ['<indirect>', 'free', 'malloc']
			assert f.params.len == 1
			found_worker = true
		}
	}
	assert found_main && found_worker
}

fn test_native_detached_spawn_detaches_and_does_not_box_result() {
	m := native_thread_test_module('detached', '
fn work(value i64) i64 { return value }
fn main() { spawn work(7) }
', true)
	mut found_main := false
	mut found_worker := false
	for f in m.funcs {
		if f.name == 'main' {
			calls := native_thread_function_calls(m, f)
			assert 'pthread_create' in calls
			assert 'pthread_detach' in calls
			assert 'work' !in calls
			found_main = true
		} else if f.name.starts_with('__ssa_spawn_') {
			assert native_thread_function_calls(m, f) == ['<indirect>', 'free']
			found_worker = true
		}
	}
	assert found_main && found_worker
}

fn test_native_spawn_accepts_function_values() {
	m := native_thread_test_module('function_value', '
fn launch(callback fn (i64) i64) i64 {
    job := spawn callback(7)
    return job.wait()
}
', false)
	mut found := false
	for f in m.funcs {
		if f.name == 'launch' {
			calls := native_thread_function_calls(m, f)
			assert 'pthread_create' in calls
			assert 'pthread_join' in calls
			assert '<indirect>' !in calls
			found = true
		}
	}
	assert found
}

fn test_native_c_array_macro_uses_helper_signature_for_pointer_receiver() {
	path := os.join_path(os.vtmp_dir(), 'ssa_c_array_receiver_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'module main
type Builder = []u8
fn (mut b Builder) append_bytes(data &u8, count i64) {
    C.array_push_many_ptr(&b, data, count)
}
fn main() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build(a)
	helpers := m.funcs.filter(it.name == 'array_push_many_ptr')
	assert helpers.len == 1
	receiver_type := m.values[helpers[0].params[0]].typ
	mut found := false
	for f in m.funcs {
		if !f.name.ends_with('Builder.append_bytes') {
			continue
		}
		for block_id in f.blocks {
			for value_id in m.blocks[block_id].instrs {
				instr := m.instrs[m.values[value_id].index]
				if instr.op != .call || m.values[instr.operands[0]].name != 'array_push_many_ptr' {
					continue
				}
				receiver := m.values[instr.operands[1]]
				assert receiver.typ == receiver_type
				assert receiver.kind == .instruction
				assert m.instrs[receiver.index].op == .load
				found = true
			}
		}
	}
	assert found
}

fn test_native_scalar_cast_does_not_dereference_mismatched_checked_c_return() {
	path := os.join_path(os.vtmp_dir(), 'ssa_scalar_cast_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'module main
fn C.scalar() i32
fn cast_scalar() int { return int(C.scalar()) }
fn main() {}
')!
	mut preferences := pref.new_preferences()
	preferences.backend = 'arm64'
	mut p := parser.Parser.new(preferences)
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, tc)
	mut checked_call := flat.NodeId(-1)
	for i, node in a.nodes {
		if node.kind != .call || node.children_count == 0 {
			continue
		}
		callee := a.node(a.child(&node, 0))
		if callee.kind != .selector || callee.value != 'scalar' {
			continue
		}
		// Duplicate C declarations can leave the checked return type wider than
		// the ABI signature, as with sysconf in builtin and os.
		for tc.expr_type_values.len <= i {
			tc.expr_type_values << types.Primitive{ props: .integer, size: 64 }
			tc.expr_type_set << false
		}
		tc.expr_type_values[i] = types.Primitive{ props: .integer, size: 64 }
		tc.expr_type_set[i] = true
		checked_call = flat.NodeId(i)
	}
	assert int(checked_call) >= 0
	checked_type := tc.expr_type(checked_call) or { panic('missing checked C return type') }
	assert checked_type.name() == 'i64'
	mut found_cast := false
	for node in a.nodes {
		if node.kind == .cast_expr && node.children_count > 0
			&& a.child(&node, 0) == checked_call {
			found_cast = true
		}
	}
	assert found_cast
	m := build_with_used(a, map[string]bool{}, tc)
	mut found := false
	for f in m.funcs {
		if f.name != 'cast_scalar' {
			continue
		}
		for block_id in f.blocks {
			for value_id in m.blocks[block_id].instrs {
				instr := m.instrs[m.values[value_id].index]
				if instr.op == .call {
					assert m.values[instr.operands[0]].name in ['C.scalar', 'scalar']
					assert m.type_size(instr.typ) == 4
					found = true
				}
				if instr.op == .load {
					assert m.type_store.types[m.values[instr.operands[0]].typ].kind == .ptr_t
				}
			}
		}
	}
	assert found
}

fn test_float_unary_minus_preserves_signed_zero_for_both_targets() ! {
	path := os.join_path(os.vtmp_dir(), 'ssa_float_negation_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'module main
fn negative_zero64() f64 { return -0.0 }
fn negative_zero32() f32 { return -f32(0.0) }
fn negate64(value f64) f64 { return -value }
fn negate32(value f32) f32 { return -value }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	for pointer_size in [4, 8] {
		m := build_with_options(a, map[string]bool{}, &tc, BuildOptions{
			target: TargetData{ ptr_size: pointer_size }
		})
		mut found := 0
		for f in m.funcs {
			if f.name !in ['negative_zero64', 'negative_zero32', 'negate64', 'negate32'] {
				continue
			}
			mut negation_found := false
			for block_id in f.blocks {
				for value in m.blocks[block_id].instrs {
					instr := m.instrs[m.values[value].index]
					if instr.op != .fsub { continue }
					zero := m.values[instr.operands[0]]
					assert zero.name == '-0.0', f.name
					assert m.type_store.types[zero.typ].kind == .float_t, f.name
					assert m.type_store.types[zero.typ].width == if f.name.ends_with('32') {
						32
					} else {
						64
					}, f.name
					assert zero.typ == m.values[instr.operands[1]].typ, f.name
					negation_found = true
				}
			}
			assert negation_found, f.name
			found++
		}
		assert found == 4
	}
}

fn test_wasm32_maps_keep_source_and_runtime_helpers_separate() ! {
	path := os.join_path(os.vtmp_dir(), 'ssa_wasm_map_names_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'module main
fn map__get(value int) int { return value + 13 }
fn lookup() int { values := {1: 7}
return values[1] }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := build_with_options(a, map[string]bool{}, &tc, BuildOptions{
		target: TargetData{ ptr_size: 4 }
	})
	assert !m.funcs.any(it.name in ['v3_native_map_hash_key', 'v3_native_map_index'])
	source := m.funcs.filter(it.name == 'map__get')[0]
	assert source.params.len == 1
	lookup := m.funcs.filter(it.name == 'lookup')[0]
	mut runtime_call_found := false
	for block_id in lookup.blocks {
		for value in m.blocks[block_id].instrs {
			instr := m.instrs[m.values[value].index]
			if instr.op != .call { continue }
			callee := m.values[instr.operands[0]]
			if !callee.name.contains('map__get') { continue }
			assert callee.name != 'map__get'
			assert m.funcs[callee.index].params.len == 3
			runtime_call_found = true
		}
	}
	assert runtime_call_found
}
