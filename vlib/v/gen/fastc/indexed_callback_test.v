module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_indexed_callbacks_preserve_signatures_payloads_and_index_evaluation() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_indexed_callback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
const capacity = 2
__global (
	callbacks [capacity]fn (u64) u64
	optional_callbacks [1]fn (u64) ?u64
	result_callbacks [1]fn (u64) !u64
	index_calls int
)
fn add_five(value u64) u64 { return value + 5 }
fn double(value u64) u64 { return value * 2 }
fn maybe(value u64) ?u64 {
	if value == 0 { return none }
	return value + 5
}
fn checked(value u64) !u64 {
	if value == 0 { return error("empty callback value") }
	return value + 5
}
fn next_index() int { index_calls++; return 0 }
fn main() {
	callbacks[0] = add_five
	callbacks[1] = double
	optional_callbacks[0] = maybe
	result_callbacks[0] = checked
	index_calls = 0
	wide := u64(1) << 60
	assert callbacks[next_index()](wide) == wide + 5
	assert index_calls == 1
	copied_table := callbacks
	assert copied_table[next_index()](wide) == wide + 5
	assert index_calls == 2
	copied := callbacks[next_index()]
	assert copied(wide) == wide + 5
	assert index_calls == 3
	assert callbacks[1](wide) == wide * 2
	values := [add_five, double]
	assert values[next_index()](wide) == wide + 5
	assert index_calls == 4
	local_table_copy := values
	assert local_table_copy[next_index()](wide) == wide + 5
	assert index_calls == 5
	local_callback := values[next_index()]
	assert local_callback(wide) == wide + 5
	assert index_calls == 6
	accepted := optional_callbacks[next_index()](wide) or { u64(0) }
	assert accepted == wide + 5
	assert index_calls == 7
	missing := optional_callbacks[0](0) or { u64(17) }
	assert missing == 17
	copied_option := optional_callbacks[0]
	assert copied_option(wide) or { u64(0) } == wide + 5
	succeeded := result_callbacks[next_index()](wide) or { u64(0) }
	assert succeeded == wide + 5
	assert index_calls == 8
	failed := result_callbacks[0](0) or {
		assert err.msg() == "empty callback value"
		u64(19)
	}
	assert failed == 19
	copied_result := result_callbacks[0]
	assert copied_result(wide) or { u64(0) } == wide + 5
	println("indexed callbacks work")
}
') or { panic(err) }
	ordinary := os.join_path(root, 'ordinary')
	ordinary_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-nocache', '-gc', 'none',
		'-enable-globals', '-o', ordinary, root], @VEXEROOT, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert ordinary_build.exit_code == 0, ordinary_build.output
	ordinary_run := cmdexec.run(ordinary, [])
	assert ordinary_run.exit_code == 0, ordinary_run.output
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.target = pref.target_from('vinix', pref.host_arch()) or { panic(err) }
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	generated := generate_files([root], prefs) or { panic(err) }
	assert generated.contains('u64 (*)(u64)')
	assert generated.contains('Option (*)(u64)')
	assert generated.contains('__v_result (*)(u64)')
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'fastc')
	os.write_file(c_path, generated) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-fno-strict-aliasing', '-D__vinix__', '-o',
		program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output == ordinary_run.output, run.output
	assert run.output == 'indexed callbacks work\n', run.output
}
