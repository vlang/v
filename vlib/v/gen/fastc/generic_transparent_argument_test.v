module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_generic_arguments_inside_unsafe_blocks_keep_indexed_global_types() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_transparent_generic_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
@[export: "native_guard_bitmap"]
__global guards [8]u64
fn load[T](value &T) T { return unsafe { *value } }
fn read_guard(slot u64) bool {
	return load(unsafe { &guards[slot / 64] }) & (u64(1) << (slot % 64)) != 0
}
fn read_grouped_guard(slot u64) bool {
	return load((unsafe { &guards[slot / 64] })) & (u64(1) << (slot % 64)) != 0
}
fn main() {
	guards[1] = u64(1) << 63
	assert load(&guards[1]) == u64(1) << 63
	assert read_guard(127)
	assert !read_guard(126)
	assert read_grouped_guard(127)
	assert !read_grouped_guard(64)
	value := u64(1) << 60
	assert load(unsafe { &value }) == value
	assert load((unsafe { &value })) == value
	println("transparent generic arguments work")
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
	assert generated.contains('load_mono_u64(')
	assert !generated.contains('load_mono_voidptr(')
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'fastc')
	os.write_file(c_path, generated) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-fno-strict-aliasing', '-D__vinix__', '-o',
		program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output == ordinary_run.output, run.output
	assert run.output == 'transparent generic arguments work\n', run.output
}
