module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_cross_target_prod_uses_target_allocator_defines() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_target_prod_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
$if prealloc ? { $compile_error("target libc must retain its own allocator") }
$if parallel ? { $compile_error("target libc must retain its own runtime defines") }
$if prod {
	fn production_target() int { return 42 }
} $else {
	$compile_error("cross-target C output must preserve production branches")
}
fn main() { production_target() }
') or { panic(err) }
	c_file := os.join_path(root, 'kernel.c')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-prod', '-nocache', '-os',
		'vinix', '-arch', pref.host_arch(), '-target-libc-headers', '-nofloat', '-gc', 'none',
		'-no-closures', '-d', 'no_backtrace', '-exclude', '@vlib/builtin/backtraces_nix.c.v', '-o',
		c_file, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	c_source := os.read_file(c_file) or { panic(err) }
	assert c_source.contains('main__production_target('), c_source
	assert !c_source.contains('static _Thread_local VMemoryBlock* g_memory_block;'), c_source
}

fn test_fastc_target_output_preserves_kernel_entry_and_native_callback_names() {
	mut prefs := pref.new_preferences()
	prefs.target_libc_headers = true
	prefs.target = pref.target_from('vinix', 'amd64') or { panic(err) }
	prefs.is_prod = true
	c_source := generate(r'module main
fn C.native_hook(value int) int
@[export: "native_callback"]
fn callback(value int) int { return value + 1 }
fn kmain() {
	C.native_hook(callback(41))
}
fn main() { kmain() }
', 'target_callbacks.v', prefs) or { panic(err) }
	assert c_source.contains('void main__kmain(void)'), c_source
	assert c_source.contains('native_callback('), c_source
	assert c_source.contains('extern int native_hook(int);'), c_source
	assert !c_source.contains('setvbuf(stdout'), c_source
	assert !c_source.contains('atexit(v_fastc'), c_source
}

fn test_fastc_staged_target_sources_keep_local_modules_and_filtered_siblings() {
	root := os.join_path(os.vtmp_dir(), 'fastc_staged_target_${os.getpid()}')
	original := os.join_path(root, 'original')
	stage := os.join_path(root, 'stage')
	vroot := os.join_path(root, 'vroot')
	for dir in [original, os.join_path(original, 'time'), os.join_path(original, 'drm'), stage,
		os.join_path(stage, 'time'), os.join_path(vroot, 'vlib', 'builtin'),
		os.join_path(vroot, 'vlib', 'time')] {
		os.mkdir_all(dir) or { panic(err) }
	}
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(stage, 'v.mod'), 'Module { name: "staged" }') or { panic(err) }
	main_source := os.join_path(original, 'main.v')
	time_source := os.join_path(original, 'time', 'timer.v')
	os.write_file(main_source, 'module main\nimport time\nimport drm\nfn main() { time.kernel_timer(); drm.inspect() }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(original, 'excluded.v'), 'module main\nfn excluded() {}\n') or {
		panic(err)
	}
	os.write_file(time_source, 'module time\npub fn kernel_timer() {}\n') or { panic(err) }
	os.write_file(os.join_path(original, 'time', 'excluded.v'), 'module time\npub fn excluded_timer() {}\n') or {
		panic(err)
	}
	drm_source := os.join_path(original, 'drm', 'drm.v')
	os.write_file(drm_source, 'module drm\nimport time\npub fn inspect() { time.kernel_timer() }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(original, 'time', 'timer.h'), 'void kernel_timer(void);\n') or {
		panic(err)
	}
	os.write_file(os.join_path(vroot, 'vlib', 'builtin', 'builtin.v'), 'module builtin\n') or {
		panic(err)
	}
	os.write_file(os.join_path(vroot, 'vlib', 'time', 'time.v'), 'module time\npub fn host_timer() {}\n') or {
		panic(err)
	}
	staged_main := os.join_path(stage, 'main.v')
	staged_time := os.join_path(stage, 'time', 'timer.v')
	os.symlink(main_source, staged_main) or { return }
	os.symlink(time_source, staged_time) or { return }
	mut prefs := pref.new_preferences()
	prefs.vroot = vroot
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.target = pref.target_from('vinix', 'amd64') or { panic(err) }
	sources, _ := fastc_resolve_source_files([stage], prefs) or { panic(err) }
	paths := sources.map(it.path)
	assert sources.len == 4, paths.str()
	assert staged_main in paths, paths.str()
	assert staged_time in paths, paths.str()
	assert os.real_path(drm_source) in paths, paths.str()
	assert !paths.any(it.ends_with('excluded.v')), paths.str()
	assert !paths.any(it.ends_with('vlib/time/time.v')), paths.str()
	include := fastc_resolve_c_pseudo_paths('include "timer.h"', vroot, staged_time)
	assert include == 'include "${os.real_path(os.join_path(original, 'time', 'timer.h'))}"', include
	assert fastc_resolve_c_pseudo_paths('include "@DIR/timer.h"', vroot, staged_time) == include
}

fn test_fastc_target_boxing_uses_only_core_headers_and_real_allocator() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_target_boxing_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'stddef.h'), 'typedef __SIZE_TYPE__ size_t;\n#define NULL ((void*)0)\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'stdint.h'), r'typedef __INT8_TYPE__ int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;
typedef __INTPTR_TYPE__ intptr_t;
typedef __UINTPTR_TYPE__ uintptr_t;
') or { panic(err) }
	os.write_file(os.join_path(root, 'stdio.h'), '') or { panic(err) }
	os.write_file(os.join_path(root, 'stdlib.h'), 'void *malloc(size_t);\nvoid free(void*);\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'string.h'), 'void *memcpy(void*, const void*, size_t);\n') or {
		panic(err)
	}
	source := fastc_target_libc_preamble() + r'
int main(void) {
	int input = 42;
	int *boxed = v_fastc_interface_box(&input, sizeof(input));
	input = 0;
	int result = boxed && *boxed == 42 ? 0 : 1;
	free(boxed);
	return result;
}
'
	source_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(source_file, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-nostdinc', '-I${root}', '-o', program, source_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}

fn test_fastc_windows_target_entry_switches_stdio_to_binary_mode() {
	source := 'module main\nfn main() {\n\tprintln(1)\n}\n'
	mut windows_prefs := pref.new_preferences()
	windows_prefs.target = pref.target_from('windows', 'amd64') or { panic(err) }
	windows_c := generate(source, 'binary_stdio.v', windows_prefs) or { panic(err) }
	// An ordinary Windows program must leave the CRT's text mode before it prints, so
	// `println` writes `\n` like the same program built with cgen, not `\r\n`.
	assert windows_c.contains('#ifdef _WIN32\n#include <io.h>\n#include <fcntl.h>\n#endif'), windows_c
	assert windows_c.contains('int wmain(void) {\n\t_setmode(1, _O_BINARY);\n\t_setmode(2, _O_BINARY);\n\tsetvbuf(stdout, NULL, _IONBF, 0);'), windows_c

	// The synthesized entry of a declaration-only program takes the same path.
	declaration_only_c := generate('module main\npub fn answer() int {\n\treturn 42\n}\n',
		'binary_stdio_decl.v', windows_prefs) or { panic(err) }
	assert declaration_only_c.contains('int wmain(void) {\n\t_setmode(1, _O_BINARY);\n\t_setmode(2, _O_BINARY);\n\tsetvbuf(stdout, NULL, _IONBF, 0);\n\treturn 0;'), declaration_only_c

	mut linux_prefs := pref.new_preferences()
	linux_prefs.target = pref.target_from('linux', 'amd64') or { panic(err) }
	linux_c := generate(source, 'binary_stdio.v', linux_prefs) or { panic(err) }
	assert !linux_c.contains('_setmode('), linux_c

	// Selfhost builds get binary streams from builtin's own startup instead.
	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.target = pref.target_from('windows', 'amd64') or { panic(err) }
	selfhost_prefs.building_v = true
	selfhost_c := generate(source, 'binary_stdio.v', selfhost_prefs) or { panic(err) }
	assert !selfhost_c.contains('_setmode(1, _O_BINARY);'), selfhost_c
}
