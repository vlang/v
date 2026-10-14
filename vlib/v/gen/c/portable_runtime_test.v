module c

import os
import v.cmdexec
import v.flat
import v.types

fn test_partial_c_closure_view_keeps_the_v_runtime_layout() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.structs['closure.Closure'] = [types.StructField{
		name: 'closure_ptr'
		typ:  types.Pointer{ base_type: types.Type(types.void_) }
	}, types.StructField{
		name: 'closure_cap'
		typ:  types.Type(types.int_)
	}]
	tc.structs['C.builtin__closure__Closure'] = [types.StructField{
		name: 'closure_cap'
		typ:  types.Type(types.int_)
	}]
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.struct_decls()
	code := g.sb.str()
	assert code.count('struct closure__Closure {') == 1
	assert code.contains('void* closure_ptr;'), code
	assert code.contains('closure_cap;'), code
}

fn test_cross_snapshot_runs_after_relocation_without_native_helper_objects() ! {
	$if windows || freestanding || vinix {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'portable_cross_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	payload := os.join_path(root, 'payload.txt')
	c_source := os.join_path(root, 'snapshot.c')
	os.write_file(payload, 'portable payload')!
	os.write_file(source, r'import runtime
import sync
const payload = $embed_file("payload.txt")
fn read_payload() string { return payload.to_string() }
fn main() {
 $if macos { assert runtime.free_memory() or { panic(err) } > 0 }
 mut wg := sync.new_waitgroup()
 wg.go(fn () { assert read_payload() == "portable payload" })
 wg.wait()
 mut workers := []thread string{}
 for _ in 0 .. 4 { workers << spawn read_payload() }
 for value in workers.wait() { assert value == "portable payload" }
 println("portable")
}

')!
	// Cached cross builds also link native helper objects; the embedded installer
	// must not export a second copy of the native signal-handler symbol.
	for _ in 0 .. 2 {
		cached := cmdexec.run('env', ['V3CACHE=${os.join_path(root, 'cache')}', @VEXE, '-cc', cc,
			'-gc', 'none', '-prealloc', '-os', 'cross', 'run', source])
		assert cached.exit_code == 0, cached.output
		assert cached.output.trim_space() == 'portable', cached.output
	}
	generated := cmdexec.run(@VEXE, ['-gc', 'none', '-prealloc', '-nocache', '-os', 'cross', '-o',
		c_source, source])
	assert generated.exit_code == 0, generated.output
	moved := os.join_path(root, 'relocated')
	os.mkdir_all(moved)!
	relocated := os.join_path(moved, 'snapshot.c')
	os.mv(c_source, relocated)!
	os.rm(source)!
	os.rm(payload)!
	executable := os.join_path(moved, 'program')
	built := cmdexec.run(cc, ['-std=gnu11', '-w', '-o', executable, relocated, '-lm', '-lpthread'])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(executable, [])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'portable', ran.output
}

fn test_portable_native_helpers_precede_gc_runtime_redirects() {
	mut g := FlatGen.new()
	g.set_output_cross_c(true)
	g.compile_defines << 'gcboehm'
	g.portable_native_runtime_helpers()
	g.thread_allocation_helpers()
	code := g.sb.str()
	definition := code.index('static int v_gc_pthread_create(') or { -1 }
	redirect := code.index('#define pthread_create v_gc_pthread_create') or { -1 }
	assert definition >= 0 && redirect > definition
	assert code.count('#include <gc.h>') == 1
	assert code.contains('static int v_runtime_free_memory_darwin(')
	assert code.contains('V_SYNC_THREAD_SCOPE int v_sync_thread_create_detached_with_stack(')
	assert g.inlined_c_declared_fns['v3_modulecache_file_metadata']
	assert g.inlined_c_declared_fns['v_os_exec_capture_start']
}

fn test_portable_peak_rss_helper_returns_the_type_of_its_generated_prototype() ! {
	$if windows || freestanding || vinix {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or {
		os.find_abs_path_of_executable('cc') or { return }
	}
	mut g := FlatGen.new()
	g.set_output_cross_c(true)
	g.portable_native_runtime_helpers()
	code := g.sb.str()
	result_type := code.index('#define V_PEAK_RSS_INT i64') or { -1 }
	definition := code.index('V_PEAK_RSS_SCOPE V_PEAK_RSS_INT v3_bench_peak_rss_kb(void)') or { -1 }
	assert result_type >= 0 && definition > result_type
	// `i64` is `int64_t` in portable C: `long` on Linux and Android, `long long` on
	// macOS. A C compiler takes the two for different types whatever their sizes, so
	// the helper is checked against a prototype with each spelling.
	header := $embed_file('@VEXEROOT/vlib/v/bench/peak_rss.h').to_string()
	root := os.join_path(os.vtmp_dir(), 'portable_peak_rss_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for index, spelling in ['long', 'long long'] {
		source := os.join_path(root, 'peak_rss_${index}.c')
		os.write_file(source, 'typedef ${spelling} i64;\n#define V_PEAK_RSS_INT i64\n${header}i64 v3_bench_peak_rss_kb(void);\nint main(void) { return v3_bench_peak_rss_kb() < 0; }\n')!
		checked := cmdexec.run(cc, ['-std=gnu11', '-fsyntax-only', source])
		assert checked.exit_code == 0, '${spelling}: ${checked.output}'
	}
}

fn test_boehm_object_runs_spawn_and_join_after_relocation() ! {
	$if !macos && !linux {
		return
	}
	collector := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib', 'libgc.a')
	if !os.exists(collector) {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'portable_boehm_object_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'library.v')
	object := os.join_path(root, 'library.o')
	os.write_file(source, r"module portable
fn worker() int {
 data := []u8{len: 4096, init: u8(index % 251)}
 gc_collect()
 for index, value in data { if value != u8(index % 251) { panic('lost allocation') } }
 return 42
}
@[export: 'portable_spawn']
pub fn start() int {
 child := spawn worker()
 return child.wait()
}
")!
	generated := cmdexec.run(@VEXE, ['-gc', 'boehm', '-d', 'use_bundled_libgc', '-nocache', '-cc',
		cc, '-is_o', '-o', object, source])
	assert generated.exit_code == 0, generated.output
	moved := os.join_path(root, 'relocated')
	os.mkdir_all(moved)!
	relocated := os.join_path(moved, 'library.o')
	os.mv(object, relocated)!
	os.rm(source)!
	main_c := os.join_path(moved, 'main.c')
	os.write_file(main_c, '#include <unistd.h>\nextern int portable_spawn(void);\nint main(void) { alarm(30); return portable_spawn() == 42 ? 0 : 1; }\n')!
	executable := os.join_path(moved, 'program')
	built := cmdexec.run(cc, ['-o', executable, relocated, main_c, collector, '-lm', '-lpthread',
		'-ldl'])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(executable, [])
	assert ran.exit_code == 0, ran.output
}

fn test_object_output_links_from_a_relocated_directory_without_native_helpers() ! {
	$if windows || freestanding || vinix {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'portable_object_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'library.v')
	object := os.join_path(root, 'library.o')
	os.write_file(source, "module portable\n@[export: 'portable_add']\npub fn add(left i32, right i32) i32 { return left + right }\n")!
	generated := cmdexec.run(@VEXE, ['-gc', 'none', '-nocache', '-cc', cc, '-is_o', '-o', object,
		source])
	assert generated.exit_code == 0, generated.output
	moved := os.join_path(root, 'relocated')
	os.mkdir_all(moved)!
	relocated := os.join_path(moved, 'library.o')
	os.mv(object, relocated)!
	os.rm(source)!
	main_c := os.join_path(moved, 'main.c')
	os.write_file(main_c, 'extern int portable_add(int, int);\nint main(void) { return portable_add(10, 20) == 30 ? 0 : 1; }\n')!
	executable := os.join_path(moved, 'program')
	built := cmdexec.run(cc, ['-o', executable, relocated, main_c, '-lm', '-lpthread'])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(executable, [])
	assert ran.exit_code == 0, ran.output
}

fn test_directory_test_runner_declares_hidden_string_comparisons() ! {
	$if windows || freestanding || vinix {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'portable_directory_test_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'sample_test.v'), 'fn test_sample() { assert 6 * 7 == 42 }\n')!
	ran := cmdexec.run(@VEXE, ['-cc', cc, '-nocache', '-stats', 'test', root])
	assert ran.exit_code == 0, ran.output
	assert ran.output.contains('1 passed'), ran.output
}

fn test_portable_gc_helper_preserves_linux_header_linkage() ! {
	$if windows || freestanding || vinix {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	mut g := FlatGen.new()
	g.set_output_cross_c(true)
	g.compile_defines << 'gcboehm'
	g.portable_native_runtime_helpers()
	code := g.sb.str()
	linux_header := $embed_file('@VEXEROOT/vlib/builtin/gc_debugger_linux.h').to_string()
	assert code.contains(linux_header)
	assert !code.contains('static inline static')
	root := os.join_path(os.vtmp_dir(), 'portable_linux_gc_guard_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'helpers.c')
	output := os.join_path(root, 'preprocessed.c')
	// Load the host collector SDK before enabling the guarded Linux helper text.
	os.write_file(source, '#include <gc.h>\n#ifndef __linux__\n#define __linux__ 1\n#endif\n' + code)!
	// Enable the Linux helper branch with the host SDK; this checks preprocessing,
	// not execution of Linux-specific root registration on another platform.
	mut args := ['-E', '-P', '-I', os.join_path(@VEXEROOT, 'thirdparty', 'libgc', 'include')]
	$if !linux {
		// The portable text also contains the Linux syscall facade. Supply its
		// unavailable SDK include for preprocessing; this fixture does not compile
		// or execute that facade, and Linux hosts keep using their real SDK.
		sdk := os.join_path(root, 'linux_sdk')
		os.mkdir_all(os.join_path(sdk, 'sys'))!
		os.write_file(os.join_path(sdk, 'sys', 'prctl.h'), '#pragma once\nint prctl(int, ...);\n')!
		args << ['-I', sdk]
	}
	args << ['-o', output, source]
	preprocessed := cmdexec.run(cc, args)
	assert preprocessed.exit_code == 0, preprocessed.output
	text := os.read_file(output)!
	assert text.contains('v_gc_can_register_main_data_roots_impl'), text
	assert !text.contains('static inline static')
}
