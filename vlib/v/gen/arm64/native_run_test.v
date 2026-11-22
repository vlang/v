module arm64

import os

$if macos && arm64 {
	import time
}

const native_test_compiler = os.join_path(os.vtmp_dir(), 'arm64_run_compiler_${os.getpid()}')

fn testsuite_end() {
	os.rm(native_test_compiler) or {}
}

// exec_native runs a compiler command. `cmd/v` leaves the ARM64 backend out by default, so
// when @VEXE reports that, the command runs with a compiler built here that includes it.
fn exec_native(args []string) os.Result {
	if !os.exists(native_test_compiler) {
		result := os.exec(args)
		if !result.output.contains('ARM64 support is not compiled into this executable') {
			return result
		}
		bootstrap := os.exec([@VEXE, '-gc', 'none', '-d', 'skip_fastc', '-compile-backend', 'arm64',
			'-o', native_test_compiler, os.join_path(@VEXEROOT, 'cmd', 'v')])
		assert bootstrap.exit_code == 0, bootstrap.output
	}
	return os.exec(args.map(if it == @VEXE { native_test_compiler } else { it }))
}

fn test_native_live_reload_program_compiles() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_live_reload_${os.getpid()}.v')
		output := os.join_path(os.vtmp_dir(), 'arm64_live_reload_${os.getpid()}')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.cp(os.join_path(@VEXEROOT, 'examples', 'hot_reload', 'message.v'), path) or { panic(err) }
		result := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
			'-nocache', '-b', 'arm64', '-os', 'macos', '-o', output, path])
		assert result.exit_code == 0, result.output
		assert os.exists(output), 'native live reload program must link'
	}
}

fn test_native_hot_fn_extracts_self_contained_function() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_hot_self_${os.getpid()}.v')
		output := os.join_path(os.vtmp_dir(), 'arm64_hot_self_${os.getpid()}.bin')
		linked := os.join_path(os.vtmp_dir(), 'arm64_hot_self_${os.getpid()}')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
			os.rm(linked) or {}
		}
		os.cp(os.join_path(@VEXEROOT, 'examples', 'hot_reload', 'native_counter.v'), path) or {
			panic(err)
		}
		link := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
			'-nocache', '-b', 'arm64', '-os', 'macos', '-o', linked, path])
		assert link.exit_code == 0, link.output
		extract := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
			'-nocache', '-b', 'arm64', '-os', 'macos', '-hot-fn', 'next_value', '-o', output, path])
		assert extract.exit_code == 0, extract.output
		assert os.file_size(output) > 0, 'extracted function must not be empty'
	}
}

fn test_native_hot_fn_extracts_linux_function() {
	root := os.join_path(os.vtmp_dir(), 'arm64_linux_hot_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'counter.v')
	output := os.join_path(root, 'next_value.bin')
	// Keep the hot function reachable in the source program; -hot-fn builds only its body.
	os.write_file(path, 'fn next_value() int { return 42 }\nfn main() { assert next_value() == 42 }\n')!
	extract := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
		'-nocache', '-b', 'arm64', '-os', 'linux', '-hot-fn', 'next_value', '-o', output, path])
	assert extract.exit_code == 0, extract.output
	code := os.read_bytes(output)!
	assert code.len > 0, 'extracted Linux function must not be empty'
	assert code.len % 4 == 0, 'extracted function must contain complete ARM64 instructions'
}

fn test_native_hot_fn_rejects_function_needing_fixups() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_hot_reject_${os.getpid()}.v')
		output := os.join_path(os.vtmp_dir(), 'arm64_hot_reject_${os.getpid()}.bin')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.cp(os.join_path(@VEXEROOT, 'examples', 'hot_reload', 'message.v'), path) or {
			panic(err)
		}
		// `print_message` calls other functions and uses string literals, so
		// extraction must fail instead of emitting broken machine code.
		extract := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
			'-nocache', '-b', 'arm64', '-os', 'macos', '-hot-fn', 'print_message', '-o', output,
			path])
		assert extract.exit_code != 0, extract.output
	}
}

fn test_native_linux_object_and_executable_output() {
	root := os.join_path(os.vtmp_dir(), 'arm64_linux_link_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'hello_world.v')
	object := os.join_path(root, 'hello_world.o')
	executable := os.join_path(root, 'hello_world')
	os.write_file(path, 'fn main() { println("hello world") }\n')!
	command := ['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none', '-nocache',
		'-b', 'native', '-arch', 'arm64', '-os', 'linux']
	mut object_args := command.clone()
	object_args << ['-o', object, path]
	built := exec_native(object_args)
	assert built.exit_code == 0, built.output
	bytes := os.read_bytes(object)!
	assert bytes[..4] == [u8(0x7f), `E`, `L`, `F`]
	assert bytes[16] == 1 // ET_REL
	assert bytes[18] == 183 // EM_AARCH64
	// Object emission must work even when a target linker is unavailable.
	mut can_link := false
	$if linux && arm64 {
		can_link = true
	}
	for cc in ['aarch64-linux-gnu-gcc', 'aarch64-linux-musl-gcc'] {
		if _ := os.find_abs_path_of_executable(cc) { can_link = true }
	}
	if !can_link { return }
	mut executable_args := command.clone()
	// Exercise the default output name as in `v -b native -arch arm64 hello_world.v`.
	executable_args << path
	linked := exec_native(executable_args)
	assert linked.exit_code == 0, linked.output
	elf := os.read_bytes(executable)!
	assert elf[..4] == [u8(0x7f), `E`, `L`, `F`]
	assert elf[16] == 2 // ET_EXEC
	assert elf[18] == 183 // EM_AARCH64
	$if linux && arm64 {
		run := os.exec([executable])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'hello world'
	}
}

fn linux_arm64_test_c_compiler() ?string {
	for candidate in ['aarch64-linux-gnu-gcc', 'aarch64-linux-musl-gcc'] {
		if path := os.find_abs_path_of_executable(candidate) {
			return path
		}
	}
	$if linux && arm64 {
		return 'cc'
	}
	return none
}

fn test_native_linux_links_source_library_directives() {
	cc := linux_arm64_test_c_compiler() or { return }
	root := os.join_path(os.vtmp_dir(), 'arm64_linux_flags_${os.getpid()} with spaces')
	dependency := os.join_path(root, 'native_dep')
	os.mkdir_all(dependency)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'arm64_source_flags_test' }\n")!
	c_source := os.join_path(root, 'dependency.c')
	c_object := os.join_path(root, 'dependency.o')
	library := os.join_path(dependency, 'libarm64_source_dependency.a')
	os.write_file(c_source, 'int arm64_source_dependency(void) { return 42; }\n')!
	compiled := os.exec([cc, '-c', c_source, '-o', c_object])
	assert compiled.exit_code == 0, compiled.output
	archiver := os.exec([cc, '-print-prog-name=ar'])
	assert archiver.exit_code == 0, archiver.output
	archived := os.exec([archiver.output.trim_space(), 'rcs', library, c_object])
	assert archived.exit_code == 0, archived.output
	os.write_file(os.join_path(dependency, 'dependency.c.v'), r'module native_dep
#flag linux -L "@DIR"
#flag linux -l arm64_source_dependency
#flag darwin -l arm64_wrong_target
fn C.arm64_source_dependency() int
pub fn value() int { return C.arm64_source_dependency() }
')!
	path := os.join_path(root, 'program.v')
	executable := os.join_path(root, 'program')
	os.write_file(path, 'import native_dep\nfn main() { exit(native_dep.value() - 42) }\n')!
	linked := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
		'-nocache', '-b', 'arm64', '-os', 'linux', '-cc', cc, '-o', executable, path])
	assert linked.exit_code == 0, linked.output
	elf := os.read_bytes(executable)!
	assert elf[..4] == [u8(0x7f), `E`, `L`, `F`]
	$if linux && arm64 {
		run := os.exec([executable])
		assert run.exit_code == 0, run.output
	}
}

fn test_native_linux_keeps_exports_called_from_c_objects() {
	cc := linux_arm64_test_c_compiler() or { return }
	root := os.join_path(os.vtmp_dir(), 'arm64_linux_export_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	c_source := os.join_path(root, 'callback.c')
	c_object := os.join_path(root, 'callback.o')
	os.write_file(c_source, 'extern int external_callback(void);\nint call_callback(void) { return external_callback(); }\n')!
	compiled := os.exec([cc, '-c', c_source, '-o', c_object])
	assert compiled.exit_code == 0, compiled.output
	path := os.join_path(root, 'program.v')
	executable := os.join_path(root, 'program')
	os.write_file(path, r'#flag linux "@DIR/callback.o"
fn C.call_callback() int
fn helper() int { return 42 }
@[export: "external_callback"]
fn callback() int { return helper() }
fn main() { exit(C.call_callback() - 42) }
')!
	linked := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
		'-nocache', '-b', 'arm64', '-os', 'linux', '-cc', cc, '-o', executable, path])
	assert linked.exit_code == 0, linked.output
	$if linux && arm64 {
		run := os.exec([executable])
		assert run.exit_code == 0, run.output
	}
}

fn test_native_linux_links_environment_cflags_before_explicit_flags() {
	cc := linux_arm64_test_c_compiler() or { return }
	root := os.join_path(os.vtmp_dir(), 'arm64_linux_cflags_${os.getpid()} with spaces')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	c_source := os.join_path(root, 'dependency.c')
	c_object := os.join_path(root, 'dependency.o')
	os.write_file(c_source, 'int environment_dependency(void) { return 42; }\n')!
	compiled := os.exec([cc, '-c', c_source, '-o', c_object])
	assert compiled.exit_code == 0, compiled.output
	os.write_file(os.join_path(root, 'order.c'), '#if V_ARM64_LINK_ORDER != 2\n#error explicit flags must follow environment flags\n#endif\n')!
	path := os.join_path(root, 'program.v')
	executable := os.join_path(root, 'program')
	os.write_file(path, r'#flag linux "@DIR/order.c"
fn C.environment_dependency() int
fn main() { exit(C.environment_dependency() - 42) }
')!
	linked := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1',
		'CFLAGS=${os.quoted_path(c_object)} -DV_ARM64_LINK_ORDER=1', @VEXE, '-gc', 'none', '-nocache',
		'-b', 'arm64', '-os', 'linux', '-cc', cc, '-cflags', '-DV_ARM64_LINK_ORDER=2', '-o',
		executable, path])
	assert linked.exit_code == 0, linked.output
	$if linux && arm64 {
		run := os.exec([executable])
		assert run.exit_code == 0, run.output
	}
}

fn test_native_linux_run_variadic_calls_arguments_and_exit_status() {
	$if linux && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_linux_run_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		path := os.join_path(root, 'program.v')
		os.write_file(path, r'fn C.printf(format &u8, ...) int
fn main() {
	args := arguments()
	if args.len != 2 || args[1] != "carried" { exit(23) }
	C.printf(c"%d %.1f %d %d %d %d %d %d %d\n", 42, 3.5, 2, 3, 4, 5, 6, 7, 8)
	exit(17)
}
')!
		result := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-b', 'native',
			'-arch', 'arm64', 'run', path, 'carried'])
		assert result.exit_code == 17, result.output
		assert result.output.contains('42 3.5 2 3 4 5 6 7 8'), result.output
		assert !os.exists(path.all_before_last('.'))
	}
}

fn test_native_run_forwards_arguments_and_exit_status() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_run_dispatch_${os.getpid()}.v')
		output := path.all_before_last('.')
		kept_output := output + '_kept'
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
			os.rm(kept_output) or {}
		}
		os.write_file(path, r'module main
fn main() {
    args := arguments()
    if args.len != 3 || args[1] != "carried" { exit(23) }
    println("native run: " + args[1])
    if args[2] == "fail" { exit(17) }
}
')!
		command := ['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none', '-nocache',
			'-b', 'arm64']
		for expected_exit in [0, 17] {
			mut args := command.clone()
			args << ['run', path, 'carried', if expected_exit == 0 { 'pass' } else { 'fail' }]
			result := exec_native(args)
			assert result.exit_code == expected_exit, result.output
			assert result.output.contains('native run: carried'), result.output
			assert !os.exists(output), 'run must remove its temporary binary'
		}
		mut args := command.clone()
		args << ['-o', kept_output, 'run', path, 'carried', 'pass']
		result := exec_native(args)
		assert result.exit_code == 0, result.output
		assert result.output.contains('native run: carried'), result.output
		assert os.exists(kept_output), 'an explicit output must survive run'
	}
}

fn test_native_test_compilation_runs_the_test_binary() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_test_dispatch_${os.getpid()}_test.v')
		defer {
			os.rm(path) or {}
			os.rm(path.all_before_last('.')) or {}
		}
		os.write_file(path, r'fn init() { println("module init") }
fn testsuite_begin() { println("suite begin") }
fn testsuite_end() { println("suite end") }
fn before_each() { println("before each") }
fn after_each() { println("after each") }
fn test_native_entrypoint() {
    println("native test executed")
    assert 7 * 6 == 42
}
fn test_second() { println("second test executed") }
')!
		command := ['env', 'VFLAGS=', 'VTEST_ONLY_FN=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc',
			'none', '-nocache', '-b', 'arm64']
		mut all_tests := command.clone()
		all_tests << path
		result := exec_native(all_tests)
		assert result.exit_code == 0, result.output
		assert result.output.contains('module init\nsuite begin\nbefore each\nnative test executed\nafter each\nbefore each\nsecond test executed\nafter each\nsuite end'), result.output
		for pattern in ['test_native*', 'main.test_native*'] {
			mut selected_tests := command.clone()
			selected_tests << ['-run-only', pattern, path]
			selected := exec_native(selected_tests)
			assert selected.exit_code == 0, selected.output
			assert selected.output.contains('native test executed'), selected.output
			assert !selected.output.contains('second test executed'), selected.output
			assert selected.output.contains('suite end'), selected.output
		}
		os.write_file(path, r'fn test_optional() ? { println("optional passed") }
fn test_result() ! { println("result passed") }
')!
		returned := exec_native(all_tests)
		assert returned.exit_code == 0, returned.output
		assert returned.output.contains('optional passed\nresult passed'), returned.output
		os.write_file(path, r'fn test_optional() ? { return none }
')!
		propagation_failure := exec_native(all_tests)
		assert propagation_failure.exit_code != 0, propagation_failure.output
		assert propagation_failure.output.contains('failed propagation'), propagation_failure.output
		os.write_file(path, r'fn test_result() ! { return error("expected failure") }
')!
		result_failure := exec_native(all_tests)
		assert result_failure.exit_code != 0, result_failure.output
		assert result_failure.output.contains('failed propagation'), result_failure.output
		os.write_file(path, r'fn test_native_entrypoint() { assert 7 * 6 == 41 }
')!
		failure := exec_native(all_tests)
		assert failure.exit_code != 0, failure.output
	}
}

fn test_native_self_launcher_executes_its_helper() {
	$if macos && arm64 {
		result := exec_native(['env', 'VFLAGS=', 'VTOOLS_NO_CACHE=1', 'V_MACOS_V3_NO_FALLBACK=1',
			'VSELF_SHOULD_FAIL=1', @VEXE, '-b', 'arm64', 'self', 'x2'])
		assert result.exit_code != 0, result.output
		assert result.output.contains('v self failed'), result.output
	}
}

fn publish_native_live_edit(path string, source string, mtime i64) ! {
	staged := path + '.tmp'
	defer { os.rm(staged) or {} }
	os.write_file(staged, source)!
	os.utime(staged, mtime, mtime)!
	// Publish the contents and timestamp together so the reloader cannot see a partial edit.
	os.rename(staged, path)!
}

@[unsafe]
fn test_native_live_reload_installs_new_code_after_an_edit() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64 \$ live reload ${os.getpid()}.v')
		output := path.all_before_last('.')
		log := output + '.log'
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
			os.rm(log) or {}
		}
		source := 'module main\nfn C.puts(s &u8) int\nfn C.fflush(f voidptr) int\nfn C.usleep(us u32) int\nfn init() { C.puts(c"initialized") }\nfn helper(n int) int { return n + 3 }\n@[live]\nfn hot(n int) int { return n + 1 }\nstruct LiveHolder { bias int }\nstruct OtherHolder {}\n@[live]\nfn (h LiveHolder) value(n int) int { return n + 1 + h.bias }\nfn (h OtherHolder) value(n int) int { return 99 }\nfn main() { holder := LiveHolder{}; other := OtherHolder{}; mut saved := hot; mut captured := false; for { value := hot(7); if other.value(7) != 99 || holder.value(7) != value { C.puts(c"method mismatch"); C.fflush(unsafe { nil }); return }; if value == 8 { C.puts(c"before") } else if value == 9 { if !captured { saved = hot; captured = true }; C.puts(c"after") } else if value == 10 && saved(7) == 9 { C.puts(c"again") }; C.fflush(unsafe { nil }); C.usleep(100000) } }\n'
		os.write_file(path, source)!
		initial_mtime := os.file_last_mod_unix(path)
		built := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
			'-nocache', '-b', 'arm64', '-os', 'macos', '-o', output, path])
		assert built.exit_code == 0, built.output
		mut process := os.new_process('/bin/sh')
		process.set_args(['-c', 'exec ${os.quoted_path(output)} > ${os.quoted_path(log)} 2>&1'])
		process.run()
		defer {
			if process.is_alive() { process.signal_kill() }
			process.wait()
			process.close()
		}
		mut saw_before := false
		for _ in 0 .. 100 {
			if (os.read_file(log) or { '' }).contains('before') {
				saw_before = true
				break
			}
			if !process.is_alive() {
				break
			}
			time.sleep(50 * time.millisecond)
		}
		assert saw_before, 'process state: ${process.status}; output: ${os.read_file(log) or { '' }}'
		assert (os.read_file(log) or { '' }).contains('initialized')
		publish_native_live_edit(path, source.replace('return n + 1', 'return n + 2'),
			initial_mtime + 1)!
		mut saw_after := false
		for _ in 0 .. 600 {
			if (os.read_file(log) or { '' }).contains('after') {
				saw_after = true
				break
			}
			if !process.is_alive() { break }
			time.sleep(50 * time.millisecond)
		}
		assert saw_after, os.read_file(log) or { '' }
		// One extraction succeeds before the method needs a fixup; publish neither and retry.
		failed_mtime := initial_mtime + 2
		publish_native_live_edit(path, source.replace('return n + 1', 'return n + 3').replace('return n + 3 + h.bias', 'return helper(n)'),
			failed_mtime)!
		mut saw_failure := false
		for _ in 0 .. 600 {
			if (os.read_file(log) or { '' }).contains('[live] rebuild failed') {
				saw_failure = true
				break
			}
			if !process.is_alive() { break }
			time.sleep(50 * time.millisecond)
		}
		assert saw_failure, os.read_file(log) or { '' }
		// Keep the failed timestamp to verify the reloader retries it after the repair.
		publish_native_live_edit(path, source.replace('return n + 1', 'return n + 3'),
			failed_mtime)!
		mut saw_again := false
		for _ in 0 .. 600 {
			if (os.read_file(log) or { '' }).contains('again') {
				saw_again = true
				break
			}
			if !process.is_alive() { break }
			time.sleep(50 * time.millisecond)
		}
		// `again` also checks that the pointer captured after the first reload remains callable.
		assert saw_again, os.read_file(log) or { '' }
	}
}

@[unsafe]
fn test_native_live_reload_watches_each_source_file() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_live_files_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		main_path := os.join_path(root, 'main.v')
		other_path := os.join_path(root, 'other.v')
		output := os.join_path(root, 'program')
		log := os.join_path(root, 'program.log')
		source := 'module main\nfn C.puts(s &u8) int\nfn C.fflush(f voidptr) int\nfn C.usleep(us u32) int\n@[live]\nfn hot_a(n int) int { return n + 1 }\nfn main() { for { a := hot_a(7); b := hot_b(7); if a == 8 && b == 8 { C.puts(c"before") } else if a == 8 && b == 9 { C.puts(c"secondary changed") } else if a == 9 && b == 9 { C.puts(c"both changed") }; C.fflush(unsafe { nil }); C.usleep(100000) } }\n'
		other_source := 'module main\n@[live]\nfn hot_b(n int) int { return n + 1 }\n'
		os.write_file(main_path, source)!
		os.write_file(other_path, other_source)!
		initial_mtime := os.file_last_mod_unix(main_path)
		built := exec_native(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
			'-nocache', '-b', 'arm64', '-o', output, root])
		assert built.exit_code == 0, built.output
		mut process := os.new_process('/bin/sh')
		process.set_args(['-c', 'exec ${os.quoted_path(output)} > ${os.quoted_path(log)} 2>&1'])
		process.run()
		defer {
			if process.is_alive() { process.signal_kill() }
			process.wait()
			process.close()
		}
		mut saw_before := false
		for _ in 0 .. 100 {
			if (os.read_file(log) or { '' }).contains('before') {
				saw_before = true
				break
			}
			if !process.is_alive() { break }
			time.sleep(50 * time.millisecond)
		}
		assert saw_before, os.read_file(log) or { '' }
		publish_native_live_edit(other_path, other_source.replace('return n + 1', 'return n + 2'),
			initial_mtime + 10)!
		mut saw_secondary := false
		for _ in 0 .. 600 {
			if (os.read_file(log) or { '' }).contains('secondary changed') {
				saw_secondary = true
				break
			}
			if !process.is_alive() { break }
			time.sleep(50 * time.millisecond)
		}
		assert saw_secondary, os.read_file(log) or { '' }
		// This timestamp remains below the other file's: a single maximum would miss it.
		publish_native_live_edit(main_path, source.replace('return n + 1', 'return n + 2'),
			initial_mtime + 2)!
		mut saw_both := false
		for _ in 0 .. 600 {
			if (os.read_file(log) or { '' }).contains('both changed') {
				saw_both = true
				break
			}
			if !process.is_alive() { break }
			time.sleep(50 * time.millisecond)
		}
		assert saw_both, os.read_file(log) or { '' }
	}
}
