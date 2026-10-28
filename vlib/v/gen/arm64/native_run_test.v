module arm64

import os
import time

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

fn test_native_linux_output_is_an_object_and_executable_requests_fail() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_linux_object_${os.getpid()}.v')
		object := path.all_before_last('.') + '.o'
		executable := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(object) or {}
			os.rm(executable) or {}
		}
		os.write_file(path, 'fn main() {}\n')!
		command := ['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none', '-nocache',
			'-b', 'arm64', '-os', 'linux']
		mut object_args := command.clone()
		object_args << ['-o', object, path]
		built := exec_native(object_args)
		assert built.exit_code == 0, built.output
		bytes := os.read_bytes(object)!
		assert bytes[..4] == [u8(0x7f), `E`, `L`, `F`]
		mut executable_args := command.clone()
		executable_args << ['-o', executable, path]
		rejected := exec_native(executable_args)
		assert rejected.exit_code != 0, rejected.output
		assert rejected.output.contains('supports object output only'), rejected.output
		assert !os.exists(executable)
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
		// The runtime watches second-resolution modification times.
		time.sleep(1100 * time.millisecond)
		os.write_file(path, source.replace('return n + 1', 'return n + 2'))!
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
		time.sleep(1100 * time.millisecond)
		os.write_file(path, source.replace('return n + 1', 'return n + 3').replace('return n + 3 + h.bias', 'return helper(n)'))!
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
		failed_mtime := os.file_last_mod_unix(path)
		os.write_file(path, source.replace('return n + 1', 'return n + 3'))!
		os.utime(path, failed_mtime, failed_mtime)!
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
