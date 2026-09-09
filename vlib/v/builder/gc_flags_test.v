module builder

import os

fn execute_without_vflags(cmd string) os.Result {
	old_vflags := os.getenv_opt('VFLAGS')
	// These assertions cover flags emitted by the established C builder. Linux
	// now defaults to V3, so select V1 explicitly after isolating ambient flags.
	os.setenv('VFLAGS', '-old-compiler', true)
	res := os.execute(cmd)
	if vflags := old_vflags {
		os.setenv('VFLAGS', vflags, true)
	} else {
		os.unsetenv('VFLAGS')
	}
	return res
}

const showcc_prefix = '> C compiler cmd: '

struct ShowccExpectation {
	tcc    string
	dylib  string
	rpath  string
	binary string
}

fn tokenize_showcc_command(command string) ![]string {
	mut tokens := []string{}
	mut token := []u8{}
	mut quote := u8(0)
	mut started := false
	mut i := 0
	for i < command.len {
		ch := command[i]
		if quote == 0 && ch in [` `, `\t`, `\r`, `\n`] {
			if started {
				tokens << token.bytestr()
				token = []u8{}
				started = false
			}
			i++
			continue
		}
		if ch in [`'`, `"`] {
			if quote == 0 {
				quote = ch
				started = true
				i++
				continue
			}
			if quote == ch {
				quote = 0
				i++
				continue
			}
		}
		if ch == `\\` && quote != `'` && i + 1 < command.len {
			next := command[i + 1]
			escapable := if quote == `"` {
				next in [`"`, `\\`]
			} else {
				next in [` `, `\t`, `\r`, `\n`, `'`, `"`, `\\`]
			}
			if escapable {
				token << next
				started = true
				i += 2
				continue
			}
		}
		token << ch
		started = true
		i++
	}
	if quote != 0 {
		return error('unterminated quote in -showcc command')
	}
	if started {
		tokens << token.bytestr()
	}
	return tokens
}

fn count_showcc_token(tokens []string, expected string) int {
	mut count := 0
	for token in tokens {
		if token == expected {
			count++
		}
	}
	return count
}

fn validate_showcc_output(output string, expected ShowccExpectation) ![]string {
	lines := output.split_into_lines().filter(it.starts_with(showcc_prefix))
	if lines.len != 1 {
		return error('expected exactly one -showcc compiler command, found ${lines.len}')
	}
	tokens := tokenize_showcc_command(lines[0][showcc_prefix.len..])!
	if tokens.len == 0 || tokens[0] != expected.tcc || count_showcc_token(tokens, expected.tcc) != 1 {
		return error('expected exactly one physical TCC token in compiler position')
	}
	if count_showcc_token(tokens, expected.dylib) != 1
		|| count_showcc_token(tokens, expected.rpath) != 1 {
		return error('expected exactly one physical dylib/rpath token pair')
	}
	if count_showcc_token(tokens, '-o') != 1 {
		return error('expected exactly one -o token')
	}
	output_index := tokens.index('-o')
	if output_index < 0 || output_index + 1 >= tokens.len
		|| tokens[output_index + 1] != expected.binary {
		return error('the compiler command does not target the expected binary')
	}
	compiler_names := ['cc', 'gcc', 'clang', 'clang++', 'tcc', 'tcc.exe']
	for i, token in tokens {
		if token.starts_with('@') {
			return error('response-file tokens are forbidden')
		}
		if token.contains('libgc.a') || token == '-lgc' {
			return error('static or implicit libgc linkage is forbidden')
		}
		if token != '-o' && token.starts_with('-o') {
			return error('joined output options are forbidden')
		}
		if token != expected.tcc
			&& (token.starts_with(expected.tcc) || (i > 0 && os.file_name(token) in compiler_names)) {
			return error('fallback, duplicate, or near-match compiler token')
		}
		if token != expected.dylib && token.contains('libgc.dylib') {
			return error('near-match or duplicate dylib token')
		}
		if token != expected.rpath && token.starts_with('-Wl,-rpath,') {
			return error('near-match or duplicate rpath token')
		}
	}
	lower_output := output.to_lower()
	for marker in ['falling back', 'retrying with', 'fallback compiler', 'backup compiler'] {
		if lower_output.contains(marker) {
			return error('compiler fallback marker found in output')
		}
	}
	return tokens
}

fn showcc_fixture(tokens []string) string {
	return showcc_prefix + tokens.map(os.quoted_path(it)).join(' ')
}

fn showcc_tokens_with_extra(tokens []string, index int, extra string) []string {
	mut result := tokens.clone()
	result.insert(index, extra)
	return result
}

fn assert_showcc_rejected(output string, expected ShowccExpectation) {
	validate_showcc_output(output, expected) or { return }
	assert false, 'invalid -showcc fixture was accepted: ${output}'
}

fn test_macos_tcc_boehm_showcc_parser_fixtures() {
	expected := ShowccExpectation{
		tcc:    '/physical/tcc/tcc.exe'
		dylib:  '/physical/tcc/lib/libgc.dylib'
		rpath:  '-Wl,-rpath,/physical/tcc/lib'
		binary: '/tmp/output'
	}
	good := [expected.tcc, expected.rpath, expected.dylib, '-o', expected.binary, '/tmp/input.c']
	validate_showcc_output(showcc_fixture(good), expected) or { assert false, err.msg() }
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, 1, expected.tcc)),
		expected)
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, 1, expected.tcc + '.bad')),
		expected)
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, 3, expected.dylib + '.bad')),
		expected)
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, 3, expected.dylib)),
		expected)
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, 2, expected.rpath + '.bad')),
		expected)
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, 2, expected.rpath)),
		expected)
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, 3, '@/tmp/evil.rsp')),
		expected)
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, 3,
		'/physical/tcc/lib/libgc.a')), expected)
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, 3, '-lgc')), expected)
	assert_showcc_rejected(showcc_fixture(showcc_tokens_with_extra(good, good.len, '/usr/bin/clang')),
		expected)
	assert_showcc_rejected(showcc_fixture(good) + '\nfalling back to clang', expected)
}

fn test_macos_amd64_tcc_boehm_uses_bundled_libgc_dylib() {
	$if !macos {
		return
	}
	$if !amd64 {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'builder_gc_flags_test_${os.getpid()}')
	os.mkdir_all(test_root) or { panic(err) }
	physical_test_root := os.real_path(test_root)
	exe_path := os.join_path(physical_test_root, 'hello_world')
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	tcc_root := os.real_path(os.join_path(@VEXEROOT, 'thirdparty', 'tcc'))
	expected := ShowccExpectation{
		tcc:    os.join_path(tcc_root, 'tcc.exe')
		dylib:  os.join_path(tcc_root, 'lib', 'libgc.dylib')
		rpath:  '-Wl,-rpath,${os.join_path(tcc_root, 'lib')}'
		binary: exe_path
	}
	cmd := '${os.quoted_path(@VEXE)} -cc tcc -gc boehm -showcc -no-retry-compilation -no-rsp -nocache -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := execute_without_vflags(cmd)
	defer {
		os.rmdir_all(test_root) or {}
	}
	assert res.exit_code == 0, res.output
	validate_showcc_output(res.output, expected) or { assert false, '${err.msg()}\n${res.output}' }
	run_res := os.execute(os.quoted_path(exe_path))
	assert run_res.exit_code == 0, run_res.output
}

fn test_linux_musl_tcc_boehm_uses_system_libgc() {
	$if !linux {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'builder_gc_flags_musl_${os.getpid()}')
	fake_tcc := os.join_path(test_root, 'fake-tcc')
	exe_path := os.join_path(test_root, 'hello_world')
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	os.write_file(fake_tcc, '#!/bin/sh\nexit 0\n') or { panic(err) }
	os.chmod(fake_tcc, 0o700) or { panic(err) }
	cmd := '${os.quoted_path(@VEXE)} -dump-c-flags - -cc ${os.quoted_path(fake_tcc)} -no-retry-compilation -musl -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := execute_without_vflags(cmd)
	assert res.exit_code == 0, res.output
	// The optional musl system libgc may not be installed. The flags are dumped
	// before linker discovery, so verify the selection without requiring it here.
	assert res.output.contains('-lgc')
	assert !res.output.contains('thirdparty/tcc/lib/libgc.a')
}

fn test_linux_musl_gcc_boehm_uses_system_libgc() {
	$if !linux {
		return
	}
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	cmd := '${os.quoted_path(@VEXE)} -dump-c-flags - -cc musl-gcc ${os.quoted_path(source_path)}'
	res := execute_without_vflags(cmd)
	// `musl-gcc` and its system libgc are optional on developer machines. The
	// flags are dumped before C compiler discovery, so inspect the selection
	// contract without requiring that optional toolchain here.
	assert res.output.contains('-lgc')
	assert !res.output.contains('thirdparty/tcc/lib/libgc.a')
}

fn test_no_gc_thread_local_alloc_uses_source_libgc_without_tla_define() {
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	cmd := '${os.quoted_path(@VEXE)} -dump-c-flags - -d no_gc_thread_local_alloc ${os.quoted_path(source_path)}'
	res := execute_without_vflags(cmd)
	assert res.exit_code == 0, res.output
	normalized := res.output.replace('\\', '/')
	assert !normalized.contains('thirdparty/tcc/lib/libgc')
	assert !normalized.contains('\n-lgc\n')
	assert normalized.contains('-D GC_THREADS=1')
	assert !normalized.contains('THREAD_LOCAL_ALLOC')
}

fn tcc_compiler_for_test() string {
	bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if tcc_compiler_is_usable(bundled_tcc) {
		return bundled_tcc
	}
	system_tcc := os.find_abs_path_of_executable('tcc') or { return '' }
	if tcc_compiler_is_usable(system_tcc) {
		return system_tcc
	}
	return ''
}

fn tcc_compiler_is_usable(tcc_path string) bool {
	if tcc_path == '' || !os.is_file(tcc_path) || !os.is_executable(tcc_path) {
		return false
	}
	probe := os.execute('${os.quoted_path(tcc_path)} -v')
	return probe.exit_code == 0
}

fn tcc_can_compile_v_program(tcc_path string, test_dir string) bool {
	exe_path := os.join_path(test_dir, 'tcc_probe')
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	res :=
		execute_without_vflags('${os.quoted_path(@VEXE)} -cc ${os.quoted_path(tcc_path)} -gc none -no-retry-compilation -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}')
	return res.exit_code == 0
}

fn test_tcc_use_libbacktrace_does_not_compile_libbacktrace() {
	test_dir := os.join_path(os.vtmp_dir(), 'builder_use_libbacktrace_tcc_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	tcc_path := tcc_compiler_for_test()
	vmodules_path := os.join_path(test_dir, 'vmodules')
	old_vmodules := os.getenv_opt('VMODULES')
	old_vcache := os.getenv_opt('VCACHE')
	os.setenv('VMODULES', vmodules_path, true)
	os.setenv('VCACHE', os.join_path(vmodules_path, '.cache'), true)
	defer {
		if vcache := old_vcache {
			os.setenv('VCACHE', vcache, true)
		} else {
			os.unsetenv('VCACHE')
		}
		if vmodules := old_vmodules {
			os.setenv('VMODULES', vmodules, true)
		} else {
			os.unsetenv('VMODULES')
		}
	}
	if tcc_path == '' || !tcc_can_compile_v_program(tcc_path, test_dir) {
		return
	}
	source_path := os.join_path(test_dir, 'main.v')
	exe_path := os.join_path(test_dir, 'main')
	os.write_file(source_path, "fn main() {\n\tpanic('aaaa')\n}\n") or { panic(err) }

	res :=
		execute_without_vflags('${os.quoted_path(@VEXE)} -cc ${os.quoted_path(tcc_path)} -gc none -no-retry-compilation -d use_libbacktrace -d trace_thirdparty_obj_files -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}')

	assert res.exit_code == 0, res.output
	normalized := res.output.replace('\\', '/')
	assert !normalized.contains('thirdparty/libbacktrace/backtrace'), res.output
	assert !normalized.contains('Failed build_thirdparty_obj_file'), res.output
}
