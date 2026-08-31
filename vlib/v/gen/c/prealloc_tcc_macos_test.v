// vtest build: macos && arm64
import os

// https://github.com/vlang/v/issues/28030
fn test_bare_main_uses_tcc_without_debug_flags() {
	test_dir := os.join_path(os.vtmp_dir(), 'bare_main_tcc_macos_test_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	assert os.is_executable(tcc), 'missing bundled TinyCC: ${tcc}'
	source_path := os.join_path(test_dir, 'main.v')
	executable_path := os.join_path(test_dir, 'bare_main')
	os.write_file(source_path, 'fn main() {}\n')!
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	defer {
		if vflags := old_vflags {
			os.setenv('VFLAGS', vflags, true)
		}
	}
	// A non-default message limit keeps this regression on the V1 C generator used by cmd/v.
	compile_cmd := '${os.quoted_path(@VEXE)} -message-limit 199 -cc ${os.quoted_path(tcc)} -no-retry-compilation -nocache -o ${os.quoted_path(executable_path)} ${os.quoted_path(source_path)}'
	compile_result := os.execute(compile_cmd)
	assert compile_result.exit_code == 0, '${compile_cmd}\n${compile_result.output}'
	run_result := os.execute(os.quoted_path(executable_path))
	assert run_result.exit_code == 0, run_result.output
}

fn test_prealloc_uses_tcc_without_native_tls() {
	test_dir := os.join_path(os.vtmp_dir(), 'prealloc_tcc_macos_test_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	assert os.is_executable(tcc), 'missing bundled TinyCC: ${tcc}'
	source_path := os.join_path(test_dir, 'prealloc_threads.v')
	executable_path := os.join_path(test_dir, 'prealloc_threads')
	os.write_file(source_path, 'fn worker(id int) int {
	mut values := []int{len: 4096}
	for i in 0 .. values.len {
		values[i] = id
	}
	return values[0] + values[values.len - 1]
}

fn main() {
	mut workers := []thread int{}
	for id in 1 .. 5 {
		workers << spawn worker(id)
	}
	mut sum := 0
	for handle in workers {
		sum += handle.wait()
	}
	println(sum)
}
')!
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	defer {
		if vflags := old_vflags {
			os.setenv('VFLAGS', vflags, true)
		}
	}
	// A non-default message limit keeps this regression on the V1 C generator used by cmd/v.
	compile_cmd := '${os.quoted_path(@VEXE)} -message-limit 199 -cc ${os.quoted_path(tcc)} -gc none -prealloc -no-retry-compilation -nocache -o ${os.quoted_path(executable_path)} ${os.quoted_path(source_path)}'
	compile_result := os.execute(compile_cmd)
	assert compile_result.exit_code == 0, '${compile_cmd}\n${compile_result.output}'
	run_result := os.execute(os.quoted_path(executable_path))
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '20', run_result.output
}

fn test_prealloc_parallel_cc_shares_tls_declaration() {
	test_dir := os.join_path(os.vtmp_dir(), 'prealloc_parallel_cc_macos_test_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path(test_dir, 'main.v')
	executable_path := os.join_path(test_dir, 'prealloc_parallel')
	os.write_file(source_path, 'fn main() { println(42) }\n')!
	compile_cmd := '${os.quoted_path(@VEXE)} -message-limit 199 -gc none -prealloc -parallel-cc -o ${os.quoted_path(executable_path)} ${os.quoted_path(source_path)}'
	compile_result := os.execute(compile_cmd)
	assert compile_result.exit_code == 0, '${compile_cmd}\n${compile_result.output}'
	run_result := os.execute(os.quoted_path(executable_path))
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '42', run_result.output
}
