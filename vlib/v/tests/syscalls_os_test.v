// A `-syscalls` program links libc, but its os module takes the raw-syscall
// routes of vlib/builtin/linux_bare. The routes are verified on Linux only.
import os

const vexe = @VEXE

const syscalls_program = "import os

fn main() {
	dir := os.args[1]
	path := os.join_path(dir, 'data.txt')
	os.write_file(path, 'abc') or { panic(err) }
	text := os.read_file(path) or { panic(err) }
	println(text)
	println(os.file_size(path))
	println(os.exists(path))
	cwd := os.getwd()
	assert !cwd.contains('\\0')
	println(os.getenv('SYSCALLS_TEST_VALUE'))
}
"

// test_syscalls_program_runs_os_on_raw_syscalls builds the program with the
// default C compiler and with each of gcc and clang that is installed, then runs it.
fn test_syscalls_program_runs_os_on_raw_syscalls() {
	$if !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'syscalls_os_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, syscalls_program)!
	mut compilers := ['']
	for cc in ['gcc', 'clang'] {
		if _ := os.find_abs_path_of_executable(cc) {
			compilers << cc
		}
	}
	os.setenv('SYSCALLS_TEST_VALUE', 'ok', true)
	for cc in compilers {
		name := if cc == '' { 'default' } else { cc }
		exe := os.join_path(root, 'main_${name}')
		mut args := [vexe, '-nocache', '-syscalls']
		if cc != '' {
			args << ['-cc', cc]
		}
		args << ['-o', exe, source]
		build := os.exec(args)
		assert build.exit_code == 0, '${name}: ${build.output}'
		run := os.exec([exe, root])
		assert run.exit_code == 0, '${name}: ${run.output}'
		assert run.output == 'abc\n3\ntrue\nok\n', '${name}: ${run.output}'
	}
}

// test_syscalls_is_rejected_where_it_is_not_supported checks the combinations that
// have no runtime yet: a native backend, and `-freestanding` (which is a separate runtime).
fn test_syscalls_is_rejected_where_it_is_not_supported() {
	$if !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'syscalls_reject_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {\n\tprintln(1)\n}\n')!
	exe := os.join_path(root, 'main')
	native := os.exec([vexe, '-nocache', '-b', 'x64', '-syscalls', '-o', exe, source])
	assert native.exit_code != 0
	assert native.output.contains('only supported by the C backend'), native.output
	both := os.exec([vexe, '-nocache', '-syscalls', '-freestanding', '-o', exe, source])
	assert both.exit_code != 0
	assert both.output.contains('separate runtimes'), both.output
	for flag in ['-syscalls', '-freestanding'] {
		openbsd := os.exec([vexe, '-nocache', flag, '-os', 'openbsd', '-o', exe, source])
		assert openbsd.exit_code != 0
		assert openbsd.output.contains('not supported for an openbsd target'), openbsd.output
	}
}
