// A `-syscalls` program selects the bare V runtime with libc available for C
// interoperability. The routes are verified on Linux only.
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

const syscalls_stdin_program = 'import os

fn main() {
	println(input_character())
	print(os.get_raw_line())
	print(os.get_raw_line())
}
'

// test_syscalls_stdin_readers_share_the_bare_descriptor checks that both readers
// use the raw descriptor without leaving unread characters in a libc buffer.
fn test_syscalls_stdin_readers_share_the_bare_descriptor() {
	$if !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'syscalls_stdin_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, syscalls_stdin_program)!
	for index, flags in [['-syscalls'], ['-d', 'vlibc'], ['-syscalls', '-d', 'vlibc']] {
		exe := os.join_path(root, 'main_${index}')
		mut command := [vexe, '-new-compiler', '-no-retry-compilation', '-nocache']
		command << flags
		command << ['-o', exe, source]
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		run := os.exec(['sh', '-c', 'printf "abc\\nde\\nrest" | "$@"', 'sh', exe])
		assert run.exit_code == 0, run.output
		assert run.output == '97\nbc\nde\n', run.output
	}
}

fn test_file_writes_respect_umask_and_preserve_existing_permissions() {
	$if !linux {
		return
	}
	$if !amd64 && !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'syscall_file_permissions_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "import os

fn main() {
	match os.args[2] {
		'text' { os.write_file(os.args[1], 'written')! }
		'bytes' { os.write_bytes(os.args[1], [u8(1), 0, 2])! }
		'empty' { os.write_file(os.args[1], '')! }
		'create' {
			mut file := os.create(os.args[1])!
			file.close()
		}
		else { assert false }
	}
}
")!

	for mode in ['hosted', 'syscalls', 'freestanding'] {
		exe := os.join_path(root, mode)
		mut args := [vexe, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none']
		if mode != 'hosted' {
			args << '-${mode}'
			// TinyCC has inline assembly only for x86, and retries are disabled above.
			$if arm64 {
				args << ['-cc', 'cc']
			}
		}
		args << ['-o', exe, source]
		build := os.exec(args)
		assert build.exit_code == 0, '${mode}: ${build.output}'
		for mask in [0o000, 0o022, 0o077] {
			for writer in ['text', 'bytes', 'empty', 'create'] {
				path := os.join_path(root, 'data')
				command := ['/bin/sh', '-c', 'umask ${mask:o}; exec "$@"', 'sh', exe, path, writer]
				created := os.exec(command)
				assert created.exit_code == 0, '${mode}/${writer}: ${created.output}'
				permissions := os.stat(path)!.mode & 0o777
				assert permissions == (0o666 & ~mask), '${mode}/${writer}/${mask:o}: ${permissions:o}'
				expected := match writer {
					'text' { 'written'.bytes() }
					'bytes' { [u8(1), 0, 2] }
					else { []u8{} }
				}
				assert os.read_bytes(path)! == expected
				// Truncation must keep existing permissions under every umask.
				os.write_file(path, 'previous longer contents')!
				os.chmod(path, 0o640)!
				rewritten := os.exec(command)
				assert rewritten.exit_code == 0, '${mode}/${writer}: ${rewritten.output}'
				assert os.stat(path)!.mode & 0o777 == 0o640, '${mode}/${writer}/${mask:o}'
				assert os.read_bytes(path)! == expected
				os.rm(path)!
			}
		}
	}
}

// test_syscalls_is_rejected_where_it_is_not_supported checks the combinations that
// have no runtime yet: native backends and OpenBSD. An explicit -freestanding
// instead disables the optional libc without changing the selected V runtime.
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
	assert both.exit_code == 0, both.output
	ran := os.exec([exe])
	assert ran.exit_code == 0 && ran.output == '1\n', ran.output
	for flag in ['-syscalls', '-freestanding'] {
		openbsd := os.exec([vexe, '-nocache', flag, '-os', 'openbsd', '-o', exe, source])
		assert openbsd.exit_code != 0
		assert openbsd.output.contains('not supported for an openbsd target'), openbsd.output
	}
}
