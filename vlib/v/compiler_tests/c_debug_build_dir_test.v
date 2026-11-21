import os
import time

// build_with_c_debug builds `source` into `executable` with `-cg` and returns the build output.
fn build_with_c_debug(source string, executable string) string {
	result := os.exec([@VEXE, '-new-compiler', '-cg', '-gc', 'none', '-nocache', '-o', executable,
		source])
	assert result.exit_code == 0, result.output
	return result.output
}

fn test_repeated_c_debug_builds_leave_one_directory_named_once() {
	root := os.join_path(os.vtmp_dir(), 'c_debug_dir_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("kept once") }')!
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	dir_prefix := '.${os.base(executable)}.v3cc.'
	mut outputs := []string{}
	for _ in 0 .. 3 {
		outputs << build_with_c_debug(source, executable)
	}
	dirs := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert dirs.len == 1, 'three -cg builds left ${dirs.len} directories: ${dirs}'
	assert os.exists(os.join_path(root, dirs[0], 'src.c'))
	for output in outputs {
		assert output.count(dirs[0]) == 1, 'the build output names ${dirs[0]} ${output.count(dirs[0])} times:\n${output}'
	}
}

fn test_a_build_without_c_debug_names_no_directory_and_leaves_none() {
	root := os.join_path(os.vtmp_dir(), 'c_debug_dir_off_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("plain") }')!
	executable := os.join_path(root, 'program')
	result := os.exec([@VEXE, '-new-compiler', '-gc', 'none', '-nocache', '-o', executable, source])
	assert result.exit_code == 0, result.output
	assert !result.output.contains('.v3cc.'), result.output
	assert os.ls(root)!.filter(it.contains('.v3cc.')).len == 0
}

// The executable that is in place names its C sources in its debug information. A rebuild that
// fails in the C compiler or the linker leaves that executable where it is, so it must leave
// those sources too: the same files, not the ones of the build that failed.
fn test_a_failed_rebuild_keeps_the_sources_of_the_executable_in_place() {
	root := os.join_path(os.vtmp_dir(), 'c_debug_dir_failed_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("first build") }')!
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	build_with_c_debug(source, executable)
	dir_prefix := '.${os.base(executable)}.v3cc.'
	first_dirs := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert first_dirs.len == 1, first_dirs.str()
	c_source := os.join_path(root, first_dirs[0], 'src.c')
	first_c := os.read_file(c_source)!
	first_executable := os.read_bytes(executable)!
	assert first_c.contains('first build')
	// The program changes, and the link cannot succeed: the library does not exist.
	os.write_file(source, 'fn main() { println("second build") }')!
	failed := os.exec([@VEXE, '-new-compiler', '-cg', '-gc', 'none', '-nocache', '-ldflags',
		'-lv3_c_debug_dir_test_no_such_library', '-o', executable, source])
	assert failed.exit_code != 0, 'the build with a missing library succeeded:\n${failed.output}'
	assert os.read_bytes(executable)! == first_executable, 'a failed build changed the executable'
	after_failure := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert after_failure == first_dirs, 'after the failed build: ${after_failure}'
	assert os.exists(c_source), 'the failed build removed ${c_source}:\n${failed.output}'
	assert os.read_file(c_source)! == first_c, 'the sources are no longer the ones of the executable in place'
	assert os.exec([executable]).output.trim_space() == 'first build'
	// A build that succeeds replaces both, and leaves one directory again.
	build_with_c_debug(source, executable)
	after_success := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert after_success == first_dirs, 'after the build that succeeded: ${after_success}'
	assert os.read_file(c_source)!.contains('second build')
	assert os.exec([executable]).output.trim_space() == 'second build'
}

// A rebuild can fail after the new directory is made and before any C is compiled: an instance
// of a generic function that does not check ends the build there. The executable in place and
// its sources must be the same files after that failure too, at the same paths.
fn test_a_rebuild_that_fails_a_generic_instance_check_keeps_the_sources_too() {
	root := os.join_path(os.vtmp_dir(), 'c_debug_dir_instance_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("first build") }')!
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	build_with_c_debug(source, executable)
	dir_prefix := '.${os.base(executable)}.v3cc.'
	first_dirs := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert first_dirs.len == 1, first_dirs.str()
	c_source := os.join_path(root, first_dirs[0], 'src.c')
	first_c := os.read_file(c_source)!
	first_executable := os.read_bytes(executable)!
	// `is_set[int]` returns an int as a bool: its instance is refused.
	os.write_file(source, 'fn is_set[T](x T) bool {\n\treturn x\n}\n\nfn main() {\n\tprintln(is_set(1))\n}\n')!
	failed := os.exec([@VEXE, '-new-compiler', '-cg', '-gc', 'none', '-nocache', '-o', executable,
		source])
	assert failed.exit_code != 0, 'the build of a generic instance that does not check succeeded:\n${failed.output}'
	// the build got as far as its new directory: it said where it keeps the C sources
	assert failed.output.contains(first_dirs[0]), failed.output
	assert os.read_bytes(executable)! == first_executable, 'a failed build changed the executable'
	after_failure := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert after_failure == first_dirs, 'after the failed build: ${after_failure}'
	assert os.exists(c_source), 'the failed build removed ${c_source}:\n${failed.output}'
	assert os.read_file(c_source)! == first_c, 'the sources are no longer the ones of the executable in place'
	assert os.exec([executable]).output.trim_space() == 'first build'
}

// A C compiler that stops when it is run in the build directory of a `-cg` build (the compiler
// runs it there, on `src.c`), and says so: the build is then interrupted at a known place,
// after the old directory was set aside.
const pausing_gcc_script = '#!/bin/sh
if [ -f "\$C_DEBUG_DIR_TEST_PAUSE" ]; then
	case "\$PWD" in
	*.v3cc.cg)
		echo paused > "\$C_DEBUG_DIR_TEST_PAUSE.started"
		sleep 600
		;;
	esac
fi
exec "\$C_DEBUG_DIR_TEST_GCC" "\$@"
'

// Ctrl-C during a rebuild ends the compiler without running its exit handlers. The executable
// in place is the old one then, so its C sources must be the old ones too, at the same path,
// as soon as the compiler is gone. The build runs in a process group of its own, as a command
// in a terminal does, and the interrupt goes to that group, as Ctrl-C does.
fn test_an_interrupted_rebuild_keeps_the_sources_of_the_executable_in_place() {
	$if windows {
		return
	}
	real_gcc := os.find_abs_path_of_executable('gcc') or {
		eprintln('skipping: no gcc')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'c_debug_dir_interrupted_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'cc'))!
	defer { os.rmdir_all(root) or {} }
	gcc := os.join_path(root, 'cc', 'gcc')
	os.write_file(gcc, pausing_gcc_script)!
	os.chmod(gcc, 0o755)!
	pause := os.join_path(root, 'pause')
	os.setenv('C_DEBUG_DIR_TEST_PAUSE', pause, true)
	os.setenv('C_DEBUG_DIR_TEST_GCC', real_gcc, true)
	defer {
		os.unsetenv('C_DEBUG_DIR_TEST_PAUSE')
		os.unsetenv('C_DEBUG_DIR_TEST_GCC')
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("first build") }')!
	executable := os.join_path(root, 'program')
	build_args := ['-new-compiler', '-cg', '-cc', gcc, '-gc', 'none', '-nocache', '-o', executable,
		source]
	mut first_args := [@VEXE]
	first_args << build_args
	first := os.exec(first_args)
	assert first.exit_code == 0, first.output
	dir_prefix := '.${os.base(executable)}.v3cc.'
	first_dirs := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert first_dirs.len == 1, first_dirs.str()
	c_source := os.join_path(root, first_dirs[0], 'src.c')
	first_c := os.read_file(c_source)!
	first_executable := os.read_bytes(executable)!
	assert first_c.contains('first build')
	// The second build stops in the C compiler.
	os.write_file(source, 'fn main() { println("second build") }')!
	os.write_file(pause, 'pause')!
	mut build := os.new_process(@VEXE)
	build.set_args(build_args)
	build.use_pgroup = true
	build.set_redirect_stdio()
	build.run()
	defer {
		build.signal_pgkill()
		build.close()
	}
	mut paused := false
	for _ in 0 .. 1800 {
		if os.exists(pause + '.started') {
			paused = true
			break
		}
		if !build.is_alive() {
			break
		}
		time.sleep(100 * time.millisecond)
	}
	assert paused, 'the second build did not reach the C compiler: ${build.stdout_slurp()} ${build.stderr_slurp()}'
	// the build is past the point where it sets the old directory aside, and writes a new one
	assert os.is_dir(os.join_path(root, first_dirs[0] + '.prev'))
	interrupt := os.exec(['/bin/kill', '-INT', '--', '-${build.pid}'])
	assert interrupt.exit_code == 0, interrupt.output
	mut gone := false
	for _ in 0 .. 100 {
		if !build.is_alive() {
			gone = true
			break
		}
		time.sleep(100 * time.millisecond)
	}
	assert gone, 'the compiler was still there ten seconds after its interrupt'
	// immediately after the compiler is gone
	assert os.read_bytes(executable)! == first_executable, 'an interrupted build changed the executable'
	assert os.exists(c_source), 'the interrupted build left no ${c_source}'
	assert os.read_file(c_source)! == first_c, 'the sources are no longer the ones of the executable in place'
	assert os.exec([executable]).output.trim_space() == 'first build'
	// Nothing is set aside any more. What the interrupted build had written may be beside the
	// directory, under a name of its own: a signal handler can rename, and no more.
	interrupted_dir := first_dirs[0] + '.interrupted'
	after_interrupt := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert after_interrupt.filter(it != interrupted_dir) == first_dirs, 'after the interrupted build: ${after_interrupt}'
	// The next build removes it, and leaves one directory again.
	os.rm(pause)!
	second := os.exec(first_args)
	assert second.exit_code == 0, second.output
	after_success := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert after_success == first_dirs, 'after the build that succeeded: ${after_success}'
	assert os.read_file(c_source)!.contains('second build')
	assert os.exec([executable]).output.trim_space() == 'second build'
}
