import os
import time

const macos_standalone_startup_program = "import os

fn finish() {
	println('finished')
}

fn main() {
	assert os.args.len == 3
	assert os.args[1] == 'first'
	assert os.args[2] == 'two words'
	at_exit(finish) or { panic(err) }
	println(os.args[1])
	println(os.args[2])
	println('ready')
}
"

fn test_macos_amd64_freestanding_initial_stack_and_exit() ! {
	$if !macos || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_standalone_startup_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, macos_standalone_startup_program)!
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	for production in [false, true] {
		binary := os.join_path(root, if production { 'prod' } else { 'debug' })
		mut command := [vexe, '-no-retry-compilation', '-cc', 'clang', '-freestanding', '-gc',
			'none', '-o', binary]
		if production {
			command << '-prod'
		}
		command << source
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		mut process := os.new_process(binary)
		process.set_args(['first', 'two words'])
		process.set_redirect_stdio()
		process.run()
		watch := time.new_stopwatch()
		for process.is_alive() && watch.elapsed() < 10 * time.second {
			time.sleep(time.millisecond)
		}
		expired := process.is_alive()
		if expired {
			process.signal_kill()
		}
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		code := process.code
		process.close()
		assert !expired && code == 0, output
		assert output == 'first\ntwo words\nready\nfinished\n', output
		commands := os.exec(['otool', '-l', binary])
		assert commands.exit_code == 0, commands.output
		assert commands.output.contains('cmd LC_UNIXTHREAD'), commands.output
		for forbidden in ['LC_LOAD_DYLINKER', 'LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB',
			'LC_LOAD_UPWARD_DYLIB', 'LC_LAZY_LOAD_DYLIB'] {
			assert !commands.output.contains('cmd ${forbidden}'), commands.output
		}
		undefined := os.exec(['nm', '-u', binary])
		assert undefined.exit_code == 0, undefined.output
		assert undefined.output.trim_space() == '', undefined.output
	}
}
