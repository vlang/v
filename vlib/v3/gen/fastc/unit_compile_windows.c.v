module fastc

import os
import v3.cmdexec

struct FastcUnitCompile {
mut:
	process &os.Process = unsafe { nil }
	object  string
}

// fastc_compile_c_units compiles the translation units to objects with
// concurrent TinyCC processes and returns the object paths, or the output of
// the first compile that failed.
pub fn fastc_compile_c_units(tcc string, base_args []string, unit_paths []string) ![]string {
	mut compiles := []FastcUnitCompile{cap: unit_paths.len}
	for unit_path in unit_paths {
		object := unit_path[..unit_path.len - 2] + '.o'
		mut args := base_args.clone()
		args << ['-c', unit_path, '-o', object]
		mut process := os.new_process(tcc)
		process.set_args(args)
		process.set_redirect_stdio_merged()
		process.run()
		compiles << FastcUnitCompile{
			process: process
			object: object
		}
	}
	mut objects := []string{cap: unit_paths.len}
	mut failure := ''
	for mut compile in compiles {
		compile.process.wait()
		output := compile.process.stdout_slurp()
		code := compile.process.code
		compile.process.close()
		if code != 0 && failure == '' {
			failure = if output.len > 0 { output } else { 'tcc failed on ${compile.object}' }
		}
		objects << compile.object
	}
	if failure != '' {
		return error(failure)
	}
	return objects
}

// fastc_run_command runs the program with the argument vector and returns
// its exit code and merged output.
pub fn fastc_run_command(program string, args []string) os.Result {
	return cmdexec.run(program, args)
}
