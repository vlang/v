module main

import os

struct PrefixCompileResult {
	exit_code int
	stdout    string
	stderr    string
}

struct PrefixCompileCase {
	name       string
	directive  string
	extra_args []string
}

fn prefix_compile_cases() []PrefixCompileCase {
	return [
		PrefixCompileCase{
			name:      'dynamic'
			directive: '#pkgconfig relocatable-compile'
		},
		PrefixCompileCase{
			name:       'global static'
			directive:  '#pkgconfig relocatable-compile'
			extra_args: ['-cflags', '-static']
		},
		PrefixCompileCase{
			name:      'directive static'
			directive: '#pkgconfig --static relocatable-compile'
		},
	]
}

fn prefix_compile_environment_value(mut env map[string]string, name string, value string) {
	lower_name := name.to_lower_ascii()
	for key in env.keys() {
		if key.to_lower_ascii() == lower_name {
			env.delete(key)
		}
	}
	env[name] = value
}

fn run_prefix_compile_process(executable string, args []string, work_dir string, env map[string]string) PrefixCompileResult {
	mut process := os.new_process(executable)
	process.set_args(args)
	process.set_work_folder(work_dir)
	process.set_environment(env)
	process.set_redirect_stdio()
	process.wait()
	result := PrefixCompileResult{
		exit_code: process.code
		stdout:    process.stdout_slurp()
		stderr:    process.stderr_slurp()
	}
	process.close()
	return result
}

fn test_relocated_pkgconfig_path_reaches_selected_compile_command() {
	$if !windows {
		return
	}
	ccompiler := @CCOMPILER
	if ccompiler !in ['gcc', 'clang'] {
		return
	}
	run_relocated_pkgconfig_compile(ccompiler)
}

fn run_relocated_pkgconfig_compile(ccompiler string) {
	compiler := os.find_abs_path_of_executable(ccompiler) or {
		panic('required ${ccompiler} executable was not found')
	}
	vexe := @VEXE
	assert os.is_file(vexe), 'required V executable was not found: ${vexe}'
	for compile_case in prefix_compile_cases() {
		root := os.join_path(os.vtmp_dir(),
			'pkgconfig compile ${os.getpid()} ${ccompiler} ${compile_case.name}')
		prefix := os.join_path(root, 'non standard sdk root')
		pc_dir := os.join_path(prefix, 'lib', 'pkgconfig')
		include_dir := os.join_path(prefix, 'include')
		lib_dir := os.join_path(prefix, 'lib')
		os.rmdir_all(root) or {}
		os.mkdir_all(pc_dir) or { panic(err) }
		os.mkdir_all(include_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}

		os.write_file(os.join_path(pc_dir, 'relocatable-compile.pc'), r'prefix=/synthetic-sdk
includedir=${prefix}/include
libdir=${prefix}/lib

Name: relocatable-compile
Description: Standalone relocated compile metadata
Version: 1.0.0
Cflags: -I${includedir}
Libs: -L${libdir}
') or {
			panic(err)
		}
		os.write_file(os.join_path(include_dir, 'relocatable_probe.h'),
			'#ifndef RELOCATABLE_PROBE_H\n#define RELOCATABLE_PROBE_H\nstatic int relocated_probe_value(void) { return 7601; }\n#endif\n') or {
			panic(err)
		}
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'relocatable_probe.exe')
		os.write_file(source,
			'module main\n${compile_case.directive}\n#include <relocatable_probe.h>\nfn C.relocated_probe_value() int\nfn main() {\n\tassert C.relocated_probe_value() == 7601\n}\n') or {
			panic(err)
		}

		mut env := os.environ()
		prefix_compile_environment_value(mut env, 'PKG_CONFIG_PATH', '')
		prefix_compile_environment_value(mut env, 'PKG_CONFIG_PATH_DEFAULTS', pc_dir)
		prefix_compile_environment_value(mut env, 'CPATH', '')
		prefix_compile_environment_value(mut env, 'C_INCLUDE_PATH', '')
		prefix_compile_environment_value(mut env, 'VFLAGS', '')
		mut args := ['-cc', compiler, '-gc', 'none', '-no-retry-compilation', '-no-rsp', '-showcc',
			'-show-c-output']
		args << compile_case.extra_args
		args << ['-o', output, source]
		compile_result := run_prefix_compile_process(vexe, args, root, env)
		assert compile_result.exit_code == 0, '${ccompiler} ${compile_case.name} compile failed:\nstdout:\n${compile_result.stdout}\nstderr:\n${compile_result.stderr}'

		command_output := '${compile_result.stdout}\n${compile_result.stderr}'.replace('\\', '/')
		physical_prefix := os.real_path(prefix).replace('\\', '/')
		assert command_output.contains('${physical_prefix}/include'), command_output
		assert command_output.contains('${physical_prefix}/lib'), command_output
		assert !command_output.contains('/synthetic-sdk'), command_output

		runtime_result := run_prefix_compile_process(output, [], root, env)
		assert runtime_result.exit_code == 0, '${ccompiler} ${compile_case.name} runtime failed:\nstdout:\n${runtime_result.stdout}\nstderr:\n${runtime_result.stderr}'
	}
}
