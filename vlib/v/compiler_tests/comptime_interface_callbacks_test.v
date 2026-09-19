import os
import v.cmdexec

const interface_callbacks_vexe = @VEXE
const interface_callbacks_compile_timeout_ms = i64(120_000)
const interface_callbacks_run_timeout_ms = i64(10_000)

struct InterfaceCallbackCase {
	name     string
	source   string
	expected string
}

fn test_comptime_interface_callbacks_without_direct_call_warmup() {
	work_dir := os.join_path(os.temp_dir(), 'v_interface_callbacks_${os.getpid()}')
	os.mkdir_all(work_dir) or { panic(err) }
	eprintln('[interface callbacks] compiler: ${interface_callbacks_vexe}')
	eprintln('[interface callbacks] fixtures and logs: ${work_dir}')
	// Use the compiler running this test, rather than silently rebuilding it.
	// -new-compiler disables the launcher's compatibility fallback, so an old
	// compiler cannot hide a failure of the reflected callback.
	cases := [
		InterfaceCallbackCase{
			name:     'auto_str'
			source:   $embed_file('testdata/comptime_interface_callbacks/auto_str.vv').to_string()
			expected: "IFoo(Foo{\n    name: 'you should see this'\n})\n".repeat(3).trim_space()
		},
		InterfaceCallbackCase{
			name:     'method_call'
			source:   $embed_file('testdata/comptime_interface_callbacks/method_call.vv').to_string()
			expected: 'you should see this\n'.repeat(3).trim_space()
		},
	]
	callback_source := $embed_file('testdata/comptime_interface_callbacks/callbacks.vv').to_string()
	mut failures := []string{}
	for case in cases {
		project := os.join_path(work_dir, case.name)
		module_dir := os.join_path(project, 'callbacks')
		os.mkdir_all(module_dir) or { panic(err) }
		os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'interface_callbacks' }\n") or {
			panic(err)
		}
		os.write_file(os.join_path(module_dir, 'callbacks.v'), callback_source) or { panic(err) }
		main_file := os.join_path(project, 'main.v')
		os.write_file(main_file, case.source) or { panic(err) }
		for mode in ['serial', 'default'] {
			mut args := ['-new-compiler', '-nocache']
			if mode == 'serial' {
				args << '-no-parallel'
			}
			output := os.join_path(project, 'program_${mode}')
			args << ['-o', output, main_file]
			compile := interface_callbacks_run('${case.name}/${mode}: compile',
				interface_callbacks_vexe, args, interface_callbacks_compile_timeout_ms,
				os.join_path(project, '${mode}_compile.log'))
			if compile.exit_code != 0 {
				failures << '${case.name}/${mode}: compilation failed (exit ${compile.exit_code})\n${compile.output}'
				continue
			}
			run := interface_callbacks_run('${case.name}/${mode}: run', output, []string{},
				interface_callbacks_run_timeout_ms, os.join_path(project, '${mode}_run.log'))
			actual := run.output.replace('\r\n', '\n').trim_space()
			if run.exit_code != 0 || actual != case.expected {
				failures << '${case.name}/${mode}: exit ${run.exit_code}\nexpected:\n${case.expected}\nactual:\n${actual}'
			}
		}
	}
	// Exercise both failures even when auto-str fails first: the issue comments
	// also report a panic when invoking an interface method from the callback.
	// Keep the reproducer and command logs on failure instead of deleting them.
	if failures.len > 0 {
		eprintln('[interface callbacks] failed; fixtures and logs retained in ${work_dir}')
	} else {
		os.rmdir_all(work_dir) or {}
	}
	assert failures.len == 0, failures.join('\n\n')
}

fn interface_callbacks_run(stage string, program string, args []string, timeout_ms i64, log_path string) os.Result {
	command := cmdexec.display(program, args)
	header := '${stage}\ncommand: ${command}\ntimeout: ${timeout_ms}ms\n'
	eprintln('[interface callbacks] ${stage} (timeout ${timeout_ms}ms)')
	eprintln('> ${command}')
	os.write_file(log_path, header) or { panic(err) }
	// The existing runner drains both pipes and kills the process group on
	// timeout, including compiler descendants that could otherwise keep it open.
	result := cmdexec.run_with_timeout(program, args, timeout_ms)
	os.write_file(log_path, '${header}exit: ${result.exit_code}\n${result.output}') or { panic(err) }
	eprintln('[interface callbacks] ${stage}: exit ${result.exit_code}')
	if result.exit_code != 0 {
		eprintln(result.output)
	}
	return result
}
