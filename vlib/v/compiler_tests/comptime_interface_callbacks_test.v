import os

const interface_callbacks_vexe = @VEXE
const interface_callbacks_v_dir = os.dir(os.dir(@FILE))
const interface_callbacks_vlib_dir = os.dir(interface_callbacks_v_dir)
const interface_callbacks_v_src = os.join_path(interface_callbacks_v_dir, 'v.v')

struct InterfaceCallbackCase {
	name     string
	source   string
	expected string
}

fn test_comptime_interface_callbacks_without_direct_call_warmup() {
	work_dir := os.join_path(os.temp_dir(), 'v_interface_callbacks_${os.getpid()}')
	os.mkdir_all(work_dir) or { panic(err) }
	defer {
		os.rmdir_all(work_dir) or {}
	}
	compiler := os.join_path(work_dir, 'v3')
	// Build and invoke the new compiler directly. A fallback to the old compiler
	// must not turn a failure of the reflected callback into a passing test.
	build := os.execute('"${interface_callbacks_vexe}" -gc none -d ownership -path "${interface_callbacks_vlib_dir}|@vlib|@vmodules" -o "${compiler}" "${interface_callbacks_v_src}"')
	assert build.exit_code == 0, build.output

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
		for mode in ['serial', 'parallel'] {
			flags := if mode == 'serial' { '-no-parallel' } else { '' }
			output := os.join_path(project, 'program_${mode}')
			compile := os.execute('"${compiler}" -nocache ${flags} -o "${output}" "${main_file}"')
			if compile.exit_code != 0 {
				failures << '${case.name}/${mode}: compilation failed\n${compile.output}'
				continue
			}
			run := os.execute('"${output}"')
			actual := run.output.replace('\r\n', '\n').trim_space()
			if run.exit_code != 0 || actual != case.expected {
				failures << '${case.name}/${mode}: exit ${run.exit_code}\nexpected:\n${case.expected}\nactual:\n${actual}'
			}
		}
	}
	// Exercise both failures even when auto-str fails first: the issue comments
	// also report a panic when invoking an interface method from the callback.
	assert failures.len == 0, failures.join('\n\n')
}
