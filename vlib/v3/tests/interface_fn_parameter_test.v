import os

const interface_fn_tests_dir = os.dir(@FILE)
const interface_fn_v3_dir = os.dir(interface_fn_tests_dir)
const interface_fn_vlib_dir = os.dir(interface_fn_v3_dir)
const interface_fn_v3_src = os.join_path(interface_fn_v3_dir, 'v3.v')
const interface_fn_v3_bin = os.join_path(os.temp_dir(), 'v3_interface_fn_${os.getpid()}')

fn build_interface_fn_v3() string {
	if os.is_executable(interface_fn_v3_bin) {
		return interface_fn_v3_bin
	}
	build :=
		os.execute('${os.quoted_path(@VEXE)} -gc none -path "${interface_fn_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(interface_fn_v3_bin)} ${os.quoted_path(interface_fn_v3_src)}')
	assert build.exit_code == 0, build.output
	return interface_fn_v3_bin
}

fn write_interface_fn_project() string {
	root := os.join_path(os.temp_dir(), 'v3_interface_fn_project_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'core')) or { panic(err) }
	os.write_file(os.join_path(root, 'core', 'core.v'),
		'module core\n\npub interface EventData {}\n\npub interface Connector {\n\tconnect(handler fn (EventData))\n}\n\npub fn use_connector(connector Connector) {\n\tconnector.connect(fn (event EventData) {})\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'),
		'module main\n\nimport core\n\nstruct App {}\n\nfn (app App) connect(handler fn (core.EventData)) {}\n\nfn main() {\n\tcore.use_connector(App{})\n\tprint("ok")\n}\n') or {
		panic(err)
	}
	return root
}

// https://github.com/vlang/v/issues/28042
fn test_interface_method_fn_parameter_uses_c_typedef() {
	v3_bin := build_interface_fn_v3()
	root := write_interface_fn_project()
	defer {
		os.rm(v3_bin) or {}
		os.rmdir_all(root) or {}
	}
	output := os.join_path(root, 'interface_fn_parameter')
	compile := os.execute('${os.quoted_path(v3_bin)} -nocache -b c -o ${os.quoted_path(output)} ${os.quoted_path(os.join_path(root,
		'main.v'))}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(os.quoted_path(output))
	assert run.exit_code == 0, run.output
	assert run.output == 'ok'
}
