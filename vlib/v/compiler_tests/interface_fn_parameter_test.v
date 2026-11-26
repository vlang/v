import os
import v.tempname

// https://github.com/vlang/v/issues/28042
fn test_interface_method_fn_parameter_uses_c_typedef() {
	root := os.join_path(os.vtmp_dir(), 'v3_interface_fn_project_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir(os.join_path(root, 'core'))!
	os.write_file(os.join_path(root, 'core', 'core.v'),
		'module core\n\npub interface EventData {}\n\npub interface Connector {\n\tconnect(handler fn (EventData))\n}\n\npub fn use_connector(connector Connector) {\n\tconnector.connect(fn (event EventData) {})\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'),
		'module main\n\nimport core\n\nstruct App {}\n\nfn (app App) connect(handler fn (core.EventData)) {}\n\nfn main() {\n\tcore.use_connector(App{})\n\tprint("ok")\n}\n') or {
		panic(err)
	}
	output := os.join_path(root, 'interface_fn_parameter')
	compile := os.exec([@VEXE, '-nocache', '-b', 'c', '-o', output, os.join_path(root, 'main.v')])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output == 'ok'
}
