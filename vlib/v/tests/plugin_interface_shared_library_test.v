import dl
import os
import rand

const vexe = @VEXE

fn test_interface_from_shared_library_can_call_methods() {
	if os.user_os() != 'linux' {
		return
	}
	workdir := os.join_path(os.vtmp_dir(), 'v_plugin_interface_shared_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	lib_src := os.join_path(workdir, 'plugin.v')
	host_src := os.join_path(workdir, 'host.v')
	lib_bin := os.join_path(workdir, 'plugin')
	lib_so := os.join_path(workdir, 'plugin${dl.dl_ext}')
	lib_path := lib_so.replace('\\', '\\\\')
	library_code := [
		'module main',
		'',
		'pub interface Plugin {',
		'\tprint_msg()',
		'}',
		'',
		'pub struct MyPlugin {}',
		'',
		'pub fn (p MyPlugin) print_msg() {',
		"\tprintln('Hello, World!')",
		'}',
		'',
		"@[export: 'create_plugin']",
		'pub fn create_plugin() Plugin {',
		'\treturn MyPlugin{}',
		'}',
	].join('\n')
	host_code := [
		'module main',
		'',
		'import dl.loader',
		'',
		'pub interface Plugin {',
		'\tprint_msg()',
		'}',
		'',
		'type CreatePlugin = fn () Plugin',
		'',
		"const lib_path = '${lib_path}'",
		'',
		'fn main() {',
		'\tmut dl_loader := loader.get_or_create_dynamic_lib_loader(',
		'\t\tkey: lib_path',
		'\t\tpaths: [lib_path]',
		'\t) or { panic(err) }',
		'\tdefer {',
		'\t\tdl_loader.unregister()',
		'\t}',
		"\tcreate_plugin_sym := dl_loader.get_sym('create_plugin') or { panic(err) }",
		'\tcreate_plugin := CreatePlugin(create_plugin_sym)',
		'\tplugin := create_plugin()',
		'\tplugin.print_msg()',
		'}',
	].join('\n')
	os.write_file(lib_src, library_code) or { panic(err) }
	os.write_file(host_src, host_code) or { panic(err) }
	compile_lib_cmd := '${os.quoted_path(vexe)} -d no_backtrace -shared -o ${os.quoted_path(lib_bin)} ${os.quoted_path(lib_src)}'
	run_cmd(compile_lib_cmd) or { panic(err) }
	run_host_cmd := '${os.quoted_path(vexe)} -d no_backtrace run ${os.quoted_path(host_src)}'
	res := run_cmd(run_host_cmd) or { panic(err) }
	assert res.output.contains('Hello, World!')
}

fn run_cmd(cmd string) !os.Result {
	res := os.execute(cmd)
	if res.exit_code != 0 {
		return error('command failed:\n${cmd}\n${res.output}')
	}
	return res
}
