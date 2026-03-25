import os

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const vexe = @VEXE

fn temp_output_path(name string) string {
	$if windows {
		return os.join_path(os.vtmp_dir(), '${name}.exe')
	}
	return os.join_path(os.vtmp_dir(), name)
}

fn project_path() string {
	return os.join_path(os.vtmp_dir(), 'local_module_submodules')
}

fn write_local_module_submodules_project() {
	basepath := project_path()
	os.rmdir_all(basepath) or {}
	os.mkdir_all(os.join_path(basepath, 'confy', 'marshalers')) or { panic(err) }
	os.write_file(os.join_path(basepath, 'main.v'),
		['module main', '', 'import confy', 'import confy.marshalers', '', 'fn main() {', '\tmut marshaler := marshalers.JSON{}', '\tload(mut marshaler)', '}', '', 'fn load(mut marshaler confy.Marshaler) {', '\tmarshaler.load()', '}'].join('\n') +
		'\n') or { panic(err) }
	os.write_file(os.join_path(basepath, 'confy', 'confy.v'),
		['module confy', '', 'pub interface Marshaler {', '\tmut:', '\t\tload()', '}'].join('\n') +
		'\n') or { panic(err) }
	os.write_file(os.join_path(basepath, 'confy', 'marshalers', 'json.v'),
		['module marshalers', '', 'pub struct JSON {}', '', 'pub fn (mut j JSON) load() {}'].join('\n') +
		'\n') or { panic(err) }
}

fn compile_local_module_submodules(target string, out_name string) os.Result {
	basepath := project_path()
	write_local_module_submodules_project()
	old_wd := os.getwd()
	os.chdir(basepath) or { panic(err) }
	defer {
		os.chdir(old_wd) or {}
		os.rmdir_all(basepath) or {}
	}
	return os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(out_name)} ${os.quoted_path(target)}')
}

fn test_local_module_submodules_compile_from_main_file() {
	out_name := temp_output_path('local_module_submodules_main')
	os.rm(out_name) or {}
	defer {
		os.rm(out_name) or {}
	}
	res := compile_local_module_submodules('main.v', out_name)
	assert res.exit_code == 0, res.output
}

fn test_local_module_submodules_compile_from_project_dir() {
	out_name := temp_output_path('local_module_submodules_dir')
	os.rm(out_name) or {}
	defer {
		os.rm(out_name) or {}
	}
	res := compile_local_module_submodules('.', out_name)
	assert res.exit_code == 0, res.output
}
