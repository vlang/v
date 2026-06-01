// vtest build: macos
module builder

import os

fn run_parallel_type_check_fixture(label string, code string) (int, string) {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_parallel_type_check_${label}_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	helper_path := os.join_path(tmp_dir, 'helper.v')
	os.write_file(source_path, code) or { panic('cannot write fixture source') }
	os.write_file(helper_path, 'import v2.builder
import v2.pref

fn main() {
	prefs := &pref.Preferences{
		backend:  .v
		no_cache: true
	}
	mut b := builder.new_builder(prefs)
	b.build(["${source_path}"])
}
') or {
		panic('cannot write helper source')
	}
	cmd := '${os.quoted_path(@VEXE)} -d parallel -gc none run ${os.quoted_path(helper_path)} 2>&1'
	res := os.execute(cmd)
	return res.exit_code, res.output
}

fn test_parallel_type_check_runs_struct_field_default_checks() {
	code, output := run_parallel_type_check_fixture('struct_default', 'module main

struct Config {
	value int = missing_default()
}

fn main() {}
')
	assert code != 0, output
	assert output.contains('missing_default'), output
}

fn test_parallel_type_check_runs_enum_value_checks() {
	code, output := run_parallel_type_check_fixture('enum_value', 'module main

enum Mode {
	ready = missing_enum_value()
}

fn main() {}
')
	assert code != 0, output
	assert output.contains('missing_enum_value'), output
}
