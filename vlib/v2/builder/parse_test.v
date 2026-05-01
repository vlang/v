module builder

import os
import v2.pref

fn test_parse_files_keeps_single_file_inputs_isolated() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_single_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'hello.v')
	sibling_file := os.join_path(tmp_dir, 'extra.v')
	os.write_file(entry_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(sibling_file, 'module main\nfn extra() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := b.parse_files([entry_file])

	assert files.len == 1
}

fn test_parse_files_expands_directory_inputs() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_dir_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'hello.v')
	sibling_file := os.join_path(tmp_dir, 'extra.v')
	os.write_file(entry_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(sibling_file, 'module main\nfn extra() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := b.parse_files([tmp_dir])

	assert files.len == 2
}

fn test_parse_files_expands_non_main_module_files() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_module_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'math_test.v')
	sibling_file := os.join_path(tmp_dir, 'math.v')
	os.write_file(entry_file, 'module math\nconst x = twice(21)\n') or { panic(err) }
	os.write_file(sibling_file, 'module math\nfn twice(v int) int { return v * 2 }\n') or {
		panic(err)
	}

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := b.parse_files([entry_file])

	assert files.len == 2
}
