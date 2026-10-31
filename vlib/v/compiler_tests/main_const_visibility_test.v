module main

import os

// The consts of main share their bare names with the consts of builtin, but
// they are main's own: another module sees only its own consts, the ones it
// imports or qualifies, and the builtin ones, as with V 0.5.2.

fn write_project(name string, main_source string, lib_source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_main_const_visibility_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'constlib')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), main_source) or { panic(err) }
	os.write_file(os.join_path(dir, 'constlib', 'constlib.v'), lib_source) or { panic(err) }
	return dir
}

// error_lines returns `file:line:col: message` for every error of a check output.
fn error_lines(output string) []string {
	mut lines := []string{}
	for line in output.split_into_lines() {
		if !line.contains(': error: ') {
			continue
		}
		location := line.all_before(': error: ').split(':')
		if location.len < 3 {
			continue
		}
		file := os.file_name(location[location.len - 3])
		lines << '${file}:${location[location.len - 2]}:${location[location.len - 1]}: ${line.all_after(': error: ')}'
	}
	return lines
}

fn test_another_module_does_not_see_the_consts_of_main() {
	dir := write_project('leak', "module main\n\nimport constlib\n\nconst routes = ['/', '/about']\n\nfn main() {\n\tprintln(routes)\n\tprintln(constlib.count())\n\tprintln(constlib.count_qualified())\n\tprintln(constlib.count_main())\n}\n",
		'module constlib\n\npub fn count() int {\n\treturn routes.len\n}\n\npub fn count_qualified() int {\n\treturn constlib.routes.len\n}\n\npub fn count_main() int {\n\treturn main.routes.len\n}\n')
	defer {
		os.rmdir_all(dir) or {}
	}
	res := os.exec([@VEXE, '-new-compiler', '-check', '-nocolor', dir])
	assert res.exit_code == 1, res.output
	errors := error_lines(res.output)
	assert errors.len == 3, res.output
	assert errors[0] == 'constlib.v:4:9: undefined ident: `routes`', res.output
	assert errors[1] == 'constlib.v:8:9: undefined ident: `constlib`', res.output
	assert errors[2].starts_with('constlib.v:12:'), res.output
}

fn test_another_module_sees_a_builtin_const_that_main_shadows() {
	dir := write_project('shadow', "module main\n\nimport constlib\n\nconst max_u8 = 'the main const'\n\nfn main() {\n\tprintln(max_u8)\n\tprintln(constlib.largest())\n\tprintln(constlib.own())\n}\n",
		'module constlib\n\nconst own_value = 3\n\npub fn largest() u8 {\n\treturn max_u8\n}\n\npub fn own() int {\n\treturn own_value\n}\n')
	defer {
		os.rmdir_all(dir) or {}
	}
	exe := os.join_path(dir, 'shadow')
	res := os.exec([@VEXE, '-new-compiler', '-nocolor', '-o', exe, dir])
	assert res.exit_code == 0, res.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['the main const', '255', '3']
}
