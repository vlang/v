import os
import v3.parser
import v3.pref
import v3.types

fn check_diagnostic_project(name string, files map[string]string, diagnostic_rels []string) []types.TypeError {
	root := os.join_path(os.temp_dir(), 'v3_checker_diagnostic_files_${name}_${os.getpid()}')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	mut paths := []string{}
	mut rels := files.keys()
	rels.sort()
	for rel in rels {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, files[rel]) or { panic(err) }
		paths << path
	}
	builtin_path := os.join_path(root, 'builtin/ierror.v')
	os.mkdir_all(os.dir(builtin_path)) or { panic(err) }
	os.write_file(builtin_path, 'module builtin

pub interface IError {
	msg() string
	code() int
}
') or {
		panic(err)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_file(builtin_path)
	p.a.user_code_start = p.a.nodes.len
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	for rel in diagnostic_rels {
		tc.diagnostic_files[os.join_path(root, rel)] = true
	}
	tc.check_semantics()
	return tc.errors.clone()
}

const invalid_ierror_return_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub fn fail() !int {
	return NotError{}
}
'
	'main.v':    'module main

import bad

fn main() {
	_ := 1
}
'
}

fn test_invalid_ierror_result_return_respects_diagnostic_files() {
	bad_errors := check_diagnostic_project('ierror_return_bad', invalid_ierror_return_project, [
		'bad/bad.v',
	])
	assert bad_errors.len == 1, bad_errors.str()
	assert bad_errors[0].msg.contains('bad.NotError')
	assert bad_errors[0].msg.contains('as `int`')

	main_errors := check_diagnostic_project('ierror_return_filtered',
		invalid_ierror_return_project, ['main.v'])
	assert main_errors.len == 0, main_errors.str()
}
