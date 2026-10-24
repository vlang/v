module main

import os
import crypto.sha256
const compilers = ['gcc', 'msvc', 'tcc']
const runner_name = 'run_multiwindow_win32_w3_green.ps1'

fn workflow_sources(root string) map[string]string {
	mut m := map[string]string{}
	for cc in compilers {
		path := os.join_path(root, '.github', 'workflows', 'windows_ci_${cc}.yml')
		m[cc] = os.read_file(path) or { '' }
	}
	return m
}

fn validate_file_hash(path string, expected string) ! {
	data := os.read_file(path) or { return error('cannot read ${path}: ${err}') }
	actual := sha256.hexhash(data)
	if actual != expected {
		return error('${path}: expected=${expected} actual=${actual}')
	}
}

fn main() {
	root := os.join_path(@DIR, '..', '..', '..')
	runner_path := os.join_path(root, '.github', 'workflows', runner_name)
	runner := os.read_file(runner_path) or {
		eprintln('cannot read runner: ${err}')
		exit(1)
	}

	// Basic sanity: runner must define knownCompositeSha256
	assert runner.contains("knownCompositeSha256 = '"), 'missing composite pin'

	workflows := workflow_sources(root)
	for cc in compilers {
		wf := workflows[cc]
		assert wf.contains(runner_name), '${cc}: missing runner invocation'
		assert wf.contains('.\\makev.bat -${cc}'), '${cc}: missing makev build step'
	}

	// File hash exactness check (smoke)
	with_tmp := os.join_path(os.temp_dir(), 'wcci_hash_test')
	os.mkdir_all(with_tmp) or {}
	defer { os.rmdir_all(with_tmp) or {} }
	p := os.join_path(with_tmp, 'input.v')
	os.write_file(p, 'fn test_probe() {}\n') or {}
	expected := sha256.hexhash('fn test_probe() {}\n')
	validate_file_hash(p, expected) or { eprintln(err); exit(1) }
	os.write_file(p, 'fn test_probe() {}\r\n') or {}
	mut failed := false
	validate_file_hash(p, expected) or { failed = true }
	assert failed, 'modified file should fail hash check'

	println('windows ci static contract tests: OK')
}
