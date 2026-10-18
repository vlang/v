module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_optional_pointer_casts_and_propagated_binary_operands() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_optional_pointer_prefix_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'models')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "optional_pointer_prefix" }') or { panic(err) }
	os.write_file(os.join_path(root, 'models', 'models.v'), 'module models\npub struct Record { pub: value int }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import models
fn read() !int { return 20 }
fn maybe() ?int { return 10 }
fn next() ?int { return maybe()? + 1 }
fn text(ok bool) ?string { if !ok { return none }; return "value" }
fn result_text(ok bool) !string { if !ok { return error("missing") }; return "result" }
fn optional_concat(ok bool) ?string { return "map[" + text(ok)? + "]" + text(true)? }
fn result_concat(ok bool) !string { return "map[" + result_text(ok)! + "]" + result_text(true)! }
fn arithmetic() ?int { return maybe()? + maybe()? * 2 }
fn identity(value string) string { return value }
fn concat_argument() ?string { return identity("[" + text(true)?) }
fn main() {
	mut pointer := ?&models.Record(none)
	if _ := pointer { assert false }
	pointer = &models.Record{ value: read()! + 1 }
	if record := pointer { assert record.value == 21 } else { assert false }
	mut local := ?&int(none)
	if _ := local { assert false }
	assert read()! & 7 == 4
	assert (next() or { 0 }) == 11
	assert (optional_concat(true) or { "bad" }) == "map[value]value"
	assert (optional_concat(false) or { "none" }) == "none"
	assert (result_concat(true) or { "bad" }) == "map[result]result"
	assert (result_concat(false) or { "error" }) == "error"
	assert (arithmetic() or { 0 }) == 30
	assert (concat_argument() or { "bad" }) == "[value"
	println("optional pointer prefixes work")
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'optional pointer prefixes work', run.output
}
