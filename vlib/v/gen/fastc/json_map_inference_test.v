module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner

fn test_map_argument_inference_keeps_parameter_identity() {
	mut prefs := pref.new_preferences()
	g := Parser{ prefs: prefs, s: scanner.new_scanner(prefs, .normal) }
	source := FastcGenericMethodSource{ type_param: 'K,T', params_source: '(val map[K]T)', type_param_parameter_index: 0 }
	assert g.infer_mono_map_arguments(source, flat.source_type_id('Map_string_int')) == flat.source_type_id('string,int')
}

fn test_json_map_methods_infer_both_type_parameters() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_json_map_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import x.json2
struct Writer { mut: count int }
fn (mut writer Writer) record[A, B](values map[B]A) { writer.count = values.len }
fn main() {
	assert json2.encode({"answer": 42}) == "{\"answer\":42}"
	assert json2.encode({"nested": {"value": true}}) == "{\"nested\":{\"value\":true}}"
	mut writer := Writer{}
	writer.record({"one": 1, "two": 2})
	assert writer.count == 2
}
')!
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-cc', 'tcc', '-no-retry-compilation', '-nocache',
		'-o', program, root])
	assert built.exit_code == 0, built.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
