import os
import v.tempname

fn for_in_review_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_for_in_review_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn for_in_review_run_good(name string, src string) string {
	root := for_in_review_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	out := os.join_path(root, 'v3_${name}')
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.exec([out])
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn for_in_review_run_bad(name string, src string, expected string) {
	root := for_in_review_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	out := os.join_path(root, 'v3_${name}')
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	result := os.exec([@VEXE, src_path, '-b', 'c', '-o', '${out}'])
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: reached C compilation\n${result.output}'
}

fn test_pointer_conditions_reject_plain_if_and_for() {
	for_in_review_run_bad('pointer_if_condition',
		'fn main() {\n\tx := 1\n\tp := &x\n\tif p {\n\t\tprintln("bad")\n\t}\n}\n',
		'non-bool type `&int` used as if condition')
	for_in_review_run_bad('pointer_for_condition',
		'fn main() {\n\tx := 1\n\tp := &x\n\tfor p {\n\t\tbreak\n\t}\n}\n',
		'if condition must be `bool`, not `&int`')
}

fn test_c_style_for_accepts_pointer_condition() {
	out := for_in_review_run_good('c_style_pointer_condition',
		'fn main() {\n\tmut x := 1\n\tmut p := &x\n\tmut count := 0\n\tfor ; p; p = unsafe { nil } {\n\t\tcount++\n\t}\n\tprintln(count)\n}\n')
	assert out == '1'
}

fn test_optional_array_for_in_skips_none_payload() {
	out := for_in_review_run_good('optional_array_for_in_guard',
		'fn maybe_values(ok bool) ?[]int {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [1, 2, 3]\n}\n\nfn main() {\n\tmut total := 0\n\tfor value in maybe_values(false) {\n\t\ttotal += value\n\t}\n\tprintln(int_str(total))\n\tfor value in maybe_values(true) {\n\t\ttotal += value\n\t}\n\tprintln(int_str(total))\n}\n')
	assert out == '0\n6'
}

fn test_optional_map_for_in_is_rejected_before_codegen() {
	for_in_review_run_bad('optional_map_for_in',
		'fn maybe_values() ?map[string]int {\n\treturn none\n}\n\nfn main() {\n\tfor key, value in maybe_values() {\n\t\tprintln(key + int_str(value))\n\t}\n}\n',
		'for in: cannot index `?map[string]int`')
}

fn test_values_copied_from_temporary_map_remain_valid() {
	out := for_in_review_run_good('temporary_map_for_in_value_lifetime',
		"fn entries(lang string) map[string]string {\n\treturn {'message': lang.repeat(64)}\n}\n\nfn main() {\n\tmut translations := map[string]map[string]string{}\n\tfor lang in ['en', 'fr', 'de', 'es'] {\n\t\tfor key, value in entries(lang) {\n\t\t\ttranslations[lang][key] = value\n\t\t}\n\t}\n\tprintln(translations['en']['message'] == 'en'.repeat(64))\n}\n")
	assert out == 'true'
}

fn test_custom_iterator_pointer_for_in_uses_single_indirection() {
	out := for_in_review_run_good('custom_iterator_pointer',
		'struct Counter {\nmut:\n\tn int\n}\n\nfn (mut c Counter) next() ?int {\n\tif c.n >= 3 {\n\t\treturn none\n\t}\n\tc.n++\n\treturn c.n\n}\n\nfn main() {\n\tmut c := Counter{}\n\tfor x in &c {\n\t\tprintln(x)\n\t}\n\tprintln("count: \${c.n}")\n}\n')
	assert out == '1\n2\n3\ncount: 3'
}
