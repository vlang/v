import os
import v.tempname

fn run_if_guard_selector_case(root string, name string, source string) string {
	bin := os.join_path(root, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// test_if_guard_selector_keeps_payload_type validates this v3 regression case.
fn test_if_guard_selector_keeps_payload_type() {
	root := os.join_path(os.vtmp_dir(), 'v3_if_guard_selector_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	if_guard_out := run_if_guard_selector_case(root, 'if_guard_selector_input',
		"struct Info {\n\tname string\n}\n\nfn maybe_info() ?Info {\n\treturn Info{name: 'abc'}\n}\n\nfn main() {\n\tif info := maybe_info() {\n\t\tname := info.name\n\t\tprintln(name)\n\t}\n}\n")
	assert if_guard_out == 'abc'

	map_for_out := run_if_guard_selector_case(root, 'map_for_in_input',
		"fn main() {\n\tmut m := map[string]int{}\n\tm['a'] = 1\n\tfor name, _ in m {\n\t\tprintln(name)\n\t}\n}\n")
	assert map_for_out == 'a'
}
