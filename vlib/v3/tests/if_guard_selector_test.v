import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// build_v3 builds v3 data for v3 tests.
fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_if_guard_selector_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0
	return v3_bin
}

// run_good supports run good handling for v3 tests.
fn run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0
	assert !compile.output.contains('C compilation failed')

	run := os.execute(bin)
	assert run.exit_code == 0
	return run.output.trim_space()
}

// test_if_guard_selector_keeps_payload_type validates this v3 regression case.
fn test_if_guard_selector_keeps_payload_type() {
	v3_bin := build_v3()
	if_guard_out := run_good(v3_bin, 'if_guard_selector_input',
		"struct Info {\n\tname string\n}\n\nfn maybe_info() ?Info {\n\treturn Info{name: 'abc'}\n}\n\nfn main() {\n\tif info := maybe_info() {\n\t\tname := info.name\n\t\tprintln(name)\n\t}\n}\n")
	assert if_guard_out == 'abc'

	map_for_out := run_good(v3_bin, 'map_for_in_input',
		"fn main() {\n\tmut m := map[string]int{}\n\tm['a'] = 1\n\tfor name, _ in m {\n\t\tprintln(name)\n\t}\n}\n")
	assert map_for_out == 'a'
}
