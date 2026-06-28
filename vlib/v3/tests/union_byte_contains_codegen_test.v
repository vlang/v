import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// build_v3 builds v3 data for v3 tests.
fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_union_byte_contains_codegen_test')
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

// test_union_aliasing_and_byte_array_contains_codegen validates this v3 regression case.
fn test_union_aliasing_and_byte_array_contains_codegen() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'union_byte_contains_codegen_input',
		"union Bits {\nmut:\n\tf f64\n\tu u64\n}\n\nfn main() {\n\tmut u := Bits{}\n\tu.f = 12.5\n\tif u.u != u64(0) {\n\t\tprintln('union ok')\n\t} else {\n\t\tprintln('union bad')\n\t}\n\tmut bytes := []u8{len: 4, init: 0}\n\tbytes[1] = `.`\n\tif `.` in bytes {\n\t\tprintln('byte contains ok')\n\t} else {\n\t\tprintln('byte contains bad')\n\t}\n\tprintln(12.5.str())\n}\n")
	assert out == 'union ok\nbyte contains ok\n12.5'
}
