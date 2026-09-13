import os

const paren_type_vexe = @VEXE
const paren_type_tests_dir = os.dir(@FILE)
const paren_type_v3_dir = os.dir(paren_type_tests_dir)
const paren_type_vlib_dir = os.dir(paren_type_v3_dir)
const paren_type_v3_src = os.join_path(paren_type_v3_dir, 'v.v')

fn paren_type_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_parenthesized_type_codegen_test')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${paren_type_vexe} -gc none -path "${paren_type_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${paren_type_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn paren_type_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A single type in parentheses is a grouped type, not a one-element
// multi-return. Keeping the parentheses in the type text made the checker look
// for a type literally named `([]u64)`, so the option unwrapped to something
// with neither `len` nor an index.
fn test_option_returning_parenthesized_array_is_the_array_type() {
	v3_bin := paren_type_build_v3()
	out := paren_type_run_good(v3_bin, 'paren_option_array', 'fn give() ?([]u64) {\n\treturn [u64(1), 2]\n}\n\nfn main() {\n\txs := give() or { return }\n\tprintln(int_str(xs.len))\n\tprintln(u64__str(xs[0]))\n}\n')
	assert out == '2\n1'
}

fn test_result_returning_parenthesized_array_is_the_array_type() {
	v3_bin := paren_type_build_v3()
	out := paren_type_run_good(v3_bin, 'paren_result_array', "fn give() !([]string) {\n\treturn ['a', 'b', 'c']\n}\n\nfn main() {\n\txs := give() or { return }\n\tprintln(int_str(xs.len))\n\tprintln(xs[2])\n}\n")
	assert out == '3\nc'
}

fn test_parenthesized_plain_return_type_is_unwrapped() {
	v3_bin := paren_type_build_v3()
	out := paren_type_run_good(v3_bin, 'paren_plain_return', 'fn give() (int) {\n\treturn 7\n}\n\nfn main() {\n\tprintln(int_str(give() + 1))\n}\n')
	assert out == '8'
}

// The parentheses still mean a multi-return when there is more than one type.
fn test_multi_return_still_works() {
	v3_bin := paren_type_build_v3()
	out := paren_type_run_good(v3_bin, 'paren_multi_return', "fn give() (int, string) {\n\treturn 2, 'two'\n}\n\nfn main() {\n\tn, s := give()\n\tprintln(int_str(n))\n\tprintln(s)\n}\n")
	assert out == '2\ntwo'
}
