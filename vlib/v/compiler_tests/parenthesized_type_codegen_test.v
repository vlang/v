import os
import v.tempname

fn paren_type_run_good(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
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

// A single type in parentheses is a grouped type, not a one-element
// multi-return. Keeping the parentheses in the type text made the checker look
// for a type literally named `([]u64)`, so the option unwrapped to something
// with neither `len` nor an index.
fn test_option_returning_parenthesized_array_is_the_array_type() {
	out := paren_type_run_good('paren_option_array', 'fn give() ?([]u64) {\n\treturn [u64(1), 2]\n}\n\nfn main() {\n\txs := give() or { return }\n\tprintln(int_str(xs.len))\n\tprintln(u64__str(xs[0]))\n}\n')
	assert out == '2\n1'
}

fn test_result_returning_parenthesized_array_is_the_array_type() {
	out := paren_type_run_good('paren_result_array', "fn give() !([]string) {\n\treturn ['a', 'b', 'c']\n}\n\nfn main() {\n\txs := give() or { return }\n\tprintln(int_str(xs.len))\n\tprintln(xs[2])\n}\n")
	assert out == '3\nc'
}

fn test_parenthesized_plain_return_type_is_unwrapped() {
	out := paren_type_run_good('paren_plain_return', 'fn give() (int) {\n\treturn 7\n}\n\nfn main() {\n\tprintln(int_str(give() + 1))\n}\n')
	assert out == '8'
}

// The parentheses still mean a multi-return when there is more than one type.
fn test_multi_return_still_works() {
	out := paren_type_run_good('paren_multi_return', "fn give() (int, string) {\n\treturn 2, 'two'\n}\n\nfn main() {\n\tn, s := give()\n\tprintln(int_str(n))\n\tprintln(s)\n}\n")
	assert out == '2\ntwo'
}
