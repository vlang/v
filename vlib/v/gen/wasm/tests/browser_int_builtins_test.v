import os

// On the `-os browser` target, integer-to-string (`int.str()`), `println(int)`,
// string interpolation/concat of integers, and the `min_int`/`max_int` builtin
// constants used to fail — their implementations live in
// `vlib/builtin/wasm/int_notd_no_imports.v` / `string_notd_no_imports.v`, which
// were only compiled for `-os wasi`. They are now in the shared
// `vlib/builtin/wasm/` dir, so browser builds pick them up too.
fn test_wasm_browser_target_can_format_integers() {
	vexe := os.quoted_path(@VEXE)
	wrkdir := os.join_path(os.vtmp_dir(), 'wasm_browser_int_tests')
	os.mkdir_all(wrkdir)!
	defer {
		os.rmdir_all(wrkdir) or {}
	}

	source_path := os.join_path(wrkdir, 'ints.v')
	output_path := os.join_path(wrkdir, 'ints.wasm')
	source := [
		'pub fn main() {',
		'\tprintln(42)', // println(int) needs int.str()
		'\tn := 7',
		"\tprintln('value=' + n.str())", // explicit int.str() + string concat
		'\t_ = max_int', // min_int/max_int builtin consts
		'\t_ = min_int',
		'}',
	].join_lines()
	os.write_file(source_path, source)!

	res :=
		os.execute('${vexe} -b wasm -os browser -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, 'browser integer formatting failed to compile: ${res.output}'
	assert os.exists(output_path), 'missing browser wasm output'
}
