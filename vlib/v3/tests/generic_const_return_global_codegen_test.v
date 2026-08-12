import os

const generic_const_global_vexe = @VEXE
const generic_const_global_tests_dir = os.dir(@FILE)
const generic_const_global_v3_dir = os.dir(generic_const_global_tests_dir)
const generic_const_global_vlib_dir = os.dir(generic_const_global_v3_dir)
const generic_const_global_v3_src = os.join_path(generic_const_global_v3_dir, 'v3.v')

fn test_inferred_generic_const_return_has_concrete_global_type() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_const_global_test')
	build :=
		os.execute('${generic_const_global_vexe} -gc none -path "${generic_const_global_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_const_global_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_generic_const_global_input.v')
	os.write_file(source,
		"import sync.stdatomic\n\nconst flag = stdatomic.new_atomic(false)\nconst number = stdatomic.new_atomic(7)\n\nfn main() {\n\tassert !flag.load()\n\tassert number.load() == 7\n\tprintln('ok')\n}\n")!
	out := os.execute('${v3_bin} -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
