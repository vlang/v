import os

const owned_array_mutation_vexe = @VEXE
const owned_array_mutation_tests_dir = os.dir(@FILE)
const owned_array_mutation_v3_dir = os.dir(owned_array_mutation_tests_dir)
const owned_array_mutation_vlib_dir = os.dir(owned_array_mutation_v3_dir)
const owned_array_mutation_v3_src = os.join_path(owned_array_mutation_v3_dir, 'v3.v')

fn test_owned_array_mutations_keep_generated_drop_loops_well_formed() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_owned_array_mutation_test')
	build :=
		os.execute('${owned_array_mutation_vexe} -gc none -d ownership -path "${owned_array_mutation_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${owned_array_mutation_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_owned_array_mutation_input.v')
	os.write_file(source,
		"fn main() {\n\tmut values := ['a', 'b', 'c']\n\tvalues.delete(1)\n\tassert values == ['a', 'c']\n\tvalues.clear()\n\tassert values.len == 0\n\tvalues = ['last']\n\tvalues.free()\n\tassert values.len == 0\n\tmut nested := [['owned']]\n\tnested.pop()\n\tassert nested.len == 0\n\tprintln('ok')\n}\n")!
	out := os.execute('${v3_bin} -ownership -d ownership -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
