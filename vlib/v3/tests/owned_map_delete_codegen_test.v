import os

const owned_map_delete_vexe = @VEXE
const owned_map_delete_tests_dir = os.dir(@FILE)
const owned_map_delete_v3_dir = os.dir(owned_map_delete_tests_dir)
const owned_map_delete_vlib_dir = os.dir(owned_map_delete_v3_dir)
const owned_map_delete_v3_src = os.join_path(owned_map_delete_v3_dir, 'v3.v')

fn test_owned_map_delete_snapshots_and_drops_the_stored_value() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_owned_map_delete_test')
	build :=
		os.execute('${owned_map_delete_vexe} -gc none -d ownership -path "${owned_map_delete_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${owned_map_delete_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_owned_map_delete_input.v')
	os.write_file(source,
		"fn main() {\n\tmut values := map[string][]string{}\n\tvalues['letters'] = ['a', 'b']\n\tvalues.delete('letters')\n\tassert values.len == 0\n\tprintln('ok')\n}\n")!
	out := os.execute('${v3_bin} -ownership -d ownership -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
