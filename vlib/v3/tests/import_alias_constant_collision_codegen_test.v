import os

const import_alias_collision_vexe = @VEXE
const import_alias_collision_tests_dir = os.dir(@FILE)
const import_alias_collision_v3_dir = os.dir(import_alias_collision_tests_dir)
const import_alias_collision_vlib_dir = os.dir(import_alias_collision_v3_dir)
const import_alias_collision_v3_src = os.join_path(import_alias_collision_v3_dir, 'v3.v')

fn test_import_alias_wins_over_same_named_exported_constant_in_call_selector() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_import_alias_collision_test')
	build :=
		os.execute('${import_alias_collision_vexe} -gc none -path "${import_alias_collision_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${import_alias_collision_v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_import_alias_collision_input')
	mod_dir := os.join_path(root, 'registry')
	os.mkdir_all(mod_dir)!
	os.write_file(os.join_path(mod_dir, 'registry.v'),
		'module registry\n\npub const registry = [1, 2]\n\npub fn value() int {\n\treturn registry.len\n}\n')!
	os.write_file(os.join_path(root, 'main.v'),
		"module main\n\nimport registry\n\nfn main() {\n\tassert registry.value() == 2\n\tprintln('ok')\n}\n")!
	out := os.execute('cd "${root}" && ${v3_bin} -no-parallel run main.v')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
