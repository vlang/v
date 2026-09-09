import os

const ownership_optional_copy_vexe = @VEXE
const ownership_optional_copy_tests_dir = os.dir(@FILE)
const ownership_optional_copy_v3_dir = os.dir(ownership_optional_copy_tests_dir)
const ownership_optional_copy_vlib_dir = os.dir(ownership_optional_copy_v3_dir)
const ownership_optional_copy_v3_src = os.join_path(ownership_optional_copy_v3_dir, 'v3.v')

fn test_copyable_optional_can_be_read_and_assigned_repeatedly() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ownership_optional_copy_test')
	build :=
		os.execute('${ownership_optional_copy_vexe} -gc none -d ownership -path "${ownership_optional_copy_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${ownership_optional_copy_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_ownership_optional_copy_input.v')
	os.write_file(source,
		"struct Cursor {\nmut:\n\tprevious ?rune\n}\n\nfn (mut cursor Cursor) replace(value ?rune) ?rune {\n\told := cursor.previous\n\tcursor.previous = value\n\treturn old\n}\n\nfn main() {\n\tmut cursor := Cursor{previous: rune(`a`)}\n\tfirst := cursor.previous\n\tassert first? == `a`\n\tassert cursor.previous? == `a`\n\tassert cursor.replace(rune(`b`))? == `a`\n\tassert cursor.previous? == `b`\n\tprintln('ok')\n}\n") or {
		panic(err)
	}
	out := os.execute('${v3_bin} -ownership -d ownership -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
