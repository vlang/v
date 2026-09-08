import os

const sentinel_vexe = @VEXE
const sentinel_tests_dir = os.dir(@FILE)
const sentinel_v3_dir = os.dir(sentinel_tests_dir)
const sentinel_vlib_dir = os.dir(sentinel_v3_dir)
const sentinel_v3_src = os.join_path(sentinel_v3_dir, 'v3.v')

fn test_ownership_generated_result_cleanup_roots_ierror_sentinels() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ownership_sentinel_test')
	build :=
		os.execute('${sentinel_vexe} -gc none -d ownership -path "${sentinel_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${sentinel_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_ownership_sentinel_input.v')
	os.write_file(source,
		"struct Node {\n\tvalue ?string\n\tchildren []Node\n}\n\nfn main() {\n\tnode := Node{\n\t\tvalue: 'kept'\n\t\tchildren: []Node{}\n\t}\n\tassert node.value or { '' } == 'kept'\n\tprintln('ok')\n}\n")!
	out :=
		os.execute('${v3_bin} -ownership -d ownership -no-parallel -macos-v3-compat-c99 run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output

	os.write_file(source,
		"fn main() {\n\t\$if ownership ? {\n\t\tprintln('ownership')\n\t} \$else {\n\t\tprintln('standard')\n\t}\n}\n")!
	autofree := os.execute('${v3_bin} -ownership -autofree -d ownership -no-parallel run ${source}')
	assert autofree.exit_code == 0, autofree.output
	assert autofree.output.contains('\nownership\n'), autofree.output

	os.write_file(source,
		"fn make_value() !string {\n\terror('discarded')\n\treturn 'ok'\n}\n\nfn main() {\n\tprintln(make_value() or { panic(err) })\n}\n")!
	discarded_error :=
		os.execute('${v3_bin} -ownership -autofree -d ownership -no-parallel run ${source}')
	assert discarded_error.exit_code == 0, discarded_error.output
	assert discarded_error.output.contains('\nok\n'), discarded_error.output
}
