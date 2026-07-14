import os

fn test_multi_return_with_enum_keeps_one_concrete_c_type() {
	dir := os.join_path(os.vtmp_dir(), 'v3_multi_return_enum_codegen')
	v3_bin := os.join_path(dir, 'v3')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'choice')) or { panic(err) }
	v3_dir := os.dir(os.dir(@FILE))
	vlib_dir := os.dir(v3_dir)
	build := os.execute('${os.quoted_path(@VEXE)} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(v3_dir,
		'v3.v'))}')
	assert build.exit_code == 0, build.output
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'multi_return_enum_codegen' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'choice', 'choice.v'),
		'module choice\n\nenum Kind as u8 { first second }\n\nenum Big as u64 {\n\tlarge = u64(1) << 40\n}\n\nfn choose() (Kind, bool, int) {\n\treturn .second, true, 7\n}\n\nfn choose_big() (Big, bool) {\n\treturn .large, true\n}\n\npub fn verify() bool {\n\tkind, ok, number := choose()\n\tbig, big_ok := choose_big()\n\treturn kind == .second && ok && number == 7 && big_ok && big == .large && u64(big) == (u64(1) << 40)\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'main.v'),
		'module main\n\nimport choice\n\nfn main() {\n\tassert choice.verify()\n}\n') or {
		panic(err)
	}
	program := os.join_path(dir, 'program')
	result := os.execute('${os.quoted_path(v3_bin)} -b c -o ${os.quoted_path(program)} ${os.quoted_path(os.join_path(dir,
		'main.v'))}')
	assert result.exit_code == 0, result.output
	run := os.execute(program)
	assert run.exit_code == 0, run.output
	os.rmdir_all(dir) or {}
}
