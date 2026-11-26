import os
import v.tempname

fn test_multi_return_with_enum_keeps_one_concrete_c_type() {
	dir := os.join_path(os.vtmp_dir(), 'v3_multi_return_enum_codegen_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	os.mkdir(os.join_path(dir, 'choice'))!
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
	result := os.exec([@VEXE, '-b', 'c', '-o', '${program}', os.join_path(dir, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([program])
	assert run.exit_code == 0, run.output
}
