import os
import v.cmdexec
import v.tempname

fn test_toml_decode_with_ownership() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_ownership_toml_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_ownership_toml.v')
	os.write_file(source, r'
module main

import toml

struct CommonStruct {
	field string
}

fn main() {
	doc := toml.parse_text("field = \"some\"")!
	decoded := doc.decode[CommonStruct]()!
	assert decoded.field == "some"
}
')!
	for mode in ['-no-parallel', '', '-autofree -no-parallel', '-autofree'] {
		out := cmdexec.run_in_merged_with_env(compiler, ['-nocache', '-ownership', '-d', 'ownership',
			...(os.split_args(mode) or { panic(err) }), 'run', source], '', {
			'VFLAGS':               ''
			'VTOOLS_CACHE_VERBOSE': ''
		})
		assert out.exit_code == 0, out.output
	}
}
