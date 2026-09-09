import os

const ownership_toml_vexe = @VEXE
const ownership_toml_tests_dir = os.dir(@FILE)
const ownership_toml_v3_dir = os.dir(ownership_toml_tests_dir)
const ownership_toml_vlib_dir = os.dir(ownership_toml_v3_dir)
const ownership_toml_v3_src = os.join_path(ownership_toml_v3_dir, 'v3.v')

fn test_toml_decode_with_ownership() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ownership_toml_${os.getpid()}')
	source := os.join_path(os.temp_dir(), 'v3_ownership_toml_${os.getpid()}.v')
	os.rm(v3_bin) or {}
	defer {
		os.rm(v3_bin) or {}
		os.rm(source) or {}
	}
	build := os.execute('${ownership_toml_vexe} -nocache -gc none -d ownership -path "${ownership_toml_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${ownership_toml_v3_src}')
	assert build.exit_code == 0, build.output
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
	for mode in ['-no-parallel', ''] {
		out := os.execute('${v3_bin} -nocache -ownership -d ownership ${mode} run ${source}')
		assert out.exit_code == 0, out.output
	}
}
