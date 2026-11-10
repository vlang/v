module wit

import os

fn preview2_case(name string, wit_file string, must_contain []string) {
	src := os.read_file(wit_file) or { panic(err) }
	doc := parse(src) or { panic(err) }
	assert doc.interfaces.len > 0, 'expected interfaces in ${wit_file}'
	gen := doc.to_v()
	assert gen.len > 0
	path := os.join_path(os.vtmp_dir(), 'wit_preview2_${name}.v')
	os.write_file(path, gen) or { panic(err) }
	res := os.exec(['v', '-check', path])
	assert res.exit_code == 0, res.output
	for s in must_contain {
		assert gen.contains(s), 'generated ${name} bindings must contain ${s}'
	}
}

fn test_preview2_io() {
	preview2_case('io', 'vlib/wasm/wit/testdata/wasi/io_streams.wit', [
		'pub struct Input_stream',
		'(mut self Input_stream) read',
	])
}

fn test_preview2_clocks() {
	preview2_case('clocks', 'vlib/wasm/wit/testdata/wasi/clocks_wall.wit', [
		'pub struct Datetime',
		'pub fn now',
	])
}

fn test_preview2_random() {
	preview2_case('random', 'vlib/wasm/wit/testdata/wasi/random.wit', [
		'pub fn get_random_bytes',
	])
}

fn test_preview2_cli() {
	src := os.read_file('vlib/wasm/wit/testdata/wasi/cli_command.wit') or { panic(err) }
	doc := parse(src) or { panic(err) }
	assert doc.worlds.len == 1
	assert doc.worlds[0].name == 'command'
	gen := doc.to_v()
	path := os.join_path(os.vtmp_dir(), 'wit_preview2_cli.v')
	os.write_file(path, gen) or { panic(err) }
	res := os.exec(['v', '-check', path])
	assert res.exit_code == 0, res.output
}

fn test_preview2_filesystem() {
	preview2_case('filesystem', 'vlib/wasm/wit/testdata/wasi/fs_types.wit', [
		'pub struct Descriptor',
		'(mut self Descriptor) read_via_stream',
	])
}
