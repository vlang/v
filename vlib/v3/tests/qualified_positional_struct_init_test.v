import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn test_qualified_positional_struct_init_keeps_later_imported_fn() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_qualified_positional_struct_init_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_qualified_positional_struct_init_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	write_project_file(root, 'main.v', 'module main

import m

fn main() {
	assert m.after() == 10
}
')
	write_project_file(root, 'm/m.v', 'module m

import geom

const shade = geom.Color{1.0, 2.0, 3.0, 4.0}

pub fn after() int {
	return int(shade.r + shade.g + shade.b + shade.a)
}
')
	write_project_file(root, 'geom/geom.v', 'module geom

pub struct Color {
pub:
	r f64
	g f64
	b f64
	a f64
}
')

	bin := os.join_path(os.temp_dir(), 'v3_qualified_positional_struct_init_bin')
	main_path := os.join_path(root, 'main.v')
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('unknown function `m.after`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('(geom__Color){'), c_code
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
}
