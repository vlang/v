import os
import v.tempname

fn qualified_init_write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn test_qualified_positional_struct_init_keeps_later_imported_fn() {
	dir := os.join_path(os.vtmp_dir(), 'v3_qualified_positional_struct_init_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	root := os.join_path(dir, 'project')
	os.mkdir(root)!
	qualified_init_write_project_file(root, 'main.v', 'module main

import m

fn main() {
	assert m.after() == 10
}
')
	qualified_init_write_project_file(root, 'm/m.v', 'module m

import geom

const shade = geom.Color{1.0, 2.0, 3.0, 4.0}

pub fn after() int {
	return int(shade.r + shade.g + shade.b + shade.a)
}
')
	qualified_init_write_project_file(root, 'geom/geom.v', 'module geom

pub struct Color {
pub:
	r f64
	g f64
	b f64
	a f64
}
')

	bin := os.join_path(dir, 'app')
	main_path := os.join_path(root, 'main.v')
	compile := os.exec([@VEXE, '-b', 'c', '-keepc', '-o', bin, main_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('unknown function `m.after`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(bin + '.c') or { panic(err) }
	// The qualified positional init reaches the C backend as a designated-initializer
	// struct definition rather than an unnamed C compound literal, which is the
	// correct form for a const declaration. What matters is that the four positional
	// values survive the import resolution, so pin them rather than the literal form.
	assert c_code.contains('m__shade = {.r = 1.0, .g = 2.0, .b = 3.0, .a = 4.0}'), c_code
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}
