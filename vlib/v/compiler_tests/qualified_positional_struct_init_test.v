import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn test_qualified_positional_struct_init_keeps_later_imported_fn() {
	v3_bin := os.join_path(os.vtmp_dir(), 'v3_qualified_positional_struct_init_test_${os.getpid()}')
	build :=
		os.exec([vexe, '-gc', 'none', '-path', '${vlib_dir}' + '|@vlib|@vmodules', '-o', v3_bin,
			'${v3_src}'])
	assert build.exit_code == 0, build.output

	root := os.join_path(os.vtmp_dir(), 'v3_qualified_positional_struct_init_project_${os.getpid()}')
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

	bin := os.join_path(os.vtmp_dir(), 'v3_qualified_positional_struct_init_bin_${os.getpid()}')
	main_path := os.join_path(root, 'main.v')
	defer {
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
		os.rm(bin) or {}
		os.rm(bin + '.c') or {}
	}
	compile := os.exec([v3_bin, '-b', 'c', '-keepc', '-o', bin, main_path])
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
