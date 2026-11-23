// vtest ownership
import os
import v.tempname

const default_clone_vexe = @VEXE
const default_clone_tests_dir = os.dir(@FILE)
const default_clone_v3_dir = os.dir(default_clone_tests_dir)
const default_clone_vlib_dir = os.dir(default_clone_v3_dir)
const default_clone_v3_src = os.join_path(default_clone_v3_dir, 'v.v')
const default_clone_compiler_dir = os.join_path(os.vtmp_dir(), 'v3_default_clone_compiler_${tempname.unique_token()}')

fn testsuite_end() {
	os.rmdir_all(default_clone_compiler_dir) or {}
}

fn default_clone_build_v3() string {
	v3_bin := os.join_path(default_clone_compiler_dir, 'v3') + $if windows { '.exe' } $else { '' }
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	os.mkdir_all(default_clone_compiler_dir) or { panic(err) }
	build :=
		os.exec([default_clone_vexe, '-gc', 'none', '-d', 'ownership', '-path',
			'${default_clone_vlib_dir}' + '|@vlib|@vmodules', '-o', v3_bin, default_clone_v3_src])
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_compiler_default_clone_uses_the_aggregate_type() {
	pid := os.getpid()
	v3_bin := default_clone_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_default_clone_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_default_clone_program_${pid}')
	defer {
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	os.write_file(src, 'module main

struct Item implements IClone {
	value int
}

fn copy_item(item Item) Item {
	return item.clone()
}

fn main() {
	item := Item{value: 7}
	copy := copy_item(item)
	assert copy.value == 7
}
') or {
		panic(err)
	}
	compile := os.exec([v3_bin, '${src}', '-d', 'ownership', '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	assert !c_source.contains('string__clone(item)'), c_source
	run := os.exec([out])
	assert run.exit_code == 0, run.output
}

// A compiler-provided IClone `clone()` (Rust `#[derive(Clone)]`) has no entry
// in the fn tables. Inside a generic function body, generic specialization must
// not diagnose `x.clone()` on such a struct as an unknown function.
fn test_compiler_default_clone_inside_generic_fn() {
	pid := os.getpid()
	v3_bin := default_clone_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_generic_clone_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_generic_clone_program_${pid}')
	defer {
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	os.write_file(src, 'module main

struct Cfg implements IClone {
	value int
}

// `.clone()` is called on a concrete IClone receiver inside a generic fn body.
fn copy_in_generic[T](c &Cfg, x T) Cfg {
	return c.clone()
}

fn main() {
	c := Cfg{value: 7}
	d := copy_in_generic[int](&c, 0)
	assert d.value == 7
}
') or {
		panic(err)
	}
	compile := os.exec([v3_bin, '${src}', '-d', 'ownership', '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
}
