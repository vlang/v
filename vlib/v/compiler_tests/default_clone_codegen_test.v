// vtest ownership
import os
import v.cmdexec
import v.tempname

fn compile_default_clone(src string, out string) os.Result {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	return cmdexec.run_in_merged_with_env(compiler, [src, '-d', 'ownership', '-b', 'c', '-o', out], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
}

fn test_compiler_default_clone_uses_the_aggregate_type() {
	root := os.join_path(os.vtmp_dir(), 'v3_default_clone_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_default_clone.v')
	out := os.join_path(root, 'v3_default_clone_program')
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
	compile := compile_default_clone(src, out)
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
	root := os.join_path(os.vtmp_dir(), 'v3_generic_clone_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_generic_clone.v')
	out := os.join_path(root, 'v3_generic_clone_program')
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
	compile := compile_default_clone(src, out)
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
}
