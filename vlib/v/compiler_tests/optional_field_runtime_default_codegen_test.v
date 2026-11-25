// vtest ownership
import os
import v.cmdexec
import v.tempname

// An `?T` field (T in another module) must not be expanded into a runtime
// default of its base struct when generating the default element of a
// `[]Outer{cap: n}` array. Cross-module `normalize_type_alias` used to strip
// the `?`, emitting `(__v_option_ig__IErr){<IErr fields>}` — invalid C, since the
// optional wrapper has fields `ok`/`err`/`value`, not the base struct's fields.
fn test_optional_field_runtime_default_is_none_cross_module() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_opt_default_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.mkdir(os.join_path(root, 'ig'))!
	out := os.join_path(root, 'program')

	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'v3optdefault' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'ig', 'ig.v'), 'module ig

pub struct IErr implements IClone {
pub mut:
	message string
	nested  []IErr
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main
import ig

struct Mid implements IClone {
pub mut:
	err  ?ig.IErr
	name string
}

struct Outer implements IClone {
pub mut:
	mid Mid
}

fn build(n int) []Outer {
	return []Outer{cap: n}
}

fn main() {
	assert build(3).len == 0
}
') or {
		panic(err)
	}

	compile := cmdexec.run_in_merged_with_env(compiler, [os.join_path(root, 'main.v'), '-d', 'ownership',
		'-b', 'c', '-o', out], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
}
