import os

const opt_default_vexe = @VEXE
const opt_default_tests_dir = os.dir(@FILE)
const opt_default_v3_dir = os.dir(opt_default_tests_dir)
const opt_default_vlib_dir = os.dir(opt_default_v3_dir)
const opt_default_v3_src = os.join_path(opt_default_v3_dir, 'v3.v')

// An `?T` field (T in another module) must not be expanded into a runtime
// default of its base struct when generating the default element of a
// `[]Outer{cap: n}` array. Cross-module `normalize_type_alias` used to strip
// the `?`, emitting `(Optional_ig__IErr){<IErr fields>}` — invalid C, since the
// optional wrapper has fields `ok`/`err`/`value`, not the base struct's fields.
fn test_optional_field_runtime_default_is_none_cross_module() {
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_opt_default_${pid}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'ig')) or { panic(err) }
	v3_bin := os.join_path(root, 'v3_opt_default_driver')
	out := os.join_path(root, 'program')
	defer {
		os.rmdir_all(root) or {}
	}
	build :=
		os.execute('${opt_default_vexe} -gc none -d ownership -path "${opt_default_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${opt_default_v3_src}')
	assert build.exit_code == 0, build.output

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

	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
}
