module fastc

import os
import v.cmdexec

// A struct field with a default value may be followed by an attribute (`@[json: '-']`).
// The default skipper used to count the attribute's `]` without its `@[`, so the struct
// body scan ran past the closing `}` and failed with "unfinished struct".
const struct_field_default_attribute_source = "module main

pub struct Graph {
pub mut:
	n     int = 3 @[json: '-']
	name  string = 'graph' @[json: 'name']
	items []int = [1, 2] @[json: '-']
	cli   bool @[json: '-']
	other int
}

// An embedded field with a default followed by an attribute uses the same skipper.
struct Inner {
pub mut:
	a int
}

pub struct Outer {
pub mut:
	Inner = Inner{a: 1} @[json: '-']
	b      int
}

fn main() {
	o := Outer{}
	assert o.a == 1
	assert o.b == 0
	g := Graph{}
	assert g.n == 3
	assert g.name == 'graph'
	assert g.items.len == 2
	assert g.items[1] == 2
	assert !g.cli
	assert g.other == 0
	println('struct field default attribute works')
}
"

fn test_fastc_struct_field_default_followed_by_attribute_builds_and_runs() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_struct_field_default_attribute_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), struct_field_default_attribute_source) or {
		panic(err)
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'struct field default attribute works', run.output
}
