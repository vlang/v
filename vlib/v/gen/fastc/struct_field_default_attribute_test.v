module fastc

import os
import strings
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

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

// An embedded field with an attribute and no default is valid V and parsed the same way.
pub struct Plain {
pub mut:
	Inner @[json: '-']
	b     int
}

fn main() {
	o := Outer{}
	assert o.a == 1
	assert o.b == 0
	p := Plain{}
	assert p.a == 0
	assert p.b == 0
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

// The flags of attributes that follow a default are recorded on the field, not dropped.
fn test_fastc_struct_field_flags_after_default_are_recorded() {
	src := 'struct S {\n\tn int = 3 @[required]\n\tm int = 4 @[skip]\n\tk int @[required]\n}\n'
	prefs := pref.new_preferences()
	file := token.File.unindexed('s.v', src.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, src)
	assert scan.scan() == .key_struct
	source_file := FastcSourceFile{
		path:   's.v'
		source: src
		header: FastcSourceHeader{
			module_name: 'main'
			imports:     map[string]string{}
		}
	}
	mut struct_fields := map[string]flat.TypeTextBindings{}
	mut struct_field_info := map[string][]FastcStructField{}
	mut composite_types := map[string]bool{}
	mut out := strings.new_builder(256)
	fastc_emit_struct_declaration(mut scan, false, FastcDeclarationAttribute{ is_enabled: true }, source_file, map[u32]bool{}, false, false, mut struct_fields, mut struct_field_info, mut composite_types, mut out) or {
		panic(err)
	}
	fields := struct_field_info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert fields.len == 3
	assert fields[0].name == 'n'
	assert fields[0].is_required && !fields[0].is_skip
	assert fields[0].default_source == '3'
	assert fields[1].name == 'm'
	assert fields[1].is_skip && !fields[1].is_required
	assert fields[1].default_source == '4'
	assert fields[2].name == 'k'
	assert fields[2].is_required && fields[2].default_source == ''
}

// Attributes written before a field name apply to that field, as trailing ones do.
fn test_fastc_struct_field_leading_attribute_flags_are_recorded() {
	src := 'struct S {\n\t@[skip] a int = 1\n\t@[required] b int\n\tc int\n}\n'
	prefs := pref.new_preferences()
	file := token.File.unindexed('s.v', src.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, src)
	assert scan.scan() == .key_struct
	source_file := FastcSourceFile{
		path:   's.v'
		source: src
		header: FastcSourceHeader{
			module_name: 'main'
			imports:     map[string]string{}
		}
	}
	mut struct_fields := map[string]flat.TypeTextBindings{}
	mut struct_field_info := map[string][]FastcStructField{}
	mut composite_types := map[string]bool{}
	mut out := strings.new_builder(256)
	fastc_emit_struct_declaration(mut scan, false, FastcDeclarationAttribute{ is_enabled: true }, source_file, map[u32]bool{}, false, false, mut struct_fields, mut struct_field_info, mut composite_types, mut out) or {
		panic(err)
	}
	fields := struct_field_info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert fields.len == 3
	assert fields[0].name == 'a'
	assert fields[0].is_skip && !fields[0].is_required
	assert fields[1].name == 'b'
	assert fields[1].is_required && !fields[1].is_skip
	assert fields[2].name == 'c'
	assert !fields[2].is_skip && !fields[2].is_required
}

// A leading attribute applies to every name of a grouped field, may be combined with a
// trailing one on the same field, and may end the struct body without a field after it.
fn test_fastc_struct_field_leading_attribute_edge_cases_are_recorded() {
	src := 'struct S {\n\t@[skip] g, h int\n\t@[skip] a int = 1 @[required]\n\tc int\n\t@[skip]\n}\n'
	prefs := pref.new_preferences()
	file := token.File.unindexed('s.v', src.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, src)
	assert scan.scan() == .key_struct
	source_file := FastcSourceFile{
		path:   's.v'
		source: src
		header: FastcSourceHeader{
			module_name: 'main'
			imports:     map[string]string{}
		}
	}
	mut struct_fields := map[string]flat.TypeTextBindings{}
	mut struct_field_info := map[string][]FastcStructField{}
	mut composite_types := map[string]bool{}
	mut out := strings.new_builder(256)
	fastc_emit_struct_declaration(mut scan, false, FastcDeclarationAttribute{ is_enabled: true }, source_file, map[u32]bool{}, false, false, mut struct_fields, mut struct_field_info, mut composite_types, mut out) or {
		panic(err)
	}
	fields := struct_field_info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert fields.len == 4
	assert fields[0].name == 'g' && fields[0].is_skip && !fields[0].is_required
	assert fields[1].name == 'h' && fields[1].is_skip && !fields[1].is_required
	assert fields[2].name == 'a' && fields[2].is_skip && fields[2].is_required
	assert fields[2].default_source == '1'
	assert fields[3].name == 'c' && !fields[3].is_skip && !fields[3].is_required
}

// A leading attribute before a section keyword skips the next field in V; fastc must agree.
fn test_fastc_struct_leading_attribute_before_section_matches_v() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_leading_attribute_section_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

import json2

pub struct S {
pub mut:
	c int
	@[skip]
	pub mut:
	a int = 5
}

fn main() {
	println(json2.encode(S{}))
}
') or { panic(err) }
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
	assert run.output.trim_space() == '{"c":0}', run.output
}
