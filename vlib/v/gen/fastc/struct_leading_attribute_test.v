module fastc

import os
import strings
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

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

// Embedded fields record no skip/required flags, so a leading @[skip] or @[required] is refused.
fn test_fastc_leading_skip_or_required_on_embedded_field_is_refused() {
	for attribute in ['@[skip]', '@[required]'] {
		src := 'struct S {\n\t${attribute} Inner\n\tb int\n}\n'
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
		mut message := ''
		_ = fastc_emit_struct_declaration(mut scan, false, FastcDeclarationAttribute{ is_enabled: true }, source_file, map[u32]bool{}, false, false, mut struct_fields, mut struct_field_info, mut composite_types, mut out) or {
			message = err.msg()
			token.Token.eof
		}
		assert message.contains('does not support @[skip] or @[required] on embedded field `Inner`'), message
	}
}

// An attribute after an embedded type with no default is refused: fastc has no such
// embedded form, and master does not accept it either.
fn test_fastc_embedded_attribute_without_default_is_refused() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_embedded_attribute_refused_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Inner {
pub mut:
	a int
}

pub struct Outer {
pub mut:
	Inner @[json: '-']
	b     int
}

fn main() {
	o := Outer{}
	println(o.b)
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code != 0, build.output
	assert build.output.contains('does not support type `@[`'), build.output
}

// fastc_leading_flags_emit emits one struct declaration and returns its field table.
fn fastc_leading_flags_emit(src string) !map[string][]FastcStructField {
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
	fastc_emit_struct_declaration(mut scan, false, FastcDeclarationAttribute{ is_enabled: true }, source_file, map[u32]bool{}, false, false, mut struct_fields, mut struct_field_info, mut composite_types, mut out)!
	return struct_field_info
}

// Two leading attribute groups before one field both contribute their flags.
fn test_fastc_struct_consecutive_leading_groups_both_apply() {
	info := fastc_leading_flags_emit('struct S {\n\t@[skip]\n\t@[required]\n\ta int = 1\n\tb int\n}\n') or {
		panic(err)
	}
	fields := info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert fields.len == 2
	assert fields[0].name == 'a' && fields[0].is_skip && fields[0].is_required
	assert fields[1].name == 'b' && !fields[1].is_skip && !fields[1].is_required
}

// Every route into the embedded branch refuses a leading @[skip] or @[required].
fn test_fastc_leading_skip_or_required_refused_on_every_embedded_route() {
	sources := [
		'struct S {\n\t@[skip] Inner = Inner{}\n\tb int\n}\n',
		'struct S {\n\t@[skip] mod.Inner\n\tb int\n}\n',
		'struct S {\n\t@[skip] Inner;\n\tb int\n}\n',
		'struct S {\n\t@[skip] Inner }\n',
	]
	for src in sources {
		mut message := ''
		fastc_leading_flags_emit(src) or { message = err.msg() }
		assert message.contains('does not support @[skip] or @[required] on embedded field'), '${src}: ${message}'
	}
}

// A leading attribute before `mut:` skips the next field, as it does before `pub mut:`.
fn test_fastc_struct_leading_attribute_before_mut_section_applies_to_next_field() {
	info := fastc_leading_flags_emit('struct S {\n\tpub mut:\n\tc int\n\t@[skip]\n\tmut:\n\ta int\n\tb int\n}\n') or {
		panic(err)
	}
	fields := info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert fields.len == 3
	assert fields[0].name == 'c' && !fields[0].is_skip
	assert fields[1].name == 'a' && fields[1].is_skip
	assert fields[2].name == 'b' && !fields[2].is_skip
}
