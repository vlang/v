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

// Without a default V refuses any attribute before an embedded field, @[skip] and @[required] too.
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
		assert message.contains('does not support attributes on embedded field `Inner`'), message
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

// Every route without a default refuses a leading @[skip] or @[required], as V does.
fn test_fastc_leading_skip_or_required_refused_on_every_embedded_route() {
	sources := [
		'struct S {\n\t@[skip] mod.Inner\n\tb int\n}\n',
		'struct S {\n\t@[skip] Inner;\n\tb int\n}\n',
		'struct S {\n\t@[skip] Inner }\n',
	]
	for src in sources {
		mut message := ''
		fastc_leading_flags_emit(src) or { message = err.msg() }
		assert message.contains('does not support attributes on embedded field'), '${src}: ${message}'
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

// The default compiler accepts a leading @[skip] on an embedded field with a default and
// ignores the flag; fastc must build it and produce the same JSON.
fn test_fastc_leading_skip_on_embedded_field_with_default_builds_as_v_does() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_leading_skip_embedded_default_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import json2

struct Inner {
pub mut:
	a int = 7
}

pub struct Outer {
pub mut:
	@[skip] Inner = Inner{a: 3}
	b int = 2
}

fn main() {
	println(json2.encode(Outer{}))
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
	assert run.output.trim_space() == '{"a":3,"b":2}', run.output
}

// An argument named `skip` inside an attribute is not the skip flag, leading or trailing.
fn test_fastc_skip_word_as_attribute_argument_is_not_the_flag() {
	leading := fastc_leading_flags_emit('struct S {\n\t@[foo: skip] a int = 1\n\tb int\n}\n') or {
		panic(err)
	}
	leading_fields := leading[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert leading_fields.len == 2
	assert leading_fields[0].name == 'a' && !leading_fields[0].is_skip && !leading_fields[0].is_required
	trailing := fastc_leading_flags_emit('struct S {\n\ta int = 1 @[foo: skip]\n\tb int\n}\n') or {
		panic(err)
	}
	trailing_fields := trailing[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert trailing_fields.len == 2
	assert trailing_fields[0].name == 'a' && !trailing_fields[0].is_skip
}

// A `skip` name after a top-level `;` inside one attribute group is still the flag, as V reads it.
fn test_fastc_skip_name_after_semicolon_in_attribute_group_is_the_flag() {
	info := fastc_leading_flags_emit("struct S {\n\t@[json: 'x'; skip] a int = 1\n\tb int\n}\n") or {
		panic(err)
	}
	fields := info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert fields.len == 2
	assert fields[0].name == 'a' && fields[0].is_skip
	assert fields[1].name == 'b' && !fields[1].is_skip
}

// A comma inside a call's parentheses separates arguments, not attribute names, so `skip`
// after it is an argument as in V, leading or trailing.
fn test_fastc_skip_after_comma_inside_call_argument_is_not_the_flag() {
	leading := fastc_leading_flags_emit('struct S {\n\t@[foo(bar, skip)] a int = 1\n\tb int\n}\n') or {
		panic(err)
	}
	leading_fields := leading[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert leading_fields.len == 2
	assert leading_fields[0].name == 'a' && !leading_fields[0].is_skip
	trailing := fastc_leading_flags_emit('struct S {\n\ta int = 1 @[foo(bar, skip)]\n\tb int\n}\n') or {
		panic(err)
	}
	trailing_fields := trailing[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert trailing_fields.len == 2
	assert trailing_fields[0].name == 'a' && !trailing_fields[0].is_skip
}

// V reads a module-qualified attribute name such as `skip.Foo` as one name, so its first part
// is not the flag, leading or trailing, alone, after a `;`, or with call arguments.
fn test_fastc_qualified_attribute_name_starting_with_a_flag_word_is_not_the_flag() {
	attributes := ['@[skip.Foo]', '@[required.Foo]', '@[skip.pub]', "@[json: 'x'; skip.Foo]",
		'@[skip.Foo(1)]']
	for attribute in attributes {
		for src in ['struct S {\n\t${attribute} a int = 1\n\tb int\n}\n',
			'struct S {\n\ta int = 1 ${attribute}\n\tb int\n}\n'] {
			info := fastc_leading_flags_emit(src) or { panic(err) }
			fields := info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
				panic('missing struct S')
			}
			assert fields.len == 2, src
			assert fields[0].name == 'a' && !fields[0].is_skip && !fields[0].is_required, src
			assert fields[1].name == 'b' && !fields[1].is_skip && !fields[1].is_required, src
		}
	}
}

// A `skip` name that a `;` ends is the flag, also when a qualified name follows in the group,
// and so is one that a `.` without a name after it follows: V keeps `@[skip.]` as `skip`.
fn test_fastc_skip_name_before_a_qualified_name_is_the_flag() {
	for src in ['struct S {\n\t@[skip; foo.Bar] a int = 1\n\tb int\n}\n',
		'struct S {\n\ta int = 1 @[skip; foo.Bar]\n\tb int\n}\n',
		'struct S {\n\t@[skip.] a int = 1\n\tb int\n}\n', 'struct S {\n\ta int = 1 @[skip.]\n\tb int\n}\n'] {
		info := fastc_leading_flags_emit(src) or { panic(err) }
		fields := info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
			panic('missing struct S')
		}
		assert fields.len == 2, src
		assert fields[0].name == 'a' && fields[0].is_skip, src
		assert fields[1].name == 'b' && !fields[1].is_skip, src
	}
}

// A `skip` name after a call's closing parenthesis and a top-level `;` is still the flag.
fn test_fastc_skip_name_after_call_and_semicolon_is_the_flag() {
	info := fastc_leading_flags_emit('struct S {\n\t@[foo(bar, skip); skip] a int = 1\n\tb int\n}\n') or {
		panic(err)
	}
	fields := info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
		panic('missing struct S')
	}
	assert fields.len == 2
	assert fields[0].name == 'a' && fields[0].is_skip
	assert fields[1].name == 'b' && !fields[1].is_skip
}

// FastcAttributeCase is an attribute spelling and what V's own compiler (the C backend) makes of
// it on a struct field, the same before the field and after it: whether `field.attrs.contains('skip')`
// holds, and whether a struct literal that leaves the field out is refused.
struct FastcAttributeCase {
	attribute string
	skip      bool
	required  bool
}

// Measured with the C backend, with a module `skip` declaring the attribute structs `Foo` and
// `Bar { n int }`, and a module `required` declaring `Foo`.
const fastc_attribute_cases = [
	FastcAttributeCase{
		attribute: '@[skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[required]'
		required:  true
	},
	FastcAttributeCase{
		attribute: '@[skip.Foo]'
	},
	FastcAttributeCase{
		attribute: '@[required.Foo]'
	},
	FastcAttributeCase{
		attribute: '@[skip.pub]'
	},
	FastcAttributeCase{
		attribute: '@[required.pub]'
	},
	FastcAttributeCase{
		attribute: '@[skip . Foo]'
	},
	FastcAttributeCase{
		attribute: '@[skip.]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip.if]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip.mut]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip.Foo skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip.Foo.skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip.Bar(1) skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[required.Foo required]'
		required:  true
	},
	FastcAttributeCase{
		attribute: '@[skip.Foo; required]'
		required:  true
	},
	FastcAttributeCase{
		attribute: '@[foo skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: "@[json: 'x' skip]"
		skip:      true
	},
	FastcAttributeCase{
		attribute: "@[json: 'x', skip]"
		skip:      true
	},
	FastcAttributeCase{
		attribute: "@[json: 'x'; skip]"
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[foo(x) skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[foo(bar(x), skip)]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[foo(x): skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[foo(skip)]'
	},
	FastcAttributeCase{
		attribute: '@[foo: skip]'
	},
	FastcAttributeCase{
		attribute: '@[foo: { skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip: x]'
	},
	FastcAttributeCase{
		attribute: '@[required: x]'
		required:  true
	},
	FastcAttributeCase{
		attribute: '@[skip(x)]'
	},
	FastcAttributeCase{
		attribute: '@[required(x)]'
		required:  true
	},
	FastcAttributeCase{
		attribute: '@[skip()]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip(a: 1)]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: "@['skip']"
		skip:      true
	},
	FastcAttributeCase{
		attribute: "@['required']"
		required:  true
	},
	FastcAttributeCase{
		attribute: '@[a[1], 2]'
	},
	FastcAttributeCase{
		attribute: '@[a[1], [2]]'
	},
	FastcAttributeCase{
		attribute: '@[foo.skip]'
	},
	FastcAttributeCase{
		attribute: '@[pub.skip]'
	},
	FastcAttributeCase{
		attribute: "@[' skip']"
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[foo(a[1]) skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[foo(a[) 1]]'
	},
	FastcAttributeCase{
		attribute: '@[foo @[bar]]'
	},
	FastcAttributeCase{
		attribute: '@[skip @[bar]]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[foo, : skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[foo; : skip]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip(a: 1, b: 2)]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip(a: .x)]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip(a: x y)]'
	},
	FastcAttributeCase{
		attribute: '@[skip(a: . 1)]'
	},
	FastcAttributeCase{
		attribute: '@[skip(1: 2)]'
	},
	FastcAttributeCase{
		attribute: '@[foo(@[) 1]]'
	},
	FastcAttributeCase{
		attribute: '@[a[1], @[x]]'
	},
	FastcAttributeCase{
		attribute: "@['a'.skip]"
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[skip foo(x)]'
		skip:      true
	},
	FastcAttributeCase{
		attribute: '@[required foo(x)]'
		required:  true
	},
	FastcAttributeCase{
		attribute: '@[required foo]'
		required:  true
	},
	FastcAttributeCase{
		attribute: '@[if skip ?]'
	},
	FastcAttributeCase{
		attribute: '@[if !skip ?]'
	},
]

// Every spelling sets the flags V gives it, before the field and after it. V ends an attribute
// where its tokens end, with or without a `;` or `,` after it; reads `a.b` with a name or `pub`
// after the dot as one name; ends a call at its first `)`; reads a quoted attribute as its text;
// and skips a field only for an attribute that is exactly `skip`.
fn test_fastc_struct_field_attribute_flags_match_v() {
	for c in fastc_attribute_cases {
		for src in ['struct S {\n\t${c.attribute} a int = 1\n\tb int\n}\n',
			'struct S {\n\ta int = 1 ${c.attribute}\n\tb int\n}\n'] {
			info := fastc_leading_flags_emit(src) or { panic('${src}: ${err}') }
			fields := info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
				panic('missing struct S')
			}
			assert fields.len == 2, src
			assert fields[0].name == 'a', src
			assert fields[0].is_skip == c.skip, src
			assert fields[0].is_required == c.required, src
			assert fields[1].name == 'b' && !fields[1].is_skip && !fields[1].is_required, src
		}
	}
}

// V refuses `name:` followed by `]`, `:`, `[` or `}`, and a call that never closes; fastc does too.
fn test_fastc_struct_field_attribute_refusals_match_v() {
	refusals := {
		'@[foo: ]':        'attribute `foo:` without an argument'
		'@[foo: :x]':      'attribute `foo:` without an argument'
		'@[foo: [1]]':     'attribute `foo:` without an argument'
		'@[foo: }]':       'attribute `foo:` without an argument'
		'@[foo(x:) skip]': 'unfinished struct field attribute'
	}
	for attribute, expected in refusals {
		for src in ['struct S {\n\t${attribute} a int = 1\n\tb int\n}\n',
			'struct S {\n\ta int = 1 ${attribute}\n\tb int\n}\n'] {
			mut message := ''
			fastc_leading_flags_emit(src) or { message = err.msg() }
			assert message.contains(expected), '${src}: ${message}'
		}
	}
	// A group that the input ends inside, in each part of it, is refused rather than read on.
	for attribute in ['@[skip', '@[if x', '@[foo:', '@[a[1]'] {
		src := 'struct S {\n\ta int = 1 ${attribute}'
		mut message := ''
		fastc_leading_flags_emit(src) or { message = err.msg() }
		assert message.contains('unfinished struct field attribute'), '${src}: ${message}'
	}
}

// fastc_struct_fields_emit emits one struct declaration in which the type `Inner` is declared, and
// returns its field table.
fn fastc_struct_fields_emit(src string) !map[string][]FastcStructField {
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
	mut declared_types := map[u32]bool{}
	declared_types[flat.source_type_id(fastc_type_key('main', 'Inner'))] = true
	mut struct_fields := map[string]flat.TypeTextBindings{}
	mut struct_field_info := map[string][]FastcStructField{}
	mut composite_types := map[string]bool{}
	mut out := strings.new_builder(256)
	fastc_emit_struct_declaration(mut scan, false, FastcDeclarationAttribute{ is_enabled: true }, source_file, declared_types, false, false, mut struct_fields, mut struct_field_info, mut composite_types, mut out)!
	return struct_field_info
}

// Without a default V refuses any attribute before an embedded field ("cannot use attributes on
// embedded structs"), so a qualified name or an attribute that sets no flag is refused too. With a
// default V accepts it, and so does fastc, unless it would set a flag fastc cannot record there.
fn test_fastc_attributes_on_embedded_fields_match_v() {
	for attribute in ['@[foo]', '@[skip.Foo]', '@[required.Foo]', "@[json: 'x']"] {
		mut message := ''
		fastc_struct_fields_emit('struct S {\n\t${attribute} Inner\n\tb int\n}\n') or {
			message = err.msg()
		}
		assert message.contains('does not support attributes on embedded field `Inner`'), '${attribute}: ${message}'
	}
	for src in ['struct S {\n\tInner\n\tb int\n}\n', 'struct S {\n\t@[skip.Foo] Inner = Inner{}\n\tb int\n}\n',
		'struct S {\n\tInner = Inner{} @[skip.Foo]\n\tb int\n}\n'] {
		info := fastc_struct_fields_emit(src) or { panic('${src}: ${err}') }
		fields := info[fastc_c_declared_type_name(fastc_type_key('main', 'S'))] or {
			panic('missing struct S')
		}
		assert fields.len == 2, src
		assert fields[1].name == 'b' && !fields[1].is_skip, src
	}
	mut message := ''
	fastc_struct_fields_emit('struct S {\n\tInner = Inner{} @[skip.Foo skip]\n\tb int\n}\n') or {
		message = err.msg()
	}
	assert message.contains('on embedded field `Inner` with a default'), message
}
