module fastc

import os
import v.cmdexec

// A constant is renamed to its C spelling wherever its name appears in the rendered C, but
// never inside a string literal: `'x = "app"' + app` keeps the literal's `app`. A field
// default that names it only inside `${...}` still renders it as the string it is.
fn test_fastc_constant_names_are_not_rewritten_inside_string_literals() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_literal_names_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_literal_names' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

const app = \'TAIL\'

struct Tagged {
	label string = \'<\${app}>\'
}

fn label(s string) string {
	return s
}

fn main() {
	println(label(\'x = "app"\' + app))
	println("y = \'app\'" + app)
	println(\'z = app\' + app)
	tagged := Tagged{}
	println(tagged.label)
}
') or {
		panic(err)
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'x = "app"TAIL\ny = \'app\'TAIL\nz = appTAIL\n<TAIL>'
}

// A struct field default depends on the constants its code names; one with an interpolation
// or a comment is taken to, since their contents are not parsed, but a constant's name in
// plain string text is not a reference.
fn test_fastc_field_defaults_reference_constants_inside_interpolations() {
	constants := {
		'main.prefix': 'string'
	}
	for default_source, references in {
		'prefix':                   true
		"'\${prefix}'":             true
		"'\${'-' + prefix}'":       true
		"'\${/* note /*/ prefix}'": true
		"r'\${prefix}'":            true
		"'prefix' /* note */":      true
		"'prefix'":                 false
		"r'C:\\' + 'prefix'":       false
	} {
		fields := {
			'main.Item': [FastcStructField{
				name:           'label'
				typ:            'string'
				default_source: default_source
			}]
		}
		assert fastc_field_defaults_reference_constants(fields, constants) == references, default_source
	}
}
