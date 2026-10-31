// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os

// field_types maps the `v generate model` field vocabulary to V types. It is
// deliberately small: a generated model is a starting point, and anything
// exotic belongs hand-written.
const field_types = {
	'string': 'string'
	'int':    'int'
	'i64':    'i64'
	'bool':   'bool'
	'f64':    'f64'
}

fn main() {
	// Invoked as `v generate ...`, the dispatcher strips the executable and
	// the verb, so the generator kind is the first remaining argument.
	args := os.args[2..]
	if args.len == 0 || args[0] in ['help', '-help', '--help', '-h'] {
		print_usage()
		return
	}
	kind := args[0]
	rest := args[1..]
	match kind {
		'model' {
			generate_model(rest) or { fail(err.msg()) }
		}
		'controller' {
			generate_controller(rest) or { fail(err.msg()) }
		}
		else {
			fail('unknown generator "${kind}"')
		}
	}
}

fn fail(msg string) {
	eprintln('v generate: ${msg}')
	eprintln('run `v generate help` for usage')
	exit(1)
}

fn print_usage() {
	println('Usage: v generate <model|controller> <Name> [fields...]')
	println('')
	println('  v generate model User name:string age:int active:bool')
	println('    writes models/user.v with an ORM struct')
	println('')
	println('  v generate controller Users')
	println('    writes users_controller.v with a veb.Controller stub')
	println('')
	println('Model field types: string, int, i64, bool, f64.')
	println('Generated files are never overwritten: rename or remove the')
	println('existing file first.')
}

// snake_case converts a CamelCase name to snake_case for file and table names.
fn snake_case(name string) string {
	mut out := []u8{cap: name.len + 4}
	for i, c in name.bytes() {
		if c >= `A` && c <= `Z` {
			if i > 0 {
				out << `_`
			}
			out << c + 32
		} else {
			out << c
		}
	}
	return out.bytestr()
}

// valid_name reports whether `name` is a usable CamelCase type name.
fn valid_name(name string) bool {
	if name.len == 0 {
		return false
	}
	if name[0] < `A` || name[0] > `Z` {
		return false
	}
	for c in name.bytes() {
		if !(c == `_`
			|| (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`)) {
			return false
		}
	}
	return true
}

// valid_field_name reports whether `name` is a usable struct field name:
// a non-empty lowercase V identifier. Callers check duplicates separately.
fn valid_field_name(name string) bool {
	if name.len == 0 {
		return false
	}
	first := name[0]
	if !(first == `_` || (first >= `a` && first <= `z`)) {
		return false
	}
	for c in name.bytes()[1..] {
		if !(c == `_` || (c >= `a` && c <= `z`)
			|| (c >= `0` && c <= `9`)) {
			return false
		}
	}
	return true
}

// write_new_file writes `content` to `path`, refusing when the file exists:
// generated code must never silently eat hand-written code.
fn write_new_file(path string, content string) ! {
	if os.exists(path) {
		return error('`${path}` already exists; rename or remove it first')
	}
	dir := os.dir(path)
	if dir != '' && dir != '.' {
		os.mkdir_all(dir)!
	}
	os.write_file(path, content)!
}

// generate_model writes an ORM struct: `v generate model User name:string age:int`.
fn generate_model(args []string) ! {
	if args.len == 0 {
		return error('missing a model name, e.g. `v generate model User name:string`')
	}
	name := args[0]
	if !valid_name(name) {
		return error('`${name}` is not a CamelCase type name')
	}
	mut names := ['id']
	mut types := ['int']
	mut attrs := ['@[primary; sql: serial]']
	for spec in args[1..] {
		parts := spec.split(':')
		if parts.len != 2 {
			return error('`${spec}` is not a `name:type` field; types: string, int, i64, bool, f64')
		}
		vtype := field_types[parts[1]] or {
			return error('unknown field type `${parts[1]}`; types: string, int, i64, bool, f64')
		}
		fname := parts[0]
		if !valid_field_name(fname) {
			return error('invalid field name `${fname}`; use snake_case identifiers')
		}
		if fname in names {
			return error('duplicate field name `${fname}`')
		}
		names << fname
		types << vtype
		attrs << ''
	}
	mut width := 0
	for fname in names {
		if fname.len > width {
			width = fname.len
		}
	}
	mut fields := []string{}
	for i, fname in names {
		mut line := '\t${fname}${' '.repeat(width - fname.len + 1)}${types[i]}'
		if attrs[i] != '' {
			line += ' ' + attrs[i]
		}
		fields << line
	}
	snake := snake_case(name)
	mut lines := []string{}
	lines << 'module models'
	lines << ''
	lines << "@[table: '${snake}s']"
	lines << 'pub struct ${name} {'
	lines << 'pub mut:'
	lines << fields.join('\n')
	lines << '}'
	content := lines.join('\n') + '\n'
	path := os.join_path('models', '${snake}.v')
	write_new_file(path, content)!
	println('created ${path}')
}

// generate_controller writes a veb.Controller stub: `v generate controller Users`.
fn generate_controller(args []string) ! {
	if args.len == 0 {
		return error('missing a controller name, e.g. `v generate controller Users`')
	}
	name := args[0]
	if !valid_name(name) {
		return error('`${name}` is not a CamelCase type name')
	}
	struct_name := '${name}Controller'
	snake := snake_case(name)
	lines := [
		'module main',
		'',
		'import veb',
		'',
		'pub struct ${struct_name} {',
		'	veb.Controller',
		'}',
		'',
		"@['/'; get]",
		'pub fn (app &${struct_name}) index(mut ctx Context) veb.Result {',
		"	return ctx.text('${snake} index')",
		'}',
		'',
		"@['/:id'; get]",
		'pub fn (app &${struct_name}) show(mut ctx Context, id int) veb.Result {',
		"	return ctx.text('${snake} \${id}')",
		'}',
		'',
		'// Register in main.v, adjusting the Context type name if yours differs:',
		'// mut ${snake} := &${struct_name}{}',
		"// app.register_controller[${struct_name}, Context]('/${snake}', mut ${snake})!",
	]
	content := lines.join('\n') + '\n'
	path := '${snake}_controller.v'
	write_new_file(path, content)!
	println('created ${path}')
}
