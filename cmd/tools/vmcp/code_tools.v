// Tools that answer questions about the code itself: the AST, the declarations
// in a file, what sits under a cursor, and where a name is mentioned.
//
// Every one of them reads the flat AST in process. That makes them fast enough
// to call per file, and it makes them work on a file that does not compile, which
// is exactly when an agent needs to know what it wrote.
module main

import os
import v.astjson
import v.astquery
import v.flat

// ast_byte_limit is the largest AST rendering, in bytes, one response may carry.
//
// An agent's context is the scarce resource here, so the default is deliberately
// bounded and the tool says what it dropped instead of silently truncating.
const ast_byte_limit = 400000

// spec_ast declares `v_ast`.
fn spec_ast() ToolSpec {
	return read_only_spec('v_ast',
		'Return the V AST of one file as JSON, in exactly the format `v ast -p`
prints. Use `terse` for the tree shape and `skip_defaults` to drop zero-valued
properties; they keep the answer small enough to read in full.',
		input_schema(['path'], {
			'path':          SchemaProperty{
				kind:        'string'
				description: 'The .v or .vsh file to dump.'
			}
			'terse':         SchemaProperty{
				kind:        'boolean'
				description: 'Only node kinds and the tree\nshape.'
			}
			'skip_defaults': SchemaProperty{
				kind:        'boolean'
				description: 'Drop properties holding a\nzero value.'
			}
			'hide':          SchemaProperty{
				kind:        'array'
				description: 'Property\nnames to leave out, for example `["pos"]`.'
			}
		}), tool_ast)
}

// tool_ast answers `v_ast`.
fn tool_ast(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.is_file(path) {
		return error_json('`${args.text('path', '')}` is not a file')
	}
	opts := astjson.Options{
		terse:         args.boolean('terse', false)
		skip_defaults: args.boolean('skip_defaults', false)
		hidden:        args.list('hide')
	}
	rendered := astjson.dump(astjson.parse(path), opts)
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(path))
	w.key('bytes')
	w.number(rendered.len)
	w.key('truncated')
	w.boolean(rendered.len > ast_byte_limit)
	if rendered.len > ast_byte_limit {
		w.key_raw('ast', 'null')
		w.key('limit')
		w.number(ast_byte_limit)
		w.key('hint')
		w.string('Use terse, skip_defaults or hide to reduce the tree, or request v_symbols.')
	} else {
		w.key_raw('ast', rendered)
	}
	w.end_object()
	return w.str()
}

// spec_symbols declares `v_symbols`.
fn spec_symbols() ToolSpec {
	return read_only_spec('v_symbols',
		'List what a file declares: functions, methods, structs and their fields,
enums, interfaces, sum types, constants and globals, each with its line, column
and doc comment. Use it to understand a file without reading all of it.',
		input_schema(['path'], {
			'path':           SchemaProperty{
				kind:        'string'
				description: 'The .v or .vsh file to inspect.'
			}
			'kind':           SchemaProperty{
				kind:        'string'
				description: 'Only declarations of this kind, for\nexample `fn`, `struct`, `field`.'
			}
			'include_nested': SchemaProperty{
				kind:        'boolean'
				description: 'Include members such as\nfields and enum values. Defaults to true.'
			}
		}), tool_symbols)
}

// tool_symbols answers `v_symbols`.
fn tool_symbols(ws &Workspace, arguments string) string {
	path := ws.resolve_arg(decode_args(arguments), 'path') or { return error_json(err.msg()) }
	if !os.is_file(path) {
		return error_json('`${path}` is not a file')
	}
	args := decode_args(arguments)
	return declarations_json(ws, astquery.declarations(astquery.parse(path)),
		args.text('kind', ''), args.boolean('include_nested', true))
}

// member_kinds are the declaration kinds that describe a member of another
// declaration rather than a top level one.
const member_kinds = ['field', 'enum_value', 'interface_method']

// declarations_json renders declarations as JSON, filtered by `kind` and
// `include_nested`.
fn declarations_json(ws &Workspace, decls []astquery.Declaration, kind string,
	include_nested bool) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('count')
	mut kept := 0
	for decl in decls {
		if matches_declaration(decl, kind, include_nested) {
			kept++
		}
	}
	w.number(kept)
	w.key('declarations')
	w.begin_array()
	for decl in decls {
		if !matches_declaration(decl, kind, include_nested) {
			continue
		}
		w.array_raw(declaration_json(ws, decl))
	}
	w.end_array()
	w.end_object()
	return w.str()
}

// matches_declaration reports whether a declaration passes the two filters.
fn matches_declaration(decl astquery.Declaration, kind string,
	include_nested bool) bool {
	if kind != '' && decl.kind.str() != kind {
		return false
	}
	return include_nested || decl.kind.str() !in member_kinds
}

// declaration_json renders one declaration.
fn declaration_json(ws &Workspace, decl astquery.Declaration) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('kind')
	w.string(decl.kind.str())
	w.key('name')
	w.string(decl.name)
	if decl.receiver != '' {
		w.key('receiver')
		w.string(decl.receiver)
	}
	if decl.type_name != '' {
		w.key('type')
		w.string(decl.type_name)
	}
	w.key('file')
	w.string(ws.relative(decl.file))
	w.key('line')
	w.number(decl.line)
	w.key('column')
	w.number(decl.column)
	if decl.doc != '' {
		w.key('doc')
		w.string(decl.doc)
	}
	w.end_object()
	return w.str()
}

// spec_symbol_at declares `v_symbol_at`.
fn spec_symbol_at() ToolSpec {
	return read_only_spec('v_symbol_at',
		'Identify what is written at a source position, for example the cursor.
Returns the innermost name at that position and, when it is a declaration, the
kind it declares and the line it starts on. `name` may be omitted to try every
declaration and reference in the file.',
		input_schema(['path', 'line', 'column'], {
			'path':   SchemaProperty{
				kind:        'string'
				description: 'The .v or .vsh file.'
			}
			'line':   SchemaProperty{
				kind:        'integer'
				description: '1-based line number.'
			}
			'column': SchemaProperty{
				kind:        'integer'
				description: '1-based column number.'
			}
			'name':   SchemaProperty{
				kind:        'string'
				description: 'The name at the position. Omit to\nsearch the whole file.'
			}
		}), tool_symbol_at)
}

// tool_symbol_at answers `v_symbol_at`.
fn tool_symbol_at(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	line := args.int('line', 0)
	column := args.int('column', 0)
	if line <= 0 || column <= 0 {
		return error_json('`line` and `column` are 1-based and both required')
	}
	a := astquery.parse(path)
	name := args.text('name', '')
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(path))
	w.key('line')
	w.number(line)
	w.key('column')
	w.number(column)
	if name != '' {
		occurrence := astquery.occurrence_at(a, name, line, column)
		w.key('name')
		w.string(name)
		w.key('found')
		w.boolean(occurrence != none)
		if occ := occurrence {
			w.key_raw('occurrence', occurrence_json(ws, occ))
		}
		w.key_raw('declaration', declaration_at_json(ws, a, name))
		w.end_object()
		return w.str()
	}
	// Without a name, report every declaration covering the position, widest
	// first, so the caller can tell a declaration from a mention.
	mut candidates := []astquery.Declaration{}
	for decl in astquery.declarations(a) {
		if decl.line == line && decl.column <= column {
			candidates << decl
		}
	}
	w.key_raw('declarations_here', declarations_list_json(ws, candidates))
	w.end_object()
	return w.str()
}

// declarations_list_json renders a list of declarations as a JSON array.
fn declarations_list_json(ws &Workspace, decls []astquery.Declaration) string {
	mut w := astjson.Writer{}
	w.begin_array()
	for decl in decls {
		w.array_raw(declaration_json(ws, decl))
	}
	w.end_array()
	return w.str()
}

// occurrence_json renders one mention of a name.
fn occurrence_json(ws &Workspace, occ astquery.Occurrence) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('file')
	w.string(ws.relative(occ.file))
	w.key('line')
	w.number(occ.line)
	w.key('column')
	w.number(occ.column)
	w.key('end_column')
	w.number(occ.end_column)
	w.key('node_kind')
	w.string(occ.node_kind)
	w.key('is_declaration')
	w.boolean(occ.declaration)
	w.end_object()
	return w.str()
}

// declaration_at_json renders the declaration of `name` in `a`, or an empty
// object when the name is not declared in this file.
fn declaration_at_json(ws &Workspace, a &flat.FlatAst, name string) string {
	for decl in astquery.declarations(a) {
		if decl.name == name {
			return declaration_json(ws, decl)
		}
	}
	return '{}'
}

// spec_references declares `v_references`.
fn spec_references() ToolSpec {
	return read_only_spec('v_references',
		'Find every mention of a name in a file, AST aware, so a comment or a
string that contains the name is not reported. Each hit says whether it is the
declaration. This is the tool to reach for before a rename.',
		input_schema(['path', 'name'], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'The .v or .vsh file to search.'
			}
			'name': SchemaProperty{
				kind:        'string'
				description: 'The identifier to find.'
			}
		}),
		tool_references)
}

// tool_references answers `v_references`.
fn tool_references(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	name := args.required_str('name') or { return error_json(err.msg()) }
	found := astquery.references(astquery.parse(path), name)
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(path))
	w.key('name')
	w.string(name)
	w.key('count')
	w.number(found.len)
	w.key('occurrences')
	w.begin_array()
	for occ in found {
		w.array_raw(occurrence_json(ws, occ))
	}
	w.end_array()
	w.end_object()
	return w.str()
}

// spec_stdlib_doc declares `v_stdlib_doc`.
fn spec_stdlib_doc() ToolSpec {
	return read_only_spec('v_stdlib_doc',
		'Look up the documentation of a standard library module or one of its
symbols, for example `strings` or `strings.Builder`. Use it to check a signature
before writing a call instead of guessing.',
		input_schema(['symbol'], {
			'symbol': SchemaProperty{
				kind:        'string'
				description: 'A module name such as `os`, or a\nsymbol such as `os.read_file`.'
			}
		}), tool_stdlib_doc)
}

// tool_stdlib_doc answers `v_stdlib_doc`.
fn tool_stdlib_doc(ws &Workspace, arguments string) string {
	symbol := decode_args(arguments).required_str('symbol') or {
		return error_json(err.msg())
	}
	return stdlib_doc_json(ws, symbol)
}
