// Tools that change files: an exact text splice, an AST aware rename, and the
// formatter.
//
// Every one of them fails closed. A splice states the text it expects to find; a
// rename states the occurrences it counted. If the file moved under the agent,
// the tool reports what it found instead of writing over the difference. That is
// the difference between a refactor and data loss.
module main

import os
import v.astjson
import v.gen.v as vfmt
import v.parser
import v.pref

// spec_edit_replace declares `v_edit_replace`.
fn spec_edit_replace() ToolSpec {
	return writing_spec('v_edit_replace',
		"Replace an exact range of lines in a file. Provide `expected_old` with the
current text of that range: the write happens only if the file still matches it,
so an edit never silently discards someone else's change. Pass an empty
`expected_old` to insert, and omit `new_text` to delete the range.",
		input_schema(['path', 'start_line'], {
			'path':         SchemaProperty{
				kind:        'string'
				description: 'The file to change.'
			}
			'start_line':   SchemaProperty{
				kind:        'integer'
				description: '1-based first line of the\nrange.'
			}
			'end_line':     SchemaProperty{
				kind:        'integer'
				description: '1-based last line of the range.\nDefaults to `start_line`.'
			}
			'expected_old': SchemaProperty{
				kind:        'string'
				description: 'The exact current text of\nthe range, including its trailing newline. An empty string means the range must\nbe empty.'
			}
			'new_text':     SchemaProperty{
				kind:        'string'
				description: 'The replacement text.'
			}
			'create_dirs':  SchemaProperty{
				kind:        'boolean'
				description: 'Create missing parent\ndirectories. Defaults to false.'
			}
		}), tool_edit_replace)
}

// tool_edit_replace answers `v_edit_replace`.
fn tool_edit_replace(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	start_line := args.int('start_line', 0)
	end_line := args.int('end_line', start_line)
	if start_line < 1 || end_line < start_line {
		return error_json('`start_line` must be 1 or more and `end_line` at least `start_line`')
	}
	expected := args.text('expected_old', '')
	new_text := args.text('new_text', '')
	mut contents := ''
	if os.is_file(path) {
		contents = os.read_file(path) or {
			return error_json('could not read `${ws.relative(path)}`: ${err.msg()}')
		}
	}
	lines := contents.split_into_lines()
	if start_line > lines.len + 1 {
		return error_json('`start_line` ${start_line} is past the end of the file, which has ${lines.len}
lines')
	}
	// `expected_old` is required whenever the file already exists, so a caller
	// cannot write without saying what it expects to replace.
	if os.is_file(path) && !args.has('expected_old') {
		return error_json('`expected_old` is required: read the range first and pass it back verbatim')
	}
	// An insert names an empty `expected_old` and puts text at `start_line` without
	// touching the line that is there. It replaces nothing, so there is nothing to
	// guard and no line to remove.
	inserting := expected == '' && new_text != '' && end_line == start_line
	// An insert drops nothing: `drop_to` sits just before `start_line`, which keeps
	// the line that is already there.
	drop_to := if inserting { start_line - 1 } else { end_line }
	if !inserting && slice_text(lines, start_line, end_line) != expected {
		return object(text_pair('error', 'the file does not match `expected_old`'),
			text_pair('expected', expected), text_pair('actual', slice_text(lines,
				start_line, end_line)), text_pair('path', ws.relative(path)))
	}
	mut out := []string{}
	for i, line in lines {
		if i + 1 == start_line && new_text != '' {
			for inserted in new_text.trim_right('\n').split_into_lines() {
				out << inserted
			}
		}
		if i + 1 < start_line || i + 1 > drop_to {
			out << line
		}
	}
	if new_text != '' && start_line > lines.len {
		// Appending past the last line has no line to insert before.
		for inserted in new_text.trim_right('\n').split_into_lines() {
			out << inserted
		}
	}
	result := join_lines(out)
	if !os.is_file(path) && !args.boolean('create_dirs', false) {
		return error_json('`${ws.relative(path)}` does not exist; pass `create_dirs` to create it')
	}
	os.mkdir_all(os.dir(path)) or { return error_json(err.msg()) }
	os.write_file(path, result) or { return error_json(err.msg()) }
	return object(text_pair('path', ws.relative(path)),
		text_pair('bytes_written', result.len.str()),
		text_pair('start_line', start_line.str()),
		text_pair('end_line', end_line.str()))
}

// join_lines renders a line array back into file text, keeping the final newline
// a source file conventionally has.
pub fn join_lines(lines []string) string {
	if lines.len == 0 {
		return ''
	}
	return lines.join('\n') + '\n'
}

// slice_text returns the text of lines `start_line` to `end_line`, inclusive,
// including each line's newline.
fn slice_text(lines []string, start_line int, end_line int) string {
	if start_line > lines.len {
		return ''
	}
	last := if end_line > lines.len { lines.len } else { end_line }
	mut out := []string{}
	for i in start_line - 1 .. last {
		out << lines[i] + '\n'
	}
	return out.join('')
}

// spec_rename_symbol declares `v_rename_symbol`.
fn spec_rename_symbol() ToolSpec {
	return writing_spec('v_rename_symbol',
		'Rename a symbol across files, AST aware, so only real mentions are changed
and a comment or string that happens to hold the name is left alone. Defaults to
`dry_run: true`: the response lists the exact edits, and the same call with
`dry_run: false` applies them.',
		input_schema(['name', 'new_name'], {
			'name':     SchemaProperty{
				kind:        'string'
				description: 'The current name.'
			}
			'new_name': SchemaProperty{
				kind:        'string'
				description: 'The new name. Must be a valid V\nidentifier.'
			}
			'paths':    SchemaProperty{
				kind:        'array'
				description: 'Files to\nchange. Defaults to every V file in the project.'
			}
			'dry_run':  SchemaProperty{
				kind:        'boolean'
				description: 'Report the edits without writing\nthem. Defaults to true.'
			}
		}), tool_rename_symbol)
}

// tool_rename_symbol answers `v_rename_symbol`.
fn tool_rename_symbol(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	old_name := args.required_str('name') or { return error_json(err.msg()) }
	new_name := args.required_str('new_name') or { return error_json(err.msg()) }
	if !is_identifier(new_name) {
		return error_json('`${new_name}` is not a valid V identifier')
	}
	if new_name == old_name {
		return error_json('`new_name` is the same as `name`')
	}
	targets := ws.rename_targets(args.list('paths')) or { return error_json(err.msg()) }
	dry_run := args.boolean('dry_run', true)
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('name')
	w.string(old_name)
	w.key('new_name')
	w.string(new_name)
	w.key('dry_run')
	w.boolean(dry_run)
	w.key('files')
	w.begin_array()
	mut total := 0
	mut changed := 0
	for file in targets {
		hits := rename_hits(file, old_name)
		if hits.len == 0 {
			continue
		}
		changed++
		total += hits.len
		w.array_raw(rename_file_json(ws, file, hits, old_name, new_name, dry_run))
	}
	w.end_array()
	w.key('files_changed')
	w.number(changed)
	w.key('edits')
	w.number(total)
	w.end_object()
	return w.str()
}

// is_identifier reports whether `name` may be spelled as a V identifier.
fn is_identifier(name string) bool {
	if name == '' || name[0] in [`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`] {
		return false
	}
	for c in name {
		if !(c.is_alnum() || c == `_`) {
			return false
		}
	}
	return true
}

// spec_format declares `v_format`.
fn spec_format() ToolSpec {
	return writing_spec('v_format',
		'Format a V file with the same formatter `v fmt` uses. Defaults to
`dry_run: true` and reports the unified-style before/after lines, so a
reformat is reviewable before it is written.',
		input_schema(['path'], {
			'path':  SchemaProperty{
				kind:        'string'
				description: 'The .v file to format.'
			}
			'write': SchemaProperty{
				kind:        'boolean'
				description: 'Write the formatted result back.\nDefaults to false.'
			}
		}), tool_format)
}

// tool_format answers `v_format`.
fn tool_format(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.is_file(path) {
		return error_json('`${path}` is not a file')
	}
	before := os.read_file(path) or { return error_json(err.msg()) }
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.is_fmt = true
	prefs.preserve_comptime_conditionals = true
	prefs.supports_inline_asm = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(path)
	mut errors := []Diagnostic{}
	for diagnostic in p.diagnostics {
		if diagnostic.severity !in ['', 'error:'] {
			continue
		}
		errors << Diagnostic{
			path:    ws.relative(diagnostic.file)
			line:    diagnostic.line
			column:  diagnostic.column
			kind:    'error'
			message: diagnostic.message
		}
	}
	if errors.len > 0 {
		return object(text_pair('error', 'the file contains parser errors'),
			raw_pair('diagnostics', diagnostics_json(errors)))
	}
	after := vfmt.format(a)
	changed := before != after
	if changed && args.boolean('write', false) {
		os.write_file(path, after) or { return error_json(err.msg()) }
	}
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(path))
	w.key('changed')
	w.boolean(changed)
	w.key('written')
	w.boolean(changed && args.boolean('write', false))
	if changed {
		w.key('before')
		w.string(before)
		w.key('after')
		w.string(after)
	}
	w.end_object()
	return w.str()
}
