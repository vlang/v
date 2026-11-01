// Optional delegation to VLS, the V Language Server.
//
// These tools are only registered when a `vls` binary is found on PATH (or
// pointed at by VLS_PATH). They give an agent access to VLS's project-aware
// diagnostics without leaving the V MCP session.
module main

import os

fn spec_vls_check() ToolSpec {
	return read_only_spec('v_vls_check',
		'Compile-check a V source file using VLS and return its diagnostics as JSON. Only available when `vls` is installed.',
		input_schema(['path'], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'The .v file to check.'
			}
		}), tool_vls_check)
}

fn spec_vls_hover() ToolSpec {
	return read_only_spec('v_vls_hover',
		'Hover information at a position in a V source file using VLS. Only available when `vls` is installed.',
		input_schema(['path', 'line', 'character'], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'The .v file.'
			}
			'line': SchemaProperty{
				kind:        'integer'
				description: 'Zero-based line number.'
			}
			'character': SchemaProperty{
				kind:        'integer'
				description: 'Zero-based character offset.'
			}
		}), tool_vls_hover)
}

fn tool_vls_check(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.exists(path) {
		return error_json('`${path}` does not exist')
	}
	vls := find_vls_binary() or { return error_json('vls not found on PATH') }
	run := os.exec([vls, 'check', path])
	if run.exit_code != 0 {
		return error_json('vls check failed: ${run.output}')
	}
	return run.output
}

fn tool_vls_hover(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.exists(path) {
		return error_json('`${path}` does not exist')
	}
	line := args.int('line', 0)
	col := args.int('character', 0)
	vls := find_vls_binary() or { return error_json('vls not found on PATH') }
	run := os.exec([vls, 'hover', path, line.str(), col.str()])
	if run.exit_code != 0 {
		return error_json('vls hover failed: ${run.output}')
	}
	return run.output
}

fn spec_vls_definition() ToolSpec {
	return read_only_spec('v_vls_definition',
		'Go to definition at a position using VLS.',
		input_schema(['path', 'line', 'character'], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'The .v file.'
			}
			'line': SchemaProperty{
				kind:        'integer'
				description: 'Zero-based line number.'
			}
			'character': SchemaProperty{
				kind:        'integer'
				description: 'Zero-based character offset.'
			}
		}), tool_vls_definition)
}

fn tool_vls_definition(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.exists(path) {
		return error_json('`${path}` does not exist')
	}
	line := args.int('line', 0)
	col := args.int('character', 0)
	vls := find_vls_binary() or { return error_json('vls not found on PATH') }
	run := os.exec([vls, 'definition', path, line.str(), col.str()])
	if run.exit_code != 0 {
		return error_json('vls definition failed: ${run.output}')
	}
	return run.output
}

fn spec_vls_references() ToolSpec {
	return read_only_spec('v_vls_references',
		'Find references at a position using VLS.',
		input_schema(['path', 'line', 'character'], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'The .v file.'
			}
			'line': SchemaProperty{
				kind:        'integer'
				description: 'Zero-based line number.'
			}
			'character': SchemaProperty{
				kind:        'integer'
				description: 'Zero-based character offset.'
			}
		}), tool_vls_references)
}

fn tool_vls_references(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.exists(path) {
		return error_json('`${path}` does not exist')
	}
	line := args.int('line', 0)
	col := args.int('character', 0)
	vls := find_vls_binary() or { return error_json('vls not found on PATH') }
	run := os.exec([vls, 'references', path, line.str(), col.str()])
	if run.exit_code != 0 {
		return error_json('vls references failed: ${run.output}')
	}
	return run.output
}

fn spec_vls_rename() ToolSpec {
	return writing_spec('v_vls_rename',
		'Rename a symbol at a position using VLS.',
		input_schema(['path', 'line', 'character', 'newName'], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'The .v file.'
			}
			'line': SchemaProperty{
				kind:        'integer'
				description: 'Zero-based line number.'
			}
			'character': SchemaProperty{
				kind:        'integer'
				description: 'Zero-based character offset.'
			}
			'newName': SchemaProperty{
				kind:        'string'
				description: 'The new name.'
			}
		}), tool_vls_rename)
}

fn tool_vls_rename(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.exists(path) {
		return error_json('`${path}` does not exist')
	}
	line := args.int('line', 0)
	col := args.int('character', 0)
	new_name := args.text('newName', '')
	if new_name == '' {
		return error_json('missing newName')
	}
	vls := find_vls_binary() or { return error_json('vls not found on PATH') }
	run := os.exec([vls, 'rename', path, line.str(), col.str(), new_name])
	if run.exit_code != 0 {
		return error_json('vls rename failed: ${run.output}')
	}
	return run.output
}

fn find_vls_binary() ?string {
	p := os.getenv('VLS_PATH')
	if p != '' && os.exists(p) {
		return p
	}
	// Search PATH.
	for dir in os.getenv('PATH').split(os.path_delimiter) {
		candidate := os.join_path(dir, 'vls')
		if os.exists(candidate) {
			return candidate
		}
		candidate_exe := os.join_path(dir, 'vls.exe')
		if os.exists(candidate_exe) {
			return candidate_exe
		}
	}
	return none
}