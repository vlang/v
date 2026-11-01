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