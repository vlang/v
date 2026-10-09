// Tools that ask the compiler itself: does this compile, and do these tests pass.
//
// These are the only tools that must start a compiler. Everything else works on
// the AST alone, so a session that only inspects code never pays for a build.
module main

import os
import v.astjson

// spec_check declares `v_check`.
fn spec_check() ToolSpec {
	return read_only_spec('v_check',
		'Type-check V code and return the compiler diagnostics as records with file,
line, column, kind and message. Points at one file or one directory. It compiles
nothing and writes nothing.',
		input_schema(['path'], {
			'path':            SchemaProperty{
				kind:        'string'
				description: 'The .v file or directory to check.'
			}
			'flags':           SchemaProperty{
				kind:        'array'
				description: 'Extra\ncompiler flags, for example `["-stats"]`.'
			}
			'max_diagnostics': SchemaProperty{
				kind:        'integer'
				description: 'Limit (100); counts stay totals.'
			}
		}), tool_check)
}

// tool_check answers `v_check`.
fn tool_check(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.exists(path) {
		return error_json('`${path}` does not exist')
	}
	// Flags go before the subcommand, as `v` documents, and are handed over raw
	// because `run_compiler` owns the quoting.
	mut compiler_args := args.list('flags')
	compiler_args << ['-check', path]
	run := run_compiler(ws, compiler_args)
	items := parse_diagnostics(run.output)
	return check_json(ws, path, run, items, args.int('max_diagnostics', max_diagnostics_default))
}

// check_json renders the outcome of a check.
fn check_json(ws &Workspace, path string, run CompilerRun, items []Diagnostic, max_diagnostics int) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(path))
	w.key('command')
	w.string(run.command)
	if !run.started() {
		// The compiler never ran, so there is no result to report and no exit code
		// to read. Saying so is the whole answer; an `error_count` of zero here
		// would look like a clean check.
		w.key('started')
		w.boolean(false)
		w.key('error')
		w.string('the compiler could not be started: ${run.launch_error}')
		w.end_object()
		return w.str()
	}
	w.key('started')
	w.boolean(true)
	w.key('exit_code')
	w.number(run.exit_code)
	w.key('error_count')
	w.number(count_errors(items))
	w.key('warning_count')
	w.number(count_by_kind(items, 'warning'))
	kept, omitted := page_diagnostics(items, max_diagnostics)
	w.key_raw('diagnostics', diagnostics_json(kept))
	w.key('diagnostics_omitted')
	w.number(omitted)
	if omitted > 0 {
		w.key('hint')
		w.string('Only the first ${kept.len} diagnostics are shown; narrow the path or raise `max_diagnostics`.')
	}
	if items.len == 0 && run.exit_code != 0 {
		// The compiler failed without saying why, which is worth showing raw.
		w.key('output')
		w.string(trim_output(run.output, 60))
	}
	w.end_object()
	return w.str()
}

// count_by_kind returns how many diagnostics carry `kind`.
fn count_by_kind(items []Diagnostic, kind string) int {
	mut n := 0
	for item in items {
		if item.kind == kind {
			n++
		}
	}
	return n
}

// spec_test_run declares `v_test_run`.
fn spec_test_run() ToolSpec {
	return read_only_spec('v_test_run',
		'Run the V tests of a file or directory and report what passed, what failed
and what the failures said. Takes the same filters as `v test`, for example a
`VTEST_ONLY` pattern through `env`.',
		input_schema([], {
			'path':            SchemaProperty{
				kind:        'string'
				description: 'The test file or directory. Defaults\nto the project root.'
			}
			'only':            SchemaProperty{
				kind:        'string'
				description: 'Run only tests whose name matches this\nglob, as VTEST_ONLY does.'
			}
			'silent':          SchemaProperty{
				kind:        'boolean'
				description: 'Hide passing tests. Defaults to\ntrue.'
			}
			'flags':           SchemaProperty{
				kind:        'array'
				description: 'Extra\ncompiler flags.'
			}
			'max_diagnostics': SchemaProperty{
				kind:        'integer'
				description: 'Limit (100); counts stay totals.'
			}
		}), tool_test_run)
}

// tool_test_run answers `v_test_run`.
fn tool_test_run(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_or_root(args.text('path', '')) or { return error_json(err.msg()) }
	mut compiler_args := args.list('flags')
	compiler_args << ['-silent', 'test', path]
	// `VTEST_ONLY` is read by the test runner itself, not by the compiler, so it
	// has to reach the child through the environment.
	//
	// The previous value is put back rather than merely cleared: this server is a
	// long-lived process, and a filter left behind by one `v_test_run` would
	// silently narrow every later one.
	only := args.text('only', '')
	saved := os.getenv_opt('VTEST_ONLY') or { '' }
	os.setenv('VTEST_ONLY', only, true)
	run := run_compiler(ws, compiler_args)
	if saved == '' {
		os.unsetenv('VTEST_ONLY')
	} else {
		os.setenv('VTEST_ONLY', saved, true)
	}
	items := parse_diagnostics(run.output)
	return check_json(ws, path, run, items, args.int('max_diagnostics', max_diagnostics_default))
}
