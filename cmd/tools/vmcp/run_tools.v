// Tools that execute code: `v run` on a project target, and a REPL-style
// evaluation of a V snippet.
//
// Both are marked as reaching outside the project (`open_world_hint`), because
// running code is what can have effects the agent cannot see.
module main

import v.astjson

// run_timeout_ms is the documentation of the ceiling these tools impose. Neither
// is enforced by the tool itself: `os.exec` waits for the child, and a build
// that takes minutes is legitimate for a large project. The value is reported so
// an agent knows the wait is unbounded by design.
const run_timeout_note = 'the child is waited on, so a build may take minutes'

// run_output_limit is how many lines of output one response carries before it is
// trimmed, with the tail kept because a compiler explains a failure there.
const run_output_limit = 400

// spec_run declares `v_run`.
fn spec_run() ToolSpec {
	return read_only_spec('v_run',
		'Run a V program: a file, a directory or a module name, with the same
arguments `v run` takes. Returns the exit code and the program output. Writes
whatever the program itself writes; it does not edit the project.',
		input_schema(['target'], {
			'target':          SchemaProperty{
				kind:        'string'
				description: 'A .v file, a directory or a module\nname.'
			}
			'args':            SchemaProperty{
				kind:        'array'
				description: 'Arguments\npassed to the program after the target.'
			}
			'flags':           SchemaProperty{
				kind:        'array'
				description: 'Compiler\nflags, for example `["-g"]`.'
			}
			'max_diagnostics': SchemaProperty{
				kind:        'integer'
				description: 'Limit (100); counts stay totals.'
			}
		}), tool_run)
}

// tool_run answers `v_run`.
fn tool_run(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	target := args.required_str('target') or { return error_json(err.msg()) }
	resolved := ws.resolve(target) or { return error_json(err.msg()) }
	// Flags go before the subcommand. Anything after the source file is handed to
	// the program as its own arguments, so `v run prog.v -d proof=present` would
	// compile `prog.v` with no define at all and pass `-d proof=present` to the
	// program instead, silently running different code from the one requested.
	return run_json(ws, resolved, run_arguments(args.list('flags'), resolved,
		args.list('args')), args.int('max_diagnostics', max_diagnostics_default))
}

// run_arguments builds the `v` argument list for `v_run`.
//
// The order is the whole point: V reads its flags before the subcommand, and
// everything after the source file is the program's own. Splitting this out lets
// the ordering be asserted without starting a compiler.
fn run_arguments(flags []string, target string, program_args []string) []string {
	mut argv := []string{}
	argv << flags
	argv << ['run', target]
	if program_args.len > 0 {
		argv << program_args
	}
	return argv
}

// spec_eval declares `v_eval`.
fn spec_eval() ToolSpec {
	return read_only_spec('v_eval',
		'Evaluate a short V snippet and return its captured output. Useful for
checking an expression or a stdlib call without writing a file. The snippet runs
in this interpreter, so only the subset of V it supports is available; for
anything substantial, write a file and use `v_run`.',
		one_string('code', 'The V snippet to evaluate.'), tool_eval)
}

// tool_eval answers `v_eval`.
fn tool_eval(ws &Workspace, arguments string) string {
	code := decode_args(arguments).required_str('code') or { return error_json(err.msg()) }
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('code')
	w.string(code)
	mut result := eval_text(ws, code)
	w.key('ok')
	w.boolean(result.ok)
	w.key('stdout')
	w.string(result.stdout)
	if result.err != '' {
		w.key('error')
		w.string(result.err)
	}
	w.end_object()
	return w.str()
}

// run_json runs the compiler and renders what it produced.
fn run_json(ws &Workspace, target string, compiler_args []string, max_diagnostics int) string {
	run := run_compiler(ws, compiler_args)
	return run_result_json(ws, target, run, max_diagnostics)
}

// run_result_json renders a completed run without starting another compiler.
fn run_result_json(ws &Workspace, target string, run CompilerRun, max_diagnostics int) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('target')
	w.string(ws.relative(target))
	w.key('command')
	w.string(run.command)
	if !run.started() {
		// The program never ran, so `ok` and `exit_code` would describe nothing.
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
	w.key('ok')
	w.boolean(run.exit_code == 0)
	w.key('note')
	w.string(run_timeout_note)
	w.key('output')
	w.string(trim_output(run.output, run_output_limit))
	items := parse_diagnostics(run.output)
	w.key('error_count')
	w.number(count_errors(items))
	w.key('warning_count')
	w.number(count_by_kind(items, 'warning'))
	if items.len > 0 {
		kept, omitted := page_diagnostics(items, max_diagnostics)
		w.key_raw('diagnostics', diagnostics_json(kept))
		w.key('diagnostics_omitted')
		w.number(omitted)
		if omitted > 0 {
			w.key('hint')
			w.string('Only the first ${kept.len} diagnostics are shown; narrow the path or raise `max_diagnostics`.')
		}
	}
	w.end_object()
	return w.str()
}
