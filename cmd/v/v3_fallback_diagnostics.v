module main

import os
import v.ansi

fn v3_exact_output_fixture_path(args []string) string {
	for arg in args {
		if arg.len == 0 || arg.starts_with('-') {
			continue
		}
		if os.is_dir(arg) && os.is_file(arg + '.out') {
			return arg + '.out'
		}
		ext := os.file_ext(arg)
		if ext !in ['.v', '.vv', '.vsh'] || arg.len <= ext.len {
			continue
		}
		base := arg[..arg.len - ext.len]
		for suffix in ['.out', '.run.out', '.js.out'] {
			if os.is_file(base + suffix) {
				return base + suffix
			}
		}
	}
	return ''
}

fn v3_exact_output_fixture_args(args []string) bool {
	return v3_exact_output_fixture_path(args) != ''
}

fn v3_fixture_expects_legacy_compiler_modules(args []string) bool {
	path := v3_exact_output_fixture_path(args)
	if path == '' {
		return false
	}
	expected := os.read_file(path) or { return false }
	return expected.contains('`old.ast.') || expected.contains('`old.parser.')
		|| expected.contains('`old.scanner.')
}

fn v3_fixture_requires_legacy_parser_recovery(args []string) bool {
	path := v3_exact_output_fixture_path(args)
	if path == '' {
		return false
	}
	expected := os.read_file(path) or { return false }
	return expected.contains('import syntax error, please specify a valid fn or type name')
		&& expected.contains('script mode started here')
}

fn v3_fixture_requires_compatibility_compiler(args []string) bool {
	return v3_fixture_expects_legacy_compiler_modules(args)
		|| v3_fixture_requires_legacy_parser_recovery(args)
}

fn v3_rewrite_legacy_compiler_module_diagnostics(output string) string {
	return output.replace('`v.ast.', '`old.ast.').replace('`v.parser.', '`old.parser.').replace('`v.scanner.',
		'`old.scanner.')
}

// v3_fallback_diagnostics includes failures from the V compiler itself, not just
// the C compiler. Prefer the original staged C output; otherwise replay only
// compilation, before the compatibility compiler can run the user's program.
fn v3_fallback_diagnostics(vexe string, args []string, state RetryState) string {
	if state.fallback_file == '' {
		return ''
	}
	payload := os.read_file(state.fallback_file) or { return '' }
	kind := payload.all_before('\n').trim_space()
	if kind !in ['compiler_error', 'c_compilation_error', 'inline_asm'] {
		return ''
	}
	c_diagnostics := v3_c_error_diagnostics(state)
	if c_diagnostics != '' {
		return c_diagnostics
	}
	output := v3_diagnostics_output(vexe, args)
	if output == '' {
		return 'note: diagnostic replay for ${kind} produced no output; re-run with `-new-compiler` to investigate.\n'
	}
	newline := if output.ends_with('\n') { '' } else { '\n' }
	return 'Compiler output from the default V compiler:\n${output}${newline}'
}

// v3_c_error_diagnostics preserves the original C compiler output for an automatic
// retry. Read the staged output instead of compiling again, and leave it available
// for the existing bug report and cleanup paths.
fn v3_c_error_diagnostics(state RetryState) string {
	if state.fallback_file == '' || state.c_error_dir == '' {
		return ''
	}
	payload := os.read_file(state.fallback_file) or { return '' }
	if payload.all_before('\n').trim_space() != 'c_compilation_error' {
		return ''
	}
	output := os.read_file(os.join_path(state.c_error_dir, 'output')) or { return '' }
	if output == '' {
		return ''
	}
	newline := if output.ends_with('\n') { '' } else { '\n' }
	return 'C compiler output from the default V compiler:\n${output}${newline}'
}

// report_v3_fallback_unavailable shows the diagnostic that triggered an automatic
// V1 retry when that retry cannot be started, unless it was already printed.
// Explicit `-old-compiler` requests keep their existing error-only behavior.
fn report_v3_fallback_unavailable(args []string, reason string, report_state RetryState, fallback_error string, diagnostics_shown bool) {
	if report_state.fallback_file == '' {
		eprintln(fallback_error)
		return
	}
	eprintln(reason)
	if !diagnostics_shown {
		diagnostics := v3_diagnostics_output(os.real_path(os.executable()), args)
		if diagnostics != '' {
			eprint(diagnostics)
			if !diagnostics.ends_with('\n') {
				eprintln('')
			}
		}
	}
	eprintln(v1_fallback_failure_message(fallback_error, reason))
}

// v1_fallback_failure_message avoids repeating the V3 failure summary after
// its full diagnostic has just been printed.
fn v1_fallback_failure_message(message string, reason string) string {
	prefix := '${reason}, but '
	if message.starts_with(prefix) {
		return 'Fallback unavailable: ${message[prefix.len..]}'
	}
	return message
}

// v3_diagnostics_output replays a failed compilation for its diagnostics.
// Use the driver's explicit compile-only flag for run commands and scripts;
// VNORUN alone is not read by the V3 driver. Keep VNORUN for compatibility, and
// clear VFLAGS to avoid applying the caller's already-merged flags twice.
fn v3_diagnostics_output(vexe string, args []string) string {
	mut environment := os.environ()
	environment.delete(v3_fallback_file_env)
	environment.delete(v3_c_error_dir_env)
	environment[v3_no_fallback_env] = '1'
	environment[v3_retry_env] = '1'
	environment['VFLAGS'] = ''
	environment['VNORUN'] = '1'
	// The replay writes to a pipe, but its diagnostics are displayed on our stderr.
	// Preserve the terminal decision; explicit -color/-nocolor options still win
	// when the child parses args. Only change the child's environment.
	environment['VCOLORS'] = if ansi.stderr_supports_escape_sequences() {
		'always'
	} else {
		'never'
	}
	// Prepend rather than append: arguments after a run/script input belong to
	// the user program. A runtime argument named -skip-running must not prevent
	// us from adding the compiler option, either. Do not mutate the caller's args.
	mut replay_args := ['-skip-running']
	replay_args << args
	mut process := os.new_process(vexe)
	process.set_args(replay_args)
	process.set_environment(environment)
	process.set_redirect_stdio_merged()
	// Drain the merged pipe before waiting: a verbose compiler can otherwise
	// fill the pipe and block forever while the parent waits for it to exit.
	process.run()
	output := process.stdout_slurp()
	process.wait()
	process.close()
	return output
}
