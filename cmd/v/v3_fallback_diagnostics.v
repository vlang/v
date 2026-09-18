module main

import os

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

// v3_diagnostics_output reruns only the failed compilation path. VNORUN keeps
// run-like commands from executing user code if the second compile happens to
// succeed, while clearing VFLAGS avoids applying already-merged flags twice.
fn v3_diagnostics_output(vexe string, args []string) string {
	mut environment := os.environ()
	environment.delete(v3_fallback_file_env)
	environment.delete(v3_c_error_dir_env)
	environment[v3_no_fallback_env] = '1'
	environment[v3_retry_env] = '1'
	environment['VFLAGS'] = ''
	environment['VNORUN'] = '1'
	mut process := os.new_process(vexe)
	process.set_args(args)
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
