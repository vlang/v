module cmdexec

import os
import strings
import time

// no_timeout waits for the child process for as long as it takes.
pub const no_timeout = i64(0)

// timeout_drain_ms bounds how long a timed out run keeps collecting whatever
// the killed command already wrote into its pipes.
const timeout_drain_ms = i64(200)

// run executes program with an exact argument vector and captures its output.
pub fn run(program string, args []string) os.Result {
	return run_in(program, args, '')
}

// run_in executes program in work_folder with an exact argument vector.
pub fn run_in(program string, args []string, work_folder string) os.Result {
	return run_in_mode(program, args, work_folder, false, no_timeout)
}

// run_with_timeout is run, bounded: after timeout_ms milliseconds the child is
// killed and a non zero result is returned. Use it for short probes that must
// never be able to block a build, so that a child which can never make progress
// is reported instead of hanging the compiler forever.
pub fn run_with_timeout(program string, args []string, timeout_ms i64) os.Result {
	return run_in_mode(program, args, '', false, timeout_ms)
}

// windows_implicit_suffixes are the extensions CreateProcessW appends to a
// program path that carries none. os.is_executable only accepts a name that
// already ends in one of them, so an extension-less `C:\\LLVM\\bin\\clang`
// has to be probed with each of these before it counts as missing.
const windows_implicit_suffixes = ['.exe', '.com', '.bat', '.cmd']

// resolve_executable returns the program path to hand to os.Process, or none
// when nothing can be started. It mirrors os.Process's own resolution:
//   - Windows expands every filename with abs_path() before CreateProcessW, so
//     even a bare name is looked up next to the caller's folder rather than
//     through PATH.
//   - Unix keeps an absolute path as is, resolves a path that carries a
//     separator against the *caller's* folder when a work folder was set (the
//     child execve()s that absolute path after it has changed folders), and
//     otherwise searches PATH.
//
// It returns the resolved path rather than a yes/no, because on Windows the
// candidate that exists may not be the name the caller passed: CreateProcessW
// binds an absolute non-batch filename as lpApplicationName, so a program that
// only exists as `${program}.exe` has to be launched under *that* name, or the
// launch fails even though the file is right there.
fn resolve_executable(program string, work_folder string) ?string {
	if program == '' {
		return none
	}
	$if windows {
		if program.contains('%') {
			// CreateProcessW expands `%VAR%` itself. Probing the literal would
			// reject `%COMSPEC%` and friends, which launch fine today.
			return program
		}
		return windows_resolve_executable(os.abs_path(program))
	} $else {
		if os.is_abs_path(program) {
			return if os.is_executable(program) { program } else { none }
		}
		if program.contains(os.path_separator) {
			probe := if work_folder.len > 0 { os.abs_path(program) } else { program }
			return if os.is_executable(probe) { program } else { none }
		}
		os.find_abs_path_of_executable(program) or { return none }
		return program
	}
}

// windows_resolve_executable returns the candidate CreateProcessW would load,
// appending the implicit extensions when the path carries none. Every branch
// errs towards letting the launch proceed: this check only exists to turn an
// unstartable command into a result instead of an abort, so a false "missing"
// would break callers that work today, while a false "present" merely restores
// the previous behaviour.
fn windows_resolve_executable(candidate string) ?string {
	if os.is_executable(candidate) {
		return candidate
	}
	if os.file_ext(candidate) != '' {
		// os.is_executable only accepts the conventional extensions, but
		// CreateProcessW runs any module it can load - including a PE binary
		// deliberately named `clang.bin`. Existence is the honest test here.
		return if os.is_file(candidate) { candidate } else { none }
	}
	for suffix in windows_implicit_suffixes {
		probed := candidate + suffix
		if os.is_executable(probed) {
			return probed
		}
	}
	// An extension-less file can still be a loadable module.
	return if os.is_file(candidate) { candidate } else { none }
}

fn run_in_mode(program string, args []string, work_folder string, merge_output bool, timeout_ms i64) os.Result {
	// Decide here whether the command can start at all. On Windows a
	// CreateProcess that cannot find the program makes os.Process abort the
	// whole compiler, with a message that does not even name what was missing;
	// a missing command has to be an ordinary failing result that the caller
	// can report, or fall back from, on every host.
	launch_program := resolve_executable(program, work_folder) or {
		return os.Result{
			exit_code: 127
			output: 'failed to find executable: ${program}\n'
		}
	}
	mut process := os.new_process(launch_program)
	process.set_args(args)
	if work_folder.len > 0 {
		process.set_work_folder(work_folder)
	}
	if timeout_ms > 0 {
		// A bounded run must be able to take down everything the command
		// started, not only the direct child: a descendant that inherited the
		// stdout/stderr pipes - `sh -c '... & wait'` and shell wrappers in
		// general - would otherwise keep the write ends open past the deadline.
		// Only bounded runs get their own process group, so unbounded ones (the
		// C compiler and linker invocations) keep sharing the caller's group,
		// and keep reacting to a Ctrl-C on the build, exactly as before.
		process.use_pgroup = true
	}
	if merge_output {
		process.set_redirect_stdio_merged()
	} else {
		process.set_redirect_stdio()
	}
	process.run()
	mut output := strings.new_builder(1024)
	mut timed_out := false
	sw := time.new_stopwatch()
	for process.is_alive() {
		stdout := process.stdout_read()
		stderr := if merge_output { '' } else { process.stderr_read() }
		output.write_string(stdout)
		output.write_string(stderr)
		// The deadline is checked on every iteration, not only when nothing was
		// read: a child that keeps writing must hit the bound just the same.
		if timeout_ms > 0 && sw.elapsed().milliseconds() >= timeout_ms {
			timed_out = true
			// Kill the group first, so that descendants holding the pipes go
			// away too, then the child itself, in case it had not reached its
			// setpgid() yet when the group kill was delivered.
			process.signal_pgkill()
			process.signal_kill()
			// signal_kill() marks the process `.aborted` before it has been
			// reaped, and wait() skips waitpid() for a process that is not
			// running, which would leave a zombie behind until this process
			// exits. Put it back into a waitable state, so that the wait()
			// below actually reaps it.
			if process.status == .aborted {
				process.status = .running
			}
			break
		}
		if stdout.len == 0 && stderr.len == 0 {
			time.sleep(time.millisecond)
		}
	}
	process.wait()
	if timed_out {
		eprintln('V: `${display(program, args)}` did not finish within ${timeout_ms}ms, the child process was killed')
		// Never block on EOF after the deadline: the slurps on the normal path
		// wait until every writer of the pipe is gone, and a descendant that
		// escaped the group kill still owns one. Collect what is already
		// buffered instead.
		drain := time.new_stopwatch()
		for drain.elapsed().milliseconds() < timeout_drain_ms {
			stdout := process.stdout_read()
			stderr := if merge_output { '' } else { process.stderr_read() }
			if stdout.len == 0 && stderr.len == 0 {
				break
			}
			output.write_string(stdout)
			output.write_string(stderr)
		}
	} else {
		output.write_string(process.stdout_slurp())
		if !merge_output {
			output.write_string(process.stderr_slurp())
		}
	}
	if process.err.len > 0 {
		output.write_string(process.err)
		output.write_string(': ')
		output.writeln(program)
	}
	exit_code := if timed_out {
		if process.code > 0 { process.code } else { 1 }
	} else if process.code >= 0 {
		process.code
	} else {
		1
	}
	process.close()
	mut output_text := output.str()
	if exit_code != 0 && output_text.contains('os: failed to find executable')
		&& !output_text.contains(program) {
		output_text += 'executable: ${program}\n'
	}
	return os.Result{
		exit_code: exit_code
		output: output_text
	}
}

// run_in_merged executes a command with an exact argument vector and captures
// both stdout and stderr.
pub fn run_in_merged(program string, args []string, work_folder string) os.Result {
	return run_in_mode(program, args, work_folder, true, no_timeout)
}

// split_args parses a directive or tool response into literal argv elements.
// Quotes and quoting backslash escapes group text; no shell expansion is performed.
pub fn split_args(input string) ![]string {
	mut args := []string{}
	mut current := strings.new_builder(input.len)
	mut quote := u8(0)
	mut has_arg := false
	mut i := 0
	for i < input.len {
		ch := input[i]
		if quote == 0 && ch in [` `, `\t`, `\r`, `\n`] {
			if has_arg {
				args << current.str()
				current = strings.new_builder(input.len - i)
				has_arg = false
			}
			i++
			continue
		}
		if ch in [`'`, `"`] {
			if quote == 0 {
				quote = ch
				has_arg = true
				i++
				continue
			}
			if quote == ch {
				quote = 0
				i++
				continue
			}
		}
		if ch == `\\` && quote != `'` {
			if i + 1 < input.len {
				next := input[i + 1]
				escapable := if quote == `"` {
					next in [`"`, `\\`]
				} else {
					next in [` `, `\t`, `\r`, `\n`, `'`, `"`, `\\`]
				}
				if escapable {
					current.write_u8(next)
					has_arg = true
					i += 2
					continue
				}
			}
			current.write_u8(ch)
			has_arg = true
			i++
			continue
		}
		current.write_u8(ch)
		has_arg = true
		i++
	}
	if quote != 0 {
		return error('unterminated quote in argument list')
	}
	if has_arg {
		args << current.str()
	}
	return args
}

// display returns a shell-escaped representation for logging only.
pub fn display(program string, args []string) string {
	mut parts := []string{cap: args.len + 1}
	parts << display_arg(program)
	for arg in args {
		parts << display_arg(arg)
	}
	return parts.join(' ')
}

fn display_arg(arg string) string {
	if arg.len > 0 {
		mut plain := true
		for ch in arg {
			if !(ch.is_alnum() || ch in [`_`, `-`, `.`, `/`, `\\`, `:`, `=`, `+`, `,`, `@`]) {
				plain = false
				break
			}
		}
		if plain {
			return arg
		}
	}
	return os.quoted_path(arg)
}
