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
// Both output pipes are drained until EOF, including writers inherited by descendants.
// Standard input is empty (the null device); the caller's input is not inherited.
pub fn run(program string, args []string) os.Result {
	return run_in(program, args, '')
}

// run_in executes program in work_folder with an exact argument vector and empty stdin.
pub fn run_in(program string, args []string, work_folder string) os.Result {
	return run_in_mode(program, args, work_folder, false, no_timeout)
}

// run_with_timeout is run, bounded: after timeout_ms milliseconds the child is
// killed and a non zero result is returned. Use it for short probes that must
// never be able to block a build, so that a child which can never make progress
// is reported instead of hanging the compiler forever. The deadline also covers
// output pipes inherited by descendants after the direct child has exited.
pub fn run_with_timeout(program string, args []string, timeout_ms i64) os.Result {
	return run_in_mode(program, args, '', false, timeout_ms)
}

// resolve_program returns the executable to hand to os.Process, or none when
// nothing can be started. Deciding this up front turns a command that cannot
// start into an ordinary failing result that the caller can report, or fall
// back from, instead of letting os.Process abort the whole compiler (as a
// failed CreateProcessW does on Windows).
fn resolve_program(program string) ?string {
	$if windows {
		// os.Process hands CreateProcessW an absolute filename *unexpanded* as
		// lpApplicationName; only the separate command-line buffer goes through
		// ExpandEnvironmentStringsW. Expand `%NAME%` here, both to probe the
		// real path and to launch it.
		return windows_resolve_program(expand_windows_env_vars(program))
	} $else {
		if os.is_executable(program) {
			// Pin the file checked above before Process can interpret a bare name
			// as a fresh PATH lookup or start the child in a different directory.
			return os.abs_path(program)
		}
		if program.contains(os.path_separator) {
			// A path is never looked up on PATH. Relative paths are anchored to the
			// caller's folder, as above, since the child may start elsewhere.
			return unix_launchable(os.abs_path(program))
		}
		return os.find_abs_path_of_executable(program) or { return none }
	}
}

// unix_launchable accepts `path` whenever something may exist there. Only a
// path that is not there at all counts as missing: a file that exists but is
// not executable has to reach os.Process anyway, because the child's execve()
// reports EACCES, and that is the actionable error. Refusing it here would
// claim an existing compiler is missing instead.
fn unix_launchable(path string) ?string {
	if os.exists(path) {
		return path
	}
	// os.exists() is access(F_OK), which also fails when a parent directory is
	// not searchable -- the file may well be there. Only call it missing when the
	// parent is readable and the entry genuinely is not; otherwise let the launch
	// run so execve() can report the real EACCES.
	parent := os.dir(path)
	if parent != path && !os.exists(parent) {
		return path
	}
	return none
}

// windows_implicit_suffixes are the extensions probed for a program *path*
// that carries none. os.Process binds an absolute, non-batch filename as
// lpApplicationName, for which CreateProcessW performs no PATHEXT search, so
// a program that only exists as `${program}.exe` has to be launched under that
// name. Probing `.com`, `.bat` or `.cmd` here would accept a program the real
// launch cannot start.
const windows_implicit_suffixes = ['.exe']

// windows_resolve_program mirrors os.Process on Windows: a bare name is looked
// up on PATH, while a path (absolute, or relative to the caller's folder) names
// exactly one file.
fn windows_resolve_program(program string) ?string {
	if program == '' {
		return none
	}
	// os.is_executable only checks existence plus a recognized extension, so a
	// *directory* called `tool.exe` satisfies it. CreateProcessW cannot start a
	// directory, so require a regular file.
	if os.is_file(program) && os.is_executable(program) {
		// Pin the file checked above before Process can interpret a bare name
		// as a fresh PATH lookup or start the child in a different directory.
		return os.abs_path(program)
	}
	if os.is_abs_path(program) || program.contains('\\') || program.contains('/') {
		return windows_resolve_executable_path(os.abs_path(program))
	}
	return os.find_abs_path_of_executable(program) or { return none }
}

// windows_resolve_executable_path returns the candidate CreateProcessW would
// load, appending the implicit extensions when the path carries none. Every
// branch errs towards letting the launch proceed: this check only exists to
// turn an unstartable command into a result instead of an abort, so a false
// "missing" would break callers that work today, while a false "present"
// merely restores the previous behaviour.
fn windows_resolve_executable_path(candidate string) ?string {
	if os.is_file(candidate) && os.is_executable(candidate) {
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
		if os.is_file(probed) && os.is_executable(probed) {
			return probed
		}
	}
	// An extension-less file can still be a loadable module.
	return if os.is_file(candidate) { candidate } else { none }
}

// expand_windows_env_vars replaces every `%NAME%` with its environment value,
// the way ExpandEnvironmentStringsW does. An unset name, an empty name (`%%`)
// and a trailing unmatched `%` are all left exactly as written, which is also
// what the Windows API does.
fn expand_windows_env_vars(text string) string {
	if !text.contains('%') {
		return text
	}
	parts := text.split('%')
	mut out := strings.new_builder(text.len)
	for i in 0 .. parts.len {
		if i % 2 == 0 {
			out.write_string(parts[i])
			continue
		}
		if i == parts.len - 1 {
			// No closing `%` for this one.
			out.write_string('%')
			out.write_string(parts[i])
			continue
		}
		name := parts[i]
		value := if name == '' { '' } else { os.getenv(name) }
		if value != '' {
			out.write_string(value)
			continue
		}
		out.write_string('%')
		out.write_string(name)
		out.write_string('%')
	}
	return out.str()
}

fn run_in_mode(program string, args []string, work_folder string, merge_output bool, timeout_ms i64) os.Result {
	executable := resolve_program(program) or {
		return os.Result{
			exit_code: 1
			output:    'os: failed to find executable `${program}`\n'
		}
	}
	mut process := os.new_process(executable)
	process.set_args(args)
	// These capture-only helpers expose no stdin writer. Give the child EOF
	// instead of an open pipe that nobody will ever write to or close.
	$if windows {
		process.set_stdin_path('NUL')
	} $else {
		process.set_stdin_path('/dev/null')
	}
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
	mut stdout_done := false
	mut stderr_done := merge_output
	sw := time.new_stopwatch()
	for {
		mut stdout := ''
		mut stderr := ''
		if !stdout_done {
			text, done := read_process_pipe(mut process, .stdout)
			stdout = text
			stdout_done = done
		}
		if !stderr_done {
			text, done := read_process_pipe(mut process, .stderr)
			stderr = text
			stderr_done = done
		}
		output.write_string(stdout)
		output.write_string(stderr)
		// A leader can exit while a descendant still owns both pipes. Keep
		// draining both: slurping stdout first can deadlock on a full stderr
		// pipe, even for an unbounded run. For bounded runs, deferring the reap
		// also reserves the leader PID until a possible process-group kill.
		if stdout_done && stderr_done && !process.is_alive() {
			break
		}
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
		// Never wait for EOF after the deadline: a descendant that escaped
		// the group kill may still own a writer. Collect what is already
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
		output:    output_text
	}
}

// run_in_merged executes a command with an exact argument vector and captures
// both stdout and stderr. Standard input is empty, as with run.
pub fn run_in_merged(program string, args []string, work_folder string) os.Result {
	return run_in_mode(program, args, work_folder, true, no_timeout)
}

// split_args parses a directive or tool response into literal argv elements.
// Quotes and quoting backslash escapes group text; no shell expansion is performed.
pub fn split_args(input string) ![]string {
	return os.split_args(input)
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
