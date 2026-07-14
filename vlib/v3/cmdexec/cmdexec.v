module cmdexec

import os
import strings
import time

// run executes program with an exact argument vector and captures its output.
pub fn run(program string, args []string) os.Result {
	return run_in(program, args, '')
}

// run_in executes program in work_folder with an exact argument vector.
pub fn run_in(program string, args []string, work_folder string) os.Result {
	mut process := os.new_process(program)
	process.set_args(args)
	if work_folder.len > 0 {
		process.set_work_folder(work_folder)
	}
	process.set_redirect_stdio()
	process.run()
	mut output := strings.new_builder(1024)
	for process.is_alive() {
		stdout := process.stdout_read()
		stderr := process.stderr_read()
		output.write_string(stdout)
		output.write_string(stderr)
		if stdout.len == 0 && stderr.len == 0 {
			time.sleep(time.millisecond)
		}
	}
	process.wait()
	output.write_string(process.stdout_slurp())
	output.write_string(process.stderr_slurp())
	if process.err.len > 0 {
		output.writeln(process.err)
	}
	exit_code := if process.code >= 0 { process.code } else { 1 }
	process.close()
	return os.Result{
		exit_code: exit_code
		output:    output.str()
	}
}

// split_args parses a directive or tool response into literal argv elements.
// Quotes and backslash escapes group text; no shell expansion is performed.
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
			if i + 1 >= input.len {
				return error('trailing backslash in argument list')
			}
			i++
			current.write_u8(input[i])
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
