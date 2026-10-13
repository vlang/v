module main

import strings

// windows_compiler_command_line encodes literal argv entries for CreateProcessW.
// No shell or environment expansion is involved in the resulting command line.
fn windows_compiler_command_line(executable string, args []string) string {
	mut quoted := [windows_compiler_arg(executable)]
	for arg in args {
		quoted << windows_compiler_arg(arg)
	}
	return quoted.join(' ')
}

// windows_compiler_arg follows Windows' backslash-before-quote rules, including
// doubling trailing backslashes before the closing quote.
fn windows_compiler_arg(arg string) string {
	mut out := strings.new_builder(arg.len + 8)
	defer { unsafe { out.free() } }
	out.write_u8(`"`)
	mut backslashes := 0
	for ch in arg {
		if ch == `\\` {
			backslashes++
			continue
		}
		escapes := if ch == `"` { backslashes * 2 + 1 } else { backslashes }
		for _ in 0 .. escapes {
			out.write_u8(`\\`)
		}
		out.write_u8(ch)
		backslashes = 0
	}
	for _ in 0 .. backslashes * 2 {
		out.write_u8(`\\`)
	}
	out.write_u8(`"`)
	return out.str()
}
