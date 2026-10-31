module os

import strings

@[manualfree]
fn requote_args(cargs []string) string {
	mut sb := strings.new_builder(128)
	defer { unsafe { sb.free() } }
	for idx, a in cargs {
		if idx > 0 {
			sb.write_rune(` `)
		}
		sb.write_string(requote_arg(a))
	}
	res := sb.str()
	return res
}

fn requote_arg(arg string) string {
	if arg.len == 0 {
		return '""'
	}
	// Leave simple tokens bare so cmd.exe can recognize its builtin names and switches.
	if !arg.contains_any(' \t"') {
		return arg
	}
	// Escape a literal argv entry using the same backslash+quote rules that
	// Windows uses when reconstructing argc/argv from CreateProcessW.
	mut sb := strings.new_builder(arg.len + 8)
	defer { unsafe { sb.free() } }
	sb.write_u8(`"`)
	mut pending_backslashes := 0
	for i := 0; i < arg.len; i++ {
		ch := arg[i]
		if ch == `\\` {
			pending_backslashes++
			continue
		}
		if ch == `"` {
			for _ in 0 .. pending_backslashes * 2 + 1 {
				sb.write_u8(`\\`)
			}
			sb.write_u8(`"`)
			pending_backslashes = 0
			continue
		}
		for _ in 0 .. pending_backslashes {
			sb.write_u8(`\\`)
		}
		pending_backslashes = 0
		sb.write_u8(ch)
	}
	for _ in 0 .. pending_backslashes * 2 {
		sb.write_u8(`\\`)
	}
	sb.write_u8(`"`)
	return sb.str()
}
