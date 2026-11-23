module main

import v.pref

// wasm_input finds a binary input without consuming compiler option values or program arguments.
fn wasm_input(args []string) ?int {
	mut skip := false
	for i, arg in args {
		if skip {
			skip = false
			continue
		}
		if is_inline_source_option(arg) { return none }
		if arg in ['-prof', '-profile'] {
			skip = profile_option_consumes_value(args, i)
			continue
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			skip = true
			continue
		}
		if arg.starts_with('-') && arg != '-' { continue }
		if arg == 'run' && i + 1 < args.len {
			return if args[i + 1].ends_with('.wasm') { i + 1 } else { none }
		}
		return if arg.ends_with('.wasm') { i } else { none }
	}
	return none
}
