module util

import strconv

// ComptimeStringScalar is the result of a pure string operation on known operands.
pub struct ComptimeStringScalar {
pub:
	typ   string
	value string
}

// comptime_string_scalar calls the builtin implementation on compile-time-known operands.
pub fn comptime_string_scalar(receiver string, method string, args []string) ?ComptimeStringScalar {
	if args.len == 0 {
		value := match method {
			'trim_space' { receiver.trim_space() }
			'to_lower' { receiver.to_lower() }
			'to_upper' { receiver.to_upper() }
			else { return none }
		}
		return ComptimeStringScalar{'string', value}
	}
	if args.len == 2 && method == 'replace' {
		return ComptimeStringScalar{'string', receiver.replace(args[0], args[1])}
	}
	if args.len != 1 {
		return none
	}
	arg := args[0]
	if method in ['starts_with', 'ends_with', 'contains'] {
		value := match method {
			'starts_with' { receiver.starts_with(arg) }
			'ends_with' { receiver.ends_with(arg) }
			else { receiver.contains(arg) }
		}
		return ComptimeStringScalar{'bool', value.str()}
	}
	if method == 'count' {
		return ComptimeStringScalar{'int', receiver.count(arg).str()}
	}
	value := match method {
		'all_before' { receiver.all_before(arg) }
		'all_after' { receiver.all_after(arg) }
		'all_before_last' { receiver.all_before_last(arg) }
		'all_after_last' { receiver.all_after_last(arg) }
		'trim' { receiver.trim(arg) }
		'trim_left' { receiver.trim_left(arg) }
		'trim_right' { receiver.trim_right(arg) }
		'trim_string_left' { receiver.trim_string_left(arg) }
		'trim_string_right' { receiver.trim_string_right(arg) }
		else { return none }
	}
	return ComptimeStringScalar{'string', value}
}

// comptime_string_bound parses a literal slice bound without truncating it to int.
pub fn comptime_string_bound(raw string) ?int {
	clean := raw.replace('_', '')
	digits := clean.trim_left('+-')
	base := if digits.starts_with('0x') || digits.starts_with('0b') || digits.starts_with('0o') {
		0
	} else {
		10
	}
	value := strconv.parse_int(clean, base, 64) or { return none }
	if value < -2147483648 || value > 2147483647 { return none }
	return int(value)
}
