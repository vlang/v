module util

import v.flat
import strconv

// ComptimeStringScalar is the result of a pure string operation on known operands.
pub struct ComptimeStringScalar {
pub:
	typ   u32
	value string
}

// comptime_string_fold_max_len bounds how long a folded string may become. V
// compiles itself with this module, so a fold that materialises a huge string
// charges the compiler instead of the program it is building.
const comptime_string_fold_max_len = 1 << 20

// comptime_string_scalar calls the builtin implementation on compile-time-known operands.
pub fn comptime_string_scalar(receiver string, method string, args []string) ?ComptimeStringScalar {
	if args.len == 0 {
		value := match method {
			'trim_space' { receiver.trim_space() }
			'to_lower' { receiver.to_lower() }
			'to_upper' { receiver.to_upper() }
			'reverse' { receiver.reverse() }
			else { return none }
		}
		return ComptimeStringScalar{flat.source_type_string, value}
	}
	if args.len == 2 && method == 'replace' {
		return ComptimeStringScalar{flat.source_type_string, receiver.replace(args[0], args[1])}
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
		return ComptimeStringScalar{flat.source_type_bool, value.str()}
	}
	if method == 'count' {
		return ComptimeStringScalar{flat.source_type_int, receiver.count(arg).str()}
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
	return ComptimeStringScalar{flat.source_type_string, value}
}

// comptime_string_repeat folds `s.repeat(count)` for compile-time-known operands.
// `repeat` is the one string operation in the wasm stub gap whose argument is an
// int, so it cannot travel through comptime_string_scalar's string-only arguments.
pub fn comptime_string_repeat(receiver string, count_text string) ?ComptimeStringScalar {
	count := comptime_string_bound(count_text)?
	// The program would allocate this string once; folding makes the compiler own
	// it for the whole build instead, so a large repeat is left to the backend.
	if count > 0 && receiver.len > 0 && count > comptime_string_fold_max_len / receiver.len {
		return none
	}
	return ComptimeStringScalar{flat.source_type_string, receiver.repeat(count)}
}

// comptime_string_index folds `s.index(needle)`. The bool reports whether the
// needle was found, which is the `none` half of the option the checker typed.
pub fn comptime_string_index(receiver string, needle string) (ComptimeStringScalar, bool) {
	idx := receiver.index(needle) or { return ComptimeStringScalar{}, false }
	return ComptimeStringScalar{flat.source_type_int, idx.str()}, true
}

// comptime_string_split folds `s.split(delim)` for compile-time-known operands.
pub fn comptime_string_split(receiver string, delim string) []string {
	return receiver.split(delim)
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

// typ_text resolves the indexed source type spelling.
@[inline]
pub fn (value &ComptimeStringScalar) typ_text() string {
	return flat.source_type_name(value.typ)
}
