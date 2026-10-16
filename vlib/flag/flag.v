module flag

import strconv

// data object storing information about a defined flag
pub struct Flag {
pub:
	name     string // name as it appears on command line
	abbr     u8     // shortcut
	usage    string // help message
	val_desc string // something like '<arg>' that appears in usage,
	// and also the default value, when the flag is not given
	default_value string
	has_default   bool
}

struct UnknownFlagError {
	Error
	flag string
}

fn (err UnknownFlagError) msg() string {
	return 'Unknown flag `${err.flag}`'
}

struct InvalidFlagValueError {
	Error
	flag   string
	value  string
	reason string
}

fn (err InvalidFlagValueError) msg() string {
	return 'Invalid value `${err.value}` for flag `${err.flag}`: ${err.reason}'
}

struct ArgsCountError {
	Error
	got  int
	want int
}

fn (err ArgsCountError) msg() string {
	if err.want == 0 {
		return 'Expected no arguments, but got ${err.got}'
	} else if err.got > err.want {
		return 'Expected at most ${err.want} arguments, but got ${err.got}'
	} else {
		return 'Expected at least ${err.want} arguments, but got ${err.got}'
	}
}

// free frees the resources associated with a given Flag
// It is called automatically when -autofree is used.
// It should be called manually in functions that use Flags,
// and are marked with [manualfree]. After you call .free() on
// a Flag instance, you should NOT use that instance any more.
@[unsafe]
fn (mut f Flag) free() {
	unsafe {
		f.name.free()
		f.usage.free()
		f.val_desc.free()
		f.default_value.free()
	}
}

// str returns a string representation of the given Flag.
pub fn (f Flag) str() string {
	return '    flag:
            name: ${f.name}
            abbr: `${f.abbr.ascii_str()}`
            usage: ${f.usage}
            desc: ${f.val_desc}
            default_value: ${f.default_value}
            has_default: ${f.has_default}'
}

// str returns a string representation of the given array of Flags.
pub fn (af []Flag) str() string {
	mut res := []string{}
	res << '\n  []Flag = ['
	for f in af {
		res << f.str()
	}
	res << '  ]'
	return res.join('\n')
}

// FlagParser is the heart of the `flag` module.
// That structure is created with `mut parser := flag.new_flag_parser(os.args)`,
// The returned instance can be further customised by calling various methods,
// for specifying the accepted options and their values. The user should finally
// call `rest := parser.finalize()!` to get the rest of the non optional arguments
// (if there are any left).
pub struct FlagParser {
mut:
	value_errors []&InvalidFlagValueError // invalid flag values, like `--n abc` for an int flag; reported by finalize()
pub:
	original_args      []string // the original arguments to be parsed
	idx_dashdash       int      // the index of a `--`, -1 if there is not any
	all_after_dashdash []string // all options after `--` are ignored, and will be passed to the application unmodified
pub mut:
	usage_examples []string // when set, --help will print:
	// Usage: ${appname} ${usage_examples[0]}`
	//    or: ${appname} ${usage_examples[1]}`
	// etc
	default_help_label      string = 'display this help and exit'
	default_version_label   string = 'output version information and exit'
	args                    []string // the current list of processed args
	max_free_args           int
	flags                   []Flag // registered flags
	application_name        string
	application_version     string
	application_description string
	min_free_args           int
	args_description        string
	allow_unknown_args      bool       // whether passing undescribed arguments is allowed
	footers                 []string   // when set, --help will display all the collected footers at the bottom.
	options                 DocOptions // documentation options
}

// free frees the resources allocated for the given FlagParser instance.
// It should be called manually in functions that use it, and that are
// marked with `@[manualfree]`,  otherwise, it is called automatically
// in programs, compiled with `-autofree`. Note: you should NOT use the
// instance over which you have called .free() for anything after the call.
@[unsafe]
fn (mut f FlagParser) free() {
	unsafe {
		//
		for flag in f.flags {
			flag.free()
		}
		f.flags.free()
		//
		f.application_name.free()
		f.application_version.free()
		f.application_description.free()
		f.args_description.free()
	}
}

// used for formatting usage message
pub const space = '                            '
pub const underline = '-----------------------------------------------'
pub const max_args_number = 4048

// new_flag_parser - create a new flag parser for the given args.
@[manualfree]
pub fn new_flag_parser(args []string) &FlagParser {
	original_args := args.clone()
	idx_dashdash := args.index('--')
	mut all_before_dashdash := args.clone()
	mut all_after_dashdash := []string{}
	if idx_dashdash >= 0 {
		all_before_dashdash.trim(idx_dashdash)
		if idx_dashdash < original_args.len {
			all_after_dashdash = unsafe { original_args[idx_dashdash + 1..] }
		}
	}
	return &FlagParser{
		original_args:      original_args
		idx_dashdash:       idx_dashdash
		all_after_dashdash: all_after_dashdash
		args:               all_before_dashdash
		max_free_args:      max_args_number
		options:            DocOptions{
			show: ~Show.zero() ^ .name
		}
	}
}

// usage_example - add an usage example.
// All examples will be listed in the help screen.
// If you do not give any examples, then a default usage
// will be shown, based on whether the application takes
// options and expects additional parameters.
pub fn (mut fs FlagParser) usage_example(example string) {
	fs.usage_examples << example
}

// footer - adds a footnote, that will be shown at the bottom of the help screen.
pub fn (mut fs FlagParser) footer(footer string) {
	fs.footers << footer
}

// application changes the application name to be used in 'usage' output.
pub fn (mut fs FlagParser) application(name string) {
	fs.application_name = name
}

// version changes the application version to be used in 'usage' output.
pub fn (mut fs FlagParser) version(vers string) {
	fs.application_version = vers
}

// description appends to the application description lines, shown in the help/usage screen.
pub fn (mut fs FlagParser) description(desc string) {
	if fs.application_description.len == 0 {
		fs.application_description = desc
	} else {
		fs.application_description += '\n${desc}'
	}
}

// skip_executable - removes the first argv (it usually contains the executable name).
// In most cases you do not need it for flag parsing.
// This method, allows you to use `mut fp := flag.new_flag_parser(arguments())` directly.
pub fn (mut fs FlagParser) skip_executable() {
	fs.args.delete(0)
}

// allow_unknown_args - call this method, if your program has sub commands, that have their own arguments.
// After calling it, the subcommand arguments (which generally are not known to your parent program),
// will not cause the validation in .finalize() to fail.
pub fn (mut fs FlagParser) allow_unknown_args() {
	fs.allow_unknown_args = true
}

// private helper to register a flag.
// This version supports abbreviations.
fn (mut fs FlagParser) add_flag(name string, abbr u8, usage string, desc string, default_value ?string) {
	mut has_default := false
	mut resolved_default_value := ''
	if value := default_value {
		has_default = true
		resolved_default_value = value
	}
	fs.flags << Flag{
		name:          name
		abbr:          abbr
		usage:         usage
		val_desc:      desc
		default_value: resolved_default_value
		has_default:   has_default
	}
}

fn flag_value_description(c FlagConfig, default_description string) string {
	return if c.val_desc == '' {
		default_description
	} else {
		c.val_desc
	}
}

fn escape_default_string(value string) string {
	return value.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\r', '\\r').replace('\t',
		'\\t')
}

fn flag_default_value[T](value T) string {
	$if T is string {
		s := '${value}'
		return '"${escape_default_string(s)}"'
	} $else {
		return '${value}'
	}
}

fn flag_default_bool_value(value bool) string {
	return '${value}'
}

fn flag_default_int_value(value int) string {
	return '${value}'
}

fn flag_default_float_value(value f64) string {
	return '${value}'
}

fn flag_default_string_value(value string) string {
	return '"${escape_default_string(value)}"'
}

fn (mut fs FlagParser) bool_flag(name string, abbr u8, usage string, c FlagConfig, default_value ?string) !bool {
	val_desc := flag_value_description(c, '<bool>')
	fs.add_flag(name, abbr, usage, val_desc, default_value)
	parsed := fs.parse_bool_value(name, abbr)
	if parsed.len == 0 {
		return error("parameter '${name}' not provided")
	}
	// every value is checked, and the last one wins, when the flag is repeated
	mut value := false
	for v in parsed {
		if v !in ['true', 'false'] {
			return fs.invalid_value(name, abbr, v, 'expected `true` or `false`')
		}
		value = v == 'true'
	}
	return value
}

fn (mut fs FlagParser) int_flag(name string, abbr u8, usage string, c FlagConfig, default_value ?string) !int {
	val_desc := flag_value_description(c, '<int>')
	fs.add_flag(name, abbr, usage, val_desc, default_value)
	parsed := fs.parse_value(name, abbr)
	if parsed.len == 0 {
		return error("parameter '${name}' not provided")
	}
	// every value is checked, and the last one wins, when the flag is repeated
	mut value := 0
	for v in parsed {
		value = fs.int_value(name, abbr, v)!
	}
	return value
}

fn (mut fs FlagParser) float_flag(name string, abbr u8, usage string, c FlagConfig, default_value ?string) !f64 {
	val_desc := flag_value_description(c, '<float>')
	fs.add_flag(name, abbr, usage, val_desc, default_value)
	parsed := fs.parse_value(name, abbr)
	if parsed.len == 0 {
		return error("parameter '${name}' not provided")
	}
	// every value is checked, and the last one wins, when the flag is repeated
	mut value := 0.0
	for v in parsed {
		value = fs.float_value(name, abbr, v)!
	}
	return value
}

// invalid_value records an error for the invalid value `value` of the flag `name`,
// so that finalize() fails, and returns that error.
fn (mut fs FlagParser) invalid_value(name string, abbr u8, value string, reason string) IError {
	err := &InvalidFlagValueError{
		flag:   if name != '' { '--${name}' } else { '-${abbr.ascii_str()}' }
		value:  value
		reason: reason
	}
	fs.value_errors << err
	return err
}

// int_value_base returns the base for parsing the integer flag value `value`, chosen like
// `string.int()` does: 0 (detect) for an explicit 0b, 0o or 0x prefix, and 10 otherwise,
// so that leading zeros stay decimal.
fn int_value_base(value string) int {
	start := if value != '' && value[0] in [`+`, `-`] { 1 } else { 0 }
	if value.len - start >= 2 && value[start] == `0`
		&& value[start + 1] in [`b`, `B`, `o`, `O`, `x`, `X`] {
		return 0
	}
	return 10
}

const value_out_of_range = 'out of range'

// parse_int_value parses the integer flag value `value` like `string.int()` does: decimal,
// or with an explicit 0b, 0o or 0x prefix, and with optional `_` separators. Unlike
// `string.int()`, it returns an error for a value that is not an integer, or that does not
// fit in `bit_size` bits (0 is the size of `int`), instead of 0 or a saturated value.
fn parse_int_value(value string, bit_size int) !i64 {
	return strconv.common_parse_int(value, int_value_base(value), bit_size, true, true) or {
		error(if err.msg().contains('overflow') { value_out_of_range } else { 'not an integer' })
	}
}

// parse_uint_value is like parse_int_value, for an unsigned value of `bit_size` bits.
fn parse_uint_value(value string, bit_size int) !u64 {
	if value.starts_with('-') {
		// of the negative integers, only zero is in range
		if parse_int_value(value, 64)! != 0 {
			return error(value_out_of_range)
		}
		return 0
	}
	digits := value.trim_string_left('+')
	return strconv.common_parse_uint(digits, int_value_base(digits), bit_size, true, true) or {
		error(if err.msg().contains('overflow') { value_out_of_range } else { 'not an integer' })
	}
}

// parse_float_value parses the flag value `value` as an `f64`, or, for `bit_size` 32, as an
// `f32`. It returns an error for a value that is not a number, or that is too large,
// instead of 0 or an infinity.
fn parse_float_value(value string, bit_size int) !f64 {
	f := strconv.atof64(value) or {
		return error(if err.msg().contains('out of range') {
			value_out_of_range
		} else {
			'not a number'
		})
	}
	// `x - x` is 0 only for a finite `x`: a finite value that becomes infinite as an `f32`
	// is too large for it
	if bit_size == 32 && f - f == 0 && f32(f) - f32(f) != 0 {
		return error(value_out_of_range)
	}
	return f
}

// int_value converts the value `value` of the int flag `name`. For a value that is not an
// integer, or that does not fit in an `int`, it records an error for finalize(), and returns it.
fn (mut fs FlagParser) int_value(name string, abbr u8, value string) !int {
	n := parse_int_value(value, 0) or {
		reason := if err.msg() == value_out_of_range {
			'out of range for int'
		} else {
			'expected an integer'
		}
		return fs.invalid_value(name, abbr, value, reason)
	}
	return int(n)
}

// float_value converts the value `value` of the float flag `name`. For a value that is not
// a number, or that is too large for an `f64`, it records an error for finalize(), and returns it.
fn (mut fs FlagParser) float_value(name string, abbr u8, value string) !f64 {
	return parse_float_value(value, 64) or {
		reason := if err.msg() == value_out_of_range {
			'out of range for f64'
		} else {
			'expected a number'
		}
		return fs.invalid_value(name, abbr, value, reason)
	}
}

fn (mut fs FlagParser) string_flag(name string, abbr u8, usage string, c FlagConfig, default_value ?string) !string {
	val_desc := flag_value_description(c, '<string>')
	fs.add_flag(name, abbr, usage, val_desc, default_value)
	parsed := fs.parse_value(name, abbr)
	if parsed.len == 0 {
		return error("parameter '${name}' not provided")
	}
	// the last value wins, when the flag is repeated
	return parsed.last()
}

// private: general parsing a single argument
// - search args for existence
// if true
// extract the defined value as string
// else
// return an (dummy) error -> argument is not defined
//
// - the name, usage are registered
// - found arguments and corresponding values are removed from args list
@[manualfree]
fn (mut fs FlagParser) parse_value(longhand string, shorthand u8) []string {
	full := '--${longhand}'
	defer {
		unsafe { full.free() }
	}
	mut found_entries := []string{}
	mut to_delete := []int{}
	defer {
		unsafe { to_delete.free() }
	}
	mut should_skip_one := false
	for i, arg in fs.args {
		if should_skip_one {
			should_skip_one = false
			continue
		}
		if arg.len == 0 || arg[0] != `-` {
			continue
		}
		if (arg.len == 2 && arg[0] == `-` && arg[1] == shorthand) || arg == full {
			if i + 1 >= fs.args.len {
				return []
			}
			nextarg := fs.args[i + 1]
			if nextarg.len > 2 {
				nextarg_rest := nextarg[..2]
				if nextarg_rest == '--' {
					// It could be end of input (--) or another argument (--abc).
					// Both are invalid so die.
					unsafe { nextarg_rest.free() }
					return []
				}
				unsafe { nextarg_rest.free() }
			}
			found_entries << fs.args[i + 1]
			to_delete << i
			to_delete << i + 1
			should_skip_one = true
			continue
		}
		// `--name=value`; an explicitly empty value, `--name=`, is a value too
		if arg.len > full.len && arg[..full.len + 1] == '${full}=' {
			found_entries << arg[full.len + 1..]
			to_delete << i
			continue
		}
	}
	for i, del in to_delete {
		// i entries are deleted so it's shifted left i times.
		fs.args.delete(del - i)
	}
	return found_entries
}

// special parsing for bool values
// see also: parse_value
//
// special: it is allowed to define bool flags without value
// -> '--flag' is parsed as true
// -> '--flag' is equal to '--flag=true'
// Like parse_value, it removes all occurrences of the flag from the args,
// and returns their values, which are empty when the flag is not given.
fn (mut fs FlagParser) parse_bool_value(longhand string, shorthand u8) []string {
	full := '--${longhand}'
	mut values := []string{}
	mut i := 0
	for i < fs.args.len {
		arg := fs.args[i]
		if arg.len == 0 || arg[0] != `-` {
			i++
			continue
		}
		if (arg.len == 2 && arg[1] == shorthand) || arg == full {
			if fs.args.len > i + 1 && fs.args[i + 1] in ['true', 'false'] {
				values << fs.args[i + 1]
				fs.args.delete(i + 1)
			} else {
				values << 'true'
			}
			fs.args.delete(i)
			continue
		}
		if arg.len > full.len && arg[..full.len + 1] == '${full}=' {
			// Flag abc=true; an empty value, `--abc=`, is rejected by bool_flag
			values << arg[full.len + 1..]
			fs.args.delete(i)
			continue
		}
		if arg.len > 1 && arg[1] != `-` {
			mut in_cluster := false
			for j in 1 .. arg.len {
				if arg[j].is_space() {
					break
				} else if arg[j] == shorthand {
					in_cluster = true
				}
			}
			if in_cluster {
				// -abc is equivalent to -a -b -c
				values << 'true'
				rest := arg.replace(shorthand.ascii_str(), '') // -abc -> -bc
				if rest == '-' {
					// the cluster only repeated this flag, e.g. `-vv`
					fs.args.delete(i)
					continue
				}
				fs.args[i] = rest
			}
		}
		i++
	}
	return values
}

@[params]
pub struct FlagConfig {
pub:
	val_desc string // descriptive string for an argument
}

// bool_opt returns an option with the bool value of the given command line flag, named `name`.
// It returns an error, when the flag is not given by the user, or when its value is not
// `true` or `false`; finalize() then fails too.
// When the flag is given more than once, the last value wins.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) bool_opt(name string, abbr u8, usage string, c FlagConfig) !bool {
	return fs.bool_flag(name, abbr, usage, c, none)
}

// bool defines and parses a bool flag/option named `name`.
// If that flag is given by the user, then it returns its parsed bool value.
// When it is not, it returns the default value in `bdefault`.
// When the given value is not `true` or `false`, it returns `bdefault`, and finalize() fails.
// When the flag is given more than once, the last value wins.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) bool(name string, abbr u8, bdefault bool, usage string, c FlagConfig) bool {
	value := fs.bool_flag(name, abbr, usage, c, flag_default_bool_value(bdefault)) or {
		return bdefault
	}
	return value
}

// bool_val is a generic version of `bool` that supports optional defaults.
// Use `?bool(none)` as default to receive an optional result.
pub fn (mut fs FlagParser) bool_val[T](name string, abbr u8, bdefault T, usage string, c FlagConfig) T {
	value := fs.bool_flag(name, abbr, usage, c, flag_default_value(bdefault)) or { return bdefault }
	return value
}

// int_multi returns all values associated with the provided flag in `name`.
// When that flag has no values, it returns an empty array.
// Values that are not integers, or do not fit in an `int`, are left out, and finalize() fails.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) int_multi(name string, abbr u8, usage string, c FlagConfig) []int {
	val_desc := flag_value_description(c, '<multiple ints>')

	fs.add_flag(name, abbr, usage, val_desc, none)
	parsed := fs.parse_value(name, abbr)
	mut value := []int{}
	for val in parsed {
		value << fs.int_value(name, abbr, val) or { continue }
	}
	return value
}

// int_opt returns an option with the integer value, associated with the flag in `name`.
// When the flag is not given by the user, it returns an error.
// When its value is not an integer, or does not fit in an `int`, it returns an error too,
// and finalize() fails.
// When the flag is given more than once, the last value wins.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) int_opt(name string, abbr u8, usage string, c FlagConfig) !int {
	return fs.int_flag(name, abbr, usage, c, none)
}

// int defines and parses an integer flag, named `name`.
// When the flag is given by the user, it returns its parsed integer value.
// When it is not, it returns the integer value in `idefault`.
// When the given value is not an integer, or does not fit in an `int`, it returns `idefault`,
// and finalize() fails.
// When the flag is given more than once, the last value wins; `int_multi` returns them all.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) int(name string, abbr u8, idefault int, usage string, c FlagConfig) int {
	value := fs.int_flag(name, abbr, usage, c, flag_default_int_value(idefault)) or {
		return idefault
	}
	return value
}

// int_val is a generic version of `int` that supports optional defaults.
// Use `?int(none)` as default to receive an optional result.
pub fn (mut fs FlagParser) int_val[T](name string, abbr u8, idefault T, usage string, c FlagConfig) T {
	value := fs.int_flag(name, abbr, usage, c, flag_default_value(idefault)) or { return idefault }
	return value
}

// float_multi returns all floating point values, associated with the flag named `name`.
// When no values for that flag are found, it returns an empty array.
// Values that are not numbers, or are too large for an `f64`, are left out, and finalize() fails.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) float_multi(name string, abbr u8, usage string, c FlagConfig) []f64 {
	val_desc := flag_value_description(c, '<multiple floats>')

	fs.add_flag(name, abbr, usage, val_desc, none)
	parsed := fs.parse_value(name, abbr)
	mut value := []f64{}
	for val in parsed {
		value << fs.float_value(name, abbr, val) or { continue }
	}
	return value
}

// float_opt returns an option with the floating point value, associated with the flag in `name`.
// When the flag is not given by the user, it returns an error.
// When its value is not a number, or is too large for an `f64`, it returns an error too,
// and finalize() fails.
// When the flag is given more than once, the last value wins.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) float_opt(name string, abbr u8, usage string, c FlagConfig) !f64 {
	return fs.float_flag(name, abbr, usage, c, none)
}

// float defines and parses a floating point flag, named `name`.
// When the flag is given by the user, it returns its parsed floating point value.
// When it is not, it returns the value in `fdefault`.
// When the given value is not a number, or is too large for an `f64`, it returns `fdefault`,
// and finalize() fails.
// When the flag is given more than once, the last value wins; `float_multi` returns them all.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) float(name string, abbr u8, fdefault f64, usage string, c FlagConfig) f64 {
	value := fs.float_flag(name, abbr, usage, c, flag_default_float_value(fdefault)) or {
		return fdefault
	}
	return value
}

// float_val is a generic version of `float` that supports optional defaults.
// Use `?f64(none)` as default to receive an optional result.
pub fn (mut fs FlagParser) float_val[T](name string, abbr u8, fdefault T, usage string, c FlagConfig) T {
	value := fs.float_flag(name, abbr, usage, c, flag_default_value(fdefault)) or {
		return fdefault
	}
	return value
}

// string_multi returns all string values, associated with the flag named `name`.
// When no values for that flag are found, it returns an empty array.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) string_multi(name string, abbr u8, usage string, c FlagConfig) []string {
	val_desc := flag_value_description(c, '<multiple strings>')

	fs.add_flag(name, abbr, usage, val_desc, none)
	return fs.parse_value(name, abbr)
}

// string_opt returns an option with the string value, associated with the flag in `name`.
// When the flag is not given by the user, it returns an error.
// When the flag is given more than once, the last value wins.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) string_opt(name string, abbr u8, usage string, c FlagConfig) !string {
	return fs.string_flag(name, abbr, usage, c, none)
}

// string defines and parses a string flag/option, named `name`.
// If that flag is given as an option, then its parsed value is returned as a string.
// When it is not, it returns the default string value in `sdefault`.
// When the flag is given more than once, the last value wins; `string_multi` returns them all.
// This version supports abbreviations.
// This version supports a custom value description.
pub fn (mut fs FlagParser) string(name string, abbr u8, sdefault string, usage string, c FlagConfig) string {
	value := fs.string_flag(name, abbr, usage, c, flag_default_string_value(sdefault)) or {
		return sdefault
	}
	return value
}

// string_val is a generic version of `string` that supports optional defaults.
// Use `?string(none)` as default to receive an optional result.
pub fn (mut fs FlagParser) string_val[T](name string, abbr u8, sdefault T, usage string, c FlagConfig) T {
	value := fs.string_flag(name, abbr, usage, c, flag_default_value(sdefault)) or {
		return sdefault
	}
	return value
}

// limit_free_args_to_at_least restricts the list of free arguments (non options) to be at least `n` in length.
// If the user gives less free arguments to the program, the parser will return an error.
pub fn (mut fs FlagParser) limit_free_args_to_at_least(n int) ! {
	if n > max_args_number {
		return error('flag.limit_free_args_to_at_least expect n to be smaller than ${max_args_number}')
	}
	if n <= 0 {
		return error('flag.limit_free_args_to_at_least expect n to be a positive number')
	}
	fs.min_free_args = n
}

// limit_free_args_to_exactly restricts the list of free arguments (non options) to be at exactly `n` in length.
// If the user gives more or less free arguments to the program, the parser will return an error.
pub fn (mut fs FlagParser) limit_free_args_to_exactly(n int) ! {
	if n > max_args_number {
		return error('flag.limit_free_args_to_exactly expect n to be smaller than ${max_args_number}')
	}
	if n < 0 {
		return error('flag.limit_free_args_to_exactly expect n to be a non negative number')
	}
	fs.min_free_args = n
	fs.max_free_args = n
}

// limit_free_args restricts the list of free arguments (non options) to be between `min` and `max` in length.
// If the user gives more or less free arguments to the program, the parser will return an error.
pub fn (mut fs FlagParser) limit_free_args(min int, max int) ! {
	if min > max {
		return error('flag.limit_free_args expect min < max, got ${min} >= ${max}')
	}
	fs.min_free_args = min
	fs.max_free_args = max
}

// arguments_description sets the description field of the parser.
// This field is usually shown when the `--help` option is given to the program.
pub fn (mut fs FlagParser) arguments_description(description string) {
	fs.args_description = description
}

// usage returns a nicely formatted usage screen, containing all the possible options, as well as the description for the program.
// That screen is usually shown when the `--help` option is given to the program.
pub fn (fs &FlagParser) usage() string {
	positive_min_arg := (fs.min_free_args > 0)
	positive_max_arg := (fs.max_free_args > 0 && fs.max_free_args != max_args_number)
	no_arguments := (fs.min_free_args == 0 && fs.max_free_args == 0)
	mut adesc := if fs.args_description.len > 0 { fs.args_description } else { '[ARGS]' }
	if no_arguments {
		adesc = ''
	}
	mut use := []string{}
	if doc_add_name_and_version(fs.application_name, fs.application_version, fs.options, mut use) {
		use << '${underline}'
	}
	if fs.usage_examples.len == 0 {
		use << 'Usage: ${fs.application_name} [options] ${adesc}'
	} else {
		for i, example in fs.usage_examples {
			if i == 0 {
				use << 'Usage: ${fs.application_name} ${example}'
			} else {
				use << '   or: ${fs.application_name} ${example}'
			}
		}
	}
	use << ''
	if fs.options.show.has(.description) && fs.application_description != '' {
		use << 'Description: ${fs.application_description}'
		use << ''
	}
	// show a message about the [ARGS]:
	if positive_min_arg || positive_max_arg || no_arguments {
		if no_arguments {
			use << 'This application does not expect any arguments'
			use << ''
		} else {
			mut s := []string{}
			if positive_min_arg {
				s << 'at least ${fs.min_free_args}'
			}
			if positive_max_arg {
				s << 'at most ${fs.max_free_args}'
			}
			if positive_min_arg && positive_max_arg && fs.min_free_args == fs.max_free_args {
				s = ['exactly ${fs.min_free_args}']
			}
			sargs := s.join(' and ')
			use << 'The arguments should be ${sargs} in number.'
			use << ''
		}
	}
	if fs.options.show.has(.flags) && fs.flags.len > 0 {
		if fs.options.show.has(.flags_header) {
			use << fs.options.flag_header.trim_space_left()
		}
		for f in fs.flags {
			mut onames := []string{}
			if f.abbr != 0 {
				onames << '-${f.abbr.ascii_str()}'
			}
			if f.name != '' {
				if !f.val_desc.contains('<bool>') {
					onames << '--${f.name} ${f.val_desc}'
				} else {
					onames << '--${f.name}'
				}
			}
			option_names := '  ' + onames.join(', ')
			mut xspace := ''
			if option_names.len > space.len - 2 {
				xspace = '\n${space}'
			} else {
				xspace = space[option_names.len..]
			}
			mut fusage := f.usage
			if f.has_default {
				fusage += ' (default ${f.default_value})'
			}
			fdesc := '${option_names}${xspace}${fusage}'
			use << fdesc
		}
	}
	if fs.options.show.has(.footer) {
		for footer in fs.footers {
			use << footer
		}
	}
	return use.join('\n').replace('- ,', '   ')
}

// find_existing_flag looks up the given flag by name, and returns
// it, if it was found in the FlagParser. If it was not, it returns an error.
fn (mut fs FlagParser) find_existing_flag(fname string) !Flag {
	for f in fs.flags {
		if f.name == fname {
			return f
		}
	}
	return error('no such flag')
}

// handle_builtin_options handles the default behaviour of the very frequently
// given options: `--help` and `--version`.
// You can change/customise that, by defining your own options with these names.
fn (mut fs FlagParser) handle_builtin_options() {
	mut show_version := false
	mut show_help := false
	fs.find_existing_flag('help') or {
		show_help = fs.bool_opt('help', `h`, fs.default_help_label) or { false }
	}
	fs.find_existing_flag('version') or {
		show_version = fs.bool_opt('version', 0, fs.default_version_label) or { false }
	}
	if show_help {
		println(fs.usage())
		exit(0)
	}
	if show_version {
		println('${fs.application_name} ${fs.application_version}')
		exit(0)
	}
}

// finalize - return all remaining arguments (non options).
// Call .finalize() after all arguments are defined.
// The remaining arguments are returned in the same order they are
// defined on the command line. If additional flags are found, i.e.
// (things starting with '--' or '-'), it returns an error.
// It also returns an error, when a defined flag was given an invalid value,
// like `--n abc` for an int flag.
pub fn (mut fs FlagParser) finalize() ![]string {
	fs.handle_builtin_options()
	if fs.value_errors.len > 0 {
		return fs.value_errors[0]
	}
	mut remaining := fs.args.clone()
	if !fs.allow_unknown_args {
		for a in remaining {
			if (a.len >= 2 && a[..2] == '--') || (a.len == 2 && a[0] == `-`) {
				return &UnknownFlagError{
					flag: a
				}
			}
		}
	}
	remaining << fs.all_after_dashdash
	if fs.min_free_args > remaining.len {
		return &ArgsCountError{
			want: fs.min_free_args
			got:  remaining.len
		}
	}
	if fs.max_free_args < remaining.len {
		return &ArgsCountError{
			want: fs.max_free_args
			got:  remaining.len
		}
	}
	return remaining
}

// remaining_parameters will return all remaining parameters.
// Call .remaining_parameters() *AFTER* you have defined all options
// that your program needs. remaining_parameters will also print any
// parsing errors and stop the program. Use .finalize() instead, if
// you want more control over the error handling.
pub fn (mut fs FlagParser) remaining_parameters() []string {
	return fs.finalize() or {
		eprintln(err.msg())
		println(fs.usage())
		exit(1)
	}
}
