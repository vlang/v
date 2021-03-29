module flag

// data object storing information about a defined flag
pub struct Flag {
pub:
	name     string // name as it appears on command line
	abbr     byte   // shortcut
	usage    string // help message
	val_desc string // something like '<arg>' that appears in usage,
	// and also the default value, when the flag is not given
}

struct UnkownFlagError {
	msg  string
	code int
}

struct MinimumArgsCountError {
	msg  string
	code int
}

struct MaximumArgsCountError {
	msg  string
	code int
}

struct NoArgsExpectedError {
	msg  string
	code int
}

[unsafe]
fn (mut f Flag) free() {
	unsafe {
		f.name.free()
		f.usage.free()
		f.val_desc.free()
	}
}

pub fn (f Flag) str() string {
	return '' + '    flag:\n' + '            name: $f.name\n' + '            abbr: $f.abbr\n' +
		'            usag: $f.usage\n' + '            desc: $f.val_desc'
}

pub fn (af []Flag) str() string {
	mut res := []string{}
	res << '\n  []Flag = ['
	for f in af {
		res << f.str()
	}
	res << '  ]'
	return res.join('\n')
}

//
pub struct FlagParser {
pub mut:
	args                    []string // the arguments to be parsed
	max_free_args           int
	flags                   []Flag // registered flags
	application_name        string
	application_version     string
	application_description string
	min_free_args           int
	args_description        string
}

[unsafe]
fn (mut f FlagParser) free() {
	unsafe {
		for a in f.args {
			a.free()
		}
		f.args.free()
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

pub const (
	// used for formating usage message
	space           = '                            '
	underline       = '-----------------------------------------------'
	max_args_number = 4048
)

// create a new flag set for parsing command line arguments
// TODO use INT_MAX some how
pub fn new_flag_parser(args []string) &FlagParser {
	return &FlagParser{
		args: args.clone()
		max_free_args: flag.max_args_number
	}
}

// change the application name to be used in 'usage' output
pub fn (mut fs FlagParser) application(name string) {
	fs.application_name = name
}

// change the application version to be used in 'usage' output
pub fn (mut fs FlagParser) version(vers string) {
	fs.application_version = vers
}

// change the application version to be used in 'usage' output
pub fn (mut fs FlagParser) description(desc string) {
	fs.application_description = desc
}

// in most cases you do not need the first argv for flag parsing
pub fn (mut fs FlagParser) skip_executable() {
	fs.args.delete(0)
}

// private helper to register a flag
fn (mut fs FlagParser) add_flag(name string, abbr byte, usage string, desc string) {
	fs.flags << Flag{
		name: name
		abbr: abbr
		usage: usage
		val_desc: desc
	}
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
[manualfree]
fn (mut fs FlagParser) parse_value(longhand string, shorthand byte) []string {
	full := '--$longhand'
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
		if arg == '--' {
			// End of input. We're done here.
			break
		}
		if arg[0] != `-` {
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
		if arg.len > full.len + 1 && arg[..full.len + 1] == '$full=' {
			found_entries << arg[full.len + 1..]
			to_delete << i
			continue
		}
	}
	for i, del in to_delete {
		// i entrys are deleted so it's shifted left i times.
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
fn (mut fs FlagParser) parse_bool_value(longhand string, shorthand byte) ?string {
	{
		full := '--$longhand'
		for i, arg in fs.args {
			if arg == '--' {
				// End of input. We're done.
				break
			}
			if arg.len == 0 {
				continue
			}
			if arg[0] != `-` {
				continue
			}
			if (arg.len == 2 && arg[0] == `-` && arg[1] == shorthand) || arg == full {
				if fs.args.len > i + 1 && (fs.args[i + 1] in ['true', 'false']) {
					val := fs.args[i + 1]
					fs.args.delete(i + 1)
					fs.args.delete(i)
					return val
				} else {
					fs.args.delete(i)
					return 'true'
				}
			}
			if arg.len > full.len + 1 && arg[..full.len + 1] == '$full=' {
				// Flag abc=true
				val := arg[full.len + 1..]
				fs.args.delete(i)
				return val
			}
			if arg.len > 1 && arg[0] == `-` && arg[1] != `-` && arg.index_byte(shorthand) != -1 {
				// -abc is equivalent to -a -b -c
				return 'true'
			}
		}
	}
	return error("parameter '$longhand' not found")
}

// bool_opt returns an optional that returns the value associated with the flag.
// In the situation that the flag was not provided, it returns null.
pub fn (mut fs FlagParser) bool_opt(name string, abbr byte, usage string) ?bool {
	mut res := false
	{
		fs.add_flag(name, abbr, usage, '<bool>')
		parsed := fs.parse_bool_value(name, abbr) or {
			return error("parameter '$name' not provided")
		}
		res = parsed == 'true'
	}
	return res
}

// defining and parsing a bool flag
// if defined
// the value is returned (true/false)
// else
// the default value is returned
// version with abbr
// TODO error handling for invalid string to bool conversion
pub fn (mut fs FlagParser) bool(name string, abbr byte, bdefault bool, usage string) bool {
	value := fs.bool_opt(name, abbr, usage) or { return bdefault }
	return value
}

// int_multi returns all instances of values associated with the flags provided
// In the case that none were found, it returns an empty array.
pub fn (mut fs FlagParser) int_multi(name string, abbr byte, usage string) []int {
	fs.add_flag(name, abbr, usage, '<multiple ints>')
	parsed := fs.parse_value(name, abbr)
	mut value := []int{}
	for val in parsed {
		value << val.int()
	}
	return value
}

// int_opt returns an optional that returns the value associated with the flag.
// In the situation that the flag was not provided, it returns null.
pub fn (mut fs FlagParser) int_opt(name string, abbr byte, usage string) ?int {
	mut res := 0
	{
		fs.add_flag(name, abbr, usage, '<int>')
		parsed := fs.parse_value(name, abbr)
		if parsed.len == 0 {
			return error("parameter '$name' not provided")
		}
		parsed0 := parsed[0]
		res = parsed0.int()
	}
	return res
}

// defining and parsing an int flag
// if defined
// the value is returned (int)
// else
// the default value is returned
// version with abbr
// TODO error handling for invalid string to int conversion
pub fn (mut fs FlagParser) int(name string, abbr byte, idefault int, usage string) int {
	value := fs.int_opt(name, abbr, usage) or { return idefault }
	return value
}

// float_multi returns all instances of values associated with the flags provided
// In the case that none were found, it returns an empty array.
pub fn (mut fs FlagParser) float_multi(name string, abbr byte, usage string) []f64 {
	fs.add_flag(name, abbr, usage, '<multiple floats>')
	parsed := fs.parse_value(name, abbr)
	mut value := []f64{}
	for val in parsed {
		value << val.f64()
	}
	return value
}

// float_opt returns an optional that returns the value associated with the flag.
// In the situation that the flag was not provided, it returns null.
pub fn (mut fs FlagParser) float_opt(name string, abbr byte, usage string) ?f64 {
	mut res := 0.0
	{
		fs.add_flag(name, abbr, usage, '<float>')
		parsed := fs.parse_value(name, abbr)
		if parsed.len == 0 {
			return error("parameter '$name' not provided")
		}
		res = parsed[0].f64()
	}
	return res
}

// defining and parsing a float flag
// if defined
// the value is returned (float)
// else
// the default value is returned
// version with abbr
// TODO error handling for invalid string to float conversion
pub fn (mut fs FlagParser) float(name string, abbr byte, fdefault f64, usage string) f64 {
	value := fs.float_opt(name, abbr, usage) or { return fdefault }
	return value
}

// string_multi returns all instances of values associated with the flags provided
// In the case that none were found, it returns an empty array.
pub fn (mut fs FlagParser) string_multi(name string, abbr byte, usage string) []string {
	fs.add_flag(name, abbr, usage, '<multiple strings>')
	return fs.parse_value(name, abbr)
}

// string_opt returns an optional that returns the value associated with the flag.
// In the situation that the flag was not provided, it returns null.
pub fn (mut fs FlagParser) string_opt(name string, abbr byte, usage string) ?string {
	mut res := ''
	{
		fs.add_flag(name, abbr, usage, '<string>')
		parsed := fs.parse_value(name, abbr)
		if parsed.len == 0 {
			return error("parameter '$name' not provided")
		}
		res = parsed[0]
	}
	return res
}

// defining and parsing a string flag
// if defined
// the value is returned (string)
// else
// the default value is returned
// version with abbr
pub fn (mut fs FlagParser) string(name string, abbr byte, sdefault string, usage string) string {
	value := fs.string_opt(name, abbr, usage) or { return sdefault }
	return value
}

pub fn (mut fs FlagParser) limit_free_args_to_at_least(n int) {
	if n > flag.max_args_number {
		panic('flag.limit_free_args_to_at_least expect n to be smaller than $flag.max_args_number')
	}
	if n <= 0 {
		panic('flag.limit_free_args_to_at_least expect n to be a positive number')
	}
	fs.min_free_args = n
}

pub fn (mut fs FlagParser) limit_free_args_to_exactly(n int) {
	if n > flag.max_args_number {
		panic('flag.limit_free_args_to_exactly expect n to be smaller than $flag.max_args_number')
	}
	if n < 0 {
		panic('flag.limit_free_args_to_exactly expect n to be a non negative number')
	}
	fs.min_free_args = n
	fs.max_free_args = n
}

// this will cause an error in finalize() if free args are out of range
// (min, ..., max)
pub fn (mut fs FlagParser) limit_free_args(min int, max int) {
	if min > max {
		panic('flag.limit_free_args expect min < max, got $min >= $max')
	}
	fs.min_free_args = min
	fs.max_free_args = max
}

pub fn (mut fs FlagParser) arguments_description(description string) {
	fs.args_description = description
}

// collect all given information and
pub fn (fs FlagParser) usage() string {
	positive_min_arg := (fs.min_free_args > 0)
	positive_max_arg := (fs.max_free_args > 0 && fs.max_free_args != flag.max_args_number)
	no_arguments := (fs.min_free_args == 0 && fs.max_free_args == 0)
	mut adesc := if fs.args_description.len > 0 { fs.args_description } else { '[ARGS]' }
	if no_arguments {
		adesc = ''
	}
	mut use := []string{}
	if fs.application_version != '' {
		use << '$fs.application_name $fs.application_version'
		use << '$flag.underline'
	}
	use << 'Usage: $fs.application_name [options] $adesc'
	use << ''
	if fs.application_description != '' {
		use << 'Description: $fs.application_description'
		use << ''
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
				s << 'at least $fs.min_free_args'
			}
			if positive_max_arg {
				s << 'at most $fs.max_free_args'
			}
			if positive_min_arg && positive_max_arg && fs.min_free_args == fs.max_free_args {
				s = ['exactly $fs.min_free_args']
			}
			sargs := s.join(' and ')
			use << 'The arguments should be $sargs in number.'
			use << ''
		}
	}
	if fs.flags.len > 0 {
		use << 'Options:'
		for f in fs.flags {
			mut onames := []string{}
			if f.abbr != 0 {
				onames << '-$f.abbr.ascii_str()'
			}
			if f.name != '' {
				if !f.val_desc.contains('<bool>') {
					onames << '--$f.name $f.val_desc'
				} else {
					onames << '--$f.name'
				}
			}
			option_names := '  ' + onames.join(', ')
			mut xspace := ''
			if option_names.len > flag.space.len - 2 {
				xspace = '\n$flag.space'
			} else {
				xspace = flag.space[option_names.len..]
			}
			fdesc := '$option_names$xspace$f.usage'
			use << fdesc
		}
	}
	return use.join('\n').replace('- ,', '   ')
}

// finalize - return all remaining arguments (non options).
// Call .finalize() after all arguments are defined.
// The remaining arguments are returned in the same order they are
// defined on the command line. If additional flags are found, i.e.
// (things starting with '--' or '-'), it returns an error.
pub fn (fs FlagParser) finalize() ?[]string {
	for a in fs.args {
		if (a.len >= 2 && a[..2] == '--') || (a.len == 2 && a[0] == `-`) {
			return IError(&UnkownFlagError{
				msg: 'Unknown flag `$a`'
			})
		}
	}
	if fs.args.len < fs.min_free_args && fs.min_free_args > 0 {
		return IError(&MinimumArgsCountError{
			msg: 'Expected at least $fs.min_free_args arguments, but given $fs.args.len'
		})
	}
	if fs.args.len > fs.max_free_args && fs.max_free_args > 0 {
		return IError(&MaximumArgsCountError{
			msg: 'Expected at most $fs.max_free_args arguments, but given $fs.args.len'
		})
	}
	if fs.args.len > 0 && fs.max_free_args == 0 && fs.min_free_args == 0 {
		return IError(&NoArgsExpectedError{
			msg: 'Expected no arguments, but given $fs.args.len'
		})
	}
	return fs.args
}
