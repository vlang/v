module cli

pub enum FlagType {
	bool
	int
	float
	string
	// If flag can set multiple time, use array type
	int_array
	float_array
	string_array
}

// Flag holds information for a command line flag.
// (flags are also commonly referred to as "options" or "switches")
// These are typically denoted in the shell by a short form `-f` and/or a long form `--flag`
pub struct Flag {
pub mut:
	flag FlagType
	// Name of flag
	name string
	// Like short option
	abbrev string
	// Desciption of flag
	description string
	global      bool
	// If flag is requierd
	required bool
	// Default value if no value provide by command line
	default_value []string = []
mut:
	// Set true if flag found.
	found bool
	// Value of flag
	value []string = []
}

// get_all_found returns an array of all `Flag`s found in the command parameters
pub fn (flags []Flag) get_all_found() []Flag {
	return flags.filter(it.found)
}

// get_bool returns `true` if the flag is set.
// get_bool returns an error if the `FlagType` is not boolean.
pub fn (flag Flag) get_bool() ?bool {
	if flag.flag != .bool {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `bool`')
	}

	val := flag.get_value_or_default_value()

	return val.len > 0 && val[0] == 'true'
}

// get_bool returns `true` if the flag specified in `name` is set.
// get_bool returns an error if the `FlagType` is not boolean.
pub fn (flags []Flag) get_bool(name string) ?bool {
	flag := flags.get(name)?
	return flag.get_bool()
}

// get_int returns the `int` value argument of the flag.
// get_int returns an error if the `FlagType` is not integer.
pub fn (flag Flag) get_int() ?int {
	if flag.flag != .int {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `int`')
	}

	val := flag.get_value_or_default_value()

	if val.len == 0 {
		return 0
	} else {
		return val[0].int()
	}
}

// get_ints returns the array of `int` value argument of the flag specified in `name`.
// get_ints returns an error if the `FlagType` is not integer.
pub fn (flag Flag) get_ints() ?[]int {
	if flag.flag != .int_array {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `int_array`')
	}

	val := flag.get_value_or_default_value()

	if val.len == 0 {
		return []int{}
	} else {
		mut values := []int{}

		for f in val {
			values << f.int()
		}

		return values
	}
}

// get_int returns the `int` value argument of the flag specified in `name`.
// get_int returns an error if the `FlagType` is not integer.
pub fn (flags []Flag) get_int(name string) ?int {
	flag := flags.get(name)?
	return flag.get_int()
}

// get_ints returns the array of `int` value argument of the flag specified in `name`.
// get_ints returns an error if the `FlagType` is not integer.
pub fn (flags []Flag) get_ints(name string) ?[]int {
	flag := flags.get(name)?
	return flag.get_ints()
}

// get_float returns the `f64` value argument of the flag.
// get_float returns an error if the `FlagType` is not floating point.
pub fn (flag Flag) get_float() ?f64 {
	if flag.flag != .float {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `float`')
	}

	val := flag.get_value_or_default_value()

	if val.len == 0 {
		return 0.0
	} else {
		return val[0].f64()
	}
}

// get_floats returns the `f64` value argument of the flag.
// get_floats returns an error if the `FlagType` is not floating point.
pub fn (flag Flag) get_floats() ?[]f64 {
	if flag.flag != .float_array {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `float_array`')
	}

	val := flag.get_value_or_default_value()

	if val.len == 0 {
		return []f64{}
	} else {
		mut values := []f64{}

		for f in val {
			values << f.f64()
		}

		return values
	}
}

// get_float returns the `f64` value argument of the flag specified in `name`.
// get_float returns an error if the `FlagType` is not floating point.
pub fn (flags []Flag) get_float(name string) ?f64 {
	flag := flags.get(name)?
	return flag.get_float()
}

// get_floats returns the array of `f64` value argument of the flag specified in `name`.
// get_floats returns an error if the `FlagType` is not floating point.
pub fn (flags []Flag) get_floats(name string) ?[]f64 {
	flag := flags.get(name)?
	return flag.get_floats()
}

// get_string returns the `string` value argument of the flag.
// get_string returns an error if the `FlagType` is not string.
pub fn (flag Flag) get_string() ?string {
	if flag.flag != .string {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `string`')
	}

	val := flag.get_value_or_default_value()

	if val.len == 0 {
		return ''
	} else {
		return val[0]
	}
}

// get_strings returns the array of `string` value argument of the flag.
// get_strings returns an error if the `FlagType` is not string.
pub fn (flag Flag) get_strings() ?[]string {
	if flag.flag != .string_array {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `string_array`')
	}

	val := flag.get_value_or_default_value()

	if val.len == 0 {
		return []string{}
	} else {
		return val
	}
}

// get_string returns the `string` value argument of the flag specified in `name`.
// get_string returns an error if the `FlagType` is not string.
pub fn (flags []Flag) get_string(name string) ?string {
	flag := flags.get(name)?
	return flag.get_string()
}

// get_strings returns the `string` value argument of the flag specified in `name`.
// get_strings returns an error if the `FlagType` is not string.
pub fn (flags []Flag) get_strings(name string) ?[]string {
	flag := flags.get(name)?
	return flag.get_strings()
}

// parse parses flag values from arguments and return
// an array of arguments with all consumed elements removed.
fn (mut flag Flag) parse(args []string, posix_mode bool) ?[]string {
	if flag.matches(args, posix_mode) {
		if flag.flag == .bool {
			new_args := flag.parse_bool(args)?
			return new_args
		} else {
			if flag.value.len > 0 && flag.flag != .int_array && flag.flag != .float_array
				&& flag.flag != .string_array {
				return error('The argument `$flag.name` accept only one value!')
			}

			new_args := flag.parse_raw(args)?
			return new_args
		}
	} else {
		return args
	}
}

// matches returns `true` if first arg in `args` matches this flag.
fn (mut flag Flag) matches(args []string, posix_mode bool) bool {
	prefix := if posix_mode { '--' } else { '-' }
	return (flag.name != '' && args[0] == '$prefix$flag.name')
		|| (flag.name != '' && args[0].starts_with('$prefix$flag.name='))
		|| (flag.abbrev != '' && args[0] == '-$flag.abbrev')
		|| (flag.abbrev != '' && args[0].starts_with('-$flag.abbrev='))
}

fn (mut flag Flag) parse_raw(args []string) ?[]string {
	if args[0].len > flag.name.len && args[0].contains('=') {
		flag.value << args[0].split('=')[1]
		return args[1..]
	} else if args.len >= 2 {
		flag.value << args[1]
		return args[2..]
	}
	return error('Missing argument for `$flag.name`')
}

fn (mut flag Flag) parse_bool(args []string) ?[]string {
	if args[0].len > flag.name.len && args[0].contains('=') {
		flag.value = [args[0].split('=')[1]]
		return args[1..]
	} else if args.len >= 2 {
		if args[1] in ['true', 'false'] {
			flag.value = [args[1]]
			return args[2..]
		}
	}
	// In fact bool cannot be multiple
	flag.value = ['true']
	return args[1..]
}

// get returns the `Flag` matching `name` or an error
// if it can't be found.
fn (flags []Flag) get(name string) ?Flag {
	for flag in flags {
		if flag.name == name {
			return flag
		}
	}
	return error('Flag `$name` not found in $flags')
}

fn (flags []Flag) contains(name string) bool {
	for flag in flags {
		if flag.name == name || flag.abbrev == name {
			return true
		}
	}
	return false
}

// Check if value is set by command line option. If not, return default value.
fn (flag Flag) get_value_or_default_value() []string {
	if flag.value.len == 0 && flag.default_value.len > 0 {
		// If default value is set and no value provide, use default value.
		return flag.default_value
	} else {
		return flag.value
	}
}
