module cli

pub enum FlagType {
	bool
	int
	float
	string
}

// Flag holds information for a command line flag.
// (flags are also commonly referred to as "options" or "switches")
// These are typically denoted in the shell by a short form `-f` and/or a long form `--flag`
pub struct Flag {
pub mut:
	flag        FlagType
	name        string
	abbrev      string
	description string
	global      bool
	required    bool
	value       string
mut:
	found bool
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
	return flag.value == 'true'
}

// get_bool returns `true` if the flag specified in `name` is set.
// get_bool returns an error if the `FlagType` is not boolean.
pub fn (flags []Flag) get_bool(name string) ?bool {
	flag := flags.get(name) ?
	return flag.get_bool()
}

// get_int returns the `int` value argument of the flag.
// get_int returns an error if the `FlagType` is not integer.
pub fn (flag Flag) get_int() ?int {
	if flag.flag != .int {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `int`')
	}
	return flag.value.int()
}

// get_int returns the `int` value argument of the flag specified in `name`.
// get_int returns an error if the `FlagType` is not integer.
pub fn (flags []Flag) get_int(name string) ?int {
	flag := flags.get(name) ?
	return flag.get_int()
}

// get_float returns the `f64` value argument of the flag.
// get_float returns an error if the `FlagType` is not floating point.
pub fn (flag Flag) get_float() ?f64 {
	if flag.flag != .float {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `float`')
	}
	return flag.value.f64()
}

// get_float returns the `f64` value argument of the flag specified in `name`.
// get_float returns an error if the `FlagType` is not floating point.
pub fn (flags []Flag) get_float(name string) ?f64 {
	flag := flags.get(name) ?
	return flag.get_float()
}

// get_string returns the `string` value argument of the flag.
// get_string returns an error if the `FlagType` is not string.
pub fn (flag Flag) get_string() ?string {
	if flag.flag != .string {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `string`')
	}
	return flag.value
}

// get_string returns the `string` value argument of the flag specified in `name`.
// get_string returns an error if the `FlagType` is not string.
pub fn (flags []Flag) get_string(name string) ?string {
	flag := flags.get(name) ?
	return flag.get_string()
}

// parse parses flag values from arguments and return
// an array of arguments with all consumed elements removed.
fn (mut flag Flag) parse(args []string, with_abbrev bool) ?[]string {
	if flag.matches(args, with_abbrev) {
		if flag.flag == .bool {
			new_args := flag.parse_bool(args) ?
			return new_args
		} else {
			new_args := flag.parse_raw(args) ?
			return new_args
		}
	} else {
		return args
	}
}

// matches returns `true` if first arg in `args` matches this flag.
fn (mut flag Flag) matches(args []string, with_abbrev bool) bool {
	if with_abbrev {
		return (flag.name != '' && args[0] == '--$flag.name') ||
			(flag.name != '' && args[0].starts_with('--$flag.name=')) ||
			(flag.abbrev != '' && args[0] == '-$flag.abbrev') ||
			(flag.abbrev != '' && args[0].starts_with('-$flag.abbrev='))
	} else {
		return (flag.name != '' && args[0] == '-$flag.name') ||
			(flag.name != '' && args[0].starts_with('-$flag.name='))
	}
}

fn (mut flag Flag) parse_raw(args []string) ?[]string {
	if args[0].len > flag.name.len && args[0].contains('=') {
		flag.value = args[0].split('=')[1]
		return args[1..]
	} else if args.len >= 2 {
		flag.value = args[1]
		return args[2..]
	}
	return error('Missing argument for `$flag.name`')
}

fn (mut flag Flag) parse_bool(args []string) ?[]string {
	if args[0].len > flag.name.len && args[0].contains('=') {
		flag.value = args[0].split('=')[1]
		return args[1..]
	} else if args.len >= 2 {
		if args[1] in ['true', 'false'] {
			flag.value = args[1]
			return args[2..]
		}
	}
	flag.value = 'true'
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

fn (flags []Flag) have_abbrev() bool {
	mut have_abbrev := false
	for flag in flags {
		if flag.abbrev != '' {
			have_abbrev = true
		}
	}
	return have_abbrev
}
