module cli

pub enum FlagType {
	bool
	int
	float
	string
}

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

pub fn (flags []Flag) get_all_found() []Flag {
	return flags.filter(it.found)
}

pub fn (flag Flag) get_bool() ?bool {
	if flag.flag != .bool {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `bool`')
	}
	return flag.value == 'true'
}

pub fn (flags []Flag) get_bool(name string) ?bool {
	flag := flags.get(name) ?
	return flag.get_bool()
}

pub fn (flag Flag) get_int() ?int {
	if flag.flag != .int {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `int`')
	}
	return flag.value.int()
}

pub fn (flags []Flag) get_int(name string) ?int {
	flag := flags.get(name) ?
	return flag.get_int()
}

pub fn (flag Flag) get_float() ?f64 {
	if flag.flag != .float {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `float`')
	}
	return flag.value.f64()
}

pub fn (flags []Flag) get_float(name string) ?f64 {
	flag := flags.get(name) ?
	return flag.get_float()
}

pub fn (flag Flag) get_string() ?string {
	if flag.flag != .string {
		return error('$flag.name: Invalid flag type `$flag.flag`, expected `string`')
	}
	return flag.value
}

pub fn (flags []Flag) get_string(name string) ?string {
	flag := flags.get(name) ?
	return flag.get_string()
}

// parse flag value from arguments and return arguments with all consumed element removed
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

// check if first arg matches flag
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
