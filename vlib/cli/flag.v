module cli

pub enum FlagType {
	bool
	int
	string
}

pub struct Flag {
mut:
	flag FlagType
	name string
	abbrev string
	description string
	global bool
	required bool
	value string
}

fn (flag mut Flag) matches(args []string) bool {
	return 
		(flag.name != '' && args[0].starts_with('--${flag.name}')) ||
		(flag.abbrev != '' && args[0].starts_with('-${flag.abbrev}'))
}

fn (flag mut Flag) parse(args []string) ?[]string {
	if flag.matches(args) {
		match flag.flag {
			.bool { 
				new_args := flag.parse_bool(args) or { return error(err) } 
				return new_args 
			}
			.int { 
				new_args := flag.parse_int(args) or { return error(err) } 
				return new_args 
			}
			.string { 
				new_args := flag.parse_string(args) or { return error(err) } 
				return new_args 
			}
		}
	}
	return args
}

fn (flag mut Flag) parse_bool(args []string) ?[]string {
	if args[0].len > flag.name.len && args[0].contains('=') {
		flag.value = args[0].split('=')[1]
		return args.right(1)
	} else if args.len >= 2 && args[1] in ['true', 'false'] {
		flag.value = args[1]
		return args.right(2)
	} else {
		flag.value = 'true'
		return args.right(1)
	}
}

fn (flag mut Flag) parse_int(args []string) ?[]string {
	if args[0].len > flag.name.len && args[0].contains('=') {
		flag.value = args[0].split('=')[1]
		return args.right(1)
	} else if args.len >= 2 {
		flag.value = args[1]
		return args.right(2)
	}
	return error('missing argument for ${flag.name}')
}

fn (flag mut Flag) parse_string(args []string) ?[]string {
	if args[0].len > flag.name.len && args[0].contains('=') {
		flag.value = args[0].split('=')[1]
		return args.right(1)
	} else if args.len >= 2 {
		flag.value = args[1]
		return args.right(2)
	} 
	return error('missing argument for ${flag.name}')
}


pub fn (flags []Flag) get_string(name string) ?string {
	for flag in flags {
		if flag.name == name {
			return flag.value
		}
	}
	return error('flag ${name} not found.')
}

pub fn (flags []Flag) get_bool(name string) ?bool {
	value := flags.get_string(name) or { return error(err) }
	return value == 'true'
}

pub fn (flags []Flag) get_int(name string) ?int {
	value := flags.get_string(name) or { return error(err) }
	return value.int()
}
