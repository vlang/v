module flag

// module flag for command-line flag parsing
//
// - parsing flags like '--flag' or '--stuff=things' or '--things stuff'
// - handles bool, int, float and string args
// - is able to print usage
// - handled unknown arguments as error
//
// Usage example:
//
//  ```v
//  module main
//
//  import os
//  import flag
//
//  fn main() {
//  	mut fp := flag.new_flag_parser(os.args)
//  	fp.application('flag_example_tool')
//  	fp.version('v0.0.0')
//  	fp.description('This tool is only designed to show how the flag lib is working')
//
//  	fp.skip_executable()
//
//  	an_int := fp.int('an_int', 666, 'some int to define 666 is default')
//  	a_bool := fp.bool('a_bool', false, 'some \'real\' flag')
//  	a_float := fp.float('a_float', 1.0, 'also floats')
//  	a_string := fp.string('a_string', 'no text', 'finally, some text')
//
//  	additional_args := fp.finalize() or {
//  		eprintln(err)
//  		println(fp.usage())
//  		return
//  	}
//
//  	println('
//  		  an_int: $an_int
//  		  a_bool: $a_bool
//  		 a_float: $a_float
//  		a_string: \'$a_string\'
//  	')
//  	println(additional_args.join_lines())
//  }
//  ```

// data object storing information about a defined flag
pub struct Flag {
	pub:
	name     string // name as it appears on command line
	abbr     byte   // shortcut
	usage    string // help message
	val_desc string // something like '<arg>' that appears in usage,
	// and also the default value, when the flag is not given
}

//
pub struct FlagParser {
	pub mut:
	args  []string                  // the arguments to be parsed
	flags []Flag                    // registered flags

	application_name        string
	application_version     string
	application_description string

	min_free_args int
	max_free_args int
	args_description        string
}

pub const (
	// used for formating usage message
	SPACE = '                            '
	UNDERLINE = '-----------------------------------------------'
	MAX_ARGS_NUMBER = 4048
)

// create a new flag set for parsing command line arguments
// TODO use INT_MAX some how
pub fn new_flag_parser(args []string) &FlagParser {
	return &FlagParser{args:args, max_free_args: MAX_ARGS_NUMBER}
}

// change the application name to be used in 'usage' output
pub fn (fs mut FlagParser) application(n string) {
	fs.application_name = n
}

// change the application version to be used in 'usage' output
pub fn (fs mut FlagParser) version(n string) {
	fs.application_version = n
}

// change the application version to be used in 'usage' output
pub fn (fs mut FlagParser) description(n string) {
	fs.application_description = n
}

// in most cases you do not need the first argv for flag parsing
pub fn (fs mut FlagParser) skip_executable() {
	fs.args.delete(0)
}

// private helper to register a flag
fn (fs mut FlagParser) add_flag(n string, a byte, u, vd string) {
	fs.flags << Flag{
		name: n,
		abbr: a,
		usage: u,
		val_desc: vd
	}
}

// private: general parsing a single argument
//  - search args for existence
//    if true
//      extract the defined value as string
//    else
//      return an (dummy) error -> argument is not defined
//
//  - the name, usage are registered
//  - found arguments and corresponding values are removed from args list
fn (fs mut FlagParser) parse_value(longhand string, shorthand byte) []string {
	full := '--$longhand'
	mut found_entries := []string
	mut to_delete := []int
	mut should_skip_one := false
	for i, arg in fs.args {
		if should_skip_one {
			should_skip_one = false
			continue
		}
		if arg == '--' {
			//End of input. We're done here.
			break
		}
		if arg == full || (arg[0] == `-` && arg[1] == shorthand && arg.len == 2) {
			if i+1 > fs.args.len {
				panic("Missing argument for '$longhand'")
			}
			nextarg := fs.args[i+1]
			if nextarg.len > 2 && nextarg[..2] == '--' {
				//It could be end of input (--) or another argument (--abc).
				//Both are invalid so die.
				panic("Missing argument for '$longhand'")
			}
			found_entries << fs.args[i+1]
			to_delete << i
			to_delete << i+1
			should_skip_one = true
			continue
		}
		if arg.len > full.len+1 && arg[..full.len+1] == '$full=' {
			found_entries << arg[full.len+1..]
			to_delete << i
			continue
		}
	}
	for i, del in to_delete {
		//i entrys are deleted so it's shifted left i times.
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
fn (fs mut FlagParser) parse_bool_value(longhand string, shorthand byte) ?string {
	full := '--$longhand'
	for i, arg in fs.args {
		if arg == '--' {
			//End of input. We're done.
			break
		}
		if arg == full || (arg[0] == `-` && arg[1] == shorthand && arg.len == 2) {
			if fs.args.len > i+1 && (fs.args[i+1] in ['true', 'false'])  {
				val := fs.args[i+1]
				fs.args.delete(i+1)
				fs.args.delete(i)
				return val
			} else {
				fs.args.delete(i)
				return 'true'
			}
		}
		if arg.len > full.len+1 && arg[..full.len+1] == '$full=' {
			// Flag abc=true
			val := arg[full.len+1..]
			fs.args.delete(i)
			return val
		}
		if arg[0] == `-` && arg.index_byte(shorthand) != -1 {
			// -abc is equivalent to -a -b -c
			return 'true'
		}
	}
	return error("parameter '$longhand' not found")
}

// bool_opt returns an optional that returns the value associated with the flag.
// In the situation that the flag was not provided, it returns null.
pub fn (fs mut FlagParser) bool_opt(n string, a byte, u string) ?bool {
	fs.add_flag(n, a, u, '<bool>')
	parsed := fs.parse_bool_value(n, a) or {
		return error("parameter '$n' not provided")
	}
	return parsed == 'true'
}

// defining and parsing a bool flag
//  if defined
//      the value is returned (true/false)
//  else
//      the default value is returned
// version with abbreviation
//TODO error handling for invalid string to bool conversion
pub fn (fs mut FlagParser) bool_(n string, a byte, v bool, u string) bool {
	value := fs.bool_opt(n, a, u) or {
		return v
	}
	return value
}

// defining and parsing a bool flag
//  if defined
//      the value is returned (true/false)
//  else
//      the default value is returned
//TODO error handling for invalid string to bool conversion
pub fn (fs mut FlagParser) bool(n string, v bool, u string) bool {
	return fs.bool_(n, `\0`, v, u)
}

// int_multi returns all instances of values associated with the flags provided
// In the case that none were found, it returns an empty array.
pub fn (fs mut FlagParser) int_multi(n string, a byte, u string) []int {
	fs.add_flag(n, a, u, '<multiple ints>')
	parsed := fs.parse_value(n, a)
	mut value := []int
	for val in parsed {
		value << val.int()
	}
	return value
}

// int_opt returns an optional that returns the value associated with the flag.
// In the situation that the flag was not provided, it returns null.
pub fn (fs mut FlagParser) int_opt(n string, a byte, u string) ?int {
	fs.add_flag(n, a, u, '<int>')
	parsed := fs.parse_value(n, a)
	if parsed.len == 0 {
		return error("parameter '$n' not provided")
	}
	return parsed[0].int()
}

// defining and parsing an int flag
//  if defined
//      the value is returned (int)
//  else
//      the default value is returned
// version with abbreviation
//TODO error handling for invalid string to int conversion
pub fn (fs mut FlagParser) int_(n string, a byte, i int, u string) int {
	value := fs.int_opt(n, a, u) or {
		return i
	}
	return value
}

// defining and parsing an int flag
//  if defined
//      the value is returned (int)
//  else
//      the default value is returned
//TODO error handling for invalid string to int conversion
pub fn (fs mut FlagParser) int(n string, i int, u string) int {
	return fs.int_(n, `\0`, i, u)
}

// float_multi returns all instances of values associated with the flags provided
// In the case that none were found, it returns an empty array.
pub fn (fs mut FlagParser) float_multi(n string, a byte, u string) []f32 {
	fs.add_flag(n, a, u, '<multiple floats>')
	parsed := fs.parse_value(n, a)
	mut value := []f32
	for val in parsed {
		value << val.f32()
	}
	return value
}

// float_opt returns an optional that returns the value associated with the flag.
// In the situation that the flag was not provided, it returns null.
pub fn (fs mut FlagParser) float_opt(n string, a byte, u string) ?f32 {
	fs.add_flag(n, a, u, '<float>')
	parsed := fs.parse_value(n, a)
	if parsed.len == 0 {
		return error("parameter '$n' not provided")
	}
	return parsed[0].f32()
}

// defining and parsing a float flag
//  if defined
//      the value is returned (float)
//  else
//      the default value is returned
// version with abbreviation
//TODO error handling for invalid string to float conversion
pub fn (fs mut FlagParser) float_(n string, a byte, f f32, u string) f32 {
	value := fs.float_opt(n, a, u) or {
		return f
	}
	return value
}

// defining and parsing a float flag
//  if defined
//      the value is returned (float)
//  else
//      the default value is returned
//TODO error handling for invalid string to float conversion
pub fn (fs mut FlagParser) float(n string, f f32, u string) f32 {
	return fs.float_(n, `\0`, f, u)
}

// string_multi returns all instances of values associated with the flags provided
// In the case that none were found, it returns an empty array.
pub fn (fs mut FlagParser) string_multi(n string, a byte, u string) []string {
	fs.add_flag(n, a, u, '<multiple floats>')
	return fs.parse_value(n, a)
}

// string_opt returns an optional that returns the value associated with the flag.
// In the situation that the flag was not provided, it returns null.
pub fn (fs mut FlagParser) string_opt(n string, a byte, u string) ?string {
	fs.add_flag(n, a, u, '<string>')
	parsed := fs.parse_value(n, a)
	if parsed.len == 0 {
		return error("parameter '$n' not provided")
	}
	return parsed[0]
}

// defining and parsing a string flag
//  if defined
//      the value is returned (string)
//  else
//      the default value is returned
// version with abbreviation
pub fn (fs mut FlagParser) string_(n string, a byte, v, u string) string {
	value := fs.string_opt(n, a, u) or {
		return v
	}
	return value
}

// defining and parsing a string flag
//  if defined
//      the value is returned (string)
//  else
//      the default value is returned
pub fn (fs mut FlagParser) string(n, v, u string) string {
	return fs.string_(n, `\0`, v, u)
}

pub fn (fs mut FlagParser) limit_free_args_to_at_least(n int) {
	if n > MAX_ARGS_NUMBER {
		panic('flag.limit_free_args_to_at_least expect n to be smaller than $MAX_ARGS_NUMBER')
	}
	if n <= 0 {
		panic('flag.limit_free_args_to_at_least expect n to be a positive number')
	}
	fs.min_free_args = n
}

pub fn (fs mut FlagParser) limit_free_args_to_exactly(n int) {
	if n > MAX_ARGS_NUMBER {
		panic('flag.limit_free_args_to_exactly expect n to be smaller than $MAX_ARGS_NUMBER')
	}
	if n < 0 {
		panic('flag.limit_free_args_to_exactly expect n to be a non negative number')
	}
	fs.min_free_args = n
	fs.max_free_args = n
}

// this will cause an error in finalize() if free args are out of range
// (min, ..., max)
pub fn (fs mut FlagParser) limit_free_args(min, max int) {
	if min > max {
		panic('flag.limit_free_args expect min < max, got $min >= $max')
	}
	fs.min_free_args = min
	fs.max_free_args = max
}

pub fn (fs mut FlagParser) arguments_description(description string){
	fs.args_description = description
}

// collect all given information and
pub fn (fs FlagParser) usage() string {

	positive_min_arg := ( fs.min_free_args > 0 )
	positive_max_arg := ( fs.max_free_args > 0 && fs.max_free_args != MAX_ARGS_NUMBER )
	no_arguments := ( fs.min_free_args == 0 && fs.max_free_args == 0 )

	mut adesc := if fs.args_description.len > 0 { fs.args_description } else { '[ARGS]' }
	if no_arguments { adesc = '' }

	mut use := ''
	use += '$fs.application_name $fs.application_version\n'
	use += '$UNDERLINE\n'
	use += 'Usage: ${fs.application_name} [options] $adesc\n'
	use += '\n'
	if fs.application_description != '' {
		use += 'Description:\n'
		use += '$fs.application_description'
		use += '\n\n'
	}

	// show a message about the [ARGS]:
	if positive_min_arg || positive_max_arg || no_arguments {
		if no_arguments {
			use += 'This application does not expect any arguments\n\n'
			goto end_of_arguments_handling
		}
		mut s:= []string
		if positive_min_arg { s << 'at least $fs.min_free_args' }
		if positive_max_arg { s << 'at most $fs.max_free_args' }
		if positive_min_arg && positive_max_arg && fs.min_free_args == fs.max_free_args {
			s = ['exactly $fs.min_free_args']
		}
		sargs := s.join(' and ')
		use += 'The arguments should be $sargs in number.\n\n'
	}
	end_of_arguments_handling:

	if fs.flags.len > 0 {
		use += 'Options:\n'
		for f in fs.flags {
			flag_desc := '  --$f.name $f.val_desc'
			space := if flag_desc.len > SPACE.len-2 {
				'\n$SPACE'
			} else {
				SPACE[flag_desc.len..]
			}
			abbr_desc := if f.abbr == `\0` { '' } else { '  -${tos(f.abbr, 1)}\n' }
			use += '${abbr_desc}${flag_desc}${space}${f.usage}\n'
		}
	}

	return use
}

// finalize argument parsing -> call after all arguments are defined
//
// all remaining arguments are returned in the same order they are defined on
// command line
//
// if additional flag are found (things starting with '--') an error is returned
// error handling is up to the application developer
pub fn (fs FlagParser) finalize() ?[]string {
	for a in fs.args {
		if a.len >= 2 && a[..2] == '--' {
			return error('Unknown argument \'${a[2..]}\'')
		}
	}
	if fs.args.len < fs.min_free_args && fs.min_free_args > 0 {
		return error('Expected at least ${fs.min_free_args} arguments, but given $fs.args.len')
	}
	if fs.args.len > fs.max_free_args && fs.max_free_args > 0 {
		return error('Expected at most ${fs.max_free_args} arguments, but given $fs.args.len')
	}
	if fs.args.len > 0 && fs.max_free_args == 0 && fs.min_free_args == 0 {
		return error('Expected no arguments, but given $fs.args.len')
	}
	return fs.args
}

