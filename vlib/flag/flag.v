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
	Error
	flag string
}

fn (err UnkownFlagError) msg() string {
	return 'Unknown flag `$err.flag`'
}

struct ArgsCountError {
	Error
	got  int
	want int
}

fn (err ArgsCountError) msg() string {
	if err.want == 0 {
		return 'Expected no arguments, but got $err.got'
	} else if err.got > err.want {
		return 'Expected at most $err.want arguments, but got $err.got'
	} else {
		return 'Expected at least $err.want arguments, but got $err.got'
	}
}

// free frees the resources associated with a given Flag
// It is called automatically when -autofree is used.
// It should be called manually in functions that use Flags,
// and are marked with [manualfree]. After you call .free() on
// a Flag instance, you should NOT use that instance any more.
[unsafe]
fn (mut f Flag) free() {
	unsafe {
		f.name.free()
		f.usage.free()
		f.val_desc.free()
	}
}

// str returns a string representation of the given Flag
pub fn (f Flag) str() string {
	return '' + '    flag:\n' + '            name: $f.name\n' +
		'            abbr: `$f.abbr.ascii_str()`\n' + '            usag: $f.usage\n' +
		'            desc: $f.val_desc'
}

// str returns a string representation of the given array of Flags
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
// call `rest := parser.finalize() ?` to get the rest of the non optional arguments
// (if there are any left).
pub struct FlagParser {
pub:
	original_args      []string // the original arguments to be parsed
	idx_dashdash       int      // the index of a `--`, -1 if there is not any
	all_after_dashdash []string // all options after `--` are ignored, and will be passed to the application unmodified
pub mut:
	usage_examples []string // when set, --help will print:
	// Usage: $appname $usage_examples[0]`
	//    or: $appname $usage_examples[1]`
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
	allow_unknown_args      bool     // whether passing undescribed arguments is allowed
	footers                 []string // when set, --help will display all the collected footers at the bottom.
}

// free frees the resources allocated for the given FlagParser instance.
// It should be called manually in functions that use it, and that are
// marked with `[manualfree]`,  otherwise, it is called automatically
// in programs, compiled with `-autofree`. Note: you should NOT use the
// instance over which you have called .free() for anything after the call.
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

// new_flag_parser - create a new flag parser for the given args
pub fn new_flag_parser(args []string) &FlagParser {
	original_args := args.clone()
	idx_dashdash := args.index('--')
	mut all_before_dashdash := args.clone()
	mut all_after_dashdash := []string{}
	if idx_dashdash >= 0 {
		all_before_dashdash.trim(idx_dashdash)
		if idx_dashdash < original_args.len {
			all_after_dashdash = original_args[idx_dashdash + 1..]
		}
	}
	return &FlagParser{
		original_args: original_args
		idx_dashdash: idx_dashdash
		all_after_dashdash: all_after_dashdash
		args: all_before_dashdash
		max_free_args: flag.max_args_number
	}
}

// usage_example - add an usage example
// All examples will be listed in the help screen.
// If you do not give any examples, then a default usage
// will be shown, based on whether the application takes
// options and expects additional parameters.
pub fn (mut fs FlagParser) usage_example(example string) {
	fs.usage_examples << example
}

// add_footer - add a footnote, that will be shown
// at the bottom of the help screen.
pub fn (mut fs FlagParser) footer(footer string) {
	fs.footers << footer
}

// change the application name to be used in 'usage' output
pub fn (mut fs FlagParser) application(name string) {
	fs.application_name = name
}

// change the application version to be used in 'usage' output
pub fn (mut fs FlagParser) version(vers string) {
	fs.application_version = vers
}

// description appends to the application description lines, shown
// in the help/usage screen
pub fn (mut fs FlagParser) description(desc string) {
	if fs.application_description.len == 0 {
		fs.application_description = desc
	} else {
		fs.application_description += '\n$desc'
	}
}

// in most cases you do not need the first argv for flag parsing
pub fn (mut fs FlagParser) skip_executable() {
	fs.args.delete(0)
}

// allow_unknown_args - if your program has sub commands, that have
// their own arguments, you can call .allow_unknown_args(), so that
// the subcommand arguments (which generally are not known to your
// parent program), will not cause the validation in .finalize() to fail.
pub fn (mut fs FlagParser) allow_unknown_args() {
	fs.allow_unknown_args = true
}

// private helper to register a flag
// This version supports abbreviations.
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

// bool_opt returns an option with the bool value of the given command line flag, named `name`.
// It returns an error, when the flag is not given by the user.
// This version supports abbreviations.
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

// bool defines and parses a string flag/option named `name`.
// If that flag is given by the user, then it returns its parsed bool value.
// When it is not, it returns the default value in `bdefault`.
// This version supports abbreviations.
pub fn (mut fs FlagParser) bool(name string, abbr byte, bdefault bool, usage string) bool {
	value := fs.bool_opt(name, abbr, usage) or { return bdefault }
	return value
}

// int_multi returns all values associated with the provided flag in `name`.
// When that flag has no values, it returns an empty array.
// This version supports abbreviations.
pub fn (mut fs FlagParser) int_multi(name string, abbr byte, usage string) []int {
	fs.add_flag(name, abbr, usage, '<multiple ints>')
	parsed := fs.parse_value(name, abbr)
	mut value := []int{}
	for val in parsed {
		value << val.int()
	}
	return value
}

// int_opt returns an option with the integer value, associated with the flag in `name`.
// When the flag is not given by the user, it returns an error.
// This version supports abbreviations.
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

// int defines and parses an integer flag, named `name`.
// When the flag is given by the user, it returns its parsed integer value.
// When it is not, it returns the integer value in `idefault`.
// This version supports abbreviations.
pub fn (mut fs FlagParser) int(name string, abbr byte, idefault int, usage string) int {
	value := fs.int_opt(name, abbr, usage) or { return idefault }
	return value
}

// float_multi returns all floating point values, associated with the flag named `name`.
// When no values for that flag are found, it returns an empty array.
// This version supports abbreviations.
pub fn (mut fs FlagParser) float_multi(name string, abbr byte, usage string) []f64 {
	fs.add_flag(name, abbr, usage, '<multiple floats>')
	parsed := fs.parse_value(name, abbr)
	mut value := []f64{}
	for val in parsed {
		value << val.f64()
	}
	return value
}

// float_opt returns an option with the floating point value, associated with the flag in `name`.
// When the flag is not given by the user, it returns an error.
// This version supports abbreviations.
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

// float defines and parses a floating point flag, named `name`.
// When the flag is given by the user, it returns its parsed floating point value.
// When it is not, it returns the value in `fdefault`.
// This version supports abbreviations.
pub fn (mut fs FlagParser) float(name string, abbr byte, fdefault f64, usage string) f64 {
	value := fs.float_opt(name, abbr, usage) or { return fdefault }
	return value
}

// string_multi returns all string values, associated with the flag named `name`.
// When no values for that flag are found, it returns an empty array.
// This version supports abbreviations.
pub fn (mut fs FlagParser) string_multi(name string, abbr byte, usage string) []string {
	fs.add_flag(name, abbr, usage, '<multiple strings>')
	return fs.parse_value(name, abbr)
}

// string_opt returns an option with the string value, associated with the flag in `name`.
// When the flag is not given by the user, it returns an error.
// This version supports abbreviations.
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

// string defines and parses a string flag/option, named `name`.
// If that flag is given as an option, then its parsed value is returned as a string.
// When it is not, it returns the default string value in `sdefault`.
// This version supports abbreviations.
pub fn (mut fs FlagParser) string(name string, abbr byte, sdefault string, usage string) string {
	value := fs.string_opt(name, abbr, usage) or { return sdefault }
	return value
}

// limit_free_args_to_at_least restricts the list of free arguments (non options) to be
// at least `n` in length. If the user gives less free arguments to the program,
// the parser will return an error.
pub fn (mut fs FlagParser) limit_free_args_to_at_least(n int) ? {
	if n > flag.max_args_number {
		return error('flag.limit_free_args_to_at_least expect n to be smaller than $flag.max_args_number')
	}
	if n <= 0 {
		return error('flag.limit_free_args_to_at_least expect n to be a positive number')
	}
	fs.min_free_args = n
}

// limit_free_args_to_exactly restricts the list of free arguments (non options) to be
// at exactly `n` in length. If the user gives more or less free arguments to the program,
// the parser will return an error.
pub fn (mut fs FlagParser) limit_free_args_to_exactly(n int) ? {
	if n > flag.max_args_number {
		return error('flag.limit_free_args_to_exactly expect n to be smaller than $flag.max_args_number')
	}
	if n < 0 {
		return error('flag.limit_free_args_to_exactly expect n to be a non negative number')
	}
	fs.min_free_args = n
	fs.max_free_args = n
}

// limit_free_args restricts the list of free arguments (non options) to be between
// `min` and `max` in length. If the user gives more or less free arguments to the program,
// the parser will return an error.
pub fn (mut fs FlagParser) limit_free_args(min int, max int) ? {
	if min > max {
		return error('flag.limit_free_args expect min < max, got $min >= $max')
	}
	fs.min_free_args = min
	fs.max_free_args = max
}

// arguments_description sets the description field of the parser.
// This field is usually shown when the `--help` option is given to the program.
pub fn (mut fs FlagParser) arguments_description(description string) {
	fs.args_description = description
}

// usage returns a nicely formatted usage screen, containing all the
// possible options, as well as the description for the program.
// That screen is usually shown when the `--help` option is given to the program.
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
	if fs.usage_examples.len == 0 {
		use << 'Usage: $fs.application_name [options] $adesc'
	} else {
		for i, example in fs.usage_examples {
			if i == 0 {
				use << 'Usage: $fs.application_name $example'
			} else {
				use << '   or: $fs.application_name $example'
			}
		}
	}
	use << ''
	if fs.application_description != '' {
		use << 'Description: $fs.application_description'
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
	for footer in fs.footers {
		use << footer
	}
	return use.join('\n').replace('- ,', '   ')
}

// find_existing_flag looks up the given flag by name, and returns
// it, if it was found in the FlagParser. If it was not, it returns an error.
fn (mut fs FlagParser) find_existing_flag(fname string) ?Flag {
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
		show_help = fs.bool('help', `h`, false, fs.default_help_label)
	}
	fs.find_existing_flag('version') or {
		show_version = fs.bool('version', 0, false, fs.default_version_label)
	}
	if show_help {
		println(fs.usage())
		exit(0)
	}
	if show_version {
		println('$fs.application_name $fs.application_version')
		exit(0)
	}
}

// finalize - return all remaining arguments (non options).
// Call .finalize() after all arguments are defined.
// The remaining arguments are returned in the same order they are
// defined on the command line. If additional flags are found, i.e.
// (things starting with '--' or '-'), it returns an error.
pub fn (mut fs FlagParser) finalize() ?[]string {
	fs.handle_builtin_options()
	mut remaining := fs.args.clone()
	if !fs.allow_unknown_args {
		for a in remaining {
			if (a.len >= 2 && a[..2] == '--') || (a.len == 2 && a[0] == `-`) {
				return IError(&UnkownFlagError{
					flag: a
				})
			}
		}
	}
	if remaining.len < fs.min_free_args && fs.min_free_args > 0 {
		return IError(&ArgsCountError{
			want: fs.min_free_args
			got: remaining.len
		})
	}
	if remaining.len > fs.max_free_args && fs.max_free_args > 0 {
		return IError(&ArgsCountError{
			want: fs.max_free_args
			got: remaining.len
		})
	}
	if remaining.len > 0 && fs.max_free_args == 0 && fs.min_free_args == 0 {
		return IError(&ArgsCountError{
			want: 0
			got: remaining.len
		})
	}
	remaining << fs.all_after_dashdash
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
