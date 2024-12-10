// Test .v_flag_parser (V flag.FlagParser) parse style
import flag

const exe_and_v_flag_parser_args = ['/path/to/exe', '--version', '-p', 'ident=val', '--o', '/path/to',
	'--test', 'abc', '--done', '--pop', 'two', '--live']
const exe_and_v_flag_parser_args_with_tail = ['/path/to/exe', '--version', '-p', 'ident=val',
	'--test', 'abc', '--done', '-p', 'two', '--live', 'run', '/path/to', 'platforms;android-21']
const error_wrong_assignment_flags = ['--o=error']

struct Prefs {
	version    bool @[short: v]
	is_live    bool @[long: live]
	is_done    bool @[long: done]
	dump_usage bool @[long: 'help'; short: h]
	test       string
	pop_flags  []string @[long: pop; short: p]
	tail       []string @[tail]
	out        string   @[only: o]
	not_mapped string = 'not changed'
}

fn test_long_v_style() {
	prefs, _ := flag.to_struct[Prefs](exe_and_v_flag_parser_args, skip: 1, style: .v_flag_parser)!
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.dump_usage == false
	assert prefs.test == 'abc'
	assert prefs.pop_flags.len == 2
	assert prefs.pop_flags[0] == 'ident=val'
	assert prefs.pop_flags[1] == 'two'
	assert prefs.tail.len == 0
	assert prefs.out == '/path/to'
	assert prefs.not_mapped == 'not changed'
}

fn test_long_v_style_no_exe() {
	prefs, _ := flag.to_struct[Prefs](exe_and_v_flag_parser_args[1..], style: .v_flag_parser)!
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.dump_usage == false
	assert prefs.test == 'abc'
	assert prefs.pop_flags.len == 2
	assert prefs.pop_flags[0] == 'ident=val'
	assert prefs.pop_flags[1] == 'two'
	assert prefs.tail.len == 0
	assert prefs.out == '/path/to'
	assert prefs.not_mapped == 'not changed'
}

fn test_long_v_style_with_tail() {
	prefs, _ := flag.to_struct[Prefs](exe_and_v_flag_parser_args_with_tail,
		skip:  1
		style: .v_flag_parser
	)!
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.dump_usage == false
	assert prefs.test == 'abc'
	assert prefs.not_mapped == 'not changed'
	assert prefs.pop_flags.len == 2
	assert prefs.pop_flags[0] == 'ident=val'
	assert prefs.pop_flags[1] == 'two'
	assert prefs.out == ''
	assert prefs.not_mapped == 'not changed'
	assert prefs.tail.len == 3
	assert prefs.tail[0] == 'run'
	assert prefs.tail[1] == '/path/to'
	assert prefs.tail[2] == 'platforms;android-21'
}

fn test_long_v_style_with_tail_no_exe() {
	prefs, _ := flag.to_struct[Prefs](exe_and_v_flag_parser_args_with_tail[1..],
		style: .v_flag_parser
	)!
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.dump_usage == false
	assert prefs.test == 'abc'
	assert prefs.not_mapped == 'not changed'
	assert prefs.pop_flags.len == 2
	assert prefs.pop_flags[0] == 'ident=val'
	assert prefs.pop_flags[1] == 'two'
	assert prefs.out == ''
	assert prefs.not_mapped == 'not changed'
	assert prefs.tail.len == 3
	assert prefs.tail[0] == 'run'
	assert prefs.tail[1] == '/path/to'
	assert prefs.tail[2] == 'platforms;android-21'
}

fn test_long_v_style_with_exe_and_short_alias() {
	prefs, _ := flag.to_struct[Prefs](['/path/to/exe', '-h'],
		skip:  1
		style: .v_flag_parser
	)!
	assert prefs.version == false
	assert prefs.is_live == false
	assert prefs.is_done == false
	assert prefs.dump_usage == true
	assert prefs.test == ''
	assert prefs.not_mapped == 'not changed'
	assert prefs.pop_flags.len == 0
	assert prefs.out == ''
	assert prefs.not_mapped == 'not changed'
	assert prefs.tail.len == 0
}

fn test_long_v_style_error_message() {
	if _, _ := flag.to_struct[Prefs](exe_and_v_flag_parser_args[1..], style: .v) {
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == 'long delimiter `--` encountered in flag `--version` in v (V) style parsing mode. Maybe you meant `.v_flag_parser`?'
	}
	if _, _ := flag.to_struct[Prefs](error_wrong_assignment_flags, style: .v_flag_parser) {
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == '`=` in flag `--o=error` is not supported in V `flag.FlagParser` (long) style parsing mode. Use `--flag value` instead'
	}
}
