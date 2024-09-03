// Test .v (V) parse style
import flag

const exe_and_v_long_args = ['/path/to/exe', '--version', '--p', 'ident=val', '--o', '/path/to',
	'--test', 'abc', '--done', '--pop', 'two', '--live']
const exe_and_v_long_args_with_tail = ['/path/to/exe', '--version', '--p', 'ident=val', '--test',
	'abc', '--done', '--p', 'two', '--live', 'run', '/path/to']
const error_wrong_assignment_flags = ['--o=error']

struct Prefs {
	version    bool @[short: v]
	is_live    bool @[long: live]
	is_done    bool @[long: done]
	test       string
	pop_flags  []string @[long: pop; short: p]
	tail       []string @[tail]
	out        string   @[only: o]
	not_mapped string = 'not changed'
}

fn test_long_v_style() {
	prefs, _ := flag.to_struct[Prefs](exe_and_v_long_args, skip: 1, style: .v_long)!
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.test == 'abc'
	assert prefs.pop_flags.len == 2
	assert prefs.pop_flags[0] == 'ident=val'
	assert prefs.pop_flags[1] == 'two'
	assert prefs.tail.len == 0
	assert prefs.out == '/path/to'
	assert prefs.not_mapped == 'not changed'
}

fn test_long_v_style_no_exe() {
	prefs, _ := flag.to_struct[Prefs](exe_and_v_long_args[1..], style: .v_long)!
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.test == 'abc'
	assert prefs.pop_flags.len == 2
	assert prefs.pop_flags[0] == 'ident=val'
	assert prefs.pop_flags[1] == 'two'
	assert prefs.tail.len == 0
	assert prefs.out == '/path/to'
	assert prefs.not_mapped == 'not changed'
}

fn test_long_v_style_with_tail() {
	prefs, _ := flag.to_struct[Prefs](exe_and_v_long_args_with_tail, skip: 1, style: .v_long)!
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.test == 'abc'
	assert prefs.not_mapped == 'not changed'
	assert prefs.pop_flags.len == 2
	assert prefs.pop_flags[0] == 'ident=val'
	assert prefs.pop_flags[1] == 'two'
	assert prefs.out == ''
	assert prefs.not_mapped == 'not changed'
	assert prefs.tail.len == 2
	assert prefs.tail[0] == 'run'
	assert prefs.tail[1] == '/path/to'
}

fn test_long_v_style_with_tail_no_exe() {
	prefs, _ := flag.to_struct[Prefs](exe_and_v_long_args_with_tail[1..], style: .v_long)!
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.test == 'abc'
	assert prefs.not_mapped == 'not changed'
	assert prefs.pop_flags.len == 2
	assert prefs.pop_flags[0] == 'ident=val'
	assert prefs.pop_flags[1] == 'two'
	assert prefs.out == ''
	assert prefs.not_mapped == 'not changed'
	assert prefs.tail.len == 2
	assert prefs.tail[0] == 'run'
	assert prefs.tail[1] == '/path/to'
}

fn test_long_v_style_error_message() {
	if _, _ := flag.to_struct[Prefs](exe_and_v_long_args[1..], style: .v) {
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == 'long delimiter `--` encountered in flag `--version` in v (V) style parsing mode. Maybe you meant `.v_long`?'
	}
	if _, _ := flag.to_struct[Prefs](error_wrong_assignment_flags, style: .v_long) {
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == '`=` in flag `--o=error` is not supported in V long style parsing mode. Use `--flag value` instead'
	}
}
