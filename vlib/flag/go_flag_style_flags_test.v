// Test .v (V) parse style
import flag

const exe_and_go_flag_args = ['/path/to/exe', '-version', '-d', 'ident=val', '-o', '/path/to',
	'-test', 'abc', '-done', '-define', 'two', '-live', '--flag', '--flag-value=ok']
const exe_and_go_flag_args_with_tail = ['/path/to/exe', '-version', '--flag', '--flag-value=ok',
	'-d', 'ident=val', '-test', 'abc', '-done', '-d', 'two', '-live', 'run', '/path/to']

struct Prefs {
	flag       bool
	flag_value string
	version    bool @[short: v]
	is_live    bool @[long: live]
	is_done    bool @[long: done]
	test       string
	defines    []string @[long: define; short: d]
	tail       []string @[tail]
	out        string   @[only: o]
	not_mapped string = 'not changed'
}

fn test_go_flag_style() {
	prefs, _ := flag.to_struct[Prefs](exe_and_go_flag_args, skip: 1, style: .go_flag)!
	assert prefs.flag
	assert prefs.flag_value == 'ok'
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.test == 'abc'
	assert prefs.defines.len == 2
	assert prefs.defines[0] == 'ident=val'
	assert prefs.defines[1] == 'two'
	assert prefs.tail.len == 0
	assert prefs.out == '/path/to'
	assert prefs.not_mapped == 'not changed'
}

fn test_go_flag_style_no_exe() {
	prefs, _ := flag.to_struct[Prefs](exe_and_go_flag_args[1..], style: .go_flag)!
	assert prefs.flag
	assert prefs.flag_value == 'ok'
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.test == 'abc'
	assert prefs.defines.len == 2
	assert prefs.defines[0] == 'ident=val'
	assert prefs.defines[1] == 'two'
	assert prefs.tail.len == 0
	assert prefs.out == '/path/to'
	assert prefs.not_mapped == 'not changed'
}

fn test_go_flag_style_with_tail() {
	prefs, _ := flag.to_struct[Prefs](exe_and_go_flag_args_with_tail, skip: 1, style: .go_flag)!
	assert prefs.flag
	assert prefs.flag_value == 'ok'
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.test == 'abc'
	assert prefs.not_mapped == 'not changed'
	assert prefs.defines.len == 2
	assert prefs.defines[0] == 'ident=val'
	assert prefs.defines[1] == 'two'
	assert prefs.out == ''
	assert prefs.not_mapped == 'not changed'
	assert prefs.tail.len == 2
	assert prefs.tail[0] == 'run'
	assert prefs.tail[1] == '/path/to'
}

fn test_go_flag_style_with_tail_no_exe() {
	prefs, _ := flag.to_struct[Prefs](exe_and_go_flag_args_with_tail[1..], style: .go_flag)!
	assert prefs.flag
	assert prefs.flag_value == 'ok'
	assert prefs.version
	assert prefs.is_live
	assert prefs.is_done
	assert prefs.test == 'abc'
	assert prefs.not_mapped == 'not changed'
	assert prefs.defines.len == 2
	assert prefs.defines[0] == 'ident=val'
	assert prefs.defines[1] == 'two'
	assert prefs.out == ''
	assert prefs.not_mapped == 'not changed'
	assert prefs.tail.len == 2
	assert prefs.tail[0] == 'run'
	assert prefs.tail[1] == '/path/to'
}
