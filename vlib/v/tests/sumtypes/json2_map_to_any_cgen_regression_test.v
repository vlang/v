module main

import x.json2

struct RegressionReader {}

fn (mut rd RegressionReader) read_map_len() !int {
	return 1
}

fn (mut rd RegressionReader) read_string() !string {
	return 'a'
}

fn (mut rd RegressionReader) read_reply() !json2.Any {
	return json2.Any('b')
}

interface RegressionCmder {
mut:
	read_reply(mut rd RegressionReader) !
}

struct RegressionCmdableStateful {
mut:
	cmdable_stateful_function fn (mut cmd RegressionCmder) ! = unsafe { nil }
}

fn (c RegressionCmdableStateful) hello() !&RegressionMapStringAnyCmd {
	mut cmd := new_regression_map_string_any_cmd('hello')
	c.cmdable_stateful_function(mut cmd)!
	return cmd
}

struct RegressionBaseCmd {
	args []json2.Any
}

struct RegressionMapStringAnyCmd {
	RegressionBaseCmd
mut:
	val json2.Any
}

fn new_regression_map_string_any_cmd(args ...json2.Any) &RegressionMapStringAnyCmd {
	return &RegressionMapStringAnyCmd{
		RegressionBaseCmd: RegressionBaseCmd{
			args: args
		}
	}
}

fn (mut cmd RegressionMapStringAnyCmd) read_reply(mut rd RegressionReader) ! {
	n := rd.read_map_len()!
	mut new_map := map[string]json2.Any{}
	for _ in 0 .. n {
		k := rd.read_string()!
		v := rd.read_reply() or {
			if err.msg() == 'nil' {
				new_map[k] = 'nil'
				continue
			}
			return err
		}
		new_map[k] = v
	}
	cmd.val = new_map
}

@[heap]
struct RegressionConnection {
	RegressionCmdableStateful
}

fn new_regression_connection() &RegressionConnection {
	mut c := &RegressionConnection{}
	c.cmdable_stateful_function = c.process
	return c
}

fn (mut c RegressionConnection) process(mut cmd RegressionCmder) ! {
	mut rd := RegressionReader{}
	cmd.read_reply(mut rd)!
}

fn test_json2_map_assignment_to_any_inside_mut_receiver_does_not_break_cgen() {
	mut conn := new_regression_connection()
	cmd := conn.hello()!
	reply := cmd.val as map[string]json2.Any
	assert reply['a']!.str() == 'b'
}
