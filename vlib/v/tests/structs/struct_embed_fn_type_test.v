type FnClick = fn () string

struct Widget {
mut:
	x     int
	y     int
	click FnClick = click
}

fn click() string {
	return 'click me'
}

struct Button {
	Widget
	title string
}

fn test_struct_embed_fn_type() {
	mut button := Button{
		title: 'Click me'
	}

	button.x = 3
	ret := button.click()

	println(ret)
	assert ret == 'click me'
}

interface FnExecutorMoreMethods {
	more() string
}

interface FnExecutorMethods {
	FnExecutorMoreMethods
	one() string
}

type CommandFn = fn (s string) string

fn (f CommandFn) more() string {
	return f('more')
}

fn (f CommandFn) one() string {
	return f('one')
}

struct CommandSet {
	CommandFn
}

fn run_command(name string) string {
	return name
}

fn use_command_api(cmd FnExecutorMethods) (string, string) {
	return cmd.one(), cmd.more()
}

fn test_embed_named_fn_type_promotes_methods() {
	cmd := CommandSet{
		CommandFn: run_command
	}

	assert cmd.CommandFn.one() == 'one'
	assert cmd.one() == 'one'
	assert cmd.more() == 'more'

	one, more := use_command_api(cmd)
	assert one == 'one'
	assert more == 'more'
}
