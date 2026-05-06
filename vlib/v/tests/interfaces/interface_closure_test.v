interface ITest {
mut:
	caller(a Test) !
}

struct Test {
}

struct Test2 {
}

fn (t2 Test2) with_reader(func fn (a Test) !) ! {
	return func(Test{})
}

fn (t Test) caller(a Test) ! {
	println('ok')
}

fn get() ITest {
	return Test{}
}

fn test_main() {
	mut a := get()

	b := Test2{}
	b.with_reader(a.caller)!
	assert true
}

interface ClosureCommand {
	value() int
mut:
	increment()
}

struct MutableClosureCommand {
mut:
	value_ int
}

fn (cmd MutableClosureCommand) value() int {
	return cmd.value_
}

fn (mut cmd MutableClosureCommand) increment() {
	cmd.value_++
}

fn with_closure_connection(func fn () !) ! {
	func()!
}

fn with_closure_writer(func fn () !) ! {
	func()!
}

fn with_closure_reader(func fn () !) ! {
	func()!
}

fn write_closure_command(cmd ClosureCommand) ! {
	assert cmd.value() == 41
}

fn read_closure_command(mut cmd ClosureCommand) ! {
	cmd.increment()
}

fn process_closure_command(mut cmd ClosureCommand) ! {
	with_closure_connection(fn [mut cmd] () ! {
		with_closure_writer(fn [cmd] () ! {
			write_closure_command(cmd)!
		})!
		with_closure_reader(fn [mut cmd] () ! {
			read_closure_command(mut cmd)!
		})!
	})!
}

fn test_nested_closure_captures_mut_interface_by_value() {
	mut cmd := ClosureCommand(MutableClosureCommand{
		value_: 41
	})
	process_closure_command(mut cmd)!
	assert cmd.value() == 42
}
