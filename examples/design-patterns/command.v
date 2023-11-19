module main

// Command is an interface that declares a method for executing a command.
// Commands can be simple or complex, depending on their implementation.
interface Command {
	execute()
}

// SimpleCommand is a concrete command that implements a simple operation.
struct SimpleCommand {
	payload string
}

// Execute prints a simple message with the payload.
fn (s SimpleCommand) execute() {
	println('SimpleCommand: See, I can do simple things like printing (${s.payload})')
}

// ComplexCommand is a concrete command that delegates more complex operations to a receiver.
struct ComplexCommand {
	receiver &Receiver
	a        string
	b        string
}

// Execute delegates complex operations to the receiver.
fn (c ComplexCommand) execute() {
	println('ComplexCommand: Complex stuff should be done by a receiver object.')
	c.receiver.do_something(c.a)
	c.receiver.do_something_else(c.b)
}

// Receiver is a class that contains important business logic.
struct Receiver {
}

// do_something simulates an operation on data a.
fn (r Receiver) do_something(a string) {
	println('Receiver: Working on (${a}.)')
}

// do_something_else simulates another operation on data b.
fn (r Receiver) do_something_else(b string) {
	println('Receiver: Also working on (${b}.)')
}

// Invoker is associated with one or several commands. It sends requests to the commands.
struct Invoker {
	on_start  ?Command
	on_finish ?Command
}

// do_something_important executes the associated commands, if any, before and after performing an important operation.
fn (i Invoker) do_something_important() {
	println('Invoker: Does anybody want something done before I begin?')
	if command := i.on_start {
		println('\non_start')
		command.execute()
	}

	println('Invoker: ...doing something really important...')

	println('Invoker: Does anybody want something done after I finish?')
	if command := i.on_finish {
		println('\non_finish')
		command.execute()
	}
}

fn main() {
	// Client code
	invoker := Invoker{
		on_start: SimpleCommand{
			payload: 'Say Hi!'
		}
		on_finish: ComplexCommand{
			receiver: &Receiver{}
			a: 'Send email'
			b: 'Save report'
		}
	}

	invoker.do_something_important()
}
