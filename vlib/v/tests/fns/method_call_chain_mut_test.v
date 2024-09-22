struct App {
	name    string
	version string
mut:
	commands []Command
	flags    []Flag
}

struct Command {
	name    string
	execute fn (cmd Command) = unsafe { nil }
}

struct Flag {
	name     string
	abbrev   string
	required bool
}

fn (mut app App) command(cmd Command) &App {
	app.commands << cmd
	return unsafe { app }
}

fn (mut app App) flag(flag Flag) &App {
	app.flags << flag
	return unsafe { app }
}

fn test_main() {
	mut app := App{
		name:     'amazing cli'
		version:  '0.1.0'
		commands: []
		flags:    []
	}

	w := app.command(Command{
		name:    'test'
		execute: fn (cmd Command) {
			println('Testing')
		}
	}).flag(Flag{
		name:     'verbose'
		abbrev:   'v'
		required: true
	})

	println(app)
	assert app.flags.len == 1
	assert app.commands.len == 1
	assert w.flags.len == 1
	assert w.commands.len == 1
}
