module cli

pub struct Command {
pub mut:
	name string
	description string
	execute fn(cmd cli.Command, args []string)

	commands []Command
	flags []Flag
	args []string
}

pub fn (cmd mut Command) add_command(command Command) {
	cmd.commands << command
}

pub fn (cmd mut Command) add_flag(flag Flag) {
	cmd.flags << flag
}

pub fn (cmd mut Command) parse(args []string) {
	cmd.args = args.right(1)

	cmd.parse_flags()
	cmd.check_required_flags()
	cmd.parse_command()
}

fn (cmd mut Command) parse_flags() {
	for {
		if cmd.args.len < 1 || !cmd.args[0].starts_with('-') {
			break
		}

		mut found := false
		for i := 0; i < cmd.flags.len; i++ {
			mut flag := &cmd.flags[i]
			if flag.matches(cmd.args) {
				found = true
				mut args := flag.parse(cmd.args) or {	// TODO: fix once options types can be assigned to struct variables
					println('failed to parse flag ${cmd.args[0]}')
					exit(1)
				}
				cmd.args = args
				break
			}
		}

		if !found {
			println('invalid flag: ${cmd.args[0]}')
			exit(1)
		}
	}
}

fn (cmd mut Command) check_required_flags() {
	for flag in cmd.flags {
		if flag.required && flag.value == '' {
			println('flag ${flag.name} is required')
			exit(1)
		}
	}
}

fn (cmd mut Command) parse_command() {
	flags := cmd.flags
	global_flags := flags.filter(it.global) // TODO: fix once filter can be applied to struct variable

	for i := 0; i < cmd.args.len; i++ {
		arg := cmd.args[i]
		for j := 0; j < cmd.commands.len; j++ {
			mut command := cmd.commands[j]
			if command.name == arg {
				command.flags << global_flags
				command.parse(cmd.args.right(i))
				return
			}
		}
	}

	if int(cmd.execute) == 0 {
		cmd.print_usage()
	} else {
		execute := cmd.execute
		execute(cmd, cmd.args) // TODO: fix once higher order function can be execute on struct variable
	}
}

pub fn (cmd mut Command) print_usage() {
	println(cmd.description + '\n')
	println('COMMAND:')
	for command in cmd.commands {
		println(command.name + '\t' + command.description)
	}
}
