module cli

fn nil() voidptr { return 0 }

pub struct Command {
pub mut:
	name string
	description string
	version string
	pre_execute fn(cmd Command)
	execute fn(cmd Command)
	post_execute fn(cmd Command)

	disable_help bool
	disable_version bool
	disable_flags bool

	parent &Command = nil()
	commands []Command
	flags []Flag
	args []string
}

pub fn (cmd Command) full_name() string {
	if isnil(cmd.parent) {
		return cmd.name
	}
	return cmd.parent.full_name() + ' ${cmd.name}'
}

pub fn (cmd Command) root() Command {
	if isnil(cmd.parent) {
		return cmd
	}
	return cmd.parent.root()
}

pub fn (mut cmd Command) add_command(command Command) {
	cmd.commands << command
}

pub fn (mut cmd Command) add_flag(flag Flag) {
	cmd.flags << flag
}

pub fn (mut cmd Command) parse(args []string) {
	if !cmd.disable_flags {
		cmd.add_default_flags()
	}
	cmd.add_default_commands()

	cmd.args = args[1..]
	for i in 0..cmd.commands.len {
		cmd.commands[i].parent = cmd
	}

	if !cmd.disable_flags {
		cmd.parse_flags()
	}
	cmd.parse_commands()
}

fn (mut cmd Command) add_default_flags() {
	if !cmd.disable_help && !cmd.flags.contains('help') && !cmd.flags.contains('h') {
		cmd.add_flag(help_flag())
	}
	if !cmd.disable_version && cmd.version != '' && !cmd.flags.contains('version') && !cmd.flags.contains('v') {
		cmd.add_flag(version_flag())
	}
}

fn (mut cmd Command) add_default_commands() {
	if !cmd.disable_help && !cmd.commands.contains('help') {
		cmd.add_command(help_cmd())
	}
	if !cmd.disable_version && cmd.version != '' && !cmd.commands.contains('version') {
		cmd.add_command(version_cmd())
	}
}

fn (mut cmd Command) parse_flags() {
	for {
		if cmd.args.len < 1 || !cmd.args[0].starts_with('-') {
			break
		}
		mut found := false
		for i in 0..cmd.flags.len {
			mut flag := &cmd.flags[i]
			if flag.matches(cmd.args) {
				found = true
				args := flag.parse(cmd.args) or {	// TODO: fix once options types can be assigned to struct variables
					println('failed to parse flag ${cmd.args[0]}: ${err}')
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

fn (cmd &Command) parse_commands() {
	flags := cmd.flags
	global_flags := flags.filter(it.global) // TODO: fix once filter can be applied to struct variable

	cmd.check_help_flag()
	cmd.check_version_flag()

	for i in 0..cmd.args.len {
		arg := cmd.args[i]
		for j in 0..cmd.commands.len {
			mut command := cmd.commands[j]
			if command.name == arg {
				for flag in global_flags {
					command.add_flag(flag)
				}
				command.parse(cmd.args[i..])
				return
			}
		}
	}

	// if no further command was found execute current command
	if int(cmd.execute) == 0 {
		if !cmd.disable_help {
			help_cmd := cmd.commands.get('help') or { return } // ignore error and handle command normally
			execute := help_cmd.execute
			execute(help_cmd)
		}
	} else {
		cmd.check_required_flags()

		if int(cmd.pre_execute) > 0 {
			pre_execute := cmd.pre_execute
			pre_execute(cmd)
		}

		execute := cmd.execute
		execute(cmd) // TODO: fix once higher order function can be execute on struct variable

		if int(cmd.post_execute) > 0 {
			post_execute := cmd.post_execute
			post_execute(cmd)
		}
	}
}

fn (cmd &Command) check_help_flag() {
	if cmd.disable_help {
		return
	}
	if cmd.flags.contains('help') {
		help_flag := cmd.flags.get_bool('help') or { return } // ignore error and handle command normally
		if help_flag {
			help_cmd := cmd.commands.get('help') or { return } // ignore error and handle command normally
			execute := help_cmd.execute
			execute(help_cmd)
			exit(0)
		}
	}
}

fn (cmd &Command) check_version_flag() {
	if cmd.disable_version {
		return
	}
	if cmd.version != '' && cmd.flags.contains('version') {
		version_flag := cmd.flags.get_bool('version') or { return } // ignore error and handle command normally
		if version_flag {
			version_cmd := cmd.commands.get('version') or { return } // ignore error and handle command normally
			execute := version_cmd.execute
			execute(version_cmd)
			exit(0)
		}
	}
}

fn (cmd &Command) check_required_flags() {
	for flag in cmd.flags {
		if flag.required && flag.value == '' {
			full_name := cmd.full_name()
			println('flag \'${flag.name}\' is required by \'${full_name}\'')
			exit(1)
		}
	}
}

fn (cmds []Command) contains(name string) bool {
	for cmd in cmds {
		if cmd.name == name {
			return true
		}
	}
	return false
}

fn (cmds []Command) get(name string) ?Command {
	for cmd in cmds {
		if cmd.name == name {
			return cmd
		}
	}
	return error('command \'${name}\' not found.')
}
