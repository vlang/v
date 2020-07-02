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

	sort_flags bool = true
	sort_commands bool = true

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

	if cmd.sort_flags {
		cmd.flags.sort()
	}
	if cmd.sort_commands {
		cmd.commands.sort()
	}

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
	if !cmd.disable_help && !cmd.flags.contains('help') {
		cmd.add_flag(help_flag(!cmd.flags.contains('h')))
	}
	if !cmd.disable_version && cmd.version != '' && !cmd.flags.contains('version') {
		cmd.add_flag(version_flag(!cmd.flags.contains('v')))
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
				flag.found = true
				cmd.args = flag.parse(cmd.args) or {
					println('failed to parse flag ${cmd.args[0]}: ${err}')
					exit(1)
				}
				break
			}
		}

		if !found {
			println('invalid flag: ${cmd.args[0]}')
			exit(1)
		}
	}
}

fn (mut cmd Command) parse_commands() {
	global_flags := cmd.flags.filter(it.global)

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

	// if no further command was found, execute current command
	if int(cmd.execute) == 0 {
		if !cmd.disable_help {
			help_cmd := cmd.commands.get('help') or { return } // ignore error and handle command normally
			help_cmd.execute(help_cmd)
		}
	} else {
		cmd.check_required_flags()

		if int(cmd.pre_execute) > 0 {
			cmd.pre_execute(*cmd)
		}

		cmd.execute(*cmd)

		if int(cmd.post_execute) > 0 {
			cmd.post_execute(*cmd)
		}
	}
}

fn (mut cmd Command) check_help_flag() {
	if cmd.disable_help {
		return
	}
	if cmd.flags.contains('help') {
		help_flag := cmd.flags.get_bool('help') or { return } // ignore error and handle command normally
		if help_flag {
			help_cmd := cmd.commands.get('help') or { return } // ignore error and handle command normally
			help_cmd.execute(help_cmd)
			exit(0)
		}
	}
}

fn (mut cmd Command) check_version_flag() {
	if cmd.disable_version {
		return
	}
	if cmd.version != '' && cmd.flags.contains('version') {
		version_flag := cmd.flags.get_bool('version') or { return } // ignore error and handle command normally
		if version_flag {
			version_cmd := cmd.commands.get('version') or { return } // ignore error and handle command normally
			version_cmd.execute(version_cmd)
			exit(0)
		}
	}
}

fn (mut cmd Command) check_required_flags() {
	for flag in cmd.flags {
		if flag.required && flag.value == '' {
			full_name := cmd.full_name()
			println('flag \'${flag.name}\' is required by \'${full_name}\'')
			exit(1)
		}
	}
}

fn (cmds []Command) get(name string) ?Command {
	for cmd in cmds {
		if cmd.name == name {
			return cmd
		}
	}
	return error('command \'${name}\' not found.')
}

fn (cmds []Command) contains(name string) bool {
	for cmd in cmds {
		if cmd.name == name {
			return true
		}
	}
	return false
}

fn (mut cmds []Command) sort() {
	cmds.sort_with_compare(fn(a &Command, b &Command) int {
		return compare_strings(&a.name, &b.name)
	})
}
