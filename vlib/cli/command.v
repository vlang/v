module cli

import term

type FnCommandCallback = fn (cmd Command) !

// str returns the `string` representation of the callback.
pub fn (f FnCommandCallback) str() string {
	return 'FnCommandCallback=>' + ptr_str(f)
}

// Command is a structured representation of a single command
// or chain of commands.
pub struct Command {
pub mut:
	name            string
	usage           string
	description     string
	man_description string
	version         string
	pre_execute     FnCommandCallback = unsafe { nil }
	execute         FnCommandCallback = unsafe { nil }
	post_execute    FnCommandCallback = unsafe { nil }
	disable_help    bool @[deprecated: 'use defaults.help instead'; deprecated_after: '2024-06-31']
	disable_man     bool @[deprecated: 'use defaults.man instead'; deprecated_after: '2024-06-31']
	disable_version bool @[deprecated: 'use defaults.version instead'; deprecated_after: '2024-06-31']
	disable_flags   bool
	sort_flags      bool
	sort_commands   bool
	parent          &Command = unsafe { nil }
	commands        []Command
	flags           []Flag
	required_args   int
	args            []string
	posix_mode      bool
	defaults        struct {
	pub:
		help    Defaults = true
		man     Defaults = true
		version Defaults = true
	mut:
		parsed struct {
		mut:
			help    CommandFlag
			version CommandFlag
			man     CommandFlag
		}
	}
}

pub struct CommandFlag {
pub mut:
	command bool = true
	flag    bool = true
}

type Defaults = CommandFlag | bool

// str returns the `string` representation of the `Command`.
pub fn (cmd Command) str() string {
	mut res := []string{}
	res << 'Command{'
	res << '	name: "${cmd.name}"'
	res << '	usage: "${cmd.usage}"'
	res << '	version: "${cmd.version}"'
	res << '	description: "${cmd.description}"'
	res << '	man_description: "${cmd.man_description}"'
	res << '	disable_help: ${cmd.disable_help}'
	res << '	disable_man: ${cmd.disable_man}'
	res << '	disable_flags: ${cmd.disable_flags}'
	res << '	disable_version: ${cmd.disable_version}'
	res << '	sort_flags: ${cmd.sort_flags}'
	res << '	sort_commands: ${cmd.sort_commands}'
	res << '	cb execute: ${cmd.execute}'
	res << '	cb pre_execute: ${cmd.pre_execute}'
	res << '	cb post_execute: ${cmd.post_execute}'
	if unsafe { cmd.parent == 0 } {
		res << '	parent: &Command(0)'
	} else {
		res << '	parent: &Command{${cmd.parent.name} ...}'
	}
	res << '	commands: ${cmd.commands}'
	res << '	flags: ${cmd.flags}'
	res << '	required_args: ${cmd.required_args}'
	res << '	args: ${cmd.args}'
	res << '}'
	return res.join('\n')
}

// is_root returns `true` if this `Command` has no parents.
pub fn (cmd Command) is_root() bool {
	return isnil(cmd.parent)
}

// root returns the root `Command` of the command chain.
pub fn (cmd Command) root() Command {
	if cmd.is_root() {
		return cmd
	}
	return cmd.parent.root()
}

// full_name returns the full `string` representation of all commands int the chain.
pub fn (cmd Command) full_name() string {
	if cmd.is_root() {
		return cmd.name
	}
	return cmd.parent.full_name() + ' ${cmd.name}'
}

// add_commands adds the `commands` array of `Command`s as sub-commands.
pub fn (mut cmd Command) add_commands(commands []Command) {
	for command in commands {
		cmd.add_command(command)
	}
}

// add_command adds `command` as a sub-command of this `Command`.
pub fn (mut cmd Command) add_command(command Command) {
	mut subcmd := command
	if cmd.commands.contains(subcmd.name) {
		eprintln_exit('Command with the name `${subcmd.name}` already exists')
	}
	subcmd.parent = unsafe { cmd }
	cmd.commands << subcmd
}

// setup ensures that all sub-commands of this `Command`
// is linked as a chain.
pub fn (mut cmd Command) setup() {
	for mut subcmd in cmd.commands {
		subcmd.parent = unsafe { cmd }
		subcmd.posix_mode = cmd.posix_mode
		subcmd.setup()
	}
}

// add_flags adds the array `flags` to this `Command`.
pub fn (mut cmd Command) add_flags(flags []Flag) {
	for flag in flags {
		cmd.add_flag(flag)
	}
}

// add_flag adds `flag` to this `Command`.
pub fn (mut cmd Command) add_flag(flag Flag) {
	if cmd.flags.contains(flag.name) {
		eprintln_exit('Flag with the name `${flag.name}` already exists')
	}
	cmd.flags << flag
}

// TODO: remove deprecated `disable_<>` switches after deprecation period.
fn (mut cmd Command) parse_defaults() {
	// Help
	if cmd.defaults.help is bool {
		// If `defaults.help` has the default value `true` and
		// `disable_help` is also set, fall back to `disable_help`.
		if cmd.defaults.help && cmd.disable_help {
			cmd.defaults.parsed.help.flag = false
			cmd.defaults.parsed.help.command = false
		} else {
			cmd.defaults.parsed.help.flag = cmd.defaults.help
			cmd.defaults.parsed.help.command = cmd.defaults.help
		}
	} else if cmd.defaults.help is CommandFlag {
		cmd.defaults.parsed.help.flag = cmd.defaults.help.flag
		cmd.defaults.parsed.help.command = cmd.defaults.help.command
	}
	// Version
	if cmd.defaults.version is bool {
		if cmd.defaults.version && cmd.disable_version {
			cmd.defaults.parsed.version.flag = false
			cmd.defaults.parsed.version.command = false
		} else {
			cmd.defaults.parsed.version.flag = cmd.defaults.version
			cmd.defaults.parsed.version.command = cmd.defaults.version
		}
	} else if cmd.defaults.version is CommandFlag {
		cmd.defaults.parsed.version.flag = cmd.defaults.version.flag
		cmd.defaults.parsed.version.command = cmd.defaults.version.command
	}
	// Man
	if cmd.defaults.man is bool {
		if cmd.defaults.man && cmd.disable_man {
			cmd.defaults.parsed.man.flag = false
			cmd.defaults.parsed.man.command = false
		} else {
			cmd.defaults.parsed.man.flag = cmd.defaults.man
			cmd.defaults.parsed.man.command = cmd.defaults.man
		}
	} else if cmd.defaults.man is CommandFlag {
		cmd.defaults.parsed.man.flag = cmd.defaults.man.flag
		cmd.defaults.parsed.man.command = cmd.defaults.man.command
	}
	// Add Flags
	if !cmd.disable_flags {
		cmd.add_default_flags()
	}
	// Add Commands
	cmd.add_default_commands()
}

// parse parses the flags and commands from the given `args` into the `Command`.
pub fn (mut cmd Command) parse(args []string) {
	cmd.parse_defaults()
	if cmd.sort_flags {
		cmd.flags.sort(a.name < b.name)
	}
	if cmd.sort_commands {
		cmd.commands.sort(a.name < b.name)
	}
	cmd.args = args[1..]
	if !cmd.disable_flags {
		cmd.parse_flags()
	}
	cmd.parse_commands()
}

// add_default_flags adds the commonly used `-h`/`--help` and
// `-v`/`--version` flags to the `Command`.
fn (mut cmd Command) add_default_flags() {
	if cmd.defaults.parsed.help.flag && !cmd.flags.contains('help') {
		use_help_abbrev := !cmd.flags.contains('h') && cmd.posix_mode
		cmd.add_flag(help_flag(use_help_abbrev))
	}
	if cmd.defaults.parsed.version.flag && cmd.version != '' && !cmd.flags.contains('version') {
		use_version_abbrev := !cmd.flags.contains('v') && cmd.posix_mode
		cmd.add_flag(version_flag(use_version_abbrev))
	}
	if cmd.defaults.parsed.man.flag && !cmd.flags.contains('man') {
		cmd.add_flag(man_flag())
	}
}

// add_default_commands adds the command functions of the
// commonly used `help` and `version` flags to the `Command`.
fn (mut cmd Command) add_default_commands() {
	if cmd.defaults.parsed.help.command && !cmd.commands.contains('help') && cmd.is_root() {
		cmd.add_command(help_cmd())
	}
	if cmd.defaults.parsed.version.command && cmd.version != '' && !cmd.commands.contains('version') {
		cmd.add_command(version_cmd())
	}
	if cmd.defaults.parsed.man.command && !cmd.commands.contains('man') && cmd.is_root() {
		cmd.add_command(man_cmd())
	}
}

fn (mut cmd Command) parse_flags() {
	for cmd.args.len > 0 {
		if !cmd.args[0].starts_with('-') {
			return
		}
		mut found := false
		for mut flag in cmd.flags {
			if flag.matches(cmd.args[0], cmd.posix_mode) {
				found = true
				flag.found = true
				// Eat flag and its values, continue with reduced args.
				cmd.args = flag.parse(cmd.args, cmd.posix_mode) or {
					eprintln_exit('Failed to parse flag `${cmd.args[0]}`: ${err}')
				}
				break
			}
		}
		if !found {
			eprintln_exit('Command `${cmd.name}` has no flag `${cmd.args[0]}`')
		}
	}
}

fn (mut cmd Command) parse_commands() {
	global_flags := cmd.flags.filter(it.global)
	cmd.check_help_flag()
	cmd.check_version_flag()
	cmd.check_man_flag()
	for i in 0 .. cmd.args.len {
		arg := cmd.args[i]
		for j in 0 .. cmd.commands.len {
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
	if cmd.is_root() && isnil(cmd.execute) {
		if cmd.defaults.parsed.help.command {
			cmd.execute_help()
			return
		}
	}
	// if no further command was found, execute current command
	if cmd.required_args > 0 {
		if cmd.required_args > cmd.args.len {
			descriptor := if cmd.required_args == 1 { 'argument' } else { 'arguments' }
			eprintln_exit('Command `${cmd.name}` needs at least ${cmd.required_args} ${descriptor}')
		}
	}
	cmd.check_required_flags()

	cmd.handle_cb(cmd.pre_execute, 'preexecution')
	cmd.handle_cb(cmd.execute, 'execution')
	cmd.handle_cb(cmd.post_execute, 'postexecution')
}

fn (mut cmd Command) handle_cb(cb FnCommandCallback, label string) {
	if !isnil(cb) {
		cb(*cmd) or {
			label_message := term.ecolorize(term.bright_red, 'cli ${label} error:')
			eprintln_exit('${label_message} ${err}')
		}
	}
}

fn (cmd Command) check_help_flag() {
	if cmd.defaults.parsed.help.flag && cmd.flags.contains('help') {
		help_flag := cmd.flags.get_bool('help') or { return } // ignore error and handle command normally
		if help_flag {
			cmd.execute_help()
			exit(0)
		}
	}
}

fn (cmd Command) check_man_flag() {
	if cmd.defaults.parsed.man.flag && cmd.flags.contains('man') {
		man_flag := cmd.flags.get_bool('man') or { return } // ignore error and handle command normally
		if man_flag {
			cmd.execute_man()
			exit(0)
		}
	}
}

fn (cmd Command) check_version_flag() {
	if cmd.defaults.parsed.version.flag && cmd.version != '' && cmd.flags.contains('version') {
		version_flag := cmd.flags.get_bool('version') or { return } // ignore error and handle command normally
		if version_flag {
			print_version_for_command(cmd) or { panic(err) }
			exit(0)
		}
	}
}

fn (cmd Command) check_required_flags() {
	for flag in cmd.flags {
		if flag.required && flag.value.len == 0 {
			full_name := cmd.full_name()
			eprintln_exit('Flag `${flag.name}` is required by `${full_name}`')
		}
	}
}

// execute_help executes the callback registered
// for the `-h`/`--help` flag option.
pub fn (cmd Command) execute_help() {
	if cmd.commands.contains('help') {
		help_cmd := cmd.commands.get('help') or { return } // ignore error and handle command normally
		if !isnil(help_cmd.execute) {
			help_cmd.execute(help_cmd) or { panic(err) }
			return
		}
	}
	print(cmd.help_message())
}

// execute_help executes the callback registered
// for the `-man` flag option.
pub fn (cmd Command) execute_man() {
	if cmd.commands.contains('man') {
		man_cmd := cmd.commands.get('man') or { return }
		man_cmd.execute(man_cmd) or { panic(err) }
	} else {
		print(cmd.manpage())
	}
}

fn (cmds []Command) get(name string) !Command {
	for cmd in cmds {
		if cmd.name == name {
			return cmd
		}
	}
	return error('Command `${name}` not found in ${cmds}')
}

fn (cmds []Command) contains(name string) bool {
	for cmd in cmds {
		if cmd.name == name {
			return true
		}
	}
	return false
}

@[noreturn]
fn eprintln_exit(message string) {
	eprintln(message)
	exit(1)
}
