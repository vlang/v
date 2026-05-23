module cli

import term
import strings

const base_indent_len = 2
const min_description_indent_len = 20
const spacing = 2

fn help_flag(with_abbrev bool) Flag {
	sabbrev := if with_abbrev { 'h' } else { '' }
	return Flag{
		flag:        .bool
		name:        'help'
		abbrev:      sabbrev
		description: 'Print help information'
	}
}

fn help_cmd() Command {
	return Command{
		name:        'help'
		usage:       '<command>'
		description: 'Print help information'
		execute:     print_help_for_command
	}
}

// print_help_for_command outputs the help message of `help_cmd`.
pub fn print_help_for_command(cmd Command) ! {
	if cmd.args.len > 0 {
		for sub_cmd in cmd.commands {
			if sub_cmd.matches(cmd.args[0]) {
				cmd_ := unsafe { &sub_cmd }
				print(cmd_.help_message())
				return
			}
		}
		print('Invalid command: ${cmd.args.join(' ')}')
	} else if cmd.parent != unsafe { nil } {
		print(cmd.parent.help_message())
	}
}

// help_message returns a generated help message as a `string` for the `Command`.
//
// The output is composed of (each section is omitted when empty):
//
//   1. `Usage:` line
//   2. The description, if any
//   3. `Flags:` — flags defined on this command
//   4. `Inherited flags:` — flags propagated from any ancestor via `Flag.global`
//   5. One section per sub-command group; the group name is rendered as the
//      user wrote it followed by `:`. Sub-commands without a group go under
//      the default `Commands:` section.
//   6. `Examples:` — `Command.examples` entries, one per line
//   7. `Learn more:` — the multi-line `Command.learn_more` field
pub fn (cmd &Command) help_message() string {
	mut help := ''
	help += 'Usage: ${cmd.full_name()}'
	if cmd.flags.len > 0 {
		help += ' [flags]'
	}
	if cmd.commands.len > 0 {
		help += ' [commands]'
	}
	if cmd.usage.len > 0 {
		help += ' ${cmd.usage}'
	} else {
		for i in 0 .. cmd.required_args {
			help += ' <arg${i}>'
		}
	}
	help += '\n'
	if cmd.description != '' {
		help += '\n${cmd.description}\n'
	}
	abbrev_len, name_len := cmd.help_columns()
	local_flags, inherited_flags := cmd.partition_flags()
	if local_flags.len > 0 {
		help += render_flag_section('Flags', local_flags, abbrev_len, name_len, cmd.posix_mode)
	}
	if inherited_flags.len > 0 {
		help += render_flag_section('Inherited flags', inherited_flags, abbrev_len, name_len,
			cmd.posix_mode)
	}
	if cmd.commands.len > 0 {
		groups, order := group_commands(cmd.commands)
		for idx, key in order {
			title := if key == '' { 'Commands' } else { key }
			help += render_command_section(title, groups[idx], name_len)
		}
	}
	if cmd.examples.len > 0 {
		help += '\nExamples:\n'
		base_indent := ' '.repeat(base_indent_len)
		for line in cmd.examples {
			help += '${base_indent}${line}\n'
		}
	}
	if cmd.learn_more != '' {
		help += '\nLearn more:\n'
		base_indent := ' '.repeat(base_indent_len)
		for line in cmd.learn_more.split_into_lines() {
			help += '${base_indent}${line}\n'
		}
	}
	return help
}

// help_columns computes the abbreviation column width and the name column
// width used by `Flags:`, `Inherited Flags:` and command sections so all
// rows align vertically.
fn (cmd &Command) help_columns() (int, int) {
	mut abbrev_len := 0
	mut name_len := min_description_indent_len
	if cmd.posix_mode {
		for flag in cmd.flags {
			if flag.abbrev != '' {
				abbrev_len = max(abbrev_len, flag.abbrev.len + spacing + 1) // + 1 for '-' in front
			}
			name_len =
				max(name_len, abbrev_len + flag.name.len + spacing + 2) // + 2 for '--' in front
		}
	} else {
		for flag in cmd.flags {
			if flag.abbrev != '' {
				abbrev_len = max(abbrev_len, flag.abbrev.len + spacing + 1) // + 1 for '-' in front
			}
			name_len =
				max(name_len, abbrev_len + flag.name.len + spacing + 1) // + 1 for '-' in front
		}
	}
	for command in cmd.commands {
		name_len = max(name_len, command.display_name().len + spacing)
	}
	return abbrev_len, name_len
}

// partition_flags splits this command's `flags` into two arrays preserving
// declaration order: locally-defined flags first, then flags inherited from
// any ancestor via `Flag.global`.
fn (cmd &Command) partition_flags() ([]Flag, []Flag) {
	inherited_names := cmd.inherited_global_flag_names()
	mut local := []Flag{}
	mut inherited := []Flag{}
	for flag in cmd.flags {
		if flag.name in inherited_names {
			inherited << flag
		} else {
			local << flag
		}
	}
	return local, inherited
}

// inherited_global_flag_names returns the names of flags that any ancestor
// declared with `global = true`. A flag is "inherited" only when an ancestor
// declared it global, never when this command itself defines it.
fn (cmd &Command) inherited_global_flag_names() []string {
	mut names := []string{}
	mut walker := cmd.parent
	for unsafe { walker != nil } {
		for flag in walker.flags {
			if flag.global && flag.name !in names {
				names << flag.name
			}
		}
		walker = walker.parent
	}
	return names
}

// group_commands buckets `commands` by their `group` field, preserving the
// declaration order of both groups and members.
fn group_commands(commands []Command) ([][]Command, []string) {
	mut order := []string{}
	mut groups := [][]Command{}
	for sub in commands {
		key := sub.group
		idx := order.index(key)
		if idx == -1 {
			order << key
			groups << [sub]
		} else {
			groups[idx] << sub
		}
	}
	return groups, order
}

fn render_flag_section(title string, flags []Flag, abbrev_len int, name_len int, posix_mode bool) string {
	mut out := '\n${title}:\n'
	prefix := if posix_mode { '--' } else { '-' }
	base_indent := ' '.repeat(base_indent_len)
	for flag in flags {
		mut flag_name := ''
		if flag.abbrev != '' {
			abbrev_indent := ' '.repeat(abbrev_len - flag.abbrev.len - 1) // - 1 for '-' in front
			flag_name = '-${flag.abbrev}${abbrev_indent}${prefix}${flag.name}'
		} else {
			abbrev_indent := ' '.repeat(abbrev_len)
			flag_name = '${abbrev_indent}${prefix}${flag.name}'
		}
		mut required := ''
		if flag.required {
			required = ' (required)'
		}
		description_indent := ' '.repeat(name_len - flag_name.len)
		out += '${base_indent}${flag_name}${description_indent}' +
			pretty_description(flag.description + required, base_indent_len + name_len) + '\n'
	}
	return out
}

fn render_command_section(title string, commands []Command, name_len int) string {
	mut out := '\n${title}:\n'
	base_indent := ' '.repeat(base_indent_len)
	for command in commands {
		command_name := command.display_name()
		description_indent := ' '.repeat(name_len - command_name.len)
		out += '${base_indent}${command_name}${description_indent}' +
			pretty_description(command.description, base_indent_len + name_len) + '\n'
	}
	return out
}

// pretty_description resizes description text depending on terminal width.
// Essentially, smart wrap-around
fn pretty_description(s string, indent_len int) string {
	width, _ := term.get_terminal_size()
	// Don't prettify if the terminal is that small, it won't be pretty anyway.
	if indent_len > width {
		return s
	}
	indent := ' '.repeat(indent_len)
	chars_per_line := width - indent_len
	// Give us enough room, better a little bigger than smaller
	mut acc := strings.new_builder(((s.len / chars_per_line) + 1) * (width + 1))
	for k, line in s.split('\n') {
		if k != 0 {
			acc.write_string('\n${indent}')
		}
		mut i := chars_per_line - 2
		mut j := 0
		for ; i < line.len; i += chars_per_line - 2 {
			for j > 0 && line[j] != ` ` {
				j--
			}
			// indent was already done the first iteration
			if j != 0 {
				acc.write_string(indent)
			}
			acc.writeln(line[j..i].trim_space())
			j = i
		}
		// We need this even though it should never happen
		if j != 0 {
			acc.write_string(indent)
		}
		acc.write_string(line[j..].trim_space())
	}
	return acc.str()
}

fn max(a int, b int) int {
	res := if a > b { a } else { b }
	return res
}
