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
		description: 'Prints help information.'
	}
}

fn help_cmd() Command {
	return Command{
		name:        'help'
		usage:       '<command>'
		description: 'Prints help information.'
		execute:     print_help_for_command
	}
}

// print_help_for_command outputs the help message of `help_cmd`.
pub fn print_help_for_command(cmd Command) ! {
	if cmd.args.len > 0 {
		for sub_cmd in cmd.commands {
			if sub_cmd.name == cmd.args[0] {
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
	mut abbrev_len := 0
	mut name_len := min_description_indent_len
	if cmd.posix_mode {
		for flag in cmd.flags {
			if flag.abbrev != '' {
				abbrev_len = max(abbrev_len, flag.abbrev.len + spacing + 1) // + 1 for '-' in front
			}
			name_len = max(name_len, abbrev_len + flag.name.len + spacing + 2) // + 2 for '--' in front
		}
		for command in cmd.commands {
			name_len = max(name_len, command.name.len + spacing)
		}
	} else {
		for flag in cmd.flags {
			if flag.abbrev != '' {
				abbrev_len = max(abbrev_len, flag.abbrev.len + spacing + 1) // + 1 for '-' in front
			}
			name_len = max(name_len, abbrev_len + flag.name.len + spacing + 1) // + 1 for '-' in front
		}
		for command in cmd.commands {
			name_len = max(name_len, command.name.len + spacing)
		}
	}
	if cmd.flags.len > 0 {
		help += '\nFlags:\n'
		for flag in cmd.flags {
			mut flag_name := ''
			prefix := if cmd.posix_mode { '--' } else { '-' }
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
			base_indent := ' '.repeat(base_indent_len)
			description_indent := ' '.repeat(name_len - flag_name.len)
			help += '${base_indent}${flag_name}${description_indent}' +
				pretty_description(flag.description + required, base_indent_len + name_len) + '\n'
		}
	}
	if cmd.commands.len > 0 {
		help += '\nCommands:\n'
		for command in cmd.commands {
			base_indent := ' '.repeat(base_indent_len)
			description_indent := ' '.repeat(name_len - command.name.len)
			help += '${base_indent}${command.name}${description_indent}' +
				pretty_description(command.description, base_indent_len + name_len) + '\n'
		}
	}
	return help
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
