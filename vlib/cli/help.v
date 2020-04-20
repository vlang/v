module cli

import (
	term
	strings
)

const (
	BASE_INDENT = 2
	ABBREV_INDENT = 5
	DESCRIPTION_INDENT = 20
)

fn help_flag() Flag {
	return Flag{
		flag: .bool,
		name: 'help',
		abbrev: 'h',
		description: 'Prints help information',
	}
}

fn help_cmd() Command {
	return Command{
		name: 'help',
		description: 'Prints help information',
		execute: help_func,
		parent: 0
	}
}

fn help_func(help_cmd Command) {
	cmd := help_cmd.parent
	full_name := cmd.full_name()

	mut help := ''
	help += 'Usage: ${full_name}'
	if cmd.flags.len > 0 { help += ' [FLAGS]'}
	if cmd.commands.len > 0 { help += ' [COMMANDS]'}
	help += '\n\n'

	if cmd.description != '' {
		help += '${cmd.description}\n\n'
	}
	if cmd.flags.len > 0 {
		help += 'Flags:\n'
		for flag in cmd.flags {
			mut flag_name := ''
			if flag.abbrev != '' {
				abbrev_indent := ' '.repeat(max(ABBREV_INDENT-(flag.abbrev.len+1), 1))
				flag_name = '-${flag.abbrev}${abbrev_indent}--${flag.name}'
			} else {
				abbrev_indent := ' '.repeat(max(ABBREV_INDENT-(flag.abbrev.len), 1))
				flag_name = '${abbrev_indent}--${flag.name}'
			}
			mut required := ''
			if flag.required {
				required = ' (required)'
			}

			base_indent := ' '.repeat(BASE_INDENT)
			description_indent := ' '.repeat(max(DESCRIPTION_INDENT-flag_name.len, 1))
			help += '${base_indent}${flag_name}${description_indent}' +
				pretty_description(flag.description + required) + '\n'
		}
		help += '\n'
	}
	if cmd.commands.len > 0 {
		help += 'Commands:\n'
		for command in cmd.commands {
			base_indent := ' '.repeat(BASE_INDENT)
			description_indent := ' '.repeat(max(DESCRIPTION_INDENT-command.name.len, 1))

			help += '${base_indent}${command.name}${description_indent}' +
				pretty_description(command.description) + '\n'
		}
		help += '\n'
	}

	print(help)
}

// pretty_description resizes description text depending on terminal width.
// Essentially, smart wrap-around
fn pretty_description(s string) string {
	width, _ := term.get_terminal_size()
	// Don't prettify if the terminal is that small, it won't be pretty anyway.
	if s.len + DESCRIPTION_INDENT < width || DESCRIPTION_INDENT > width {
		return s
	}
	indent := ' '.repeat(DESCRIPTION_INDENT + 1)
	chars_per_line := width - DESCRIPTION_INDENT
	// Give us enough room, better a little bigger than smaller
	mut acc := strings.new_builder(((s.len / chars_per_line) + 1) * (width + 1))

	mut i := chars_per_line - 1
	mut j := 0
	for ; i < s.len ; i += chars_per_line - 1 {
		for s.str[i] != ` ` { i-- }
		// indent was already done the first iteration
		if j != 0 { acc.write(indent) }
		acc.writeln(s[j..i])
		j = i
	}
	// We need this even though it should never happen
	if j != 0 {
		acc.write(indent)
	}
	acc.write(s[j..])
	return acc.str()
}

fn max(a, b int) int {
	return if a > b {a} else {b}
}
