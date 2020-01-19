module cli

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

fn help_func(help_cmd cli.Command) {
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
				abbrev_indent := ' '.repeat(ABBREV_INDENT-(flag.abbrev.len+1))
				flag_name = '-${flag.abbrev}${abbrev_indent}--${flag.name}'
			} else {
				abbrev_indent := ' '.repeat(ABBREV_INDENT-(flag.abbrev.len))
				flag_name = '${abbrev_indent}--${flag.name}'
			}
			mut required := ''
			if flag.required {
				required = ' (required)'
			}

			base_indent := ' '.repeat(BASE_INDENT)
			description_indent := ' '.repeat(DESCRIPTION_INDENT-flag_name.len)
			help += '${base_indent}${flag_name}${description_indent}${flag.description}${required}\n'
		}
		help += '\n'
	}
	if cmd.commands.len > 0 {
		help += 'Commands:\n'
		for command in cmd.commands {
			base_indent := ' '.repeat(BASE_INDENT)
			description_indent := ' '.repeat(DESCRIPTION_INDENT-command.name.len)

			help += '${base_indent}${command.name}${description_indent}${command.description}\n'
		}
		help += '\n'
	}

	print(help)
}
