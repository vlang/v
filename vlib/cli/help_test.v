module cli

fn test_help_message() {
	mut cmd := Command{
		name:        'command'
		description: 'description'
		commands:    [
			Command{
				name:        'sub'
				description: 'subcommand'
			},
			Command{
				name:        'sub2'
				description: 'another subcommand'
			},
		]
		flags:       [
			Flag{
				flag:        .string
				name:        'str'
				description: 'str flag'
			},
			Flag{
				flag:        .bool
				name:        'bool'
				description: 'bool flag'
				abbrev:      'b'
			},
			Flag{
				flag:     .string
				name:     'required'
				abbrev:   'r'
				required: true
			},
		]
	}
	assert cmd.help_message() == r'Usage: command [flags] [commands]

description

Flags:
      -str            str flag
  -b  -bool           bool flag
  -r  -required       (required)

Commands:
  sub                 subcommand
  sub2                another subcommand
'

	cmd.posix_mode = true
	assert cmd.help_message() == r'Usage: command [flags] [commands]

description

Flags:
      --str           str flag
  -b  --bool          bool flag
  -r  --required      (required)

Commands:
  sub                 subcommand
  sub2                another subcommand
'
}

fn test_help_message_groups_commands_by_group_field() {
	cmd := Command{
		name:        'app'
		description: 'app'
		commands:    [
			Command{
				name:        'auth'
				description: 'Authenticate'
				group:       'Core commands'
			},
			Command{
				name:        'repo'
				description: 'Work with repositories'
				group:       'Core commands'
			},
			Command{
				name:        'alias'
				description: 'Define shortcuts'
				group:       'General commands'
			},
			Command{
				name:        'misc'
				description: 'Other things'
			},
		]
	}
	assert cmd.help_message() == r'Usage: app [commands]

app

Core commands:
  auth                Authenticate
  repo                Work with repositories

General commands:
  alias               Define shortcuts

Commands:
  misc                Other things
'
}

fn test_help_message_renders_group_title_verbatim() {
	// Group string is rendered as-is; capitalisation is the caller's
	// choice. Non-ASCII first characters must work without mojibake.
	cmd := Command{
		name:     'app'
		commands: [
			Command{
				name:        'edit'
				description: 'Edit something'
				group:       'éditeurs'
			},
		]
	}
	assert cmd.help_message() == r'Usage: app [commands]

éditeurs:
  edit                Edit something
'
}

fn test_help_message_separates_inherited_flags_from_locals() {
	mut root := Command{
		name:     'app'
		flags:    [
			Flag{
				flag:        .string
				name:        'config'
				abbrev:      'c'
				description: 'Path to config'
				global:      true
			},
		]
		commands: [
			Command{
				name:        'run'
				description: 'Run something'
				flags:       [
					Flag{
						flag:        .bool
						name:        'verbose'
						abbrev:      'v'
						description: 'Verbose output'
					},
				]
			},
		]
	}
	root.setup()
	mut sub := root.commands[0]
	sub.parent = unsafe { &root }
	// Mirror what `parse_commands` does when a child is dispatched.
	sub.flags << root.flags[0]
	assert sub.help_message() == r'Usage: app run [flags]

Run something

Flags:
  -v  -verbose        Verbose output

Inherited flags:
  -c  -config         Path to config
'
}

fn test_help_message_renders_examples_and_learn_more() {
	cmd := Command{
		name:        'app'
		description: 'app'
		examples:    ['\$ app run', '\$ app help run']
		learn_more:  'Use `app help <command>` for details about a command.\nDocumentation lives at https://example.test'
	}
	assert cmd.help_message() == r'Usage: app

app

Examples:
  $ app run
  $ app help run

Learn more:
  Use `app help <command>` for details about a command.
  Documentation lives at https://example.test
'
}

fn test_help_message_omits_empty_optional_sections() {
	cmd := Command{
		name: 'app'
	}
	// No description, no flags, no commands, no examples, no learn_more.
	assert cmd.help_message() == 'Usage: app\n'
}
