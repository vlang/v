module cli

fn test_help_message() {
	mut cmd := Command{
		name: 'command'
		description: 'description'
		commands: [
			Command{
				name: 'sub'
				description: 'subcommand'
			},
			Command{
				name: 'sub2'
				description: 'another subcommand'
			},
		]
		flags: [
			Flag{
				flag: .string
				name: 'str'
				description: 'str flag'
			},
			Flag{
				flag: .bool
				name: 'bool'
				description: 'bool flag'
				abbrev: 'b'
			},
			Flag{
				flag: .string
				name: 'required'
				abbrev: 'r'
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
