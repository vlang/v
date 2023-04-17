module main

import cli { Command, Flag }

fn test_long_description() {
	mut cmd := Command{
		name: 'cli'
		description: 'An example of the cli library.'
		version: '1.0.0'
	}
	mut greet_cmd := Command{
		name: 'greet'
		description: 'Prints greeting in different languages.'
		usage: '<name>'
		required_args: 1
	}
	greet_cmd.add_flag(Flag{
		flag: .string_array
		name: 'fun'
		description: 'foo '.repeat(50) + 'a'.repeat(100)
	})
	cmd.add_command(greet_cmd)
	cmd.setup()
	cmd.parse(['cli', 'greet', '-help'])
}
