module main

import (
	cli
	os
)

fn main() {
	mut cmd := cli.Command{
		name: 'cli', 
		description: 'An example of the cli library',
		version: '1.0.0',
		parent: 0
	}

	mut greet_cmd := cli.Command{
		name: 'greet',
		description: 'Prints greeting in different languages',
		execute: greet_func,
		parent: 0
	}
	greet_cmd.add_flag(cli.Flag{
		flag: .string, 
		required: true,
		name: 'language',
		abbrev: 'l',
		description: 'Language of the message'
	})
	greet_cmd.add_flag(cli.Flag{
		flag: .int,
		name: 'times',
		value: '3',
		description: 'Number of times the message gets printed'
	})

	cmd.add_command(greet_cmd)
	cmd.parse(os.args)
}

fn greet_func(cmd cli.Command) {
	language := cmd.flags.get_string('language') or { panic('failed to get \'language\' flag: $err') }
	times := cmd.flags.get_int('times') or { panic('failed to get \'times\' flag: $err') }

	for i in 0..times {
		match language {
			'english' { println('Hello World') }
			'german' { println('Hallo Welt') }
			'dutch' { println('Hallo Wereld') }
			else { println('unsupported language') }
		}
	}
}
