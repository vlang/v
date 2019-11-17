module main

import (
	cli
	os
)

fn main() {
	mut cmd := cli.Command{
		name: 'cli', 
		description: 'An example of the cli library',
	}

	mut greet_cmd := cli.Command{
		name: 'greet',
		description: 'Prints greeting in different languages',
		execute: greet_func,
	}
	greet_cmd.add_flag(cli.Flag{
		flag: .string, 
		required: true,
		name: 'language',
		abbrev: 'l',
	})

	cmd.add_command(greet_cmd)
	cmd.parse(os.args)
}

fn greet_func(cmd cli.Command, args []string) {
	language := cmd.flags.get_string('language') or { panic('failed to get language flag') }
	match language {
		'english' { println('Hello World') }
		'german' { println('Hallo Welt') }
		'dutch' { println('Hallo Wereld') }
		else { println('unsupported language') }
	}
}
