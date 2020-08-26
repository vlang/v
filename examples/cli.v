module main

import cli { Command, Flag }
import os

fn main() {
	mut cmd := Command{
		name: 'cli'
		description: 'An example of the cli library.'
		version: '1.0.0'
	}
	mut greet_cmd := Command{
		name: 'greet'
		description: 'Prints greeting in different languages.'
		required_args: 1
		pre_execute: greet_pre_func
		execute: greet_func
		post_execute: greet_post_func
	}
	greet_cmd.add_flag(Flag{
		flag: .string
		required: true
		name: 'language'
		abbrev: 'l'
		description: 'Language of the message.'
	})
	greet_cmd.add_flag(Flag{
		flag: .int
		name: 'times'
		value: '3'
		description: 'Number of times the message gets printed.'
	})
	cmd.add_command(greet_cmd)
	cmd.parse(os.args)
}

fn greet_func(cmd Command) {
	language := cmd.flags.get_string('language') or {
		panic('Failed to get `language` flag: $err')
	}
	times := cmd.flags.get_int('times') or {
		panic('Failed to get `times` flag: $err')
	}
	name := cmd.args[0]
	for _ in 0 .. times {
		match language {
			'english' {
				println('Welcome $name')
			}
			'german' {
				println('Willkommen $name')
			}
			'dutch' {
				println('Welkom $name')
			}
			else {
				println('Unsupported language')
				println('Supported are `englisch`, `german` and `dutch`.')
				break
			}
		}
	}
}

fn greet_pre_func(cmd Command) {
	println('This is a function running before the main function.\n')
}

fn greet_post_func(cmd Command) {
	println('\nThis is a function running after the main function.')
}
