module main

import cli
import os
import v.builder

fn run_cmd() &cli.Command {
	run_cmd := &cli.Command{
		name: 'run'
		description: 'Build & Run V code in the provided path'
		execute: fn(cmd cli.Command) {
			mut prefs := parse_build_preferences(cmd.flags) or { panic(err)}

			if cmd.args.len == 1 && (cmd.args[0].ends_with('.v') || os.exists(cmd.args[0])) {
				prefs.path = cmd.args[0]
				prefs.run_args = cmd.args[0..]
				prefs.is_run = true		// TODO: handle as own function
				prefs.fill_with_defaults() // TODO: defaults should be set before applying user input

				builder.compile(cmd.args[0], prefs)	
			} else {
				panic('no V source code file found')
			}
		}
	}

	for flag in build_flags() { 
		run_cmd.add_flag(flag)
	}

	return run_cmd
}
