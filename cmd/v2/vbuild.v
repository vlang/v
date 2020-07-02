module main

import cli
import os
import v.builder

fn build_cmd() cli.Command {
	return cli.Command{
		name: 'build'
		description: 'Build V code in the provided path'
		execute: build_cmd_func
		flags: build_flags()
	}
}

fn build_cmd_func(cmd cli.Command) {
	mut prefs := parse_build_preferences(cmd.flags) or { panic(err)}

	if cmd.args.len == 1 && (cmd.args[0].ends_with('.v') || os.exists(cmd.args[0])) {
		prefs.path = cmd.args[0]
		prefs.run_args = cmd.args[0..]
		prefs.fill_with_defaults() // TODO: defaults should be set before applying user input

		builder.compile(cmd.args[0], prefs)	
	} else {
		panic('no V source code file found')
	}
}
