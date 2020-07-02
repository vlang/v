module main

import cli
import os
import v.util

fn main() {
	mut v_cmd := &cli.Command{
		name: 'v'
		description: 'V is a tool for managing V source code'
		version: util.full_v_version(false)
	}

	for flag in default_flags() { 
		v_cmd.add_flag(flag) 
	}

	v_cmd.add_command(build_cmd())
	v_cmd.add_command(run_cmd())

	v_cmd.parse(os.args)
}

fn default_flags() []&cli.Flag {
	mut flags := []&cli.Flag{}
	flags << &cli.Flag{
		flag: .bool
		name: 'verbose'
		abbrev: 'v'
		description: 'Enables verbose output'
	}
	flags << &cli.Flag{
		flag: .bool
		name: 'silent'
		abbrev: 's'
		description: 'Disables output'
	}
	return flags
}
