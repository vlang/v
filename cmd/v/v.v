// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import help
import os
import v.pref
import v.util
import v.builder

const (
	simple_cmd                          = [
		'fmt',
		'up',
		'vet',
		'self',
		'tracev',
		'symlink',
		'bin2v',
		'test',
		'test-fmt',
		'test-compiler',
		'test-fixed',
		'test-cleancode',
		'repl',
		'complete',
		'build-tools',
		'build-examples',
		'build-vbinaries',
		'setup-freetype',
		'doc',
		'doctor',
	]
	list_of_flags_that_allow_duplicates = ['cc', 'd', 'define', 'cf', 'cflags']
)

fn main() {
	args := os.args[1..]
	// args = 123
	if args.len == 0 || args[0] in ['-', 'repl'] {
		// Running `./v` without args launches repl
		if args.len == 0 {
			if is_atty(0) != 0 {
				println('Welcome to the V REPL (for help with V itself, type `exit`, then run `v help`).')
			} else {
				mut args_and_flags := util.join_env_vflags_and_os_args()[1..].clone()
				args_and_flags << ['run', '-']
				pref.parse_args(args_and_flags)
			}
		}
		util.launch_tool(false, 'vrepl', os.args[1..])
		return
	}
	args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, command := pref.parse_args(args_and_flags)
	if prefs.is_verbose {
		// println('args= ')
		// println(args) // QTODO
		// println('prefs= ')
		// println(prefs) // QTODO
	}
	if command == 'test-vet' {
		println('Please use `v test-cleancode` instead.')
		return
	}
	// Start calling the correct functions/external tools
	// Note for future contributors: Please add new subcommands in the `match` block below.
	if command in simple_cmd {
		// External tools
		util.launch_tool(prefs.is_verbose, 'v' + command, os.args[1..])
		return
	}
	match command {
		'help' {
			invoke_help_and_exit(args)
		}
		'new', 'init' {
			util.launch_tool(prefs.is_verbose, 'vcreate', os.args[1..])
			return
		}
		'translate' {
			println('Translating C to V will be available in V 0.3')
			return
		}
		'search', 'install', 'update', 'upgrade', 'outdated', 'list', 'remove' {
			util.launch_tool(prefs.is_verbose, 'vpm', os.args[1..])
			return
		}
		'vlib-docs' {
			util.launch_tool(prefs.is_verbose, 'vdoc', ['doc', 'vlib'])
		}
		'get' {
			println('V Error: Use `v install` to install modules from vpm.vlang.io')
			exit(1)
		}
		'version' {
			println(util.full_v_version(prefs.is_verbose))
			return
		}
		else {}
	}
	if command in ['run', 'build', 'build-module'] || command.ends_with('.v') || os.exists(command) {
		// println('command')
		// println(prefs.path)
		builder.compile(command, prefs)
		return
	}
	eprintln('v $command: unknown command\nRun "v help" for usage.')
	exit(1)
}

fn invoke_help_and_exit(remaining []string) {
	match remaining.len {
		0, 1 { help.print_and_exit('default') }
		2 { help.print_and_exit(remaining[1]) }
		else {}
	}
	println('V Error: Expected only one help topic to be provided.')
	println('For usage information, use `v help`.')
	exit(1)
}
