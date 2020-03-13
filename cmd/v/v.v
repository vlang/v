// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	compiler
	internal.compile
	internal.flag
	internal.help
	os
	v.table
	v.doc
)

const (
	simple_cmd = ['fmt',
	'up', 'self',
	'create',
	'test', 'test-fmt', 'test-compiler',
	'bin2v',
	'repl',
	'build-tools', 'build-examples', 'build-vbinaries',
	'setup-freetype']
)

fn main() {
	prefs := flag.MainCmdPreferences{}
	values := flag.parse_main_cmd(os.args, parse_flags, &prefs) or {
		println('V Error: An error has occured while parsing flags: ')
		println(err)
		exit(1)
	}
	if prefs.verbosity.is_higher_or_equal(.level_two) {
		println('V $compiler.Version $compiler.vhash()')
	}
	if prefs.verbosity.is_higher_or_equal(.level_three) {
		println('Parsed preferences: ')
		println(prefs)
		println('Remaining: $values')
	}
	// Do a quick check for `v -v`. Too much error has been made this way.
	if prefs.verbosity == .level_one && values.len == 0 {
		println("`v -v` now runs V with verbose mode set to level one which doesn't do anything.")
		println('Did you mean `v -version` instead?')
		exit(1)
	}
	// Start calling the correct functions/external tools
	// Note for future contributors: Please add new subcommands in the `match` block below.
	if prefs.action == .version {
		disallow_unknown_flags(prefs)
		print_version_and_exit()
	}
	if values.len == 0 && prefs.action == .help {
		println('Use `v help` for usage information.')
		exit(1)
	}
	if values.len == 0 || values[0] == '-' || values[0] == 'repl' {
		// Check for REPL.
		if values.len == 0 {
			println('Running REPL as no arguments are provided.')
			println('For usage information, quit V REPL using `exit` and use `v help`.')
		}
		launch_tool(prefs.verbosity, 'vrepl')
	}
	command := values[0]
	if command in simple_cmd {
		// External tools
		launch_tool(prefs.verbosity, 'v' + command)
		return
	}
	match command {
		'translate' {
			println('Translating C to V will be available in V 0.3')
			return
		}
		'search', 'install', 'update', 'remove' {
			launch_tool(prefs.verbosity, 'vpm')
			return
		}
		'get' {
			println('V Error: Use `v install` to install modules from vpm.vlang.io')
			exit(1)
		}
		'symlink' {
			disallow_unknown_flags(prefs)
			create_symlink()
			return
		}
		'doc' {
			disallow_unknown_flags(prefs)
			if values.len == 1 {
				println('V Error: Expected argument: Module name to output documentations for')
				exit(1)
			}
			table := table.new_table()
			println(doc.doc(values[1], table))
			return
		}
		'help' {
			// We check if the arguments are empty as we don't want to steal it from tools
			// TODO Call actual help tool
			disallow_unknown_flags(prefs)
			if prefs.verbosity.is_higher_or_equal(.level_one) {
				println(help.verbose_help_text)
			}
			else {
				println(help.help_text)
			}
			if values.len > 1 {
				println('Note: Actual help module is coming soon. Feel free to ask on the official channels for clarification.')
			}
			return
		}
		'version' {
			disallow_unknown_flags(prefs)
			print_version_and_exit()
			return
		}
		else {}
	}
	if command == 'run' || command == 'build' || command.ends_with('.v') || os.exists(command) {
		arg := join_flags_and_argument()
		compile.compile(command, arg)
		return
	}
	eprintln('v $command: unknown command\nRun "v help" for usage.')
	exit(1)
}

fn print_version_and_exit() {
	version_hash := compiler.vhash()
	println('V $compiler.Version $version_hash')
	exit(0)
}

[inline]
fn disallow_unknown_flags(prefs flag.MainCmdPreferences) {
	if prefs.unknown_flag == '' {
		return
	}
	println('V Error: Unexpected flag found: $prefs.unknown_flag')
	exit(1)
}
