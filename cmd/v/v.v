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
	v.pref
)

const (
	simple_cmd = ['fmt',
	'up', 'self',
	'test', 'test-fmt', 'test-compiler',
	'bin2v',
	'repl',
	'build-tools', 'build-examples', 'build-vbinaries',
	'setup-freetype']
)

fn main() {
	prefs := flag.MainCmdPreferences{}
	values := flag.parse_main_cmd(os.args, parse_flags, prefs) or {
		println('V Error: An error has occurred while parsing flags: ')
		println(err)
		exit(1)
	}
	if prefs.verbosity.is_higher_or_equal(.level_two) {
		println('V $compiler.Version $compiler.vhash()')
	}
	if prefs.verbosity.is_higher_or_equal(.level_three) {
		println('Parsed preferences: ')
		//println(prefs) // QTODO
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
		invoke_help_and_exit(values)
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
		'create', 'init' {
			launch_tool(prefs.verbosity, 'vcreate')
			return
		}
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
			disallow_unknown_flags(prefs)
			invoke_help_and_exit(values)
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

fn invoke_help_and_exit(remaining []string) {
	match remaining.len {
		0, 1 {
			help.print_and_exit('default')
		}
		2 {
			help.print_and_exit(remaining[1])
		}
		else {}
	}
	println('V Error: Expected only one help topic to be provided.')
	println('For usage information, use `v help`.')
	exit(1)
}

[inline]
fn disallow_unknown_flags(prefs flag.MainCmdPreferences) {
	if prefs.unknown_flag == '' {
		return
	}
	println('V Error: Unexpected flag found: $prefs.unknown_flag')
	exit(1)
}

fn create_symlink() {
	$if windows {
		return
	}
	vexe := pref.vexe_path()
	mut link_path := '/usr/local/bin/v'
	mut ret := os.exec('ln -sf $vexe $link_path') or { panic(err) }
	if ret.exit_code == 0 {
		println('Symlink "$link_path" has been created')
	}
	else if os.system('uname -o | grep -q \'[A/a]ndroid\'') == 0 {
		println('Failed to create symlink "$link_path". Trying again with Termux path for Android.')
		link_path = '/data/data/com.termux/files/usr/bin/v'
		ret = os.exec('ln -sf $vexe $link_path') or { panic(err) }
		if ret.exit_code == 0 {
			println('Symlink "$link_path" has been created')
		} else {
			println('Failed to create symlink "$link_path". Try again with sudo.')
		}
	} else {
			println('Failed to create symlink "$link_path". Try again with sudo.')
	}
}
