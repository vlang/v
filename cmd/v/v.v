// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	internal.compile
	internal.flag
	internal.help
	os
	os.cmdline
	v.table
	v.doc
	v.pref
	v.util
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
	args := os.args[1..]
	//args = 123
	if args.len == 0 || args[0] in ['-', 'repl'] {
		// Running `./v` without args launches repl
		println('For usage information, quit V REPL using `exit` and use `v help`')
		launch_tool(false, 'vrepl')
		return
	}
	if args.len > 0 && (args[0] in ['version', '-V', '-version', '--version'] || (args[0] == '-v' && args.len == 1) ) {
		// `-v` flag is for setting verbosity, but without any args it prints the version, like Clang
		println(util.full_v_version())
		return
	}
	prefs2 := parse_args(args)
	prefs := flag.MainCmdPreferences{}
	values := flag.parse_main_cmd(os.args, parse_flags, prefs) or {
		println('V Error: An error has occurred while parsing flags: ')
		println(err)
		exit(1)
	}
	if prefs2.is_verbose {
		println(util.full_v_version())
	}
	if prefs2.is_verbose {
		println('Parsed preferences: ')
		//println(prefs) // QTODO
		println('Remaining: $values')
	}
	// Start calling the correct functions/external tools
	// Note for future contributors: Please add new subcommands in the `match` block below.
	if values.len == 0 && prefs.action == .help {
		invoke_help_and_exit(values)
	}
	command := if values.len > 0 { values[0] } else { '' }
	if command in simple_cmd {
		// External tools
		launch_tool(prefs2.is_verbose, 'v' + command)
		return
	}
	match command {
		'create', 'init' {
			launch_tool(prefs2.is_verbose, 'vcreate')
			return
		}
		'translate' {
			println('Translating C to V will be available in V 0.3')
			return
		}
		'search', 'install', 'update', 'remove' {
			launch_tool(prefs2.is_verbose, 'vpm')
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

fn parse_args(args []string) &pref.Preferences{
	mut res := &pref.Preferences{}
	for i, arg in args {
		match arg {
			'-v' {	res.is_verbose = true	}
			'-cg' { res.ccompiler = cmdline.option(args, '-cc', 'cc') }
			else { }
		}
	}
	return res
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
