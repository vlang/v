// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	compiler
	os
)

const (
	simple_cmd = ['fmt',
	'up',
	'create',
	'test', 'test-fmt', 'test-compiler',
	'bin2v',
	'repl',
	'build-tools', 'build-examples', 'build-vbinaries']
)

fn main() {
	arg := join_flags_and_argument()
	command,option := get_basic_command_and_option(arg)
	is_verbose := '-verbose' in arg || '--verbose' in arg
	if '-v' in option || '--version' in option || command == 'version' {
		// Print the version and exit.
		version_hash := compiler.vhash()
		println('V $compiler.Version $version_hash')
		return
	}
	if '-h' in option || '--help' in option || command == 'help' {
		if is_verbose {
			println(verbose_help_text)
		}
		else {
			println(help_text)
		}
		return
	}
	if is_verbose {
		eprintln('v    args: $arg')
		eprintln('v command: $command')
		eprintln('v options: $option')
	}
	if command in simple_cmd {
		// External tools
		launch_tool(is_verbose, 'v' + command, command)
		return
	}
	if command == 'run' || command == 'build' || command.ends_with('.v') || os.exists(command) {
		compile(command, arg)
		return
	}
	match command {
		'', '-' {
			if arg.len == 1 {
				println('Running REPL as no arguments are provided. For usage information, use `v help`.')
			}
			launch_tool(is_verbose, 'vrepl', '')
		}
		'translate' {
			println('Translating C to V will be available in V 0.3 (January)')
		}
		'search', 'install', 'update', 'remove' {
			launch_tool(is_verbose, 'vpm', command)
		}
		'get' {
			println('Use `v install` to install modules from vpm.vlang.io.')
		}
		'symlink' {
			create_symlink()
		}
		'doc' {
			println('Currently unimplemented')
		}
		else {
			eprintln('v $command: unknown command\nRun "v help" for usage.')
			exit(1)
		}
	}
}
