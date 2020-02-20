// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os

const (
	//list_of_flags contains a list of flags where an argument is expected past it.
	list_of_flags = [
		'-o', '-os', '-cc', '-cflags', '-d'
	]
)

fn get_basic_command_and_option(args []string) (string, []string) {
	mut option := []string
	for i, arg in args {
		if i == 0 {
			//Skip executable
			continue
		}
		if arg == '--' {
			//End of list of options. The next one is the command.
			if i+1 < os.args.len {
				return os.args[i+1], option
			}
			//There's no option past this
			return '', option
		}
		if arg in list_of_flags {
			i++
			continue
		}
		if arg[0] == `-` {
			option << arg
			continue
		}
		//It's not a flag. We did not skip it. It's a command.
		return arg, option
	}

	//There's no arguments that were not part of a flag.
	return '', option
}

fn non_empty(arg []string) []string {
	return arg.filter(it != '')
}

fn join_flags_and_argument() []string {
	vosargs := os.getenv('VOSARGS')
	if vosargs != '' {
		return non_empty(vosargs.split(' '))
	}

	mut args := []string
	vflags := os.getenv('VFLAGS')
	if vflags != '' {
		args << os.args[0]
		args << vflags.split(' ')
		if os.args.len > 1 {
			args << os.args[1..]
		}
		return non_empty(args)
	}

	return non_empty(os.args)
}
