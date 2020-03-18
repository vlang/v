// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	internal.flag
	os
)

const (
	//list_of_flags contains a list of flags where an argument is expected past it.
	list_of_flags = [
		'o', 'output', 'd', 'define', 'b', 'backend', 'cc', 'os', 'target-os', 'arch',
			'csource', 'cf', 'cflags', 'path'
	]
)

fn join_flags_and_argument() []string {
	vosargs := os.getenv('VOSARGS')
	if vosargs != '' {
		return vosargs.split(' ')
	}
	// No VOSARGS? Look for VFLAGS and concat it with command-line options.
	mut args := []string
	vflags := os.getenv('VFLAGS')
	if vflags != '' {
		args << os.args[0]
		args << vflags.split(' ')
		if os.args.len > 1 {
			args << os.args[1..]
		}
		return args
	}
	// No VFLAGS too? Just return command-line args.
	return os.args
}

fn parse_flags(flag string, f mut flag.Instance, prefs mut flag.MainCmdPreferences) {
	match flag {
		'v' {
			f.is_equivalent_to(['v', 'vv', 'vvv', 'verbose'])
			prefs.verbosity = .level_one
		}
		'vv' {
			f.is_equivalent_to(['v', 'vv', 'vvv', 'verbose'])
			prefs.verbosity = .level_two
		}
		'vvv' {
			f.is_equivalent_to(['v', 'vv', 'vvv', 'verbose'])
			prefs.verbosity = .level_three
		}
		'verbose' {
			f.is_equivalent_to(['v', 'vv', 'vvv', 'verbose'])
			level := f.int() or {
				println('V error: Expected `0`, `1`, `2` or `3` as argument to `-verbose` to specify verbosity level.')
				exit(1)
			}
			match level {
				0 {} //Zero verbosity is already default.
				1 {
					prefs.verbosity = .level_one
				}
				2 {
					prefs.verbosity = .level_two
				}
				3 {
					prefs.verbosity = .level_three
				}
				else {
					println('V error: Expected `0`, `1`, `2` or `3` as argument to `-verbose` to specify verbosity level.')
					exit(1)
				}
			}
		}
		'h', 'help' {
			f.is_equivalent_to(['h', 'help'])
			prefs.action = .help
		}
		'v', 'version' {
			f.is_equivalent_to(['v', 'version'])
			prefs.action = .version
		}
		'-version', '-help' {
			println('V error: `-$flag` has been deprecated. Use `$flag` instead.')
			exit(1)
		}
		else {
			prefs.unknown_flag = '-$flag'
			if !(flag in list_of_flags) {
				return
			}
			f.string() or {
				println('V error: Error parsing flag. Expected value for `-$flag`.')
				exit(1)
			}
			return
		}
	}
}
