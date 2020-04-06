// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compile

// This file contains the options specific to the C backend of V.
// To add a flag to all backends at once, please add the flag to `parse_options()` in `compile_options.v`.
// To add a flag to both x64 and C, please add the flag to `parse_executable_options()` in `compile_x64_options.v`.

import (
	internal.flag
	v.pref
)

fn parse_c_options(flag string, f mut flag.Instance, prefs mut pref.Preferences) {
	match flag {
		'cc', 'compiler' {
			f.is_equivalent_to(['cc', 'compiler'])
			//TODO Remove `tmp` variable when it doesn't error out in C.
			tmp := f.string() or {
				println('V error: Expected argument after `-$flag`.')
				exit(1)
			}
			prefs.ccompiler = tmp
		}
		'cg', 'cdebug' {
			f.is_equivalent_to(['cg', 'cdebug', 'g', 'debug'])
			if f.bool() {
				prefs.is_debug = true
				prefs.is_vlines = false
			}
		}
		'live' {
			prefs.is_live = f.bool()
		}
		'csource' {
			operation := f.string() or {
				println('V error: Expected argument after `-csource`.')
				exit(1)
			}
			match operation {
				'keep' {
					prefs.is_keep_c = true
				}
				'drop' {} //Default
				else {
					println('V error: Unknown argument for `-csource` (`$operation`).')
					println('Allowed options: `keep`, `prettify` and `drop`.')
					exit(1)
				}
			}
		}
		'sanitize' {
			prefs.sanitize = f.bool()
		}
		'cf', 'cflags' {
			cflag := f.string() or {
				println('V error: Expected argument after `-$flag`.')
				exit(1)
			}
			prefs.cflags += ' $cflag'
		}
		// TODO Deprecate these once v2 parser is live
		'repl' {
			prefs.is_repl = f.bool()
		}
		'solive' {
			prefs.is_solive = f.bool()
			prefs.is_so = prefs.is_solive
		}
		else {
			parse_executable_options(flag, mut f, mut prefs)
		}
	}
}
