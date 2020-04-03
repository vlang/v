// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compile

import (
	internal.flag
	os.cmdline
	v.pref
	v.util
)

fn parse_arguments(args []string) (pref.Preferences, []string) {
	mut p := pref.Preferences{
		//TODO: Refactor away this preference.
		// It's no longer controlled by a command-line flag.
		enable_globals: true
	}
	mut backend := cmdline.options(args, '-b')
	backend << cmdline.options(args, '-backend')
	if backend.len > 1 {
		println('V error: Only one backend may be enabled at once. (Multiple `-b`/`-backend` flags provided)')
		exit(1)
	}
	if backend.len == 1 {

// TODO remove tmp var after cgen optional bug is fixed
		x := pref.backend_from_string(backend[0]) or {
			println('V error: Unknown backend ${backend[0]} provided.')
			exit(1)
		}
		p.backend = x
	} else {
		p.backend = .c
	}
	remaining2 := flag.parse_pref(args, parse_options, p) or {
		println('V error: Error while parsing flags.')
		println(err)
		println('Args:')
		println(args)
		exit(1)
	}
	mut remaining := remaining2 // TODO cgen bug
	match remaining[0] {
		'run' {
			p.is_run = true
			remaining = remaining[1..]
		}
		'build' {
			remaining = remaining[1..]
			if remaining.len > 0 && remaining[0] == 'module' {
				remaining = remaining[1..]
				//TODO Figure out module
				println('V error: Module compilation is not ready yet.')
				exit(1)
			}
		}
		else {}
	}
	if remaining.len == 0 {
		println('V error: Expected file/directory to compile.')
		exit(1)
	}
	if !p.is_run && remaining.len > 1 {
		println('V error: Expected only one file/directory to compile.')
		println('Did you perhaps put flags after the file/directory?')
		exit(1)
	}
	p.path = remaining[0]
	p.fill_with_defaults()
	return p, remaining
}

fn parse_options(flag string, f mut flag.Instance, prefs mut pref.Preferences) {
	match flag {
		'color' {
		    f.is_equivalent_to(['color','nocolor'])
			util.emanager.set_support_color(true)
		}
		'nocolor' {
		    f.is_equivalent_to(['color','nocolor'])
			util.emanager.set_support_color(false)
		}
		'path' {
			// -path
			path_str := f.string() or {
				println('V error: Expected argument for `-path`.')
				exit(1)
			}
			prefs.lookup_path = path_str.split('|')
		}
		'o', 'output' {
			f.is_equivalent_to(['o', 'output'])
			//TODO Remove `tmp` variable when it doesn't error out in C.
			tmp := f.string() or {
				println('V error: Expected argument for `-$flag`.')
				exit(1)
			}
			prefs.out_name = tmp
		}
		'd', 'define' {
			define := f.string() or {
				println('V error: Expected argument for `-$flag`.')
				exit(1)
			}
			parse_define(mut prefs, define)
		}
		'g', 'debug' {
			f.is_equivalent_to(['g', 'debug', 'cg', 'cdebug'])
			if f.bool() {
				prefs.is_debug = true
				prefs.is_vlines = true
			}
		}
		'e', 'experiments' {
			to_enable := f.string() or {
				println('V error: Expected argument for `-$flag`.')
				exit(1)
			}
			match to_enable {
				'prealloc' {
					prefs.prealloc = true
				}
				else {
					println('V error: Unknown experiment `$to_enable`.')
					exit(1)
				}
			}
		}
		'prod' {
			prefs.is_prod = true
		}
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
		'full-rebuild' {
			prefs.is_cache = !f.bool()
		}
		'stats' {
			prefs.is_stats = f.bool()
		}
		'obf', 'obfuscate' {
			f.is_equivalent_to(['-obf', '-obfuscate'])
			prefs.obfuscate = f.bool()
		}
		'prof', 'profile' {
			f.is_equivalent_to(['-prof', '-profile'])
			prefs.is_prof = f.bool()
		}
		'translated' {
			prefs.translated = f.bool()
		}
		'b', 'backend' {
			// Just consume it. The option is handled outside of this function
			f.string() or { return }
		}
		else {
			match prefs.backend {
				.c, .experimental {
					parse_c_options(flag, mut f, mut prefs)
				}
				.x64 {
					parse_x64_options(flag, mut f, mut prefs)
				}
				.js {
					parse_js_options(flag, f, prefs)
				}
				else {
					// TODO: Remove when compiler allows for it.
					// This enum matching IS exhaustive.
					panic('unexpected backend type: $prefs.backend')
				}
			}
		}
	}
}

[inline]
fn parse_define(prefs mut pref.Preferences, define string) {
	define_parts := define.split('=')
	if define_parts.len == 1 {
		prefs.compile_defines << define
		prefs.compile_defines_all << define
		return
	}
	if define_parts.len == 2 {
		prefs.compile_defines_all << define_parts[0]
		match define_parts[1] {
			'0' {}
			'1' {
				prefs.compile_defines << define_parts[0]
			}
			else {
				println('V error: Unknown define argument value `${define_parts[1]}` for ${define_parts[0]}.' +
					'Expected `0` or `1`.')
				exit(1)
			}
		}
		return
	}
	println('V error: Unknown define argument: ${define}. Expected at most one `=`.')
	exit(1)
}
