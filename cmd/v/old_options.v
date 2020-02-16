// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	os.cmdline
)

struct Deprecated {
	old string
	new string
	not_exactly bool
}

//parse_and_output_new_format parses the old format and tells the user how to use the new options.
//This function exits if it detects the old format and returns if it doesn't.
fn parse_and_output_new_format(args []string) {
	mut list := []Deprecated
	mut obsolete := []string
	//Check output suffixes
	output := cmdline.option(args, '-o', '')
	backend := cmdline.option(args, '-backend', '')
	if output.ends_with('.c') {
		list << Deprecated {
			old: '-o $output'
			new: '-csource keep'
		}
	} else if output.ends_with('.js') && backend != 'js' {
		list << Deprecated {
			old: '-o $output'
			new: '-backend js'
		}
	}
	//Check `-os msvc`
	os := cmdline.option(args, '-os', '')
	if os == 'msvc' {
		list << Deprecated {
			old: '-os msvc'
			new: '-cc msvc'
		}
	}
	//Check simple options
	//TODO Refactor them to actually just modify mutable arrays
	list << add_if_found_deprecated(args, '-cache', '-full-rebuild=false')
	list << add_if_found_deprecated(args, 'translated', '-translated')
	list << add_if_found_deprecated(args, '-x64', '-backend x64')
	list << add_if_found_deprecated(args, '-v2', '-backend experimental')
	list << add_if_found_deprecated(args, '-keep_c', '-csource keep')
	list << add_if_found_deprecated(args, '-pretty_c', '-csource prettify')
	list << add_if_found_deprecated(args, '-show_c_cmd', '-v')
	list << add_if_found_deprecated(args, '-autofree', '-manual-free=false')
	list << add_if_found_deprecated(args, '-fast', '-cc tcc')
	list << add_if_found_deprecated(args, '-output-cross-platform-c', '-os cross')
	list << add_if_found_deprecated(args, '-m32', '-arch x86')
	list << add_if_found_deprecated(args, '-bare', '-freestanding')
	obsolete << add_if_found_string(args, '--enable-globals')
	obsolete << add_if_found_string(args, '-prealloc')
	list << add_if_found_deprecated(args, '-user_mod_path', '-path*')
	list << add_if_found_deprecated(args, '-vlib-path', '-path*')
	list << add_if_found_deprecated(args, '-vpath', '-path*')
	//Nothing to do here
	if list.len == 0 && obsolete.len == 0 {
		return
	}
	//Output messages
	println('V has encountered deprecated/obsolete options. Please edit your command.\n')
	if list.len > 0 {
		println('Deprecated options that have been replaced:')
		for deprecation in list {
			if deprecation.not_exactly {
				println('   `$deprecation.old` has been superseded by `$deprecation.new` (see help for more details)')
			} else {
				println('   use `$deprecation.new` instead of `$deprecation.old`')
			}
		}
		println('')
	}
	if obsolete.len > 0 {
		println('Obsolete options that are no longer supported:')
		for obsoleted in obsolete {
			println('   `$obsoleted` has been removed')
		}
		println('')
	}
	println('For more details, please use the command `v help build` for a list of options.')
	exit(1)
}

//=====
//HELPER FUNCTIONS
//=====

[inline]
fn add_if_found_deprecated(args []string, deprecated string, alt string) []Deprecated {
	if deprecated in args {
		new := if alt.ends_with('*') {
			Deprecated {
				old: deprecated
				new: alt[..alt.len-1]
				not_exactly: true
			}
		} else {
			Deprecated {
				old: deprecated
				new: alt
				not_exactly: false
			}
		}
		return [new]
	}
	return []
}

[inline]
fn add_if_found_string(args []string, deprecated string) []string {
	if deprecated in args {
		return [deprecated]
	}
	return []
}
