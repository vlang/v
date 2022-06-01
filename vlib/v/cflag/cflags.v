// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module cflag

import os

// parsed cflag
pub struct CFlag {
pub:
	mod   string // the module in which the flag was given
	os    string // eg. windows | darwin | linux
	name  string // eg. -I
	value string // eg. /path/to/include
pub mut:
	cached string // eg. ~/.vmodules/cache/ea/ea9878886727367672163.o (for .o files)
}

pub fn (c &CFlag) str() string {
	return 'CFlag{ name: "$c.name" value: "$c.value" mod: "$c.mod" os: "$c.os" cached: "$c.cached" }'
}

const fexisting_literal = r'$first_existing'

// expand the flag value
pub fn (cf &CFlag) eval() string {
	mut value := ''
	cflag_eval_outer_loop: for i := 0; i < cf.value.len; i++ {
		x := cf.value[i]
		if x == `$` {
			remainder := cf.value[i..]
			if remainder.starts_with(cflag.fexisting_literal) {
				sparams := remainder[cflag.fexisting_literal.len + 1..].all_before(')')
				i += sparams.len + cflag.fexisting_literal.len + 1
				svalues := sparams.replace(',', '\n').split_into_lines().map(it.trim(' \'"'))
				// mut found_spath := ''
				for spath in svalues {
					if os.exists(spath) {
						// found_spath = spath
						value += spath
						continue cflag_eval_outer_loop
					}
				}
				panic('>> error: none of the paths $svalues exist')
				continue
			}
		}
		value += x.ascii_str()
	}
	return value
}

// format flag
pub fn (cf &CFlag) format() string {
	mut value := ''
	if cf.cached != '' {
		value = cf.cached
	} else {
		value = cf.eval()
	}
	if cf.name in ['-l', '-Wa', '-Wl', '-Wp'] && value.len > 0 {
		return '$cf.name$value'.trim_space()
	}
	// convert to absolute path
	if cf.name == '-I' || cf.name == '-L' || value.ends_with('.o') {
		value = '"' + os.real_path(value) + '"'
	}
	return '$cf.name $value'.trim_space()
}

// TODO: implement msvc specific c_options_before_target and c_options_after_target ...
pub fn (cflags []CFlag) c_options_before_target_msvc() []string {
	return []
}

pub fn (cflags []CFlag) c_options_after_target_msvc() []string {
	return []
}

pub fn (cflags []CFlag) c_options_before_target() []string {
	defines, others, _ := cflags.defines_others_libs()
	mut args := []string{}
	args << defines
	args << others
	return args
}

pub fn (cflags []CFlag) c_options_after_target() []string {
	_, _, libs := cflags.defines_others_libs()
	return libs
}

pub fn (cflags []CFlag) c_options_without_object_files() []string {
	mut args := []string{}
	for flag in cflags {
		if flag.value.ends_with('.o') || flag.value.ends_with('.obj') {
			continue
		}
		args << flag.format()
	}
	return args
}

pub fn (cflags []CFlag) c_options_only_object_files() []string {
	mut args := []string{}
	for flag in cflags {
		if flag.value.ends_with('.o') || flag.value.ends_with('.obj') {
			args << flag.format()
		}
	}
	return args
}

pub fn (cflags []CFlag) defines_others_libs() ([]string, []string, []string) {
	copts_without_obj_files := cflags.c_options_without_object_files()
	mut defines := []string{}
	mut others := []string{}
	mut libs := []string{}
	for copt in copts_without_obj_files {
		if copt.starts_with('-l') {
			libs << copt
			continue
		}
		if copt.ends_with('.a') {
			libs << '"$copt"'
			continue
		}
		if copt.starts_with('-D') {
			defines << copt
			continue
		}
		others << copt
	}
	return defines, others, libs
}
