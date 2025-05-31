// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module cflag

import os
import strings

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
	return 'CFlag{ name: "${c.name}" value: "${c.value}" mod: "${c.mod}" os: "${c.os}" cached: "${c.cached}" }'
}

const fexisting_literal = r'$first_existing'
const wexisting_literal = r'$when_first_existing'

fn find_first_existing_path(remainder string, literal string) (bool, string, int, []string) {
	sparams := remainder[literal.len + 1..].all_before(')')
	delta_i := sparams.len + literal.len + 1
	svalues := sparams.replace(',', '\n').split_into_lines().map(it.trim('\t \'"'))
	for spath in svalues {
		if os.exists(spath) {
			return true, spath, delta_i, []string{}
		}
	}
	return false, '', delta_i, svalues
}

// expand the flag value
pub fn (cf &CFlag) eval() ?string {
	mut value_builder := strings.new_builder(10 * cf.value.len)
	cflag_eval_outer_loop: for i := 0; i < cf.value.len; i++ {
		x := cf.value[i]
		if x == `$` {
			remainder := cf.value[i..]
			if remainder.starts_with(fexisting_literal) {
				found, spath, delta_i, svalues := find_first_existing_path(remainder,
					fexisting_literal)
				if found {
					value_builder.write_string(spath)
					i += delta_i
					continue
				}
				panic('>> error: none of the paths ${svalues} exist')
			}
			if remainder.starts_with(wexisting_literal) {
				found, spath, delta_i, _ := find_first_existing_path(remainder, wexisting_literal)
				if found {
					value_builder.write_string(spath)
					i += delta_i
					continue
				}
				return none
			}
		}
		value_builder.write_string(x.ascii_str())
	}
	return value_builder.str()
}

// format flag
pub fn (cf &CFlag) format() ?string {
	mut value := ''
	if cf.cached != '' {
		value = cf.cached
	} else {
		value = cf.eval()?
	}
	if cf.name in ['-l', '-Wa', '-Wl', '-Wp'] && value != '' {
		return '${cf.name}${value}'.trim_space()
	}
	// convert to absolute path
	if cf.name == '-I' || cf.name == '-L' || value.ends_with('.o') {
		value = '"' + os.real_path(value) + '"'
	}
	return '${cf.name} ${value}'.trim_space()
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
	mut args := []string{cap: defines.len + others.len}
	args << defines
	args << others
	return uniq_non_empty(args)
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
		args << flag.format() or { continue }
	}
	return uniq_non_empty(args)
}

pub fn (cflags []CFlag) c_options_only_object_files() []string {
	mut args := []string{}
	for flag in cflags {
		// TODO figure out a better way to copy cross compiling flags to the linker
		if flag.value.ends_with('.o') || flag.value.ends_with('.obj')
			|| (flag.name == '-l' && flag.value == 'pq') {
			args << flag.format() or { continue }
		}
	}
	return uniq_non_empty(args)
}

pub fn (cflags []CFlag) defines_others_libs() ([]string, []string, []string) {
	copts_without_obj_files := cflags.c_options_without_object_files()
	mut defines := []string{}
	mut others := []string{}
	mut libs := []string{}
	for copt in copts_without_obj_files {
		if copt.ends_with('@START_LIBS') {
			libs.insert(0, copt.all_before('@START_LIBS'))
			continue
		}
		if copt.starts_with('-l') {
			libs << copt
			continue
		}
		if copt.ends_with('.a') {
			libs << '"${copt}"'
			continue
		}

		if copt.ends_with('@START_DEFINES') {
			defines.insert(0, copt.all_before('@START_DEFINES'))
			continue
		}
		if copt.starts_with('-D') {
			defines << copt
			continue
		}

		if copt.ends_with('@START_OTHERS') {
			others.insert(0, copt.all_before('@START_OTHERS'))
			continue
		}
		others << copt
	}
	return uniq_non_empty(defines), uniq_non_empty(others), uniq_non_empty(libs)
}

fn uniq_non_empty(args []string) []string {
	mut uniq_args := []string{}
	for a in args {
		if a == '' {
			continue
		}
		if a !in uniq_args {
			uniq_args << a
		}
	}
	return uniq_args
}
