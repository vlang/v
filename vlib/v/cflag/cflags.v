// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
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

// format flag
pub fn (cf &CFlag) format() string {
	mut value := cf.value
	if cf.cached != '' {
		value = cf.cached
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
pub fn (cflags []CFlag) c_options_before_target_msvc() string {
	return ''
}

pub fn (cflags []CFlag) c_options_after_target_msvc() string {
	return ''
}

pub fn (cflags []CFlag) c_options_before_target() string {
	// -I flags, optimization flags and so on
	mut args := []string{}
	for flag in cflags {
		if flag.name != '-l' && !flag.value.ends_with('.o') {
			args << flag.format()
		}
	}
	return args.join(' ')
}

pub fn (cflags []CFlag) c_options_after_target() string {
	// -l flags (libs)
	mut args := []string{}
	for flag in cflags {
		if flag.name == '-l' {
			args << flag.format()
		}
	}
	return args.join(' ')
}

pub fn (cflags []CFlag) c_options_without_object_files() string {
	mut args := []string{}
	for flag in cflags {
		if flag.value.ends_with('.o') || flag.value.ends_with('.obj') {
			continue
		}
		args << flag.format()
	}
	return args.join(' ')
}

pub fn (cflags []CFlag) c_options_only_object_files() string {
	mut args := []string{}
	for flag in cflags {
		if flag.value.ends_with('.o') || flag.value.ends_with('.obj') {
			args << flag.format()
		}
	}
	return args.join(' ')
}
