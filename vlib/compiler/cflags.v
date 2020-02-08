// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import os
// parsed cflag
struct CFlag {
	mod   string // the module in which the flag was given
	os    string // eg. windows | darwin | linux
	name  string // eg. -I
	value string // eg. /path/to/include
}

pub fn (c &CFlag) str() string {
	return 'CFlag{ name: "$c.name" value: "$c.value" mod: "$c.mod" os: "$c.os" }'
}

// get flags for current os
fn (v &V) get_os_cflags() []CFlag {
	mut flags := []CFlag
	mut ctimedefines := []string
	if v.pref.compile_defines.len > 0 {
		ctimedefines << v.pref.compile_defines
	}
	
	for flag in v.table.cflags {
		if flag.os == '' || (flag.os == 'linux' && v.pref.os == .linux) || (flag.os == 'darwin' && v.pref.os == .mac) || (flag.os == 'freebsd' && v.pref.os == .freebsd) || (flag.os == 'windows' && v.pref.os == .windows) {
			flags << flag
		}
		if flag.os in ctimedefines {
			flags << flag
		}
	}
	return flags
}

fn (v &V) get_rest_of_module_cflags(c &CFlag) []CFlag {
	mut flags := []CFlag
	cflags := v.get_os_cflags()
	for flag in cflags {
		if c.mod == flag.mod {
			if c.name == flag.name && c.value == flag.value && c.os == flag.os {
				continue
			}
			flags << flag
		}
	}
	return flags
}

// format flag
fn (cf &CFlag) format() string {
	mut value := cf.value
	if cf.name in ['-l', '-Wa', '-Wl', '-Wp'] && value.len > 0 {
		return '${cf.name}${value}'.trim_space()
	}
	// convert to absolute path
	if cf.name == '-I' || cf.name == '-L' || value.ends_with('.o') {
		value = '"' + os.realpath(value) + '"'
	}
	return '$cf.name $value'.trim_space()
}

// check if cflag is in table
fn (table &Table) has_cflag(cflag CFlag) bool {
	for cf in table.cflags {
		if cf.os == cflag.os && cf.name == cflag.name && cf.value == cflag.value {
			return true
		}
	}
	return false
}

// parse the flags to (table.cflags) []CFlag
// Note: clean up big time (joe-c)
fn (table mut Table) parse_cflag(cflag string, mod string, ctimedefines []string) ?bool {
	allowed_flags := ['framework', 'library', 'Wa', 'Wl', 'Wp', 'I', 'l', 'L', ]
	flag_orig := cflag.trim_space()
	mut flag := flag_orig
	if flag == '' {
		return true
	}
	mut fos := ''
	mut allowed_os_overrides := ['linux','darwin','freebsd','windows']
	allowed_os_overrides << ctimedefines
	for os_override in allowed_os_overrides {
		if !flag.starts_with( os_override ) { continue }
		pos := flag.index(' ') or {
			return none
		}
		fos = flag[..pos].trim_space()
		flag = flag[pos..].trim_space()
	}
	for {
		mut name := ''
		mut value := ''
		if flag[0] == `-` {
			for f in allowed_flags {
				i := 1 + f.len
				if i <= flag.len && f == flag[1..i] {
					name = flag[..i].trim_space()
					flag = flag[i..].trim_space()
					break
				}
			}
		}
		mut index := flag.index(' -') or {
			-1
		}
		for index > -1 {
			mut has_next := false
			for f in allowed_flags {
				i := index + 2 + f.len
				if i <= flag.len && f == flag[index + 2..i] {
					value = flag[..index + 1].trim_space()
					flag = flag[index + 1..].trim_space()
					has_next = true
					break
				}
			}
			if has_next {
				break
			}
			index = flag.index_after(' -', index + 1)
		}
		if index == -1 {
			value = flag.trim_space()
		}
		if (name in ['-I', '-l', '-L']) && value == '' {
			hint := if name == '-l' { 'library name' } else { 'path' }
			return error('bad #flag `$flag_orig`: missing $hint after `$name`')
		}
		cf := CFlag{
			mod: mod
			os: fos
			name: name
			value: value
		}
		if !table.has_cflag(cf) {
			table.cflags << cf
		}
		if index == -1 {
			break
		}
	}
	return true
}

// TODO: implement msvc specific c_options_before_target and c_options_after_target ...
fn (cflags []CFlag) c_options_before_target_msvc() string {
	return ''
}

fn (cflags []CFlag) c_options_after_target_msvc() string {
	return ''
}

fn (cflags []CFlag) c_options_before_target() string {
	// -I flags, optimization flags and so on
	mut args := []string
	for flag in cflags {
		if flag.name != '-l' {
			args << flag.format()
		}
	}
	return args.join(' ')
}

fn (cflags []CFlag) c_options_after_target() string {
	// -l flags (libs)
	mut args := []string
	for flag in cflags {
		if flag.name == '-l' {
			args << flag.format()
		}
	}
	return args.join(' ')
}

fn (cflags []CFlag) c_options_without_object_files() string {
	mut args := []string
	for flag in cflags {
		if flag.value.ends_with('.o') || flag.value.ends_with('.obj') {
			continue
		}
		args << flag.format()
	}
	return args.join(' ')
}

fn (cflags []CFlag) c_options_only_object_files() string {
	mut args := []string
	for flag in cflags {
		if flag.value.ends_with('.o') || flag.value.ends_with('.obj') {
			args << flag.format()
		}
	}
	return args.join(' ')
}

