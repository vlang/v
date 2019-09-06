// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import os

// C flag
struct CFlag{
	os    string // eg. windows | darwin | linux
	name  string // eg. -I
	value string // eg. /path/to/incude
}

fn (table &Table) has_cflag(cflag CFlag) bool {
	for cf in table.cflags {
		if cf.os == cflag.os && cf.name == cflag.name && cf.value == cflag.value {
			return true
		}
	}
	return false
}

// parse the flags to []CFlag
// Note: clean up big time (joe-c)
fn (table mut Table) parse_cflag(cflag string) {
	allowed_flags := [
		'framework',
		'library',
		'I', 'l', 'L', 
	]
	mut flag := cflag.trim_space()
	if flag == '' {
		return
	}
	mut fos := ''
	mut name := ''
	if flag.starts_with('linux') || flag.starts_with('darwin') || flag.starts_with('windows') {
		pos := flag.index(' ')
		fos = flag.left(pos).trim_space()
		flag = flag.right(pos).trim_space()
	}
	for {
		mut index := -1
		mut value := ''
		if flag[0] == `-` {
			for f in allowed_flags {
				i := 1+f.len
				if i < flag.len && f == flag.substr(1,i) {
					name = flag.left(i).trim_space()
					flag = flag.right(i).trim_space()
					break
				}
			}
		}
		for i in [flag.index(' '), flag.index(',')] {
			if index == -1 || (i != -1 && i < index) {
				index = i
			}
		} if index != -1 && flag[index] == ` ` && flag[index+1] == `-` {
			for f in allowed_flags {
				i := index+f.len
				if i < flag.len && f == flag.substr(index, i) {
					index = i
					break
				}
			}
			value = flag.left(index).trim_space()
			flag = flag.right(index).trim_space()
		} else if index != -1 && index < flag.len-2 && flag[index] == `,` {
			value = flag.left(index).trim_space()
			flag = flag.right(index+1).trim_space()
		} else {
			value = flag.trim_space()
			index = -1
		}
		cf := CFlag{
			os:    fos,
			name:  name,
			value: value
		}
		if !table.has_cflag(cf) {
			table.cflags << cf
		}
		if index == -1 {
			break
		}
	}
}

fn (v V) get_os_cflags() []CFlag {
	mut flags := []CFlag
	for flag in v.table.cflags {
		if flag.os == ''
		|| (flag.os == 'linux' && v.os == .linux)
		|| (flag.os == 'darwin' && v.os == .mac)
		|| (flag.os == 'windows' && (v.os == .windows || v.os == .msvc)) {
			flags << flag
		}
	}
	return flags
}

fn (cf &CFlag) tos() string {
	mut value := cf.value
	// convert to absolute path
	if cf.name == '-I' || cf.name == '-L' || value.ends_with('.o') {
		// msvc expects .obj not .o
		$if msvc {
			if value.ends_with('.o') {
				value = '${value}bj'
			}
		}
		value = '"'+os.realpath(value)+'"'
	}
	return '$cf.name $cf.value'.trim_space()
}
