// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.cflag

// check if cflag is in table
fn (t &Table) has_cflag(flag cflag.CFlag) bool {
	for cf in t.cflags {
		if cf.os == flag.os && cf.name == flag.name && cf.value == flag.value {
			return true
		}
	}
	return false
}

// parse the flags to (ast.cflags) []CFlag
// Note: clean up big time (joe-c)
pub fn (mut t Table) parse_cflag(cflg string, mod string, ctimedefines []string) ! {
	allowed_flags := ['framework', 'library', 'Wa', 'Wl', 'Wp', 'I', 'l', 'L', 'D']
	flag_orig := cflg.trim_space()
	mut flag := flag_orig
	if flag == '' {
		return error('flag is empty')
	}
	mut fos := ''
	mut allowed_os_overrides := ['linux', 'darwin', 'freebsd', 'openbsd', 'windows', 'mingw',
		'solaris', 'android', 'termux']
	allowed_os_overrides << ctimedefines
	for os_override in allowed_os_overrides {
		if !flag.starts_with(os_override) {
			continue
		}
		pos := flag.index(' ') or { return error('none') }
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
		mut index := flag.index(' -') or { -1 }
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
			return error('bad #flag `${flag_orig}`: missing ${hint} after `${name}`')
		}
		cf := cflag.CFlag{
			mod: mod
			os: fos
			name: name
			value: value
		}
		if !t.has_cflag(cf) {
			t.cflags << cf
		}
		if index == -1 {
			break
		}
	}
}
