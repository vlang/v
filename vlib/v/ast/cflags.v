// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.cflag

const allowed_cflag_names = ['framework', 'library', 'Wa', 'Wl', 'Wp', 'I', 'l', 'L', 'D']

struct CFlagInput {
	original string
	flag     string
	os       string
}

// check if cflag is in table
pub fn (t &Table) has_cflag(flag cflag.CFlag) bool {
	for cf in t.cflags {
		if cf.os == flag.os && cf.name == flag.name && cf.value == flag.value {
			return true
		}
	}
	return false
}

fn prepare_cflag_input(cflg string, ctimedefines []string) !CFlagInput {
	flag_orig := cflg.trim_space()
	mut flag := flag_orig
	if flag == '' {
		return error('flag is empty')
	}
	if flag.contains('`') {
		return error('bad #flag `${flag_orig}`: shell command substitution with backticks is not supported; use #pkgconfig or explicit flags instead')
	}
	mut fos := ''
	mut allowed_os_overrides := []string{}
	allowed_os_overrides << valid_comptime_not_user_defined
	allowed_os_overrides << ctimedefines
	for os_override in allowed_os_overrides {
		if !flag.starts_with(os_override) {
			continue
		}
		pos := flag.index(' ') or { return error('none') }
		fos = flag[..pos].trim_space()
		flag = flag[pos..].trim_space()
	}
	return CFlagInput{
		original: flag_orig
		flag:     flag
		os:       fos
	}
}

fn cflag_name_end(flag string) int {
	if flag.len == 0 || flag[0] != `-` {
		return 0
	}
	for name in allowed_cflag_names {
		end := 1 + name.len
		if end <= flag.len && name == flag[1..end] {
			return end
		}
	}
	return 0
}

fn parse_cflag_fragment(fragment string, original string, mod string, fos string) !cflag.CFlag {
	name_end := cflag_name_end(fragment)
	name := fragment[..name_end].trim_space()
	value := fragment[name_end..].trim_space()
	if name in ['-I', '-l', '-L'] && value == '' {
		hint := if name == '-l' { 'library name' } else { 'path' }
		return error('bad #flag `${original}`: missing ${hint} after `${name}`')
	}
	return cflag.CFlag{
		mod:   mod
		os:    fos
		name:  name
		value: value
	}
}

fn parse_cflags(cflg string, mod string, ctimedefines []string) ![]cflag.CFlag {
	input := prepare_cflag_input(cflg, ctimedefines)!
	mut flag := input.flag
	mut parsed := []cflag.CFlag{}
	for {
		// -I/usr/local/a b c/include -m64 -I/usr/include
		name_end := cflag_name_end(flag)
		value := flag[name_end..].trim_space()
		index := value.index_(' -')
		if index > -1 {
			fragment := flag[..name_end] + value[..index]
			parsed << parse_cflag_fragment(fragment, input.original, mod, input.os)!
			flag = value[index..].trim_space()
			continue
		}
		parsed << parse_cflag_fragment(flag, input.original, mod, input.os)!
		break
	}
	return parsed
}

fn (mut t Table) add_unique_cflags(parsed []cflag.CFlag) []cflag.CFlag {
	mut added := []cflag.CFlag{cap: parsed.len}
	for cf in parsed {
		if !t.has_cflag(cf) {
			t.cflags << cf
			added << cf
		}
	}
	return added
}

// parse the flags to (ast.cflags) []CFlag
// Note: clean up big time (joe-c)
pub fn (mut t Table) parse_cflag(cflg string, mod string, ctimedefines []string) ! {
	t.add_unique_cflags(parse_cflags(cflg, mod, ctimedefines)!)
}

// parse_cflag_with_link_segment keeps the source position of ordinary flags available
// when a static pkg-config directive requires a unified linker stream.
pub fn (mut t Table) parse_cflag_with_link_segment(cflg string, mod string, ctimedefines []string) ! {
	parsed := parse_cflags(cflg, mod, ctimedefines)!
	t.add_unique_cflags(parsed)
	if parsed.len > 0 {
		t.link_flag_segments << LinkFlagSegment{
			flags: parsed
		}
	}
}

// parse_pkgconfig_link_flags anchors structured linker fragments at the current
// source position, including repeated libraries and positional linker controls.
pub fn (mut t Table) parse_pkgconfig_link_flags(fragments []string, mod string, ctimedefines []string) ! {
	mut parsed := []cflag.CFlag{cap: fragments.len}
	mut i := 0
	for i < fragments.len {
		fragment := fragments[i]
		if fragment in ['-L', '-l'] {
			if i + 1 >= fragments.len || fragments[i + 1].trim_space() == '' {
				hint := if fragment == '-l' { 'library name' } else { 'path' }
				return error('bad #flag `${fragment}`: missing ${hint} after `${fragment}`')
			}
			input := prepare_cflag_input(fragment + fragments[i + 1], ctimedefines)!
			parsed << parse_cflag_fragment(input.flag, input.original, mod, input.os)!
			i += 2
			continue
		}
		input := prepare_cflag_input(fragment, ctimedefines)!
		parsed << parse_cflag_fragment(input.flag, input.original, mod, input.os)!
		i++
	}
	t.link_flag_segments << LinkFlagSegment{
		is_pkgconfig: true
		flags:        parsed
	}
}
