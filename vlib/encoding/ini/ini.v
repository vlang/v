module ini

import os

// IniFile is a parsed INI document: ordered sections of key/value pairs.
// It is the V-idiomatic equivalent of Python's `configparser`.
pub struct IniFile {
mut:
	sections map[string]map[string]string
	order    []string
}

// parse_string parses an INI document. Sections are `[name]` headers; entries
// are `key = value` or `key : value`. Full-line `;`/`#` comments and blank
// lines are skipped; keys before any section, duplicate sections/keys and
// lines without a separator are errors. There are intentionally no inline
// comments, so values like URLs with fragments survive intact.
pub fn parse_string(s string) !IniFile {
	mut f := IniFile{}
	mut current := ''
	mut has_section := false
	for ln, raw_line in s.split('\n') {
		line := raw_line.trim_space()
		if line.len == 0 || line[0] == `;` || line[0] == `#` {
			continue
		}
		if line[0] == `[` {
			if line[line.len - 1] != `]` {
				return error('ini: line ${ln + 1}: unbalanced section header')
			}
			name := line[1..line.len - 1].trim_space()
			if name in f.sections {
				return error('ini: line ${ln + 1}: duplicate section `${name}`')
			}
			f.sections[name] = map[string]string{}
			f.order << name
			current = name
			has_section = true
			continue
		}
		if !has_section {
			return error('ini: line ${ln + 1}: key outside any section')
		}
		sep := separator_pos(line) or {
			return error('ini: line ${ln + 1}: missing `=` or `:` separator')
		}
		key := line[..sep].trim_space()
		value := line[sep + 1..].trim_space()
		if key in f.sections[current] {
			return error('ini: line ${ln + 1}: duplicate key `${key}`')
		}
		f.sections[current][key] = value
	}
	return f
}

// parse_file reads and parses the INI file at path.
pub fn parse_file(path string) !IniFile {
	return parse_string(os.read_file(path)!)
}

// get returns the value for section/key, or none if absent.
pub fn (f &IniFile) get(section string, key string) ?string {
	if section !in f.sections {
		return none
	}
	if key !in f.sections[section] {
		return none
	}
	return f.sections[section][key]
}

// get_or returns the value for section/key, or default if absent.
pub fn (f &IniFile) get_or(section string, key string, default string) string {
	return f.get(section, key) or { default }
}

// set stores value under section/key, creating the section on first use.
pub fn (mut f IniFile) set(section string, key string, value string) {
	if section !in f.sections {
		f.sections[section] = map[string]string{}
		f.order << section
	}
	f.sections[section][key] = value
}

// sections returns section names in file order.
pub fn (f &IniFile) sections() []string {
	return f.order.clone()
}

// encode renders sections in insertion order and keys in sorted order.
// Names and values must use the single-line syntax accepted by parse_string;
// surrounding whitespace is trimmed when the encoded text is parsed again.
pub fn (f &IniFile) encode() string {
	mut out := []string{}
	for name in f.order {
		out << '[${name}]'
		// iterate keys in sorted order for deterministic output
		mut keys := f.sections[name].keys()
		keys.sort()
		for k in keys {
			out << '${k} = ${f.sections[name][k]}'
		}
		out << ''
	}
	return out.join('\n')
}

// separator_pos finds the first top-level `=` or `:` separator.
fn separator_pos(line string) ?int {
	for i, c in line.bytes() {
		if c == `=` || c == `:` {
			return i
		}
	}
	return none
}
