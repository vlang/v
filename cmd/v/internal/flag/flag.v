// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This module is designed to be general purpose. The only reason it currently only lives here is because there's no
// generics support and all types are defined manually.
// TODO Move to vlib once generics are implemented properly.
module flag

const (
	// List as taken from `pkg.go.dev/flag`
	truthy = ['1', 't', 'T', 'true', 'TRUE', 'True']
	falsey = ['0', 'f', 'F', 'false', 'FALSE', 'False']
)

pub struct Instance {
	args        []string
mut:
	current_flag string
	current_pos  int
	equal_val    string
	encountered  map[string]bool
}

fn (p mut Instance) parse_impl(args []string, value voidptr, callback void_cb) ?[]string {
	for p.current_pos+1 < p.args.len {
		p.current_pos++
		next := p.args[p.current_pos]
		if !next.starts_with('-') || next == '-' {
			// End of arguments.
			// Note: - by itself is not considered a flag.
			return args[p.current_pos..]
		}
		if next == '--' {
			// Terminator. End of arguments.
			return args[p.current_pos+1..]
		}
		// Start parsing flag by determining flag name
		mut flag_name := ''
		if idx := next.index('=') {
			p.equal_val = next[idx+1..]
			flag_name = next[1..idx]
		} else {
			p.equal_val = ''
			flag_name = next[1..]
		}
		if p.encountered[flag_name] {
			// Duplicate flags are opt-in
			// This can be prevented with p.allow_duplicate()
			return error('Duplicate flag: -$flag_name')
		}
		p.encountered[flag_name] = true
		p.current_flag = flag_name
		callback(flag_name, p, value)
	}
	// We didn't hit any exit condition. There's no argument left so nothing.
	return []string
}

pub fn (p mut Instance) string() ?string {
	if p.equal_val != '' {
		return p.equal_val
	}
	p.current_pos++
	if p.current_pos >= p.args.len {
		return error('out of arguments')
	}
	return p.args[p.current_pos]
}

pub fn (p mut Instance) int() ?int {
	val := p.string() or {
		return error(err)
	}
	return val.int()
}

pub fn (p mut Instance) f32() ?f32 {
	val := p.string() or {
		return error(err)
	}
	return val.f32()
}

pub fn (p mut Instance) f64() ?f64 {
	val := p.string() or {
		return error(err)
	}
	return val.f64()
}

pub fn (p mut Instance) i64() ?i64 {
	val := p.string() or {
		return error(err)
	}
	return val.i64()
}

pub fn (p mut Instance) bool() bool {
	val := p.string() or {
		// Could not fetch arguments? Parse it as `true`.
		return true
	}
	if val in truthy {
		return true
	}
	if val in falsey {
		return false
	}
	// Unrecognized boolean type. Probably is not related to boolean.
	p.current_pos--
	return true
}

pub fn (p mut Instance) allow_duplicate() {
	p.encountered[p.current_flag] = false
}

pub fn (p mut Instance) is_equivalent_to(flags []string) {
	for v in flags {
		p.encountered[v] = true
	}
}
