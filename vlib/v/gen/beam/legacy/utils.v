module beam

import strings

fn (mut g Gen) next_var(name string) string {
	if name in g.var_versions {
		g.var_versions[name]++
		ver := g.var_versions[name]
		return g.erl_var_versioned(name, ver)
	} else {
		g.var_versions[name] = 0
		// First assignment uses plain name (no version suffix)
		return g.erl_var(name)
	}
}

// cur_var returns the current version of a variable without incrementing
fn (mut g Gen) cur_var(name string) string {
	if name in g.var_versions {
		ver := g.var_versions[name]
		if ver == 0 {
			return g.erl_var(name)
		}
		return g.erl_var_versioned(name, ver)
	}
	// Variable not yet declared - return plain name
	return g.erl_var(name)
}

fn (mut g Gen) erl_var(name string) string {
	// Erlang variables must start with uppercase
	return name.capitalize()
}

fn (mut g Gen) erl_var_versioned(name string, version int) string {
	// SSA versioned variable: X1, X2, etc. (version 0 uses plain name)
	base := name.capitalize()
	return '${base}${version}'
}

fn (mut g Gen) indent_str() string {
	mut s := strings.new_builder(g.indent * 4)
	for _ in 0 .. g.indent {
		s.write_string('    ')
	}
	return s.str()
}

fn escape_erlang_string(s string) string {
	mut result := strings.new_builder(s.len)
	for c in s {
		match c {
			`\\` { result.write_string('\\\\') }
			`"` { result.write_string('\\"') }
			`\n` { result.write_string('\\n') }
			`\r` { result.write_string('\\r') }
			`\t` { result.write_string('\\t') }
			else { result.write_u8(c) }
		}
	}
	return result.str()
}

fn escape_erlang_atom(s string) string {
	// Atoms with special chars need quoting
	// Check if atom needs quoting
	if s.len == 0 {
		return "''"
	}

	first := s[0]
	// Must start with lowercase letter for unquoted atoms
	if first < `a` || first > `z` {
		return "'${s}'"
	}

	// Check for special characters
	for c in s {
		if !((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` || c == `@`) {
			return "'${s}'"
		}
	}
	return s
}
