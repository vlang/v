module beam

import strings
import v.token

// core_charlist converts a V string to Core Erlang char list representation
// "Hello" -> [72|[101|[108|[108|[111|[]]]]]]
fn core_charlist(s string) string {
	if s.len == 0 {
		return '[]'
	}
	mut result := strings.new_builder(s.len * 4)
	mut rune_count := 0
	for i, c in s {
		_ = i
		result.write_string('[${int(c)}|')
		rune_count++
	}
	result.write_string('[]')
	for _ in 0 .. rune_count {
		result.write_u8(`]`)
	}
	return result.str()
}

// core_bitstring converts a V string to Core Erlang bitstring literal
// "Hi" -> #{#<72>(8,1,'integer',['unsigned'|['big']]),#<105>(8,1,'integer',['unsigned'|['big']])}#
fn core_bitstring(s string) string {
	if s.len == 0 {
		return '#{  }#'
	}
	mut result := strings.new_builder(s.len * 50)
	result.write_string('#{')
	for i, c in s {
		if i > 0 {
			result.write_string(',')
		}
		result.write_string("#<${int(c)}>(8,1,'integer',['unsigned'|['big']])")
	}
	result.write_string('}#')
	return result.str()
}

// core_op maps a V operator token to the Erlang BIF atom name
fn core_op(op token.Kind) string {
	return match op {
		.plus { '+' }
		.minus { '-' }
		.mul { '*' }
		.div { '/' }
		.mod { 'rem' }
		.eq { '=:=' }
		.ne { '=/=' }
		.lt { '<' }
		.gt { '>' }
		.le { '=<' }
		.ge { '>=' }
		.and { 'and' }
		.logical_or { 'or' }
		.amp { 'band' }
		.pipe { 'bor' }
		.xor { 'bxor' }
		.left_shift { 'bsl' }
		.right_shift { 'bsr' }
		else { op.str() }
	}
}

// temp var generation for CoreGen
fn (mut g CoreGen) new_temp() string {
	g.temp_counter++
	return '_${g.temp_counter}'
}

// core_var converts a V variable name to Core Erlang variable name
// Core Erlang variables are capitalized like Erlang
fn (mut g CoreGen) core_var(name string) string {
	// Check var_map for existing mapping
	if mapped := g.var_map[name] {
		return mapped
	}
	// Create new mapping
	cname := name.capitalize()
	g.var_map[name] = cname
	return cname
}

// next_core_var creates a new unique variable name for SSA
fn (mut g CoreGen) next_core_var(name string) string {
	// Each assignment creates a new unique name
	cname := name.capitalize()
	if name in g.var_map {
		// Already exists - create versioned name
		g.temp_counter++
		versioned := '${cname}_${g.temp_counter}'
		g.var_map[name] = versioned
		return versioned
	}
	g.var_map[name] = cname
	return cname
}

// core_escape_charlist escapes a string for use as a charlist in ETF literal format.
// In ETF, a charlist is represented as a string literal (list of integers).
fn core_escape_charlist(s string) string {
	// In ETF term text, a charlist literal is just a double-quoted string
	mut result := strings.new_builder(s.len + 16)
	for c in s {
		match c {
			`\\` { result.write_string('\\\\') }
			`"` { result.write_string('\\"') }
			else { result.write_u8(c) }
		}
	}
	return result.str()
}

// core_escape_binary escapes a string for use in binary literal <<"...">> format.
// Escapes backslash, double-quote, and control characters.
fn core_escape_binary(s string) string {
	if s.len == 0 {
		return ''
	}
	mut result := strings.new_builder(s.len + 16)
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

// core_atom quotes an atom for Core Erlang (always quoted)
fn core_atom(s string) string {
	return "'${s}'"
}
