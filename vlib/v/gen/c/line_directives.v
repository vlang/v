module c

import os
import v.flat
import v.token
import v.types

// generated_code_file is the file name of the C code that the compiler generates without a
// V source position, like thread wrappers or `str()` methods, once `#line` directives are on.
const generated_code_file = '<generated>'

// line_directive_scan_limit bounds how many nodes of a statement are searched for its start.
const line_directive_scan_limit = 32

// set_race generates C for a race build (`-race`). The generated C maps every V statement
// back to its source line with `#line` directives, so the C debug info carries the V
// file:line positions that the race detector shows in its stack traces, and the few V
// constructs that read memory without the C compiler loading it do load it.
pub fn (mut g FlatGen) set_race(enabled bool) {
	g.race = enabled
	g.line_directives = enabled || g.vlines
}

// set_vlines maps generated C back to V sources for `-g`, while `-cg` keeps C positions.
// Race builds always need V positions, regardless of the selected debug flag.
pub fn (mut g FlatGen) set_vlines(enabled bool) {
	g.vlines = enabled
	g.line_directives = enabled || g.race
}

// write_fn_line_directive points the C declaration of a V function to its source line.
fn (mut g FlatGen) write_fn_line_directive(node flat.Node) {
	if !g.line_directives {
		return
	}
	g.line_directive_fn_start = if node.pos.is_valid() { int(node.pos.offset) } else { 0 }
	g.write_line_directive(node)
}

// write_line_directive points the C line of the statement or function `node` to its V
// source line. Type declarations inside a function body generate no code there.
fn (mut g FlatGen) write_line_directive(node flat.Node) {
	if !g.line_directives || g.cur_fn_name.len == 0
		|| node.kind in [.struct_decl, .type_decl, .enum_decl, .interface_decl, .c_fn_decl] {
		return
	}
	position := g.a.source_position(g.statement_start_pos(node)) or { return }
	mut path := g.line_directive_paths[position.filename] or { '' }
	if path.len == 0 {
		path = c_escape(os.real_path(position.filename).replace('\\', '/'))
		g.line_directive_paths[position.filename] = path
	}
	g.write_line_directive_text('#line ${position.line} "${path}"')
}

// statement_start_pos returns the position of the first token of the statement `node`. The
// parser positions statement nodes like `x++`, `f()`, `x := ...` or `for ... {}` where it
// finished parsing them, often on a later line, while their children keep the positions of
// their own tokens. So the start is the earliest position among the first nodes of the
// statement that lie between the start of the current function and the statement's end.
fn (g &FlatGen) statement_start_pos(node flat.Node) token.Pos {
	if !node.pos.is_valid() {
		return node.pos
	}
	mut start := node.pos
	mut pending := []flat.NodeId{cap: line_directive_scan_limit}
	for i := int(node.children_count) - 1; i >= 0; i-- {
		pending << g.a.child(&node, i)
	}
	mut scanned := 0
	for pending.len > 0 && scanned < line_directive_scan_limit {
		id := pending.pop()
		if int(id) < 0 || int(id) >= g.a.nodes.len {
			continue
		}
		child := g.a.nodes[int(id)]
		scanned++
		if child.pos.id == node.pos.id && child.pos.offset < start.offset
			&& int(child.pos.offset) >= g.line_directive_fn_start {
			start = child.pos
		}
		for i := int(child.children_count) - 1; i >= 0; i-- {
			pending << g.a.child(&child, i)
		}
	}
	return start
}

// end_fn_line_directives attributes the C code that follows a V function to the compiler
// instead of letting it continue the line numbers of that function's last statement.
fn (mut g FlatGen) end_fn_line_directives() {
	if g.line_directives {
		g.write_line_directive_text('#line 1 "${generated_code_file}"')
	}
}

// write_line_directive_text writes a preprocessor directive on a line of its own.
fn (mut g FlatGen) write_line_directive_text(directive string) {
	if g.sb.len > 0 && g.sb[g.sb.len - 1] != `\n` {
		g.sb.write_string('\n')
	}
	g.sb.write_string(directive)
	g.sb.write_string('\n')
	g.line_start = true
}

// gen_race_blank_read generates `_ = expr` in a race build. Like Go, V reads the value of
// `expr`, but the C compiler does not load a struct for `(void)(expr)`, and drops a value
// that is computed without side effects, like `c.n + 1`: a race on what `expr` reads would
// go unreported. Copying the value to a volatile variable keeps it, and its loads.
fn (mut g FlatGen) gen_race_blank_read(rhs_id flat.NodeId) bool {
	if !g.race {
		return false
	}
	rhs := g.a.nodes[int(rhs_id)]
	mut typ := g.usable_expr_type(rhs_id)
	// The value of a `mut` parameter, also in parentheses, is behind the pointer that C passes.
	mut inner := rhs
	for inner.kind == .paren && inner.children_count > 0 {
		inner = g.a.nodes[int(g.a.child(&inner, 0))]
	}
	mut deref := ''
	if inner.kind == .ident && g.current_param_is_mut(inner.value) {
		if param_type := g.current_param_type(inner.value) {
			if param_type is types.Pointer {
				typ = param_type.base_type
				deref = '*'
			}
		}
	}
	if fixed := array_fixed_type(typ) {
		// A C array cannot be copied, and `(void)(array)` only takes its address: tell the
		// race detector that all its elements are read. The size comes from the V type,
		// as a fixed array parameter is a pointer in C.
		elem_c_type, dims := g.fixed_array_decl_parts(fixed)
		if inner.kind in [.ident, .selector, .index, .prefix] {
			g.write('__tsan_read_range((void*)(${deref}')
			g.gen_expr(rhs_id)
			g.writeln('), sizeof(${elem_c_type}${dims}));')
			return true
		}
		// Any other array, like `[c.n, 0]!` or a call result, is a value that an optimized
		// build would drop with its loads: copy it into an array that the race detector reads.
		g.write('{ ${elem_c_type} __race_blank_read${dims}; memmove(__race_blank_read, ')
		g.gen_fixed_array_copy_source(rhs_id, typ)
		g.writeln(', sizeof(__race_blank_read)); __tsan_read_range((void*)__race_blank_read, sizeof(__race_blank_read)); }')
		return true
	}
	if g.tc.c_type(typ) in ['', 'void'] {
		return false
	}
	// Race builds need clang or gcc, which both deduce the type with `__auto_type`. The copy
	// is volatile, as an optimized build (`-prod`) would drop an unused copy, and its load.
	g.write('{ volatile __auto_type __race_blank_read = ${deref}(')
	g.gen_expr(rhs_id)
	g.writeln('); (void)__race_blank_read; }')
	return true
}
