module beam

import v.ast
import v.pref
import strings
import os

// FnInfo tracks function declarations for exports
struct FnInfo {
	name   string
	arity  int
	is_pub bool
}

pub struct Gen {
	table &ast.Table
	prefs &pref.Preferences
mut:
	out          strings.Builder
	indent       int
	cur_mod      string
	cur_file     string
	out_dir      string
	// SSA variable versioning
	var_versions map[string]int
	// Track function declarations
	fn_infos     []FnInfo
	// Current function being generated
	cur_fn       &ast.FnDecl = unsafe { nil }
	// Track if we're in the last statement of a function body
	is_last_stmt bool
	// Compile-time reflection environment stack
	comptime_stack []ComptimeEnv
}

pub fn gen(files []&ast.File, mut table ast.Table, out_name string, prefs_ &pref.Preferences) {
	mut g := Gen{
		table: &table
		prefs: prefs_
		out: strings.new_builder(4096)
		out_dir: if out_name.len > 0 { out_name } else { 'beam_output' }
	}

	// Create output directory
	if !os.exists(g.out_dir) {
		os.mkdir_all(g.out_dir) or {}
	}

	for file in files {
		g.gen_file(file)
	}
}

fn (mut g Gen) gen_file(file &ast.File) {
	mod_name := g.v_mod_to_erl_mod(file.mod.name)
	g.cur_mod = file.mod.name
	g.cur_file = file.path

	// Reset state for new file
	g.out.clear()
	g.fn_infos.clear()
	g.var_versions.clear()

	// First pass: collect function info
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			g.collect_fn_info(stmt)
		}
	}

	// Generate module header
	g.writeln("-module('${mod_name}').")
	g.gen_exports()

	// Second pass: generate code
	for stmt in file.stmts {
		g.stmt(stmt)
	}

	erl_source := g.out.str()

	// Write to file
	erl_filename := '${g.out_dir}/${mod_name}.erl'
	os.write_file(erl_filename, erl_source) or {
		eprintln('Error writing ${erl_filename}: ${err}')
	}

	println('=== Generated Erlang: ${erl_filename} ===')
	println(erl_source)
}

fn (mut g Gen) collect_fn_info(node ast.FnDecl) {
	name := g.fn_name(node)
	// Note: In V's AST, node.params already includes the receiver for methods
	arity := node.params.len
	// Export: main, public functions, and all module-level functions for now
	// Use short_name to check for 'main' since node.name includes module prefix
	// For simplicity, export all functions (Erlang needs explicit exports for inter-module calls)
	is_pub := node.is_pub || node.short_name == 'main' || node.is_main || !node.is_anon
	g.fn_infos << FnInfo{
		name: name
		arity: arity
		is_pub: is_pub
	}
}

fn (mut g Gen) gen_exports() {
	mut exports := []string{}
	for info in g.fn_infos {
		if info.is_pub {
			// Simple names don't need quoting, complex ones do
			if g.needs_atom_quote(info.name) {
				exports << "'${info.name}'/${info.arity}"
			} else {
				exports << '${info.name}/${info.arity}'
			}
		}
	}
	if exports.len > 0 {
		g.writeln('-export([${exports.join(', ')}]).')
	}
}

fn (g Gen) needs_atom_quote(name string) bool {
	// Check if the name needs quoting as an Erlang atom
	if name.len == 0 {
		return true
	}
	// Must start with lowercase letter for unquoted atoms
	first := name[0]
	if first < `a` || first > `z` {
		return true
	}
	// Check for special characters (like . in method names)
	for c in name {
		if !((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`) {
			return true
		}
	}
	return false
}

fn (mut g Gen) fn_name(node ast.FnDecl) string {
	if node.is_method {
		// Method: Type.method_name (use short type name)
		rec_type := node.receiver.typ
		type_sym := g.table.sym(rec_type)
		// Strip module prefix from type name
		short_type := type_sym.name.all_after_last('.')
		return '${short_type}.${node.short_name}'
	}
	// Use short_name - the name without module prefix
	return node.short_name
}

fn (mut g Gen) v_mod_to_erl_mod(v_mod string) string {
	return 'v.${v_mod}'
}

fn (mut g Gen) writeln(s string) {
	g.write_indent()
	g.out.writeln(s)
}

fn (mut g Gen) write(s string) {
	g.out.write_string(s)
}

fn (mut g Gen) write_indent() {
	for _ in 0 .. g.indent {
		g.out.write_string('    ')
	}
}
