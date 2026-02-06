module beam

import v.ast
import v.pref
import strings
import os

pub struct CoreGen {
	table &ast.Table
	prefs &pref.Preferences
mut:
	out            strings.Builder
	indent         int
	cur_mod        string
	cur_file       string
	out_dir        string
	temp_counter   int
	fn_infos       []FnInfo // reuse FnInfo from beamgen.v
	cur_fn         &ast.FnDecl = unsafe { nil }
	var_map        map[string]string // V variable name -> Core Erlang variable name
	comptime_stack []ComptimeEnv
}

pub fn core_gen(files []&ast.File, mut table ast.Table, out_name string, prefs_ &pref.Preferences) {
	mut g := CoreGen{
		table: &table
		prefs: prefs_
		out: strings.new_builder(4096)
		out_dir: if out_name.len > 0 { out_name } else { 'beam_output' }
	}

	// Create output directory
	if !os.exists(g.out_dir) {
		os.mkdir_all(g.out_dir) or {}
	}

	// Group files by module name (same as Erlang backend)
	mut module_files := map[string][]&ast.File{}
	for file in files {
		mod_name := file.mod.name
		if mod_name in module_files {
			module_files[mod_name] << file
		} else {
			module_files[mod_name] = [file]
		}
	}

	// Generate one .core per module
	for mod_name, mod_files in module_files {
		g.core_gen_module(mod_name, mod_files)
	}
}

fn (mut g CoreGen) core_gen_module(v_mod string, files []&ast.File) {
	erl_mod := g.core_v_mod_to_erl_mod(v_mod)
	g.cur_mod = v_mod

	// Reset state
	g.out.clear()
	g.fn_infos.clear()
	g.var_map.clear()
	g.temp_counter = 0

	// First pass: collect function info from ALL files
	for file in files {
		g.cur_file = file.path
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				g.core_collect_fn_info(stmt)
			}
		}
	}

	// Generate module header
	g.core_module_header(erl_mod)

	// Second pass: generate code from ALL files
	for file in files {
		g.cur_file = file.path
		g.var_map.clear()
		g.temp_counter = 0
		for stmt in file.stmts {
			g.core_stmt(stmt)
		}
	}

	// Generate module_info boilerplate
	g.core_module_info(erl_mod)

	// Close module
	g.writeln_core('end')

	core_source := g.out.str()

	// Write to file
	core_filename := '${g.out_dir}/${erl_mod}.core'
	os.write_file(core_filename, core_source) or {
		eprintln('Error writing ${core_filename}: ${err}')
	}

	println('=== Generated Core Erlang: ${core_filename} ===')
	println(core_source)
}

fn (mut g CoreGen) core_collect_fn_info(node ast.FnDecl) {
	name := g.core_fn_name(node)
	arity := node.params.len
	is_pub := node.is_pub || node.short_name == 'main' || node.is_main || !node.is_anon
	g.fn_infos << FnInfo{
		name: name
		arity: arity
		is_pub: is_pub
	}
}

fn (mut g CoreGen) core_module_header(erl_mod string) {
	// Module declaration with exports
	mut exports := []string{}
	for info in g.fn_infos {
		if info.is_pub {
			exports << "'${info.name}'/${info.arity}"
		}
	}
	// Always export module_info
	exports << "'module_info'/0"
	exports << "'module_info'/1"

	g.write_core("module '${erl_mod}' [${exports.join(',\n                ')}]")
	g.out.writeln('')
	g.writeln_core('    attributes []')
}

fn (mut g CoreGen) core_module_info(erl_mod string) {
	// module_info/0
	g.writeln_core("'module_info'/0 =")
	g.writeln_core('    fun () ->')
	g.writeln_core("        call 'erlang':'get_module_info'")
	g.writeln_core("            ('${erl_mod}')")

	// module_info/1
	g.writeln_core("'module_info'/1 =")
	g.writeln_core('    fun (_0) ->')
	g.writeln_core("        call 'erlang':'get_module_info'")
	g.writeln_core("            ('${erl_mod}', _0)")
}

fn (mut g CoreGen) core_fn_name(node ast.FnDecl) string {
	if node.is_method {
		rec_type := node.receiver.typ
		type_sym := g.table.sym(rec_type)
		short_type := type_sym.name.all_after_last('.')
		return '${short_type}.${node.short_name}'
	}
	return node.short_name
}

fn (mut g CoreGen) core_v_mod_to_erl_mod(v_mod string) string {
	return 'v.${v_mod}'
}

// Output helpers
fn (mut g CoreGen) writeln_core(s string) {
	g.write_indent_core()
	g.out.writeln(s)
}

fn (mut g CoreGen) write_core(s string) {
	g.out.write_string(s)
}

fn (mut g CoreGen) write_indent_core() {
	for _ in 0 .. g.indent {
		g.out.write_string('    ')
	}
}
