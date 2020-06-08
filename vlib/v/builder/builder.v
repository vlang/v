module builder

import os
import v.ast
import v.table
import v.pref
import v.util
import v.checker
import v.depgraph

pub struct Builder {
pub:
	table               &table.Table
	compiled_dir        string // contains os.real_path() of the dir of the final file beeing compiled, or the dir itself when doing `v .`
	module_path         string
mut:
	checker             checker.Checker
	parsed_files        []ast.File
	global_scope        &ast.Scope
	out_name_c          string
	out_name_js         string
	max_nr_errors       int = 100
pub mut:
	pref                &pref.Preferences
}

pub fn new_builder(pref &pref.Preferences) Builder {
	rdir := os.real_path(pref.path)
	compiled_dir := if os.is_dir(rdir) { rdir } else { os.dir(rdir) }
	table := table.new_table()
	if pref.use_color == .always {
		util.emanager.set_support_color(true)
	}
	if pref.use_color == .never {
		util.emanager.set_support_color(false)
	}
	return Builder{
		pref: pref
		table: table
		checker: checker.new_checker(table, pref)
		global_scope: &ast.Scope{
			parent: 0
		}
		compiled_dir: compiled_dir
		max_nr_errors: if pref.error_limit > 0 {
			pref.error_limit
		} else {
			100
		}
	}
	// max_nr_errors: pref.error_limit ?? 100 TODO potential syntax?
}

// parse all deps from already parsed files
pub fn (mut b Builder) parse_imports() {
	b.resolve_deps()
	if b.pref.print_v_files {
		for p in b.parsed_files {
			println(p.path)
		}
		exit(0)
	}
}

pub fn (mut b Builder) resolve_deps() {
	graph := b.import_graph()
	deps_resolved := graph.resolve()
	cycles := deps_resolved.display_cycles()
	if cycles.len > 1 {
		eprintln('warning: import cycle detected between the following modules: \n' + cycles)
		// TODO: error, when v itself does not have v.table -> v.ast -> v.table cycles anymore
		return
	}
	if b.pref.is_verbose {
		eprintln('------ resolved dependencies graph: ------')
		eprintln(deps_resolved.display())
		eprintln('------------------------------------------')
	}
	mut mods := []string{}
	for node in deps_resolved.nodes {
		mods << node.name
	}
	if b.pref.is_verbose {
		eprintln('------ imported modules: ------')
		eprintln(mods.str())
		eprintln('-------------------------------')
	}
	mut reordered_parsed_files := []ast.File{}
	for m in mods {
		for pf in b.parsed_files {
			if m == pf.mod.name {
				reordered_parsed_files << pf
				// eprintln('pf.mod.name: $pf.mod.name | pf.path: $pf.path')
			}
		}
	}
	b.parsed_files = reordered_parsed_files
}

// graph of all imported modules
pub fn (b &Builder) import_graph() &depgraph.DepGraph {
	mut builtins := util.builtin_module_parts
	builtins << 'builtin'
	mut graph := depgraph.new_dep_graph()
	for p in b.parsed_files {
		mut deps := []string{}
		if p.mod.name !in builtins {
			deps << 'builtin'
		}
		for _, m in p.imports {
			deps << m.mod
		}
		graph.add(p.mod.name, deps)
	}
	return graph
}

pub fn (b Builder) log(s string) {
	if b.pref.is_verbose {
		println(s)
	}
}

pub fn (b Builder) info(s string) {
	if b.pref.is_verbose {
		println(s)
	}
}

fn (b &Builder) print_warnings_and_errors() {
	if b.pref.output_mode == .silent {
		if b.checker.nr_errors > 0 {
			exit(1)
		}
		return
	}
	if b.pref.is_verbose && b.checker.nr_warnings > 1 {
		println('$b.checker.nr_warnings warnings')
	}
	if b.checker.nr_warnings > 0 && !b.pref.skip_warnings {
		for i, err in b.checker.warnings {
			kind := if b.pref.is_verbose { '$err.reporter warning #$b.checker.nr_warnings:' } else { 'warning:' }
			ferror := util.formatted_error(kind, err.message, err.file_path, err.pos)
			eprintln(ferror)
			// eprintln('')
			if i > b.max_nr_errors {
				return
			}
		}
	}
	//
	if b.pref.is_verbose && b.checker.nr_errors > 1 {
		println('$b.checker.nr_errors errors')
	}
	if b.checker.nr_errors > 0 {
		for i, err in b.checker.errors {
			kind := if b.pref.is_verbose { '$err.reporter error #$b.checker.nr_errors:' } else { 'error:' }
			ferror := util.formatted_error(kind, err.message, err.file_path, err.pos)
			eprintln(ferror)
			// eprintln('')
			if i > b.max_nr_errors {
				return
			}
		}
		exit(1)
	}
	if b.table.redefined_fns.len > 0 {
		for fn_name in b.table.redefined_fns {
			eprintln('redefinition of function `$fn_name`')
			// eprintln('previous declaration at')
			// Find where this function was already declared
			for file in b.parsed_files {
				for stmt in file.stmts {
					if stmt is ast.FnDecl {
						f := stmt as ast.FnDecl
						if f.name == fn_name {
							fline := f.pos.line_nr
							println('${file.path}:${fline}:')
						}
					}
				}
			}
			exit(1)
		}
	}
}

fn verror(s string) {
	util.verror('builder error', s)
}
