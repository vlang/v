module builder

import os
import v.ast
import v.token
import v.table
import v.pref
import v.util
import v.vmod
import v.checker
import v.parser
import v.depgraph

pub struct Builder {
pub:
	compiled_dir string // contains os.real_path() of the dir of the final file beeing compiled, or the dir itself when doing `v .`
	module_path  string
mut:
	pref          &pref.Preferences
	checker       checker.Checker
	global_scope  &ast.Scope
	out_name_c    string
	out_name_js   string
	max_nr_errors int = 100
pub mut:
	module_search_paths []string
	parsed_files        []ast.File
	cached_msvc         MsvcResult
	table               &table.Table
	ccoptions           CcompilerOptions
}

pub fn new_builder(pref &pref.Preferences) Builder {
	rdir := os.real_path(pref.path)
	compiled_dir := if os.is_dir(rdir) { rdir } else { os.dir(rdir) }
	mut table := table.new_table()
	table.is_fmt = false
	if pref.use_color == .always {
		util.emanager.set_support_color(true)
	}
	if pref.use_color == .never {
		util.emanager.set_support_color(false)
	}
	msvc := find_msvc(pref.m64) or {
		if pref.ccompiler == 'msvc' {
			// verror('Cannot find MSVC on this OS')
		}
		MsvcResult{
			valid: false
		}
	}
	util.timing_set_should_print(pref.show_timings || pref.is_verbose)
	return Builder{
		pref: pref
		table: table
		checker: checker.new_checker(table, pref)
		global_scope: &ast.Scope{
			parent: 0
		}
		compiled_dir: compiled_dir
		max_nr_errors: if pref.error_limit > 0 { pref.error_limit } else { 100 }
		cached_msvc: msvc
	}
	// max_nr_errors: pref.error_limit ?? 100 TODO potential syntax?
}

// parse all deps from already parsed files
pub fn (mut b Builder) parse_imports() {
	mut done_imports := []string{}
	if b.pref.is_vsh {
		done_imports << 'os'
	}
	// TODO (joe): decide if this is correct solution.
	// in the case of building a module, the actual module files
	// are passed via cmd line, so they have already been parsed
	// by this stage. note that if one files from a module was
	// parsed (but not all of them), then this will cause a problem.
	// we could add a list of parsed files instead, but I think
	// there is a better solution all around, I will revisit this.
	// NOTE: there is a very similar occurance with the way
	// internal module test's work, and this was the reason there
	// were issues with duplicate declarations, so we should sort
	// that out in a similar way.
	for file in b.parsed_files {
		if file.mod.name != 'main' && file.mod.name !in done_imports {
			done_imports << file.mod.name
		}
	}
	// NB: b.parsed_files is appended in the loop,
	// so we can not use the shorter `for in` form.
	for i := 0; i < b.parsed_files.len; i++ {
		ast_file := b.parsed_files[i]
		for imp in ast_file.imports {
			mod := imp.mod
			if mod == 'builtin' {
				error_with_pos('cannot import module "builtin"', ast_file.path, imp.pos)
				break
			}
			if mod in done_imports {
				continue
			}
			import_path := b.find_module_path(mod, ast_file.path) or {
				// v.parsers[i].error_with_token_index('cannot import module "$mod" (not found)', v.parsers[i].import_table.get_import_tok_idx(mod))
				// break
				error_with_pos('cannot import module "$mod" (not found)', ast_file.path,
					imp.pos)
				break
			}
			v_files := b.v_files_from_dir(import_path)
			if v_files.len == 0 {
				// v.parsers[i].error_with_token_index('cannot import module "$mod" (no .v files in "$import_path")', v.parsers[i].import_table.get_import_tok_idx(mod))
				error_with_pos('cannot import module "$mod" (no .v files in "$import_path")',
					ast_file.path, imp.pos)
			}
			// Add all imports referenced by these libs
			parsed_files := parser.parse_files(v_files, b.table, b.pref, b.global_scope)
			for file in parsed_files {
				if file.mod.name != mod {
					// v.parsers[pidx].error_with_token_index('bad module definition: ${v.parsers[pidx].file_path} imports module "$mod" but $file is defined as module `$p_mod`', 1
					error_with_pos('bad module definition: $ast_file.path imports module "$mod" but $file.path is defined as module `$file.mod.name`',
						ast_file.path, imp.pos)
				}
			}
			b.parsed_files << parsed_files
			done_imports << mod
		}
	}
	b.resolve_deps()
	//
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
	if b.pref.is_verbose {
		eprintln('------ resolved dependencies graph: ------')
		eprintln(deps_resolved.display())
		eprintln('------------------------------------------')
	}
	if cycles.len > 1 {
		verror('error: import cycle detected between the following modules: \n' + cycles)
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
	b.table.modules = mods
	b.parsed_files = reordered_parsed_files
}

// graph of all imported modules
pub fn (b &Builder) import_graph() &depgraph.DepGraph {
	builtins := util.builtin_module_parts.clone()
	mut graph := depgraph.new_dep_graph()
	for p in b.parsed_files {
		mut deps := []string{}
		if p.mod.name !in builtins {
			deps << 'builtin'
			if b.pref.backend == .c {
				// TODO JavaScript backend doesn't handle os for now
				if b.pref.is_vsh && p.mod.name != 'os' {
					deps << 'os'
				}
			}
		}
		for m in p.imports {
			if m.mod == p.mod.name {
				continue
			}
			deps << m.mod
		}
		graph.add(p.mod.name, deps)
	}
	return graph
}

pub fn (b Builder) v_files_from_dir(dir string) []string {
	if !os.exists(dir) {
		if dir == 'compiler' && os.is_dir('vlib') {
			println('looks like you are trying to build V with an old command')
			println('use `v -o v cmd/v` instead of `v -o v compiler`')
		}
		verror("$dir doesn't exist")
	} else if !os.is_dir(dir) {
		verror("$dir isn't a directory!")
	}
	mut files := os.ls(dir) or { panic(err) }
	if b.pref.is_verbose {
		println('v_files_from_dir ("$dir")')
	}
	return b.pref.should_compile_filtered_files(dir, files)
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

[inline]
fn module_path(mod string) string {
	// submodule support
	return mod.replace('.', os.path_separator)
}

// TODO: try to merge this & util.module functions to create a
// reliable multi use function. see comments in util/module.v
pub fn (b &Builder) find_module_path(mod string, fpath string) ?string {
	// support @VROOT/v.mod relative paths:
	mut mcache := vmod.get_cache()
	vmod_file_location := mcache.get_by_file(fpath)
	mod_path := module_path(mod)
	mut module_lookup_paths := []string{}
	if vmod_file_location.vmod_file.len != 0
		&& vmod_file_location.vmod_folder !in b.module_search_paths {
		module_lookup_paths << vmod_file_location.vmod_folder
	}
	module_lookup_paths << b.module_search_paths
	module_lookup_paths << os.getwd()
	// go up through parents looking for modules a folder.
	// we need a proper solution that works most of the time. look at vdoc.get_parent_mod
	if fpath.contains(os.path_separator + 'modules' + os.path_separator) {
		parts := fpath.split(os.path_separator)
		for i := parts.len - 2; i >= 0; i-- {
			if parts[i] == 'modules' {
				module_lookup_paths << parts[0..i + 1].join(os.path_separator)
				break
			}
		}
	}
	for search_path in module_lookup_paths {
		try_path := os.join_path(search_path, mod_path)
		if b.pref.is_verbose {
			println('  >> trying to find $mod in $try_path ..')
		}
		if os.is_dir(try_path) {
			if b.pref.is_verbose {
				println('  << found $try_path .')
			}
			return try_path
		}
	}
	// look up through parents
	path_parts := fpath.split(os.path_separator)
	for i := path_parts.len - 2; i > 0; i-- {
		p1 := path_parts[0..i].join(os.path_separator)
		try_path := os.join_path(p1, mod_path)
		if b.pref.is_verbose {
			println('  >> trying to find $mod in $try_path ..')
		}
		if os.is_dir(try_path) {
			return try_path
		}
	}
	smodule_lookup_paths := module_lookup_paths.join(', ')
	return error('module "$mod" not found in:\n$smodule_lookup_paths')
}

fn (b &Builder) show_total_warns_and_errors_stats() {
	if b.pref.is_stats {
		estring := util.bold(b.checker.nr_errors.str())
		wstring := util.bold(b.checker.nr_warnings.str())
		println('checker summary: $estring V errors, $wstring V warnings')
	}
}

fn (b &Builder) print_warnings_and_errors() {
	defer {
		b.show_total_warns_and_errors_stats()
	}
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
			kind := if b.pref.is_verbose {
				'$err.reporter warning #$b.checker.nr_warnings:'
			} else {
				'warning:'
			}
			ferror := util.formatted_error(kind, err.message, err.file_path, err.pos)
			eprintln(ferror)
			if err.details.len > 0 {
				eprintln('Details: $err.details')
			}
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
			kind := if b.pref.is_verbose {
				'$err.reporter error #$b.checker.nr_errors:'
			} else {
				'error:'
			}
			ferror := util.formatted_error(kind, err.message, err.file_path, err.pos)
			eprintln(ferror)
			if err.details.len > 0 {
				eprintln('Details: $err.details')
			}
			// eprintln('')
			if i > b.max_nr_errors {
				return
			}
		}
		b.show_total_warns_and_errors_stats()
		exit(1)
	}
	if b.table.redefined_fns.len > 0 {
		mut total_conflicts := 0
		for fn_name in b.table.redefined_fns {
			// Find where this function was already declared
			mut redefines := []FunctionRedefinition{}
			mut redefine_conflicts := map[string]int{}
			for file in b.parsed_files {
				for stmt in file.stmts {
					if stmt is ast.FnDecl {
						if stmt.name == fn_name {
							fheader := stmt.stringify(b.table, 'main', map[string]string{})
							redefines << FunctionRedefinition{
								fpath: file.path
								fline: stmt.pos.line_nr
								f: stmt
								fheader: fheader
							}
							redefine_conflicts[fheader]++
						}
					}
				}
			}
			if redefines.len > 0 {
				eprintln('redefinition of function `$fn_name`')
				for redefine in redefines {
					eprintln(util.formatted_error('conflicting declaration:', redefine.fheader,
						redefine.fpath, redefine.f.pos))
				}
				total_conflicts++
			}
		}
		if total_conflicts > 0 {
			b.show_total_warns_and_errors_stats()
			exit(1)
		}
	}
}

struct FunctionRedefinition {
	fpath   string
	fline   int
	fheader string
	f       ast.FnDecl
}

fn error_with_pos(s string, fpath string, pos token.Position) {
	ferror := util.formatted_error('builder error:', s, fpath, pos)
	eprintln(ferror)
	exit(1)
}

fn verror(s string) {
	util.verror('builder error', s)
}
