module builder

import os
import time
import v.ast
import v.table
import v.pref
import v.util
import v.vmod
import v.checker
import v.parser
import v.errors
import v.gen
import v.gen.js
import v.gen.x64
import v.depgraph

pub struct Builder {
pub:
	pref                &pref.Preferences
	table               &table.Table
	checker             checker.Checker
	compiled_dir        string // contains os.real_path() of the dir of the final file beeing compiled, or the dir itself when doing `v .`
	module_path         string
mut:
	module_search_paths []string
	parsed_files        []ast.File
	global_scope        &ast.Scope
	out_name_c          string
	out_name_js         string
}

pub fn new_builder(pref &pref.Preferences) Builder {
	rdir := os.real_path(pref.path)
	compiled_dir := if os.is_dir(rdir) { rdir } else { os.dir(rdir) }
	table := table.new_table()
	return Builder{
		pref: pref
		table: table
		checker: checker.new_checker(table, pref)
		global_scope: &ast.Scope{
			parent: 0
		}
		compiled_dir: compiled_dir
	}
}

// parse all deps from already parsed files
pub fn (mut b Builder) parse_imports() {
	mut done_imports := []string{}
	// NB: b.parsed_files is appended in the loop,
	// so we can not use the shorter `for in` form.
	for i := 0; i < b.parsed_files.len; i++ {
		ast_file := b.parsed_files[i]
		for _, imp in ast_file.imports {
			mod := imp.mod
			if mod in done_imports {
				continue
			}
			import_path := b.find_module_path(mod, ast_file.path) or {
				// v.parsers[i].error_with_token_index('cannot import module "$mod" (not found)', v.parsers[i].import_table.get_import_tok_idx(mod))
				// break
				// println('module_search_paths:')
				// println(b.module_search_paths)
				panic('cannot import module "$mod" (not found)')
			}
			v_files := b.v_files_from_dir(import_path)
			if v_files.len == 0 {
				// v.parsers[i].error_with_token_index('cannot import module "$mod" (no .v files in "$import_path")', v.parsers[i].import_table.get_import_tok_idx(mod))
				panic('cannot import module "$mod" (no .v files in "$import_path")')
			}
			// Add all imports referenced by these libs
			parsed_files := parser.parse_files(v_files, b.table, b.pref, b.global_scope)
			for file in parsed_files {
				if file.mod.name != mod {
					// v.parsers[pidx].error_with_token_index('bad module definition: ${v.parsers[pidx].file_path} imports module "$mod" but $file is defined as module `$p_mod`', 1
					verror('bad module definition: ${ast_file.path} imports module "$mod" but $file.path is defined as module `$file.mod.name`')
				}
			}
			b.parsed_files << parsed_files
			done_imports << mod
		}
	}
	b.resolve_deps()
}

pub fn (mut b Builder) resolve_deps() {
	graph := b.import_graph()
	deps_resolved := graph.resolve()
	if !deps_resolved.acyclic {
		eprintln('warning: import cycle detected between the following modules: \n' + deps_resolved.display_cycles())
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

pub fn (b Builder) v_files_from_dir(dir string) []string {
	mut res := []string{}
	if !os.exists(dir) {
		if dir == 'compiler' && os.is_dir('vlib') {
			println('looks like you are trying to build V with an old command')
			println('use `v -o v cmd/v` instead of `v -o v compiler`')
		}
		verror("$dir doesn't exist")
	} else if !os.is_dir(dir) {
		verror("$dir isn't a directory!")
	}
	mut files := os.ls(dir) or {
		panic(err)
	}
	if b.pref.is_verbose {
		println('v_files_from_dir ("$dir")')
	}
	files.sort()
	for file in files {
		if !file.ends_with('.v') && !file.ends_with('.vh') {
			continue
		}
		if file.ends_with('_test.v') {
			continue
		}
		if b.pref.backend == .c && !b.should_compile_c(file) {
			continue
		}
		if b.pref.backend == .js && !b.should_compile_js(file) {
			continue
		}
		if b.pref.compile_defines_all.len > 0 && file.contains('_d_') {
			mut allowed := false
			for cdefine in b.pref.compile_defines {
				file_postfix := '_d_${cdefine}.v'
				if file.ends_with(file_postfix) {
					allowed = true
					break
				}
			}
			if !allowed {
				continue
			}
		}
		res << os.join_path(dir, file)
	}
	return res
}

[inline]
fn (b Builder) should_compile_c(file string) bool {
	if !file.ends_with('.c.v') && file.split('.').len > 2 {
		// Probably something like `a.js.v`.
		return false
	}
	if file.ends_with('_windows.c.v') && b.pref.os != .windows {
		return false
	}
	if file.ends_with('_linux.c.v') && b.pref.os != .linux {
		return false
	}
	if file.ends_with('_darwin.c.v') && b.pref.os != .mac {
		return false
	}
	if file.ends_with('_nix.c.v') && b.pref.os == .windows {
		return false
	}
	if file.ends_with('_android.c.v') && b.pref.os != .android {
		return false
	}
	if file.ends_with('_freebsd.c.v') && b.pref.os != .freebsd {
		return false
	}
	if file.ends_with('_solaris.c.v') && b.pref.os != .solaris {
		return false
	}
	return true
}

[inline]
fn (b Builder) should_compile_js(file string) bool {
	if !file.ends_with('.js.v') && file.split('.').len > 2 {
		// Probably something like `a.c.v`.
		return false
	}
	return true
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

pub fn (b Builder) find_module_path(mod, fpath string) ?string {
	// support @VROOT/v.mod relative paths:
	vmod_file_location := vmod.mod_file_cacher.get(fpath)
	mod_path := module_path(mod)
	mut module_lookup_paths := []string{}
	if vmod_file_location.vmod_file.len != 0 && vmod_file_location.vmod_folder !in b.module_search_paths {
		module_lookup_paths << vmod_file_location.vmod_folder
	}
	module_lookup_paths << b.module_search_paths
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
	smodule_lookup_paths := module_lookup_paths.join(', ')
	return error('module "$mod" not found in:\n$smodule_lookup_paths')
}

fn (b &Builder) print_errors(errors []errors.Error) {
	for err in errors {
		kind := if b.pref.is_verbose { '$err.reporter error #$b.checker.nr_errors:' } else { 'error:' }
		ferror := util.formatted_error(kind, err.message, err.file_path, err.pos)
		eprintln(ferror)
	}
}

fn verror(s string) {
	util.verror('builder error', s)
}
