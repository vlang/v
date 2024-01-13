module builder

import os
import v.token
import v.pref
import v.errors
import v.util
import v.ast
import v.vmod
import v.checker
import v.transformer
import v.parser
import v.markused
import v.depgraph
import v.callgraph
import v.dotgraph

pub struct Builder {
pub:
	compiled_dir string // contains os.real_path() of the dir of the final file being compiled, or the dir itself when doing `v .`
	module_path  string
pub mut:
	checker             &checker.Checker = unsafe { nil }
	transformer         &transformer.Transformer = unsafe { nil }
	out_name_c          string
	out_name_js         string
	stats_lines         int // size of backend generated source code in lines
	stats_bytes         int // size of backend generated source code in bytes
	nr_errors           int // accumulated error count of scanner, parser, checker, and builder
	nr_warnings         int // accumulated warning count of scanner, parser, checker, and builder
	nr_notices          int // accumulated notice count of scanner, parser, checker, and builder
	pref                &pref.Preferences = unsafe { nil }
	module_search_paths []string
	parsed_files        []&ast.File
	//$if windows {
	cached_msvc MsvcResult
	//}
	table     &ast.Table = unsafe { nil }
	ccoptions CcompilerOptions
	//
	// Note: changes in mod `builtin` force invalidation of every other .v file
	mod_invalidates_paths map[string][]string // changes in mod `os`, invalidate only .v files, that do `import os`
	mod_invalidates_mods  map[string][]string // changes in mod `os`, force invalidation of mods, that do `import os`
	path_invalidates_mods map[string][]string // changes in a .v file from `os`, invalidates `os`
	crun_cache_keys       []string // target executable + top level source files; filled in by Builder.should_rebuild
	executable_exists     bool     // if the executable already exists, don't remove new executable after `v run`
}

pub fn new_builder(pref_ &pref.Preferences) Builder {
	rdir := os.real_path(pref_.path)
	compiled_dir := if os.is_dir(rdir) { rdir } else { os.dir(rdir) }
	mut table := ast.new_table()
	table.is_fmt = false
	if pref_.use_color == .always {
		util.emanager.set_support_color(true)
	}
	if pref_.use_color == .never {
		util.emanager.set_support_color(false)
	}
	table.pointer_size = if pref_.m64 { 8 } else { 4 }
	mut msvc := MsvcResult{}
	$if windows {
		msvc = find_msvc(pref_.m64) or {
			if pref_.ccompiler == 'msvc' {
				// verror('cannot find MSVC on this OS')
			}
			MsvcResult{
				valid: false
			}
		}
	}
	util.timing_set_should_print(pref_.show_timings || pref_.is_verbose)
	if pref_.show_callgraph || pref_.show_depgraph {
		dotgraph.start_digraph()
	}
	mut executable_name := pref_.out_name
	$if windows {
		executable_name += '.exe'
	}
	return Builder{
		pref: pref_
		table: table
		checker: checker.new_checker(table, pref_)
		transformer: transformer.new_transformer_with_table(table, pref_)
		compiled_dir: compiled_dir
		cached_msvc: msvc
		executable_exists: os.is_file(executable_name)
	}
}

pub fn (mut b Builder) interpret_text(code string, v_files []string) ! {
	b.parsed_files = parser.parse_files(v_files, b.table, b.pref)
	b.parsed_files << parser.parse_text(code, '', b.table, .skip_comments, b.pref)
	b.parse_imports()

	if b.pref.only_check_syntax {
		return error_with_code('stop_after_parser', 7001)
	}

	b.middle_stages()!
}

pub fn (mut b Builder) front_stages(v_files []string) ! {
	mut timers := util.get_timers()
	util.timing_start('ALL_FRONT_STAGES')
	defer {
		timers.show('ALL_FRONT_STAGES')
	}
	util.timing_start('PARSE')

	util.timing_start('Builder.front_stages.parse_files')
	b.parsed_files = parser.parse_files(v_files, b.table, b.pref)
	timers.show('Builder.front_stages.parse_files')

	b.parse_imports()

	timers.show('SCAN')
	timers.show('PARSE')
	timers.show_if_exists('PARSE stmt')
	if b.pref.only_check_syntax {
		return error_with_code('stop_after_parser', 7001)
	}
}

pub fn (mut b Builder) middle_stages() ! {
	util.timing_start('CHECK')

	util.timing_start('Checker.generic_insts_to_concrete')
	b.table.generic_insts_to_concrete()
	util.timing_measure('Checker.generic_insts_to_concrete')

	b.checker.check_files(b.parsed_files)
	util.timing_measure('CHECK')
	b.print_warnings_and_errors()
	if b.checker.should_abort {
		return error('too many errors/warnings/notices')
	}
	if b.pref.check_only {
		return error_with_code('stop_after_checker', 8001)
	}
	util.timing_start('TRANSFORM')
	b.transformer.transform_files(b.parsed_files)
	util.timing_measure('TRANSFORM')
	//
	b.table.complete_interface_check()
	if b.pref.skip_unused {
		markused.mark_used(mut b.table, b.pref, b.parsed_files)
	}
	if b.pref.show_callgraph {
		callgraph.show(mut b.table, b.pref, b.parsed_files)
	}
	if b.pref.dump_defines != '' {
		b.dump_defines()
	}
}

pub fn (mut b Builder) front_and_middle_stages(v_files []string) ! {
	b.front_stages(v_files)!
	b.middle_stages()!
}

// parse all deps from already parsed files
pub fn (mut b Builder) parse_imports() {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
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
	// NOTE: there is a very similar occurrence with the way
	// internal module test's work, and this was the reason there
	// were issues with duplicate declarations, so we should sort
	// that out in a similar way.
	for file in b.parsed_files {
		if file.mod.name != 'main' && file.mod.name !in done_imports {
			done_imports << file.mod.name
		}
	}
	// Note: b.parsed_files is appended in the loop,
	// so we can not use the shorter `for in` form.
	for i := 0; i < b.parsed_files.len; i++ {
		ast_file := b.parsed_files[i]
		b.path_invalidates_mods[ast_file.path] << ast_file.mod.name
		if ast_file.mod.name != 'builtin' {
			b.mod_invalidates_paths['builtin'] << ast_file.path
			b.mod_invalidates_mods['builtin'] << ast_file.mod.name
		}
		for imp in ast_file.imports {
			mod := imp.mod
			b.mod_invalidates_paths[mod] << ast_file.path
			b.mod_invalidates_mods[mod] << ast_file.mod.name
			if mod == 'builtin' {
				b.parsed_files[i].errors << b.error_with_pos('cannot import module "builtin"',
					ast_file.path, imp.pos)
				break
			}
			if mod in done_imports {
				continue
			}
			import_path := b.find_module_path(mod, ast_file.path) or {
				// v.parsers[i].error_with_token_index('cannot import module "$mod" (not found)', v.parsers[i].import_ast.get_import_tok_idx(mod))
				// break
				b.parsed_files[i].errors << b.error_with_pos('cannot import module "${mod}" (not found)',
					ast_file.path, imp.pos)
				break
			}
			v_files := b.v_files_from_dir(import_path)
			if v_files.len == 0 {
				// v.parsers[i].error_with_token_index('cannot import module "$mod" (no .v files in "$import_path")', v.parsers[i].import_ast.get_import_tok_idx(mod))
				b.parsed_files[i].errors << b.error_with_pos('cannot import module "${mod}" (no .v files in "${import_path}")',
					ast_file.path, imp.pos)
				continue
			}
			// eprintln('>> ast_file.path: $ast_file.path , done: $done_imports, `import $mod` => $v_files')
			// Add all imports referenced by these libs
			parsed_files := parser.parse_files(v_files, b.table, b.pref)
			for file in parsed_files {
				mut name := file.mod.name
				if name == '' {
					name = file.mod.short_name
				}
				sname := name.all_after_last('.')
				smod := mod.all_after_last('.')
				if sname != smod {
					msg := 'bad module definition: ${ast_file.path} imports module "${mod}" but ${file.path} is defined as module `${name}`'
					b.parsed_files[i].errors << b.error_with_pos(msg, ast_file.path, imp.pos)
				}
			}
			b.parsed_files << parsed_files
			done_imports << mod
		}
	}
	b.resolve_deps()
	if b.pref.print_v_files {
		for p in b.parsed_files {
			println(p.path)
		}
		exit(0)
	}
	if b.pref.print_watched_files {
		for p in b.parsed_files {
			println(p.path)
			for tp in p.template_paths {
				println(tp)
			}
		}
		exit(0)
	}
	if b.pref.dump_files != '' {
		b.dump_files(b.parsed_files.map(it.path))
	}
	b.rebuild_modules()
}

pub fn (mut b Builder) resolve_deps() {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	graph := b.import_graph()
	deps_resolved := graph.resolve()
	if b.pref.is_verbose {
		eprintln('------ resolved dependencies graph: ------')
		eprintln(deps_resolved.display())
		eprintln('------------------------------------------')
	}
	if b.pref.show_depgraph {
		depgraph.show(deps_resolved, b.pref.path)
	}
	cycles := deps_resolved.display_cycles()
	if cycles.len > 1 {
		verror('error: import cycle detected between the following modules: \n' + cycles)
	}
	mut mods := []string{}
	for node in deps_resolved.nodes {
		mods << node.name
	}
	b.dump_modules(mods)
	if b.pref.is_verbose {
		eprintln('------ imported modules: ------')
		eprintln(mods.str())
		eprintln('-------------------------------')
	}
	unsafe {
		mut reordered_parsed_files := []&ast.File{}
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
}

// graph of all imported modules
pub fn (b &Builder) import_graph() &depgraph.DepGraph {
	builtins := util.builtin_module_parts.clone()
	mut graph := depgraph.new_dep_graph()
	for p in b.parsed_files {
		// eprintln('p.path: $p.path')
		mut deps := []string{}
		if p.mod.name !in builtins {
			deps << 'builtin'
			if b.pref.backend == .c {
				// TODO JavaScript backend doesn't handle os for now
				// os import libraries so we exclude anything which could cause a loop
				// git grep import vlib/os | cut -f2 -d: | cut -f2 -d" " | sort -u
				// dl, os, os.cmdline, os.filelock, os.notify, strings, strings.textscanner, term.termios, time
				if b.pref.is_vsh
					&& p.mod.name !in ['os', 'dl', 'strings.textscanner', 'term.termios'] {
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
	$if trace_import_graph ? {
		eprintln(graph.display())
	}
	return graph
}

pub fn (b Builder) v_files_from_dir(dir string) []string {
	if !os.exists(dir) {
		if dir == 'compiler' && os.is_dir('vlib') {
			println('looks like you are trying to build V with an old command')
			println('use `v -o v cmd/v` instead of `v -o v compiler`')
		}
		verror("${dir} doesn't exist")
	} else if !os.is_dir(dir) {
		verror("${dir} isn't a directory!")
	}
	mut files := os.ls(dir) or { panic(err) }
	if b.pref.is_verbose {
		println('v_files_from_dir ("${dir}")')
	}
	res := b.pref.should_compile_filtered_files(dir, files)
	if res.len == 0 {
		// Perhaps the .v files are stored in /src/ ?
		src_path := os.join_path(dir, 'src')
		if os.is_dir(src_path) {
			if b.pref.is_verbose {
				println('v_files_from_dir ("${src_path}") (/src/)')
			}
			files = os.ls(src_path) or { panic(err) }
			return b.pref.should_compile_filtered_files(src_path, files)
		}
	}
	return res
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

@[inline]
pub fn module_path(mod string) string {
	// submodule support
	return mod.replace('.', os.path_separator)
}

// TODO: try to merge this & util.module functions to create a
// reliable multi use function. see comments in util/module.v
pub fn (b &Builder) find_module_path(mod string, fpath string) !string {
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
			println('  >> trying to find ${mod} in ${try_path} ..')
		}
		if os.is_dir(try_path) {
			if b.pref.is_verbose {
				println('  << found ${try_path} .')
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
			println('  >> trying to find ${mod} in ${try_path} ..')
		}
		if os.is_dir(try_path) {
			return try_path
		}
	}
	smodule_lookup_paths := module_lookup_paths.join(', ')
	return error('module "${mod}" not found in:\n${smodule_lookup_paths}')
}

pub fn (b &Builder) show_total_warns_and_errors_stats() {
	if b.nr_errors == 0 && b.nr_warnings == 0 && b.nr_notices == 0 {
		return
	}
	if b.pref.is_stats {
		mut nr_errors := b.checker.errors.len
		mut nr_warnings := b.checker.warnings.len
		mut nr_notices := b.checker.notices.len

		if b.pref.check_only {
			nr_errors = b.nr_errors
			nr_warnings = b.nr_warnings
			nr_notices = b.nr_notices
		}

		estring := util.bold(nr_errors.str())
		wstring := util.bold(nr_warnings.str())
		nstring := util.bold(nr_notices.str())

		if b.pref.check_only {
			println('summary: ${estring} V errors, ${wstring} V warnings, ${nstring} V notices')
		} else {
			println('checker summary: ${estring} V errors, ${wstring} V warnings, ${nstring} V notices')
		}
	}
	if b.checker.nr_errors > 0 && b.pref.path.ends_with('.v') && os.is_file(b.pref.path) {
		for err in b.checker.errors {
			if err.message.starts_with('unknown ') {
				// Sometimes users try to `v main.v`, when they have several .v files in their project.
				// Then, they encounter puzzling errors about missing or unknown types. In this case,
				// the intended command may have been `v .` instead, so just suggest that:
				old_cmd := util.bold('v ${b.pref.path}')
				new_cmd := util.bold('v ${os.dir(b.pref.path)}')
				eprintln(util.color('notice', 'If the code of your project is in multiple files, try with `${new_cmd}` instead of `${old_cmd}`'))
			}
		}
	}
}

pub fn (mut b Builder) print_warnings_and_errors() {
	defer {
		b.show_total_warns_and_errors_stats()
	}

	for file in b.parsed_files {
		b.nr_errors += file.errors.len
		b.nr_warnings += file.warnings.len
		b.nr_notices += file.notices.len
	}

	if b.pref.output_mode == .silent {
		if b.nr_errors > 0 {
			exit(1)
		}
		return
	}

	if b.pref.check_only {
		if !b.pref.skip_notes {
			for file in b.parsed_files {
				for err in file.notices {
					kind := if b.pref.is_verbose {
						'${err.reporter} notice #${b.nr_notices}:'
					} else {
						'notice:'
					}
					util.show_compiler_message(kind, err.CompilerMessage)
				}
			}
		}

		for file in b.parsed_files {
			for err in file.errors {
				kind := if b.pref.is_verbose {
					'${err.reporter} error #${b.nr_errors}:'
				} else {
					'error:'
				}
				util.show_compiler_message(kind, err.CompilerMessage)
			}
		}

		if !b.pref.skip_warnings {
			for file in b.parsed_files {
				for err in file.warnings {
					kind := if b.pref.is_verbose {
						'${err.reporter} warning #${b.nr_warnings}:'
					} else {
						'warning:'
					}
					util.show_compiler_message(kind, err.CompilerMessage)
				}
			}
		}

		b.show_total_warns_and_errors_stats()
		if b.nr_errors > 0 {
			exit(1)
		}
		exit(0)
	}

	if b.pref.is_verbose && b.checker.nr_warnings > 1 {
		println('${b.checker.nr_warnings} warnings')
	}
	if b.pref.is_verbose && b.checker.nr_notices > 1 {
		println('${b.checker.nr_notices} notices')
	}
	if b.checker.nr_notices > 0 && !b.pref.skip_notes {
		for err in b.checker.notices {
			kind := if b.pref.is_verbose {
				'${err.reporter} notice #${b.checker.nr_notices}:'
			} else {
				'notice:'
			}
			util.show_compiler_message(kind, err.CompilerMessage)
		}
	}
	if b.checker.nr_warnings > 0 && !b.pref.skip_warnings {
		for err in b.checker.warnings {
			kind := if b.pref.is_verbose {
				'${err.reporter} warning #${b.checker.nr_warnings}:'
			} else {
				'warning:'
			}
			util.show_compiler_message(kind, err.CompilerMessage)
		}
	}
	//
	if b.pref.is_verbose && b.checker.nr_errors > 1 {
		println('${b.checker.nr_errors} errors')
	}
	if b.checker.nr_errors > 0 {
		for err in b.checker.errors {
			kind := if b.pref.is_verbose {
				'${err.reporter} error #${b.checker.nr_errors}:'
			} else {
				'error:'
			}
			util.show_compiler_message(kind, err.CompilerMessage)
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
							fheader := b.table.stringify_fn_decl(&stmt, 'main', map[string]string{})
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
				util.show_compiler_message('builder error:',
					message: 'redefinition of function `${fn_name}`'
				)
				for redefine in redefines {
					util.show_compiler_message('conflicting declaration:',
						message: redefine.fheader
						file_path: redefine.fpath
						pos: redefine.f.pos
					)
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

pub fn (b &Builder) error_with_pos(s string, fpath string, pos token.Pos) errors.Error {
	if !b.pref.check_only {
		util.show_compiler_message('builder error:', pos: pos, file_path: fpath, message: s)
		exit(1)
	}

	return errors.Error{
		file_path: fpath
		pos: pos
		reporter: .builder
		message: s
	}
}

@[noreturn]
pub fn verror(s string) {
	util.verror('builder error', s)
}
