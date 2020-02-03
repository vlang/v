module builder

import (
	os
	time
	filepath
	v.ast
	v.table
	v.checker
	v.parser
	v.gen
	v.gen.x64
)

pub struct Builder {
pub:
	table   &table.Table
	checker checker.Checker
mut:
	prefs   Preferences
	parsed_files []ast.File
}

pub fn new_builder(prefs Preferences) Builder {
	table := table.new_table()
	mut b:= Builder{
		prefs: prefs
		table: table
		checker: checker.new_checker(table)
	}
	b.set_module_search_paths()
	return b
}

pub fn (b mut Builder) gen_c(v_files []string) string {
	b.parsed_files = parser.parse_files(v_files, b.table)
	b.parse_imports()
	b.checker.check_files(b.parsed_files)
	return gen.cgen(b.parsed_files, b.table)
}

pub fn (b mut Builder) build_c(v_files []string, out_file string) {
	os.write_file(out_file, b.gen_c(v_files))
}

pub fn (b mut Builder) build_x64(v_files []string, out_file string) {
	ticks := time.ticks()
	b.parsed_files = parser.parse_files(v_files, b.table)
	b.parse_imports()
	println('PARSE: ${time.ticks() - ticks}ms')
	b.checker.check_files(b.parsed_files)
	println('CHECK: ${time.ticks() - ticks}ms')
	x64.gen(b.parsed_files, out_file)
	println('x64 GEN: ${time.ticks() - ticks}ms')
}


fn (b &Builder) parse_module_files() {

}

// parse all deps from already parsed files
pub fn (b mut Builder) parse_imports() {
	mut done_imports := []string
	for i in 0 .. b.parsed_files.len {
		ast_file := b.parsed_files[i]
		for _, imp in ast_file.imports {
			mod := imp.mod
			if mod in done_imports {
				continue
			}
			import_path := b.find_module_path(mod) or {
				//v.parsers[i].error_with_token_index('cannot import module "$mod" (not found)', v.parsers[i].import_table.get_import_tok_idx(mod))
				//break
				panic('cannot import module "$mod" (not found)')
			}
			v_files := b.v_files_from_dir(import_path)
			if v_files.len == 0 {
				//v.parsers[i].error_with_token_index('cannot import module "$mod" (no .v files in "$import_path")', v.parsers[i].import_table.get_import_tok_idx(mod))
				panic('cannot import module "$mod" (no .v files in "$import_path")')
			}
			// Add all imports referenced by these libs
			parsed_files := parser.parse_files(v_files, b.table)
			for file in parsed_files {
				if file.mod.name != mod {
					//v.parsers[pidx].error_with_token_index('bad module definition: ${v.parsers[pidx].file_path} imports module "$mod" but $file is defined as module `$p_mod`', 1
					panic('bad module definition: ${ast_file.path} imports module "$mod" but $file.path is defined as module `$ast_file.mod.name`')
				}
			}
			b.parsed_files << parsed_files
			done_imports << mod
		}
	}
}

pub fn (b &Builder) v_files_from_dir(dir string) []string {
	mut res := []string
	if !os.exists(dir) {
		if dir == 'compiler' && os.is_dir('vlib') {
			println('looks like you are trying to build V with an old command')
			println('use `v -o v v.v` instead of `v -o v compiler`')
		}
		verror("$dir doesn't exist")
	}
	else if !os.is_dir(dir) {
		verror("$dir isn't a directory")
	}
	mut files := os.ls(dir)or{
		panic(err)
	}
	if b.prefs.is_verbose {
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
		if (file.ends_with('_win.v') || file.ends_with('_windows.v')) && b.prefs.os != .windows {
			continue
		}
		if (file.ends_with('_lin.v') || file.ends_with('_linux.v')) && b.prefs.os != .linux {
			continue
		}
		if (file.ends_with('_mac.v') || file.ends_with('_darwin.v')) && b.prefs.os != .mac {
			continue
		}
		if file.ends_with('_nix.v') && b.prefs.os == .windows {
			continue
		}
		if file.ends_with('_js.v') && b.prefs.os != .js {
			continue
		}
		if file.ends_with('_c.v') && b.prefs.os == .js {
			continue
		}
		/*
		if v.compile_defines_all.len > 0 && file.contains('_d_') {
			mut allowed := false
			for cdefine in v.compile_defines {
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
		*/
		res << filepath.join(dir,file)
	}
	return res
}

fn verror(err string) {
	panic('v error: $err')
}

pub fn (b &Builder) log(s string) {
	if b.prefs.is_verbose {
		println(s)
	}
}
