module util

// 2022-01-30 TODO: this whole file should not exist :-|. It should just use the existing `v.vmod` instead,
// 2022-01-30 that already does handle v.mod lookup properly, stopping at .git folders, supporting `.v.mod.stop` etc.
import os
import v.pref

[if trace_util_qualify ?]
fn trace_qualify(callfn string, mod string, file_path string, kind_res string, result string, detail string) {
	eprintln('> ${callfn:15}: ${mod:-18} | file_path: ${file_path:-71} | => ${kind_res:14}: ${result:-18} ; ${detail}')
}

// 2022-01-30 qualify_import - used by V's parser, to find the full module name of import statements
// 2022-01-30 i.e. when parsing `import automaton` inside a .v file in examples/game_of_life/life_gg.v
// 2022-01-30 it returns just 'automaton'
// 2022-01-30 TODO: this seems to always just return `mod` itself, for modules inside the V main folder.
// 2022-01-30 It does also return `mod` itself, for stuff installed in ~/.vmodules like `vls` but for
// 2022-01-30 other reasons (see res 2 below).
pub fn qualify_import(pref &pref.Preferences, mod string, file_path string) string {
	// comments are from workdir: /v/vls
	mut mod_paths := pref.lookup_path.clone()
	mod_paths << os.vmodules_paths()
	mod_path := mod.replace('.', os.path_separator)
	for search_path in mod_paths {
		try_path := os.join_path_single(search_path, mod_path)
		if os.is_dir(try_path) {
			if m1 := mod_path_to_full_name(pref, mod, try_path) {
				trace_qualify(@FN, mod, file_path, 'import_res 1', m1, try_path)
				// >  qualify_import: term | file_path: /v/vls/server/diagnostics.v | => import_res 1: term  ; /v/cleanv/vlib/term
				return m1
			}
		}
	}
	if m1 := mod_path_to_full_name(pref, mod, file_path) {
		trace_qualify(@FN, mod, file_path, 'import_res 2', m1, file_path)
		// >  qualify_module: analyzer           | file_path: /v/vls/analyzer/store.v  | =>   module_res 2: analyzer           ; clean_file_path - getwd == mod
		// >  qualify_import: analyzer.depgraph  | file_path: /v/vls/analyzer/store.v  | =>   import_res 2: analyzer.depgraph  ; /v/vls/analyzer/store.v
		// >  qualify_import: tree_sitter        | file_path: /v/vls/analyzer/store.v  | =>   import_res 2: tree_sitter        ; /v/vls/analyzer/store.v
		// >  qualify_import: tree_sitter_v      | file_path: /v/vls/analyzer/store.v  | =>   import_res 1: tree_sitter_v      ; ~/.vmodules/tree_sitter_v
		// >  qualify_import: jsonrpc            | file_path: /v/vls/server/features.v | =>   import_res 2: jsonrpc            ; /v/vls/server/features.v
		return m1
	}
	trace_qualify(@FN, mod, file_path, 'import_res 3', mod, '---, mod_path: ${mod_path}')
	// >  qualify_import: server | file_path: cmd/vls/host.v | =>   import_res 3: server ; ---
	// >  qualify_import: cli    | file_path: cmd/vls/main.v | =>   import_res 1: cli    ; /v/cleanv/vlib/cli
	// >  qualify_import: server | file_path: cmd/vls/main.v | =>   import_res 3: server ; ---
	// >  qualify_import: os     | file_path: cmd/vls/main.v | =>   import_res 1: os     ; /v/cleanv/vlib/os
	return mod
}

// 2022-01-30 qualify_module - used by V's parser to find the full module name
// 2022-01-30 i.e. when parsing `module textscanner`, inside vlib/strings/textscanner/textscanner.v
// 2022-01-30 it will return `strings.textscanner`
pub fn qualify_module(pref &pref.Preferences, mod string, file_path string) string {
	if mod == 'main' {
		trace_qualify(@FN, mod, file_path, 'module_res 1', mod, 'main')
		return mod
	}
	clean_file_path := file_path.all_before_last(os.path_separator)
	// relative module (relative to working directory)
	// TODO: find most stable solution & test with -usecache
	//
	// TODO 2022-01-30: Using os.getwd() here does not seem right *at all* imho.
	// TODO 2022-01-30: That makes lookup dependent on fragile enviroment factors.
	// TODO 2022-01-30: The lookup should be relative to the folder, in which the current file is,
	// TODO 2022-01-30: *NOT* to the working folder of the compiler, which can change easily.
	if clean_file_path.replace(os.getwd() + os.path_separator, '') == mod {
		trace_qualify(@FN, mod, file_path, 'module_res 2', mod, 'clean_file_path - getwd == mod, clean_file_path: ${clean_file_path}')
		return mod
	}
	if m1 := mod_path_to_full_name(pref, mod, clean_file_path) {
		trace_qualify(@FN, mod, file_path, 'module_res 3', m1, 'm1 == f(${clean_file_path})')
		// >  qualify_module: net  | file_path: /v/cleanv/vlib/net/util.v     | =>   module_res 3: net     ; m1 == f(/v/cleanv/vlib/net)
		// >  qualify_module: term | file_path: /v/cleanv/vlib/term/control.v | =>   module_res 3: term    ; m1 == f(/v/cleanv/vlib/term)
		// >  qualify_module: log  | file_path: /v/vls/lsp/log/log.v          | =>   module_res 3: lsp.log ; m1 == f(/v/vls/lsp/log)

		// zzz BUG: when ../v.mod exists above V root folder:
		// zzz >  qualify_module: help | file_path: /v/cleanv/cmd/v/help/help.v   | =>   module_res 3: v.cmd.v.help  ; m1 == f(/v/cleanv/cmd/v/help)
		return m1
	}
	// zzzzzzz WORKING, when there is NO ../v.mod:
	// zzzzzzz >  qualify_module: help | file_path: /v/cleanv/cmd/v/help/help.v   | =>   module_res 4: help          ; ---, clean_file_path: /v/cleanv/cmd/v/help
	trace_qualify(@FN, mod, file_path, 'module_res 4', mod, '---, clean_file_path: ${clean_file_path}')
	return mod
}

// TODO:
// * properly define module location / v.mod rules
// * if possible split this function in two, one which gets the
// parent module path and another which turns it into the full name
// * create shared logic between these fns and builder.find_module_path
// 2022-01-30 TODO: the reliance on os.path_separator here, is also a potential problem.
// 2022-01-30 On windows that leads to:
// 2022-01-30 `v path/subfolder/` behaving very differently than `v path\subfolder\`
// 2022-01-30 (see daa5be4, that skips checking `vlib/v/checker/tests/modules/deprecated_module`
// 2022-01-30 just on windows, because while `vlib\v\checker\tests\modules\deprecated_module` works,
// 2022-01-30 it leads to path differences, and the / version on windows triggers a module lookip bug,
// 2022-01-30 leading to completely different errors)
fn mod_path_to_full_name(pref &pref.Preferences, mod string, path string) !string {
	// TODO: explore using `pref.lookup_path` & `os.vmodules_paths()`
	// absolute paths instead of 'vlib' & '.vmodules'
	mut vmod_folders := ['vlib', '.vmodules', 'modules']
	bases := pref.lookup_path.map(os.base(it))
	for base in bases {
		if base !in vmod_folders {
			vmod_folders << base
		}
	}
	mut in_vmod_path := false
	for vmod_folder in vmod_folders {
		if path.contains(vmod_folder + os.path_separator) {
			in_vmod_path = true
			break
		}
	}
	path_parts := path.split(os.path_separator)
	mod_path := mod.replace('.', os.path_separator)
	// go back through each parent in path_parts and join with `mod_path` to see the dir exists
	for i := path_parts.len - 1; i > 0; i-- {
		try_path := os.join_path_single(path_parts[0..i].join(os.path_separator), mod_path)
		// found module path
		if os.is_dir(try_path) {
			// we know we are in one of the `vmod_folders`
			if in_vmod_path {
				// so we can work our way backwards until we reach a vmod folder
				for j := i; j >= 0; j-- {
					path_part := path_parts[j]
					// we reached a vmod folder
					if path_part in vmod_folders {
						mod_full_name := try_path.split(os.path_separator)[j + 1..].join('.')
						return mod_full_name
					}
				}
				// not in one of the `vmod_folders` so work backwards through each parent
				// looking for for a `v.mod` file and break at the first path without it
			} else {
				mut try_path_parts := try_path.split(os.path_separator)
				// last index in try_path_parts that contains a `v.mod`
				mut last_v_mod := -1
				for j := try_path_parts.len; j > 0; j-- {
					parent := try_path_parts[0..j].join(os.path_separator)
					if ls := os.ls(parent) {
						// currently CI clones some modules into the v repo to test, the condition
						// after `'v.mod' in ls` can be removed once a proper solution is added
						if 'v.mod' in ls
							&& (try_path_parts.len > i && try_path_parts[i] != 'v' && 'vlib' !in ls) {
							last_v_mod = j
						}
						continue
					}
					break
				}
				if last_v_mod > -1 {
					mod_full_name := try_path_parts[last_v_mod..].join('.')
					return mod_full_name
				}
			}
		}
	}
	if os.is_abs_path(pref.path) && os.is_abs_path(path) && os.is_dir(path) { // && path.contains(mod )
		rel_mod_path := path.replace(pref.path.all_before_last(os.path_separator) +
			os.path_separator, '')
		if rel_mod_path != path {
			full_mod_name := rel_mod_path.replace(os.path_separator, '.')
			return full_mod_name
		}
	}
	return error('module not found')
}
