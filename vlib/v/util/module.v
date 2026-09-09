module util

// 2022-01-30 TODO: this whole file should not exist :-|. It should just use the existing `v.vmod` instead,
// 2022-01-30 that already does handle v.mod lookup properly, stopping at .git folders, supporting `.v.mod.stop` etc.
import os
import v.pref
import v.vmod

@[if trace_util_qualify ?]
fn trace_qualify(callfn string, mod string, file_path string, kind_res string, result string, detail string) {
	eprintln('> ${callfn:15}: ${mod:-18} | file_path: ${file_path:-71} | => ${kind_res:14}: ${result:-18} ; ${detail}')
}

// 2022-01-30 qualify_import - used by V's parser, to find the full module name of import statements
// 2022-01-30 i.e. when parsing `import automaton` inside a .v file in examples/game_of_life/life_gg.v
// 2022-01-30 it returns just 'automaton'
// 2022-01-30 TODO: this seems to always just return `mod` itself, for modules inside the V main folder.
// 2022-01-30 It does also return `mod` itself, for stuff installed in ~/.vmodules like `vls` but for
// 2022-01-30 other reasons (see res 2 below).

// qualify_import is used by V's parser, to find the full module name of import statements.
// Do not use it.
pub fn qualify_import(pref_ &pref.Preferences, mod string, file_path string) string {
	// comments are from workdir: /v/vls
	mut mod_paths := pref_.lookup_path.clone()
	mod_paths << os.vmodules_paths()
	mod_path := mod.replace('.', os.path_separator)
	for search_path in mod_paths {
		try_path := os.join_path_single(search_path, mod_path)
		if os.is_dir(try_path) {
			if m1 := import_path_to_full_name(pref_, mod, try_path) {
				trace_qualify(@FN, mod, file_path, 'import_res 1', m1, try_path)
				// >  qualify_import: term | file_path: /v/vls/server/diagnostics.v | => import_res 1: term  ; /v/cleanv/vlib/term
				return m1
			}
		}
	}
	// Use absolute file_path so mod_path_to_full_name can walk up to find v.mod
	abs_file_path := if os.is_abs_path(file_path) {
		file_path
	} else {
		os.join_path_single(os.getwd(), file_path)
	}
	if m1 := import_path_to_full_name(pref_, mod, abs_file_path) {
		trace_qualify(@FN, mod, file_path, 'import_res 2', m1, abs_file_path)
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

// qualify_module - used by V's parser to find the full module name. Do not use it.
pub fn qualify_module(pref_ &pref.Preferences, mod string, file_path string) string {
	if mod == 'main' {
		trace_qualify(@FN, mod, file_path, 'module_res 1', mod, 'main')
		return mod
	}
	clean_file_path := file_path.all_before_last(os.path_separator)
	// Use absolute path so mod_path_to_full_name can walk up to find v.mod
	abs_clean_file_path := if os.is_abs_path(clean_file_path) {
		clean_file_path
	} else {
		os.join_path_single(os.getwd(), clean_file_path)
	}
	// relative module (relative to working directory)
	// TODO: find most stable solution & test with -usecache
	//
	// TODO: 2022-01-30: Using os.getwd() here does not seem right *at all* imho.
	// TODO: 2022-01-30: That makes lookup dependent on fragile environment factors.
	// TODO: 2022-01-30: The lookup should be relative to the folder, in which the current file is,
	// TODO: 2022-01-30: *NOT* to the working folder of the compiler, which can change easily.
	if clean_file_path.replace(os.getwd() + os.path_separator, '') == mod {
		if m1 := mod_path_to_full_name(pref_, mod, abs_clean_file_path) {
			if m1 != mod {
				trace_qualify(@FN, mod, file_path, 'module_res 2', m1,
					'clean_file_path - getwd == mod, m1 == f(${abs_clean_file_path})')
				return m1
			}
		}
		trace_qualify(@FN, mod, file_path, 'module_res 2', mod,
			'clean_file_path - getwd == mod, clean_file_path: ${clean_file_path}')
		return mod
	}
	if m1 := mod_path_to_full_name(pref_, mod, abs_clean_file_path) {
		trace_qualify(@FN, mod, file_path, 'module_res 3', m1, 'm1 == f(${abs_clean_file_path})')
		return m1
	}
	trace_qualify(@FN, mod, file_path, 'module_res 4', mod,
		'---, clean_file_path: ${clean_file_path}')
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
fn mod_path_to_full_name(pref_ &pref.Preferences, mod string, path string) !string {
	return mod_path_to_full_name_with_options(pref_, mod, path, false)
}

fn import_path_to_full_name(pref_ &pref.Preferences, mod string, path string) !string {
	return mod_path_to_full_name_with_options(pref_, mod, path, true)
}

fn mod_path_to_full_name_with_options(pref_ &pref.Preferences, mod string, path string, allow_shorter_name bool) !string {
	// TODO: explore using `pref.lookup_path` & `os.vmodules_paths()`
	// absolute paths instead of 'vlib' & '.vmodules'
	mut vmod_folders := ['vlib', '.vmodules', 'modules']
	bases := pref_.lookup_path.map(os.base(it))
	for base in bases {
		if base !in vmod_folders {
			vmod_folders << base
		}
	}
	mut in_vmod_path := false
	parts := path.split(os.path_separator)
	for vmod_folder in vmod_folders {
		if vmod_folder in parts {
			in_vmod_path = true
			break
		}
	}
	// Anchor the module-name boundary to the v.mod that contains the
	// current compilation (`pref_.path`). Without this, a nested v.mod
	// inside the project (e.g. a vendored sub-project at `dep/v.mod`)
	// would shrink the qualified name: files at `dep/mymod` would become
	// `mymod` instead of `dep.mymod`, breaking `import dep.mymod` from
	// the outer project. See issue #27138.
	pref_project_root := if in_vmod_path { '' } else { project_root_vmod_folder(pref_) }
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
						mod_full_name := normalize_base_url_mod_name(try_path.split(os.path_separator)[
							j + 1..].join('.'), try_path)
						return mod_full_name
					}
				}
				// not in one of the `vmod_folders` so work backwards through each parent
				// looking for for a `v.mod` file and break at the first path without it
			} else {
				if pref_project_root != '' {
					real_try_path := os.real_path(try_path)
					prefix := pref_project_root + os.path_separator
					if real_try_path.starts_with(prefix) {
						relative_parts := real_try_path.all_after(prefix).split(os.path_separator)
						mod_full_name := normalize_base_url_mod_name(relative_parts.join('.'),
							try_path)
						if !allow_shorter_name && mod_full_name.len < mod.len {
							return mod
						}
						if !module_name_has_empty_part(mod_full_name) {
							return mod_full_name
						}
					}
				}
				mut try_path_parts := try_path.split(os.path_separator)
				// last index in try_path_parts that contains a `v.mod`
				mut last_v_mod := -1
				mut hit_project_boundary := false
				for j := try_path_parts.len; j > 0; j-- {
					parent := try_path_parts[0..j].join(os.path_separator)
					if ls := os.ls(parent) {
						// currently CI clones some modules into the v repo to test, the condition
						// after `'v.mod' in ls` can be removed once a proper solution is added
						if 'v.mod' in ls
							&& (try_path_parts.len > i && try_path_parts[i] != 'v' && 'vlib' !in ls) {
							last_v_mod = j
							break
						}
						if has_vmod_boundary_marker(ls) {
							hit_project_boundary = true
							break
						}
						continue
					}
					break
				}
				if last_v_mod > -1 {
					mod_full_name := normalize_base_url_mod_name(try_path_parts[last_v_mod..].join('.'),
						try_path)
					if !allow_shorter_name && mod_full_name.len < mod.len {
						return mod
					}
					if !module_name_has_empty_part(mod_full_name) {
						return mod_full_name
					}
				}
				if hit_project_boundary {
					// Do not construct candidates above the current checkout/project.
					break
				}
			}
		}
	}
	if os.is_abs_path(path) && os.is_dir(path) { // && path.contains(mod )
		abs_pref_path := if os.is_abs_path(pref_.path) {
			pref_.path
		} else {
			os.join_path_single(os.getwd(), pref_.path)
		}
		logical_abs_pref_path := os.norm_path(abs_pref_path)
		normalized_abs_pref_path := os.real_path(logical_abs_pref_path)
		mut pref_module_prefix := ''
		abs_pref_base := if os.is_dir(normalized_abs_pref_path) {
			if compilation_path_is_module_root(pref_, logical_abs_pref_path) {
				pref_module_prefix = os.base(logical_abs_pref_path)
			}
			normalized_abs_pref_path
		} else {
			pref_file_dir := os.dir(normalized_abs_pref_path)
			if compilation_path_is_module_root(pref_, logical_abs_pref_path) {
				pref_module_prefix = os.base(os.dir(logical_abs_pref_path))
			}
			pref_file_dir
		}
		normalized_path := os.real_path(path)
		mut rel_mod_path := ''
		if normalized_path != abs_pref_base {
			prefix := abs_pref_base + os.path_separator
			if !normalized_path.starts_with(prefix) {
				return error('module not found')
			}
			rel_mod_path = normalized_path.all_after(prefix).replace(os.path_separator, '.')
		}
		if pref_module_prefix != '' {
			rel_mod_path = if rel_mod_path == '' {
				pref_module_prefix
			} else {
				'${pref_module_prefix}.${rel_mod_path}'
			}
		}
		if rel_mod_path != '' {
			return normalize_base_url_mod_name(rel_mod_path, path)
		}
	}
	return error('module not found')
}

fn compilation_path_is_module_root(pref_ &pref.Preferences, path string) bool {
	module_dir := if os.is_dir(path) { path } else { os.dir(path) }
	expected_module := os.base(module_dir)
	if expected_module == '' || expected_module == 'main' {
		return false
	}
	if os.is_file(path) {
		source_module := source_file_module_name(path) or { return false }
		if source_module == expected_module {
			return true
		}
		// External tests use `module foo_test`, while the production sources in
		// the same directory still define the module root as `foo`.
		if source_module != '${expected_module}_test' {
			return false
		}
		_ := test_source_filter_alias(os.base(path)) or { return false }
		entries := os.ls(module_dir) or { return false }
		for source_path in active_module_source_files(pref_, module_dir, entries, false) {
			if source_file_module_name(source_path) or { '' } == expected_module {
				return true
			}
		}
		return false
	}
	entries := os.ls(path) or { return false }
	for source_path in active_module_source_files(pref_, path, entries, pref_.is_test) {
		if source_file_module_name(source_path) or { '' } == expected_module {
			return true
		}
	}
	return false
}

fn active_module_source_files(pref_ &pref.Preferences, dir string, entries []string, include_tests bool) []string {
	mut sources := pref_.should_compile_filtered_files(dir, entries)
	if !include_tests {
		return sources
	}
	mut test_files_by_alias := map[string][]string{}
	mut test_aliases := []string{}
	for entry in entries {
		alias := test_source_filter_alias(entry) or { continue }
		test_aliases << alias
		test_files_by_alias[alias] << entry
	}
	for alias_path in pref_.should_compile_filtered_files(dir, test_aliases) {
		for test_file in test_files_by_alias[os.base(alias_path)] {
			sources << os.join_path(dir, test_file)
		}
	}
	return sources
}

fn test_source_filter_alias(file string) ?string {
	if file.ends_with('_test.v') {
		return file[..file.len - '_test.v'.len] + '.v'
	}
	if !file.ends_with('.v') {
		return none
	}
	stem := file.all_before_last('.v')
	if !stem.contains('.') {
		return none
	}
	base := stem.all_before_last('.')
	if !base.ends_with('_test') {
		return none
	}
	return base[..base.len - '_test'.len] + '.' + stem.all_after_last('.') + '.v'
}

fn source_file_module_name(path string) ?string {
	source := read_file(path) or { return none }
	mut start := 0
	if source.len >= 2 && source[0] == `#` && source[1] == `!` {
		start = 2
		for start < source.len && source[start] !in [`\n`, `\r`] {
			start++
		}
	}
	for start < source.len {
		start = skip_source_space_and_comments(source, start) or { return none }
		if start + 1 < source.len && source[start] == `@` && source[start + 1] == `[` {
			start += 2
			mut brackets := 1
			mut quote := u8(0)
			for start < source.len && brackets > 0 {
				if quote == 0 {
					start = skip_source_space_and_comments(source, start) or { return none }
					if start >= source.len {
						break
					}
				}
				ch := source[start]
				if quote != 0 {
					if ch == `\\` && start + 1 < source.len {
						start += 2
						continue
					}
					if ch == quote {
						quote = 0
					}
				} else if ch in [`'`, `"`] {
					quote = ch
				} else if ch == `[` {
					brackets++
				} else if ch == `]` {
					brackets--
				}
				start++
			}
			if brackets > 0 {
				return none
			}
			continue
		}
		break
	}
	if start + 6 >= source.len || source[start..start + 6] != 'module' {
		return none
	}
	module_suffix := source[start + 6]
	if module_suffix !in [` `, `\t`, `\v`, `\f`] && !(module_suffix == `/` && start + 7 < source.len
		&& source[start + 7] in [`/`, `*`]) {
		return none
	}
	name_start := skip_source_space_and_comments(source, start + 6) or { return none }
	mut name_end := name_start
	for name_end < source.len && source[name_end] !in [` `, `\t`, `\v`, `\f`, `\n`, `\r`, `;`, `/`] {
		name_end++
	}
	if name_start == name_end {
		return none
	}
	name := source[name_start..name_end]
	return if name.starts_with('@') { name[1..] } else { name }
}

fn skip_source_space_and_comments(source string, pos int) ?int {
	mut start := pos
	for start < source.len {
		if source[start] in [` `, `\t`, `\v`, `\f`, `\n`, `\r`] {
			start++
			continue
		}
		if start + 1 < source.len && source[start] == `/` && source[start + 1] == `/` {
			start += 2
			for start < source.len && source[start] !in [`\n`, `\r`] {
				start++
			}
			continue
		}
		if start + 1 < source.len && source[start] == `/` && source[start + 1] == `*` {
			start += 2
			mut depth := 1
			for start + 1 < source.len && depth > 0 {
				if source[start] == `/` && source[start + 1] == `*` {
					depth++
					start += 2
					continue
				}
				if source[start] == `*` && source[start + 1] == `/` {
					depth--
					start += 2
					continue
				}
				start++
			}
			if depth > 0 {
				return none
			}
			continue
		}
		break
	}
	return start
}

fn module_name_has_empty_part(name string) bool {
	if name == '' {
		return true
	}
	for part in name.split('.') {
		if part == '' {
			return true
		}
	}
	return false
}

fn has_vmod_boundary_marker(ls []string) bool {
	return '.v.mod.stop' in ls || '.git' in ls || '.hg' in ls || '.svn' in ls
}

// project_root_vmod_folder returns the absolute folder of the closest
// enclosing v.mod for the current compilation (`pref_.path`). Module-name
// qualification uses this as the boundary so a nested v.mod inside the
// project does not silently rename its sub-modules.
// It also respects `.v.mod.stop` and version-control metadata as project
// boundaries to prevent walking past the current project's root.
fn project_root_vmod_folder(pref_ &pref.Preferences) string {
	if pref_.path == '' {
		return ''
	}
	abs_pref_path := if os.is_abs_path(pref_.path) {
		pref_.path
	} else {
		os.join_path_single(os.getwd(), pref_.path)
	}
	start := if os.is_dir(abs_pref_path) { abs_pref_path } else { os.dir(abs_pref_path) }
	if start == '' {
		return ''
	}
	mut cfolder := os.real_path(start)
	for {
		if os.is_file(os.join_path(cfolder, 'v.mod')) {
			return cfolder
		}
		// `.v.mod.stop` and version-control metadata mark project boundaries;
		// stop walking up to avoid picking up a v.mod from an unrelated parent project
		// (e.g. the V compiler repo when compiling tests in a temp dir).
		// These markers are NOT v.mod roots — they only stop the search.
		if listing := os.ls(cfolder) {
			if has_vmod_boundary_marker(listing) {
				return ''
			}
		}
		parent := os.dir(cfolder)
		if parent == cfolder || parent == '' {
			return ''
		}
		cfolder = parent
	}
	return ''
}

// normalize_base_url_mod_name strips the `base_url` prefix from `mod_full_name`
// when the module lives in a folder configured via v.mod's `base_url`. Without
// this, a module rooted at `<pkg>/source/feature` would be named `pkg.source.feature`
// instead of `pkg.feature`. The implicit `src/` fallback is intentionally gone.
fn normalize_base_url_mod_name(mod_full_name string, path string) string {
	real_path := os.real_path(path)
	mut mcache := vmod.get_cache()
	vmod_file_location := mcache.get_by_folder(real_path)
	if vmod_file_location.vmod_file == '' {
		return mod_full_name
	}
	vmod_prefix := vmod_file_location.vmod_folder + os.path_separator
	if !real_path.starts_with(vmod_prefix) {
		return mod_full_name
	}
	manifest := vmod.from_file(vmod_file_location.vmod_file) or { return mod_full_name }
	if manifest.base_url == '' {
		return mod_full_name
	}
	base_parts := os.norm_path(manifest.base_url).split(os.path_separator).filter(it.len > 0
		&& it != '.')
	if base_parts.len == 0 {
		return mod_full_name
	}
	rel_path := real_path.all_after(vmod_prefix)
	rel_parts := rel_path.split(os.path_separator)
	if rel_parts.len < base_parts.len || rel_parts[..base_parts.len] != base_parts {
		return mod_full_name
	}
	full_parts := mod_full_name.split('.')
	if rel_parts.len > full_parts.len {
		return mod_full_name
	}
	mut normalized_parts := full_parts[..full_parts.len - rel_parts.len].clone()
	normalized_parts << rel_parts[base_parts.len..]
	return normalized_parts.join('.')
}
