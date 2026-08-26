module fastc

import os
import strings
import v3.pref
import v3.scanner
import v3.token

fn fastc_vmod_root_for_file(source_file string) string {
	mut dir := if source_file.len > 0 { os.dir(source_file) } else { os.getwd() }
	if dir.len == 0 {
		dir = os.getwd()
	}
	original_dir := dir
	for {
		if os.exists(os.join_path(dir, 'v.mod')) {
			return os.real_path(dir)
		}
		parent := os.dir(dir)
		if parent == dir || parent.len == 0 {
			return os.real_path(original_dir)
		}
		dir = parent
	}
	return os.real_path(original_dir)
}

fn fastc_resolve_c_pseudo_paths(raw string, vroot string, source_file string) string {
	mut result := raw
	if result.contains('@VEXEROOT') && vroot.len > 0 {
		result = result.replace('@VEXEROOT', vroot)
	}
	if result.contains('@VROOT') {
		result = result.replace('@VROOT', '@VMODROOT')
	}
	if result.contains('@VMODROOT') {
		result = result.replace('@VMODROOT', fastc_vmod_root_for_file(source_file))
	}
	if result.contains('@DIR') {
		dir := if source_file.len > 0 { os.dir(source_file) } else { os.getwd() }
		result = result.replace('@DIR', os.real_path(dir))
	}
	return result
}

fn fastc_resolve_source_files(paths []string, prefs &pref.Preferences) ![]FastcSourceFile {
	mut queue := []FastcQueuedSource{}
	if prefs.building_v {
		builtin_dir := prefs.get_vlib_module_path('builtin')
		for builtin_file in pref.get_v_files_from_dir_for_target(builtin_dir, prefs.user_defines,
			prefs.target) {
			if fastc_source_file_matches_backend(builtin_file) {
				queue << FastcQueuedSource{
					path:        builtin_file
					module_name: 'builtin'
				}
			}
		}
	}
	for path in paths {
		queue << FastcQueuedSource{
			path: path
		}
	}
	mut seen := map[string]bool{}
	mut sources := []FastcSourceFile{}
	// Modules are re-discovered once per importing file; canonicalization and
	// directory enumeration are syscalls, so both are memoized for the walk.
	mut real_path_cache := map[string]string{}
	mut module_dir_files := map[string][]string{}
	for queue.len > 0 {
		queued := queue[0]
		queue.delete(0)
		mut path := ''
		if cached := real_path_cache[queued.path] {
			path = cached
		} else {
			path = os.real_path(queued.path)
			real_path_cache[queued.path] = path
		}
		if seen[path] {
			continue
		}
		if !os.is_file(path) {
			return error('fastc source file `${path}` does not exist')
		}
		seen[path] = true
		source := os.read_file(path)!
		mut header := fastc_scan_source_header(source, path, prefs)!
		if queued.module_name != '' {
			expected_module_name := queued.module_name.all_after_last('.')
			if header.module_name != expected_module_name {
				return error('fastc imported source `${path}` declares module `${header.module_name}` instead of `${expected_module_name}`')
			}
			header = FastcSourceHeader{
				module_name:   queued.module_name
				imports:       header.imports
				import_order:  header.import_order
				blank_imports: header.blank_imports
				has_globals:   header.has_globals
			}
		}
		sources << FastcSourceFile{
			path:   path
			source: source
			header: header
		}
		mut discovered_imports := map[string]bool{}
		for imported_module in fastc_header_imported_modules(header) {
			if discovered_imports[imported_module] {
				continue
			}
			discovered_imports[imported_module] = true
			module_dir := prefs.get_module_path(imported_module, path)
			if module_dir == '' {
				return error('fastc cannot resolve imported module `${imported_module}` from `${path}`')
			}
			mut module_files := []string{}
			if cached := module_dir_files[module_dir] {
				module_files = cached.clone()
			} else {
				for module_file in pref.get_v_files_from_dir_for_target(module_dir,
					prefs.user_defines, prefs.target) {
					if fastc_source_file_matches_backend(module_file) {
						module_files << module_file
					}
				}
				module_dir_files[module_dir] = module_files
			}
			for module_file in module_files {
				mut module_file_real := ''
				if cached := real_path_cache[module_file] {
					module_file_real = cached
				} else {
					module_file_real = os.real_path(module_file)
					real_path_cache[module_file] = module_file_real
				}
				if !seen[module_file_real] {
					queue << FastcQueuedSource{
						path:        module_file
						module_name: imported_module
					}
				}
			}
		}
	}
	return sources
}

fn fastc_header_imported_modules(header FastcSourceHeader) []string {
	if header.import_order.len > 0 {
		return header.import_order.clone()
	}
	mut fallback := header.imports.values()
	fallback << header.blank_imports
	return fallback
}

fn fastc_sources_in_dependency_order(sources []FastcSourceFile) ![]FastcSourceFile {
	mut module_order := []string{}
	for source_file in sources {
		module_name := source_file.header.module_name
		if module_name !in module_order {
			module_order << module_name
		}
	}
	mut visiting := []string{}
	mut visited := []string{}
	mut ordered := []FastcSourceFile{cap: sources.len}
	for module_name in module_order {
		fastc_append_module_sources(module_name, sources, mut visiting, mut visited, mut ordered)!
	}
	return ordered
}

fn fastc_module_init_calls(sources []FastcSourceFile, functions map[string]FastcFunctionSignature) ![]string {
	return fastc_module_lifecycle_calls(sources, functions, 'init', false)
}

fn fastc_module_cleanup_calls(sources []FastcSourceFile, functions map[string]FastcFunctionSignature) ![]string {
	return fastc_module_lifecycle_calls(sources, functions, 'cleanup', true)
}

fn fastc_module_lifecycle_calls(sources []FastcSourceFile, functions map[string]FastcFunctionSignature, hook_name string, reverse bool) ![]string {
	ordered_sources := fastc_sources_in_dependency_order(sources)!
	mut seen_modules := map[string]bool{}
	mut ordered_modules := []string{}
	for source_file in ordered_sources {
		module_name := source_file.header.module_name
		if seen_modules[module_name] {
			continue
		}
		seen_modules[module_name] = true
		ordered_modules << module_name
	}
	modules := if reverse { ordered_modules.reverse() } else { ordered_modules }
	mut calls := []string{}
	for module_name in modules {
		function_key := fastc_function_key(module_name, hook_name)
		if signature := functions[function_key] {
			if signature.parameter_types.len > 0 {
				return error('fastc parser does not support module `${hook_name}` with parameters in ${signature.path}')
			}
			calls << fastc_c_function_name(module_name, hook_name)
		}
	}
	return calls
}

fn fastc_generate_startup_initializers(sources []FastcSourceFile, constant_initializers map[string]string, global_initializers map[string]string, module_init_calls []string) !string {
	ordered_sources := fastc_sources_in_dependency_order(sources)!
	mut seen_modules := map[string]bool{}
	mut out := strings.new_builder(1024)
	for source_file in ordered_sources {
		module_name := source_file.header.module_name
		if seen_modules[module_name] {
			continue
		}
		seen_modules[module_name] = true
		constant_initializer := constant_initializers[module_name] or { '' }
		global_initializer := global_initializers[module_name] or { '' }
		out.write_string(constant_initializer)
		out.write_string(global_initializer)
		init_call := fastc_c_function_name(module_name, 'init')
		if init_call in module_init_calls {
			out.writeln('\t${init_call}();')
		}
	}
	return out.str()
}

fn fastc_append_module_sources(module_name string, sources []FastcSourceFile, mut visiting []string, mut visited []string, mut ordered []FastcSourceFile) ! {
	if module_name in visited {
		return
	}
	if module_name in visiting {
		return error('fastc parser does not support cyclic module dependency involving `${module_name}`')
	}
	visiting << module_name
	mut dependencies := []string{}
	for source_file in sources {
		if source_file.header.module_name != module_name {
			continue
		}
		for dependency in fastc_header_imported_modules(source_file.header) {
			if dependency != module_name && dependency !in dependencies {
				dependencies << dependency
			}
		}
	}
	for dependency in dependencies {
		fastc_append_module_sources(dependency, sources, mut visiting, mut visited, mut ordered)!
	}
	for source_file in sources {
		if source_file.header.module_name == module_name {
			ordered << source_file
		}
	}
	visiting.delete(visiting.len - 1)
	visited << module_name
}

fn fastc_source_file_matches_backend(path string) bool {
	return !path.ends_with('.arm64.v') && !path.ends_with('.amd64.v')
		&& !path.ends_with('.native.v') && !path.ends_with('.wasm.v') && !path.ends_with('.rv64.v')
		&& !path.ends_with('.js.v')
}

fn fastc_scan_source_header(source string, path string, prefs &pref.Preferences) !FastcSourceHeader {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines_without_digest(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut module_name := ''
	mut imports := map[string]string{}
	mut import_order := []string{}
	mut blank_imports := []string{}
	mut has_globals := false
	mut brace_depth := 0
	mut tok := scan.scan()
	for tok != .eof {
		if module_name == '' && tok == .attribute {
			mut attribute_depth := 1
			tok = scan.scan()
			for attribute_depth > 0 && tok != .eof {
				if tok == .name && scan.lit == 'has_globals' {
					has_globals = true
				}
				if tok == .lsbr {
					attribute_depth++
				} else if tok == .rsbr {
					attribute_depth--
				}
				tok = scan.scan()
			}
			continue
		}
		if module_name == '' && tok == .key_module {
			tok = scan.scan()
			if tok != .name {
				return error('fastc parser does not support module declaration in ${path}')
			}
			module_name = scan.lit
			tok = scan.scan()
			continue
		}
		if brace_depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					selected_header := fastc_scan_source_header(selected.source, path, prefs)!
					fastc_merge_source_header_imports(selected_header, path, mut imports, mut
						import_order, mut blank_imports)!
					has_globals = has_globals || selected_header.has_globals
				}
				tok = selected.tok
				continue
			}
		}
		if brace_depth == 0
			&& tok in [.key_fn, .key_struct, .key_enum, .key_interface, .key_type, .key_const, .key_global] {
			break
		}
		if tok != .key_import || brace_depth > 0 {
			if tok == .lcbr {
				brace_depth++
			} else if tok == .rcbr && brace_depth > 0 {
				brace_depth--
			}
			tok = scan.scan()
			continue
		}
		tok = scan.scan()
		if tok == .lpar {
			tok = scan.scan()
			for tok != .rpar && tok != .eof {
				if tok == .semicolon || tok == .comma {
					tok = scan.scan()
					continue
				}
				import_path, alias, selected_names, next_token :=
					fastc_scan_import(mut scan, tok, path)!
				fastc_register_import_alias(import_path, alias, path, mut imports, mut
					blank_imports)!
				fastc_register_selective_imports(import_path, selected_names, path, mut imports)!
				if import_path !in import_order {
					import_order << import_path
				}
				tok = next_token
			}
			if tok == .rpar {
				tok = scan.scan()
			}
			continue
		}
		import_path, alias, selected_names, next_token := fastc_scan_import(mut scan, tok, path)!
		fastc_register_import_alias(import_path, alias, path, mut imports, mut blank_imports)!
		fastc_register_selective_imports(import_path, selected_names, path, mut imports)!
		if import_path !in import_order {
			import_order << import_path
		}
		tok = next_token
	}
	if module_name == '' {
		module_name = 'main'
	}
	if prefs.building_v && prefs.backend == 'fastc' && imports['driver'] == 'v3.driver'
		&& 'fastcdriver' in imports {
		fastcdriver_module := imports['fastcdriver']
		imports['driver'] = fastcdriver_module
		for i, imported_module in import_order {
			if imported_module == 'v3.driver' {
				import_order[i] = fastcdriver_module
			}
		}
	}
	return FastcSourceHeader{
		module_name:   module_name
		imports:       imports
		import_order:  import_order
		blank_imports: blank_imports
		has_globals:   has_globals
	}
}

fn fastc_merge_source_header_imports(header FastcSourceHeader, path string, mut destination_imports map[string]string, mut destination_import_order []string, mut destination_blank_imports []string) ! {
	for alias, imported_module in header.imports {
		if alias.starts_with('#select#') {
			fastc_register_selective_imports(imported_module, [alias['#select#'.len..]], path, mut
				destination_imports)!
		} else {
			fastc_register_import_alias(imported_module, alias, path, mut destination_imports, mut
				destination_blank_imports)!
		}
	}
	for imported_module in header.blank_imports {
		fastc_register_import_alias(imported_module, '_', path, mut destination_imports, mut
			destination_blank_imports)!
	}
	for imported_module in header.import_order {
		if imported_module !in destination_import_order {
			destination_import_order << imported_module
		}
	}
}

fn fastc_register_import_alias(import_path string, alias string, path string, mut imports map[string]string, mut blank_imports []string) ! {
	if alias == '_' {
		blank_imports << import_path
		return
	}
	if existing_module := imports[alias] {
		if existing_module != import_path {
			return error('fastc parser cannot reuse import alias `${alias}` for `${import_path}` after `${existing_module}` in ${path}')
		}
	}
	imports[alias] = import_path
}

fn fastc_scan_import(mut scan scanner.Scanner, first token.Token, path string) !(string, string, []string, token.Token) {
	mut tok := first
	if tok != .name {
		return error('fastc parser does not support import `${tok.str()}` in ${path}')
	}
	mut parts := [scan.lit]
	tok = scan.scan()
	for tok == .dot {
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support import path in ${path}')
		}
		parts << scan.lit
		tok = scan.scan()
	}
	mut alias := parts.last()
	if tok == .key_as {
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support import alias in ${path}')
		}
		alias = scan.lit
		tok = scan.scan()
	}
	mut selected_names := []string{}
	if tok == .lcbr {
		mut depth := 1
		for depth > 0 {
			tok = scan.scan()
			if tok == .eof {
				return error('fastc parser does not support unfinished selective import in ${path}')
			}
			if tok == .lcbr {
				depth++
			} else if tok == .rcbr {
				depth--
			} else if depth == 1 && tok == .name {
				selected_names << scan.lit
			}
		}
		tok = scan.scan()
	}
	return parts.join('.'), alias, selected_names, tok
}

fn fastc_selective_import_key(name string) string {
	return '#select#${name}'
}

fn fastc_register_selective_imports(import_path string, selected_names []string, path string, mut imports map[string]string) ! {
	for name in selected_names {
		key := fastc_selective_import_key(name)
		if existing_module := imports[key] {
			if existing_module != import_path {
				return error('fastc parser cannot resolve ambiguous selective import `${name}` in ${path}')
			}
		}
		imports[key] = import_path
	}
}
