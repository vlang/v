module driver

import os
import strings
import v.cmdexec
import v.gen.c as cgen
import v.modulecache
import v.pref
import v.tempname
import v.util.cache_refresh

// vfmt off
$if !skip_fastc ? {
	import v.gen.fastc
}
// vfmt on

$if !skip_fastc ? {
	struct V3FastcNativeBridge {
		closure  V3NativeInputClosure
		bindings V3TccDylibBindings
		library  string
	}

	fn compile_v3_fastc_tcc_generation(input string, generation &fastc.GenerationResult, bin_file string, prefs &pref.Preferences, environment_flags []string, user_flags []string, link_flags []string, sdk string, is_debug bool, cache_enabled bool, keep_c bool, mut prestarted fastc.FastcPrestartedCUnits) V3FastCCompileResult {
		mut flags := environment_flags.clone()
		flags << generation.c_flags
		flags << user_flags
		flags << link_flags
		if prefs.normalized_target_os() == 'macos' && prefs.ccompiler == 'tinyc'
			&& (c_flags_need_objective_c(flags) || '-framework' in flags
				|| generation.c_pieces[..generation.units.head_end].any(it.starts_with('#include ') && (it.contains('.m"') || it.contains('.mm"')))) {
			bridge := v3_fastc_native_bridge(generation, prefs, flags, sdk) or {
				return V3FastCCompileResult{ command: 'FastC native bridge', output: err.msg() }
			}
			aliased := fastc.generate_files_with_native_aliases([input], prefs, bridge.closure.functions.keys()) or {
				return V3FastCCompileResult{ command: 'FastC native bridge', output: err.msg() }
			}
			pieces := v3_fastc_native_program_pieces(&aliased, &bridge)
			if keep_c {
				fastc.write_c_pieces(keep_c_output_file(bin_file), pieces) or {
					return V3FastCCompileResult{ command: 'FastC native bridge', output: err.msg() }
				}
			}
			mut program_flags := tcc_cached_main_flags(v3_tcc_dylib_native_flags(flags))
			program_flags << v3_tcc_dylib_sdk_flags(os.dir(bridge.library), bridge.closure.inputs['__v3_c_flags__'])
			program_flags << bridge.library
			program_flags << v3_tcc_dylib_program_link_flags(flags, pieces[..aliased.units.head_end].join(''), sdk)
			return compile_v3_fastc_source(pieces, aliased.units, bin_file, prefs, [], program_flags, [], [], sdk, is_debug, aliased.uses_threads, cache_enabled, mut prestarted)
		}
		return compile_v3_fastc_source(generation.c_pieces, generation.units, bin_file, prefs, environment_flags, generation.c_flags, user_flags, link_flags, sdk, is_debug, generation.uses_threads, cache_enabled, mut prestarted)
	}

	// Keep native implementations separate from generated V bodies. TinyCC cannot
	// parse Objective-C, and its ELF objects cannot consume Clang's Mach-O objects.
	fn v3_fastc_native_bridge(generation &fastc.GenerationResult, prefs &pref.Preferences, flags []string, sdk string) !V3FastcNativeBridge {
		mut head := strings.new_builder(65536)
		mut externs := map[int]string{}
		for i, index in generation.units.extern_indexes {
			externs[index] = generation.units.extern_texts[i]
		}
		for i in 0 .. generation.units.head_end {
			head.write_string(externs[i] or { generation.c_pieces[i] })
		}
		source := modulecache.declaration_header(head.str()).split_into_lines().filter(!it.contains('/sync/stdatomic/tcc_compat_') && it.trim_space() != '#define inline __attribute((weak)) inline').join('\n') + '\n'
		mut directories := []string{}
		include_dirs := v3_parallel_c_include_dirs(flags)
		for line in source.split_into_lines() {
			arg := v3_tcc_dylib_include_argument(line) or { continue }
			if !arg.starts_with('"') { continue }
			mut path := arg.trim('"')
			if !os.is_abs_path(path) {
				for dir in include_dirs {
					candidate := os.join_path(dir, path)
					if os.is_file(candidate) {
						path = candidate
						break
					}
				}
			}
			if !cgen.native_path_is_shipped(path, prefs.vroot) {
				directories << os.dir(path)
			} else {
				text := os.read_file(path) or { continue }
				if text.contains('<Cocoa/') || text.contains('<AppKit/') || text.contains('__bridge')
					|| text.contains('@interface') || text.contains('@implementation') {
					directories << os.dir(path)
				}
			}
		}
		compiler := '/usr/bin/cc'
		mut native_flags := v3_tcc_dylib_native_flags(flags)
		if sdk != '' { native_flags << ['-isysroot', sdk] }
		mut wanted := map[string]bool{}
		for name in generation.native_symbols { wanted[name] = true }
		closure := v3_tcc_native_source_closure(source, prefs, native_flags, compiler, 'objective-c', directories, wanted, wanted, &cgen.CacheNativeInputs{}, generation.native_declarations)
		if closure.unassignable != '' {
			return error('cannot build the FastC native bridge: ' + closure.unassignable)
		}
		bindings := v3_tcc_dylib_function_bindings(closure.functions)!
		owner := source + bindings.owner
		dir := os.join_path(cache_refresh.root_dir(prefs.vroot, prefs.vexe), 'fastc-native')
		os.mkdir_all(dir)!
		mut stats := CObjectCacheStats{}
		prepared := prepare_c_flags_for_link(native_flags, [], [], ['-O0'], true, false, '', [], prefs.target, compiler, true, dir, mut stats)!
		mut signature := c_hash_bytes(u64(1469598103934665603), owner.bytes())
		signature = c_hash_bytes(signature, native_flags.join('\x00').bytes())
		signature = c_hash_bytes(signature, prepared.join('\x00').bytes())
		signature = c_hash_bytes(signature, v3_shared_c_compiler_identity(compiler).bytes())
		for path in closure.inputs['__v3_c_flags__'] {
			signature = c_hash_bytes(signature, (path + modulecache.file_signature(path)).bytes())
		}
		for path in prepared {
			if !c_flag_is_existing_file(path) { continue }
			signature = c_hash_bytes(signature, (path + modulecache.file_signature(path)).bytes())
			if c_flag_is_c_source_file(path) {
				dependencies := c_object_dependencies(compiler, c_object_compile_flags(prepared), path)
				if dependencies.used_fallback {
					return error('cannot determine native source dependencies for ' + path)
				}
				for input in dependencies.files {
					signature = c_hash_bytes(signature, (input + modulecache.file_signature(input)).bytes())
				}
			}
		}
		library := os.join_path(dir, '${signature.hex()}.dylib')
		if !os.is_file(library) {
			path := os.join_path(dir, '${signature.hex()}.${tempname.unique_token()}.m')
			defer { os.rm(path) or {} }
			os.write_file(path, owner)!
			staged := path + '.dylib'
			defer { os.rm(staged) or {} }
			mut args := ['-std=gnu11', '-O0', '-Wno-incompatible-function-pointer-types',
				'-Wno-typedef-redefinition']
			args << c_object_compile_flags(prepared)
			args << ['-x', 'objective-c', '-dynamiclib', '-undefined', 'dynamic_lookup',
				'-Wl,-install_name,' + library, path, '-o', staged]
			args << ['-x', 'none']
			args << prepared.filter(c_flag_is_c_source_file(it))
			args << c_dylib_link_flags(prepared)
			result := cmdexec.run(compiler, args)
			if result.exit_code != 0 { return error(result.output) }
			os.mv(staged, library)!
		}
		return V3FastcNativeBridge{ closure: closure, bindings: bindings, library: library }
	}

	fn v3_fastc_native_program_pieces(generation &fastc.GenerationResult, bridge &V3FastcNativeBridge) []string {
		mut pieces := generation.c_pieces.clone()
		mut active := map[string]bool{}
		mut declared_objc_types := false
		for i in 0 .. generation.units.head_end {
			if pieces[i].contains('#include') || pieces[i].contains('#import') {
				pieces[i] = tcc_cached_main_source(v3_tcc_dylib_declarations_in(pieces[i], true, '', bridge.closure.include_dirs, bridge.closure.tcc_headers, mut active), '')
			}
			if !declared_objc_types && i !in generation.units.extern_indexes {
				pieces[i] = bridge.closure.tcc_headers['__v3_objc_types__'] + pieces[i]
				declared_objc_types = true
			}
		}
		pieces[generation.units.head_end - 1] += bridge.bindings.declarations
		return pieces
	}
}
