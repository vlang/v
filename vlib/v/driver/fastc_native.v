module driver

// vfmt off
$if !skip_fastc ? {
	import os
	import strings
	import v.cmdexec
	import v.modulecache
	import v.tempname
	import v.pref
	import time
	import v.gen.fastc
}
// vfmt on

$if !skip_fastc ? {
	fn compile_v3_fastc_tcc_generation(generation &fastc.GenerationResult, bin_file string, prefs &pref.Preferences, environment_flags []string, user_flags []string, link_flags []string, sdk string, is_debug bool, cache_enabled bool, keep_c bool, mut prestarted fastc.FastcPrestartedCUnits) V3FastCCompileResult {
		mut flags := environment_flags.clone()
		flags << generation.c_flags
		flags << user_flags
		// Executable linker options must not affect the native owner or its cache key.
		if prefs.normalized_target_os() == 'macos' && prefs.ccompiler == 'tinyc'
			&& (c_flags_need_objective_c(flags) || '-framework' in flags
				|| generation.c_pieces[..generation.units.head_end].any(it.starts_with('#include ') && (it.contains('.m"') || it.contains('.mm"')))) {
			bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
			bridge_sw := time.new_stopwatch()
			bridge := v3_fastc_native_bridge(generation, prefs, flags, sdk) or {
				return V3FastCCompileResult{ command: 'FastC native bridge', output: err.msg() }
			}
			if bench_phases {
				eprintln('fastc-phase native.bridge ${bridge_sw.elapsed().microseconds()}us')
			}
			pieces := v3_fastc_native_program_pieces(generation, &bridge)
			if keep_c {
				fastc.write_c_pieces(keep_c_output_file(bin_file), pieces) or {
					return V3FastCCompileResult{ command: 'FastC native bridge', output: err.msg() }
				}
			}
			mut program_flags := tcc_cached_main_flags(v3_tcc_dylib_native_flags(flags))
			program_flags << v3_tcc_dylib_sdk_flags(os.dir(bridge.library), bridge.dependencies)
			program_flags << bridge.library
			program_deps := v3_fastc_native_program_dependencies(pieces, generation.units, program_flags, sdk, prefs.target, fastc.fastc_tcc_job_count(prefs)) or {
				return V3FastCCompileResult{ command: 'FastC native dependencies', output: err.msg() }
			}
			if bench_phases {
				eprintln('fastc-phase native.program_dependencies ${bridge_sw.elapsed().microseconds()}us')
			}
			program_flags << v3_tcc_dylib_program_link_flags(flags, program_deps, sdk)
			// TinyCC must reopen its own selected header branches on every build.
			// The native owner's dependency manifest validates only the native library.

			// Native inputs rule out executable caching; the payload object names none of them.
			return compile_v3_fastc_source(pieces, generation.units, bin_file, prefs, [], program_flags, [], link_flags, sdk, is_debug, generation.uses_threads, false, cache_enabled, mut prestarted)
		}
		return compile_v3_fastc_source(generation.c_pieces, generation.units, bin_file, prefs, environment_flags, generation.c_flags, user_flags, link_flags, sdk, is_debug, generation.uses_threads, cache_enabled, cache_enabled, mut prestarted)
	}

	struct V3FastcNativeBridge {
		functions []string
		// adapters carry the calls whose arguments TinyCC cannot place on the stack.
		adapters     []fastc.FastcNativeArgsAdapter
		sources      []string
		dependencies []string
		library      string
	}

	// v3_fastc_native_args_adapters lists the bridge functions that TinyCC calls through
	// an argument struct (see fastc.FastcNativeArgsAdapter). Only arm64 packs stack
	// arguments differently from TinyCC.
	fn v3_fastc_native_args_adapters(generation &fastc.GenerationResult, prefs &pref.Preferences, functions map[string]bool) []fastc.FastcNativeArgsAdapter {
		mut adapters := []fastc.FastcNativeArgsAdapter{}
		if prefs.target.arch != 'arm64' {
			return adapters
		}
		mut names := functions.keys()
		names.sort()
		for name in names {
			if adapter := fastc.fastc_native_args_adapter(name, generation.native_declarations[name]) {
				adapters << adapter
			}
		}
		return adapters
	}

	fn v3_fastc_native_bridge(generation &fastc.GenerationResult, prefs &pref.Preferences, flags []string, sdk string) !V3FastcNativeBridge {
		bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
		sw := time.new_stopwatch()
		mut head := strings.new_builder(65536)
		mut externs := map[int]string{}
		for i, index in generation.units.extern_indexes {
			externs[index] = generation.units.extern_texts[i]
		}
		mut sources := generation.units.native_headers.clone()
		for i in 0 .. generation.units.head_end {
			piece := externs[i] or { generation.c_pieces[i] }
			head.write_string(piece)
			if !piece.contains('#include') && !piece.contains('#import') {
				continue
			}
			for line in piece.split_into_lines() {
				if arg := v3_tcc_dylib_include_argument(line) {
					path := arg.trim('"')
					if c_flag_is_c_source_file(path) {
						sources << os.real_path(path)
					}
				}
			}
		}
		mut functions := map[string]bool{}
		for name, _ in generation.native_declarations {
			if !name.starts_with('__builtin_') && !name.starts_with('__atomic_') && !name.starts_with('__sync_')
				&& name !in ['va_arg', 'va_start', 'va_end', 'va_copy'] {
				functions[name] = true
			}
		}
		// This source contains generated V declarations and opaque native includes.
		// Only the platform compiler opens the headers and native implementations.
		dir := os.join_path(os.vtmp_dir(), 'v3_fastc_native')
		ensure_native_object_cache_dir(dir)!
		mut owner := v3_fastc_native_declarations(dir, head.str()) + v3_tcc_dylib_function_bindings(functions).owner
		adapters := v3_fastc_native_args_adapters(generation, prefs, functions)
		for adapter in adapters {
			owner += adapter.native_side()
		}
		if bench_phases {
			eprintln('fastc-phase native.owner ${sw.elapsed().microseconds()}us')
		}
		mut stats := CObjectCacheStats{}
		mut native_flags := v3_tcc_dylib_native_flags(flags)
		if sdk != '' { native_flags << ['-isysroot', sdk] }
		prepared := prepare_c_flags_for_link(native_flags, [], [], ['-O0'], true, false, '', [], prefs.target, '/usr/bin/clang', true, dir, mut stats)!
		link_flags := c_dylib_link_flags(prepared)
		mut archives := c_dylib_named_static_archive_inputs(link_flags)
		for archive in c_dylib_force_loaded_static_archive_inputs(link_flags) {
			if archive !in archives { archives << archive }
		}
		// Invalidate manifests written before archives were tracked. Include their
		// resolved paths so a changed library search result cannot reuse an old owner.
		mut hash := c_hash_bytes(u64(1469598103934665603), 'v3-fastc-native-v2'.bytes())
		hash = c_hash_bytes(hash, owner.bytes())
		hash = c_hash_bytes(hash, prepared.join('\x00').bytes())
		hash = c_hash_bytes(hash, archives.join('\x00').bytes())
		key := hash.hex()
		path := os.join_path(dir, key + '.m')
		if !os.is_file(path) {
			staged_source := path + '.tmp.' + tempname.unique_token()
			os.write_file(staged_source, owner)!
			publish_native_c_object(staged_source, path)!
		}
		mut args := ['-std=gnu11', '-O0', '-fwrapv', '-Wno-incompatible-function-pointer-types',
			'-Wno-typedef-redefinition']
		args << c_object_compile_flags(prepared)
		args << ['-x', 'objective-c']
		manifest := c_object_manifest_path(dir, path, '/usr/bin/clang', prepared, prefs.target, mut stats)
		if cached := v3_fastc_native_cached_library(manifest, fastc.fastc_tcc_job_count(prefs)) {
			if bench_phases {
				eprintln('fastc-phase native.validated ${sw.elapsed().microseconds()}us dependencies=${cached.dependencies.len}')
			}
			return V3FastcNativeBridge{ functions: functions.keys(), adapters: adapters, sources: sources, dependencies: cached.dependencies, library: cached.library }
		}
		deps := c_object_dependencies('/usr/bin/clang', args, path)
		if deps.used_fallback {
			return error('FastC native dependency manifest is unavailable: ' + path)
		}
		mut dependencies := deps.files.clone()
		for archive in archives {
			if archive !in dependencies { dependencies << archive }
		}
		for input in prepared {
			if !c_flag_is_existing_file(input) { continue }
			if input !in dependencies { dependencies << input }
			if c_flag_is_c_source_file(input) {
				inputs := c_object_dependencies('/usr/bin/clang', c_object_compile_flags(prepared), input)
				if inputs.used_fallback {
					return error('FastC native dependency manifest is unavailable: ' + input)
				}
				for dependency in inputs.files {
					if dependency !in dependencies { dependencies << dependency }
				}
			}
		}
		mut signatures := []string{}
		for dependency in dependencies {
			signature := c_object_file_signature(dependency, false, mut stats)
			if signature == '' { return error('cannot sign native input: ' + dependency) }
			signatures << signature
			hash = c_hash_bytes(hash, (dependency + '\x00' + signature).bytes())
		}
		hash = c_hash_bytes(hash, v3_shared_c_compiler_identity('/usr/bin/clang').bytes())
		library := os.join_path(dir, hash.hex() + '.dylib')
		if !os.is_file(library) {
			staged := library + '.tmp.' + tempname.unique_token()
			defer { os.rm(staged) or {} }
			args << ['-dynamiclib', '-undefined', 'dynamic_lookup', '-Wl,-install_name,' + library,
				path, '-o', staged, '-x', 'none']
			args << prepared.filter(c_flag_is_c_source_file(it))
			args << link_flags
			result := cmdexec.run('/usr/bin/clang', args)
			if result.exit_code != 0 { return error(result.output) }
			for i, dependency in dependencies {
				if c_object_file_signature(dependency, true, mut stats) != signatures[i] {
					return error('FastC native input changed during compilation: ' + dependency)
				}
			}
			publish_native_c_object(staged, library)!
		}
		write_c_object_manifest(manifest, library, dependencies, mut stats) or {}
		return V3FastcNativeBridge{ functions: functions.keys(), adapters: adapters, sources: sources, dependencies: dependencies, library: library }
	}

	// v3_fastc_native_declarations derives the native owner's declarations from the program
	// head. It is a function of that text and of this compiler, so the result is kept beside
	// the native cache under both; recomputing it dominated reuse of a warm native library.
	fn v3_fastc_native_declarations(dir string, head string) string {
		executable := os.executable()
		identity := modulecache.file_metadata_signature(executable)
		if identity == '' {
			return v3_fastc_native_owner_declarations(modulecache.declaration_header(head))
		}
		mut hash := c_hash_bytes(u64(1469598103934665603), 'v3-fastc-native-declarations-v1'.bytes())
		hash = c_hash_bytes(hash, '${executable}\x00${identity}\x00'.bytes())
		hash = c_hash_bytes(hash, head.bytes())
		path := os.join_path(dir, 'declarations_${hash.hex()}.h')
		if cached := os.read_file(path) {
			return cached
		}
		declarations := v3_fastc_native_owner_declarations(modulecache.declaration_header(head))
		staged := path + '.tmp.' + tempname.unique_token()
		os.write_file(staged, declarations) or { return declarations }
		os.mv(staged, path) or { os.rm(staged) or {} }
		return declarations
	}

	// v3_fastc_native_owner_declarations drops TinyCC's atomic compatibility lines from the
	// declarations the native compiler sees, and ends them with a newline.
	fn v3_fastc_native_owner_declarations(declarations string) string {
		if !declarations.contains('\r') && !declarations.contains('/sync/stdatomic/tcc_compat_')
			&& !declarations.contains('#define inline __attribute((weak)) inline') {
			// Splitting and joining the lines would not change this text.
			return if declarations.ends_with('\n') { declarations } else { declarations + '\n' }
		}
		return declarations.split_into_lines().filter(!it.contains('/sync/stdatomic/tcc_compat_') && it.trim_space() != '#define inline __attribute((weak)) inline').join('\n') + '\n'
	}

	// Validate opaque native inputs concurrently. SDK manifests contain thousands
	// of files; every byte is still hashed on each reuse, without reading headers
	// to infer declarations or relying on timestamps for cache correctness.
	fn v3_fastc_native_cached_library(manifest string, jobs int) ?V3FastcNativeBridge {
		lines := (os.read_file(manifest) or { return none }).split_into_lines()
		if lines.len < 3 || lines[0] != 'format=v3-c-object-manifest-v1'
			|| !lines[1].starts_with('object=') {
			return none
		}
		library := lines[1].all_after('object=')
		if !os.is_file(library) { return none }
		mut dependencies := []string{}
		mut signatures := []string{}
		for line in lines[2..] {
			if !line.starts_with('dependency=') { return none }
			entry := line.all_after('dependency=')
			tab := entry.last_index('\t') or { return none }
			if tab == 0 || tab + 1 >= entry.len { return none }
			dependencies << entry[..tab]
			signatures << entry[tab + 1..]
		}
		if !v3_fastc_native_signatures_match(dependencies, signatures, jobs) { return none }
		return V3FastcNativeBridge{ library: library, dependencies: dependencies }
	}

	// v3_fastc_native_signatures_match hashes every dependency again, concurrently.
	fn v3_fastc_native_signatures_match(dependencies []string, signatures []string, jobs int) bool {
		workers := if dependencies.len < 64 || jobs < 2 {
			1
		} else if jobs < 8 {
			jobs
		} else {
			8
		}
		mut pending := []thread bool{}
		for i in 0 .. workers {
			start := dependencies.len * i / workers
			end := dependencies.len * (i + 1) / workers
			pending << spawn v3_fastc_native_dependencies_match(dependencies[start..end], signatures[start..end])
		}
		mut valid := true
		for worker in pending { if !worker.wait() { valid = false } }
		return valid
	}

	fn v3_fastc_native_dependencies_match(dependencies []string, signatures []string) bool {
		mut stats := CObjectCacheStats{}
		for i, dependency in dependencies {
			if c_object_file_signature(dependency, false, mut stats) != signatures[i] {
				return false
			}
		}
		return true
	}

	fn v3_fastc_native_program_dependencies(pieces []string, units fastc.FastcUnitLayout, flags []string, sdk string, target pref.Target, jobs int) ![]string {
		mut source := strings.new_builder(65536)
		for i in 0 .. units.head_end {
			index := units.extern_indexes.index(i)
			source.write_string(if index >= 0 { units.extern_texts[index] } else { pieces[i] })
		}
		text := source.str()
		dir := os.join_path(os.vtmp_dir(), 'v3_fastc_native')
		path := os.join_path(dir, 'consumer_' + c_hash_bytes(u64(1469598103934665603), text.bytes()).hex() + '.c')
		if !os.is_file(path) {
			staged := path + '.tmp.' + tempname.unique_token()
			os.write_file(staged, text)!
			publish_native_c_object(staged, path)!
		}
		mut args := flags.clone()
		if sdk != '' { args << ['-isysroot', sdk] }
		// Declarations cannot change which headers the C compiler opens. Reuse the
		// compiler's previous answer for the same directives, compiler and flags while
		// every listed header keeps its content.
		mut stats := CObjectCacheStats{}
		directives_key := c_hash_bytes(u64(1469598103934665603), v3_fastc_preprocessor_lines(text).bytes()).hex()
		manifest := c_object_manifest_path(dir, os.join_path(dir, 'consumer_directives_' + directives_key + '.c'), '/usr/bin/clang', args, target, mut stats)
		canonical_path := os.real_path(path)
		if cached := v3_fastc_cached_program_dependencies(manifest, jobs) {
			mut dependencies := cached.clone()
			dependencies << canonical_path
			return dependencies
		}
		dependencies := v3_tcc_dylib_program_dependencies('/usr/bin/clang', args, path, '-std=gnu11')!
		// The TinyCC framework include directory contains symlinks to SDK Headers.
		// Canonical manifest paths retain their owning .framework for link selection.
		canonical := dependencies.map(os.real_path(it))
		mut reported := []string{cap: dependencies.len}
		for i, dependency in dependencies {
			if canonical[i] != canonical_path { reported << dependency }
		}
		v3_fastc_write_program_dependencies(manifest, reported, mut stats)
		return canonical
	}

	const v3_fastc_program_dependencies_format = 'format=v3-fastc-program-dependencies-v2'

	// v3_fastc_write_program_dependencies records each header the C compiler reported
	// opening with the file it resolved to and that file's signature. A symlink in an
	// include path can be retargeted to another framework without changing either file.
	fn v3_fastc_write_program_dependencies(manifest string, reported []string, mut stats CObjectCacheStats) {
		mut out := strings.new_builder(64 + reported.len * 160)
		out.writeln(v3_fastc_program_dependencies_format)
		for dependency in reported {
			resolved := os.real_path(dependency)
			signature := c_object_file_signature(resolved, false, mut stats)
			if signature == '' { return }
			out.writeln('dependency=${dependency}\t${resolved}\t${signature}')
		}
		staged := manifest + '.tmp.' + tempname.unique_token()
		os.write_file(staged, out.str()) or { return }
		os.mv(staged, manifest) or { os.rm(staged) or {} }
	}

	// v3_fastc_cached_program_dependencies returns the resolved headers of a manifest
	// while every reported path still resolves to the recorded file and every file keeps
	// its signature.
	fn v3_fastc_cached_program_dependencies(manifest string, jobs int) ?[]string {
		lines := (os.read_file(manifest) or { return none }).split_into_lines()
		if lines.len < 2 || lines[0] != v3_fastc_program_dependencies_format { return none }
		mut dependencies := []string{cap: lines.len - 1}
		mut signatures := []string{cap: lines.len - 1}
		for line in lines[1..] {
			if !line.starts_with('dependency=') { return none }
			entry := line.all_after('dependency=')
			signature_tab := entry.last_index('\t') or { return none }
			paths := entry[..signature_tab]
			resolved_tab := paths.last_index('\t') or { return none }
			if resolved_tab == 0 || resolved_tab + 1 >= paths.len || signature_tab + 1 >= entry.len {
				return none
			}
			resolved := paths[resolved_tab + 1..]
			if os.real_path(paths[..resolved_tab]) != resolved { return none }
			dependencies << resolved
			signatures << entry[signature_tab + 1..]
		}
		if !v3_fastc_native_signatures_match(dependencies, signatures, jobs) { return none }
		return dependencies
	}

	// v3_fastc_preprocessor_lines keeps the lines of generated C that can change what
	// the preprocessor opens: directives (also after a comment, or spelled `%:`), comment
	// delimiters, and continued lines. The other lines are ordinary declarations.
	fn v3_fastc_preprocessor_lines(text string) string {
		mut out := strings.new_builder(text.len / 8)
		mut start := 0
		mut continued := false
		for start < text.len {
			mut end := start
			mut keep := continued
			for end < text.len && text[end] != `\n` {
				c := text[end]
				if c == `#` || (c == `%` && end + 1 < text.len && text[end + 1] == `:`)
					|| (c == `/` && end + 1 < text.len && text[end + 1] == `*`)
					|| (c == `*` && end + 1 < text.len && text[end + 1] == `/`) {
					keep = true
				}
				end++
			}
			mut last := end - 1
			if last >= start && text[last] == `\r` {
				last--
			}
			continued = last >= start && text[last] == `\\`
			if keep || continued {
				out.write_string(text[start..end])
				out.write_u8(`\n`)
			}
			start = end + 1
		}
		return out.str()
	}

	fn v3_fastc_native_program_pieces(generation &fastc.GenerationResult, bridge &V3FastcNativeBridge) []string {
		mut pieces := generation.c_pieces.clone()
		for i in 0 .. generation.units.head_end {
			// An embedded file's bytes may spell a directive inside its string literal.
			if pieces[i].starts_with('const u8 __vf_embed_') {
				continue
			}
			if pieces[i].contains('#include') || pieces[i].contains('#import') {
				pieces[i] = tcc_cached_main_source(v3_tcc_dylib_declarations(pieces[i], bridge.sources), '')
			}
			if pieces[i].contains('#define SOKOL_') {
				pieces[i] = pieces[i].split_into_lines().filter(!it.starts_with('#define ')
					|| !tcc_cached_main_native_sokol_define(it.all_after('#define '))).join('\n') + '\n'
			}
		}
		for i in 1 .. generation.units.head_end {
			if i !in generation.units.extern_indexes {
				pieces[i] = v3_fastc_tcc_dispatch_prelude + pieces[i]
				break
			}
		}
		mut declarations := strings.new_builder(1024)
		for name in bridge.functions {
			prototype := generation.native_declarations[name]
			declarations.writeln('extern ' + prototype.replace(name + '(', '(*v3_native_bridge_' + name + ')('))
		}
		for adapter in bridge.adapters {
			declarations.write_string(adapter.program_side())
		}
		for i := generation.units.head_end - 1; i >= 0; i-- {
			if i !in generation.units.extern_indexes && !(i >= generation.units.prototype_start && i < generation.units.prototype_end) {
				pieces[i] += declarations.str()
				break
			}
		}
		return pieces
	}

	// TinyCC does not apply Clang's transparent-union argument conversion.
	// libdispatch's object union contains only pointers, so these C ABI calls
	// pass the pointer directly. Native headers remain owned by the C toolchain.
	const v3_fastc_tcc_dispatch_prelude = '
#include <dispatch/dispatch.h>
#define dispatch_release(value) ((void (*)(void *))(dispatch_release))((void *)(value))
#define dispatch_retain(value) ((void (*)(void *))(dispatch_retain))((void *)(value))
#define dispatch_resume(value) ((void (*)(void *))(dispatch_resume))((void *)(value))
#define dispatch_suspend(value) ((void (*)(void *))(dispatch_suspend))((void *)(value))
#define dispatch_activate(value) ((void (*)(void *))(dispatch_activate))((void *)(value))
#define dispatch_set_context(value, context) ((void (*)(void *, void *))(dispatch_set_context))((void *)(value), (context))
#define dispatch_get_context(value) ((void *(*)(void *))(dispatch_get_context))((void *)(value))
#define dispatch_set_finalizer_f(value, callback) ((void (*)(void *, void (*)(void *)))(dispatch_set_finalizer_f))((void *)(value), (callback))
'
}
