module driver

// vfmt off
$if !skip_fastc ? {
	import os
	import strings
	import hash as contenthash
	import v.cmdexec
	import v.modulecache
	import v.tempname
	import v.pref
	import time
	import v.gen.fastc
}
// vfmt on

$if !skip_fastc ? {
	fn v3_fastc_native_compile_jobs(prefs &pref.Preferences, parallel_cc bool) int {
		jobs := fastc.fastc_tcc_job_count(prefs)
		// Each unit reopens the native headers. Keep the measured default,
		// while allowing -parallel-cc and VJOBS to request more C workers.
		if !parallel_cc && os.getenv('VJOBS').int() <= 0 && jobs > 6 { return 6 }
		return jobs
	}

	// v3_fastc_native_compiler executes the native owners and answers the dependency
	// queries of FastC builds that link native code with TinyCC on macOS.
	const v3_fastc_native_compiler = '/usr/bin/clang'

	// v3_fastc_native_compiler_version asks the native compiler for its version, the
	// identity in every native manifest key. The driver asks while it generates C.
	fn v3_fastc_native_compiler_version() string {
		return c_object_compiler_version(v3_fastc_native_compiler, v3_fastc_native_compiler)
	}

	fn compile_v3_fastc_tcc_generation(generation &fastc.GenerationResult, bin_file string, prefs &pref.Preferences, environment_flags []string, user_flags []string, link_flags []string, sdk string, is_debug bool, cache_enabled bool, keep_c bool, parallel_cc bool, native_compiler_version string, mut prestarted fastc.FastcPrestartedCUnits) V3FastCCompileResult {
		result := compile_v3_fastc_tcc_generation_mode(generation, bin_file, prefs, environment_flags, user_flags, link_flags, sdk, is_debug, cache_enabled, keep_c, parallel_cc, native_compiler_version, true, mut prestarted)
		if !result.native_inputs_changed {
			return result
		}
		// A native input changed after its manifest was written. Build again and
		// validate every reused manifest before compiling, as without the overlap.
		if os.getenv('FASTC_BENCH_PHASES') != '' {
			eprintln('fastc-phase native.retry 0us')
		}
		mut no_prestarted := fastc.FastcPrestartedCUnits{}
		return compile_v3_fastc_tcc_generation_mode(generation, bin_file, prefs, environment_flags, user_flags, link_flags, sdk, is_debug, cache_enabled, keep_c, parallel_cc, native_compiler_version, false, mut no_prestarted)
	}

	// compile_v3_fastc_tcc_generation_mode compiles and links a generation. With
	// `overlap_validation`, reused native manifests are hashed while TinyCC compiles
	// the program; the link waits for that and reports `native_inputs_changed` when
	// an input no longer matches, before anything is linked or published.
	fn compile_v3_fastc_tcc_generation_mode(generation &fastc.GenerationResult, bin_file string, prefs &pref.Preferences, environment_flags []string, user_flags []string, link_flags []string, sdk string, is_debug bool, cache_enabled bool, keep_c bool, parallel_cc bool, native_compiler_version string, overlap_validation bool, mut prestarted fastc.FastcPrestartedCUnits) V3FastCCompileResult {
		mut validation := V3FastcNativeValidation{
			overlap: overlap_validation
		}
		if native_compiler_version != '' {
			validation.stats.compiler_versions[v3_fastc_native_compiler] = native_compiler_version
		}
		result := compile_v3_fastc_tcc_generation_attempt(generation, bin_file, prefs, environment_flags, user_flags, link_flags, sdk, is_debug, cache_enabled, keep_c, parallel_cc, mut prestarted, mut validation)
		return v3_fastc_native_checked_result(result, mut validation)
	}

	// v3_fastc_native_checked_result joins the validation of a build's reused native
	// manifests. A build that failed while one of them no longer matched its inputs (a
	// retained header that now includes a framework the recorded dependencies lack)
	// reports `native_inputs_changed`, so it runs again as a cache miss instead of
	// failing against the stale manifest.
	fn v3_fastc_native_checked_result(result V3FastCCompileResult, mut validation V3FastcNativeValidation) V3FastCCompileResult {
		valid := validation.wait()
		if !result.success && !result.native_inputs_changed && !valid {
			return V3FastCCompileResult{
				native_inputs_changed: true
			}
		}
		return result
	}

	fn compile_v3_fastc_tcc_generation_attempt(generation &fastc.GenerationResult, bin_file string, prefs &pref.Preferences, environment_flags []string, user_flags []string, link_flags []string, sdk string, is_debug bool, cache_enabled bool, keep_c bool, parallel_cc bool, mut prestarted fastc.FastcPrestartedCUnits, mut validation V3FastcNativeValidation) V3FastCCompileResult {
		mut flags := environment_flags.clone()
		flags << generation.c_flags
		flags << user_flags
		// Executable linker options must not affect the native owner or its cache key.
		if prefs.normalized_target_os() == 'macos' && prefs.ccompiler == 'tinyc'
			&& (c_flags_need_objective_c(flags) || '-framework' in flags
				|| generation.c_pieces[..generation.units.head_end].any(it.starts_with('#include ') && (it.contains('.m"') || it.contains('.mm"')))) {
			bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
			bridge_sw := time.new_stopwatch()
			bridge := v3_fastc_native_bridge(generation, prefs, flags, sdk, mut validation) or {
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
			program_deps := v3_fastc_native_program_dependencies(pieces, generation.units, program_flags, sdk, prefs.target, fastc.fastc_tcc_job_count(prefs), mut validation) or {
				return V3FastCCompileResult{ command: 'FastC native dependencies', output: err.msg() }
			}
			if bench_phases {
				eprintln('fastc-phase native.program_dependencies ${bridge_sw.elapsed().microseconds()}us')
			}
			program_flags << v3_tcc_dylib_program_link_flags(flags, program_deps, sdk)
			// TinyCC must reopen its own selected header branches on every build.
			// The native owner's dependency manifest validates only the native library.

			// Native inputs rule out executable caching; the payload object names none of them.
			jobs := v3_fastc_native_compile_jobs(prefs, parallel_cc)
			return compile_v3_fastc_source(pieces, generation.units, bin_file, prefs, [], program_flags, [], link_flags, sdk, is_debug, generation.uses_threads, false, cache_enabled, jobs, mut prestarted, mut validation)
		}
		return compile_v3_fastc_source(generation.c_pieces, generation.units, bin_file, prefs, environment_flags, generation.c_flags, user_flags, link_flags, sdk, is_debug, generation.uses_threads, cache_enabled, cache_enabled, fastc.fastc_tcc_job_count(prefs), mut prestarted, mut validation)
	}

	// V3FastcNativeValidation hashes the inputs of reused native manifests beside
	// the program's compilation. Nothing may be linked before `wait` reports them
	// unchanged.
	struct V3FastcNativeValidation {
		overlap bool
	mut:
		pending []thread bool
		// stats carries the file signatures and compiler answers that the native
		// steps of one build share.
		stats CObjectCacheStats
	}

	// add starts validating a reused manifest's dependencies (and that each reported
	// path still resolves to its dependency), or validates them at once without overlap.
	fn (mut validation V3FastcNativeValidation) add(dependencies []string, signatures []string, reported []string, jobs int) bool {
		if !validation.overlap {
			return v3_fastc_native_signatures_match(dependencies, signatures, reported, jobs)
		}
		validation.pending << spawn v3_fastc_native_signatures_match(dependencies, signatures, reported, jobs)
		return true
	}

	// v3_fastc_hash_pieces hashes text pieces in order without joining them.
	fn v3_fastc_hash_pieces(pieces []string, seed u64) u64 {
		mut hash := seed
		for piece in pieces {
			hash = contenthash.sum64_string(piece, hash)
		}
		return hash
	}

	// wait reports whether every pending manifest validated.
	fn (mut validation V3FastcNativeValidation) wait() bool {
		mut valid := true
		for pending in validation.pending {
			if !pending.wait() { valid = false }
		}
		validation.pending.clear()
		return valid
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

	fn v3_fastc_native_bridge(generation &fastc.GenerationResult, prefs &pref.Preferences, flags []string, sdk string, mut validation V3FastcNativeValidation) !V3FastcNativeBridge {
		bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
		sw := time.new_stopwatch()
		mut head := []string{cap: generation.units.head_end}
		mut externs := map[int]string{}
		for i, index in generation.units.extern_indexes {
			externs[index] = generation.units.extern_texts[i]
		}
		mut sources := []string{cap: generation.units.native_headers.len}
		for header in generation.units.native_headers {
			// Match the resolved include paths used by the program-side filter.
			sources << os.real_path(header)
		}
		// The prototypes, constants and globals after `native_owner_end` follow every
		// native directive, so no native source can name them. They change with ordinary
		// edits of V code, and the owner must not: only the exported V functions among
		// them are declared for the bindings below. A directive after them that can
		// open a file or change a macro (a conditional include) keeps the whole head.
		mut owner_end := generation.units.native_owner_end
		if owner_end <= 0 || owner_end > generation.units.head_end {
			owner_end = generation.units.head_end
		}
		for i in owner_end .. generation.units.head_end {
			piece := generation.c_pieces[i]
			if piece.contains('#') && (piece.contains('#include') || piece.contains('#import')
				|| piece.contains('#define') || piece.contains('#undef') || piece.contains('#pragma')) {
				owner_end = generation.units.head_end
				break
			}
		}
		// The constants and types of the V program sit between the native headers and
		// the native sources. See v3_fastc_native_lean_bridge.
		decl_start := generation.units.native_decl_start
		decl_end := generation.units.native_decl_end
		sliceable := owner_end < generation.units.head_end && decl_start > 0 && decl_start < decl_end
			&& decl_end <= owner_end
		mut before := []string{}
		mut region := []string{}
		mut after := []string{}
		for i in 0 .. generation.units.head_end {
			piece := externs[i] or { generation.c_pieces[i] }
			// An embedded file is declared under a name made from its path, which
			// only generated code knows.
			if i < owner_end && !generation.c_pieces[i].starts_with('const u8 __vf_embed_') {
				head << piece
				if sliceable {
					if i < decl_start {
						before << piece
					} else if i < decl_end {
						region << piece
					} else {
						after << piece
					}
				}
			}
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
		if owner_end < generation.units.head_end {
			head << generation.units.native_exports
			after << generation.units.native_exports
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
		bindings := v3_tcc_dylib_function_bindings(functions).owner
		adapters := v3_fastc_native_args_adapters(generation, prefs, functions)
		mut adapter_sides := strings.new_builder(adapters.len * 512)
		for adapter in adapters {
			adapter_sides.write_string(adapter.native_side())
		}
		adapter_text := adapter_sides.str()
		parts := V3FastcNativeOwnerParts{
			dir:          dir
			functions:    functions.keys()
			adapters:     adapters
			sources:      sources
			bindings:     bindings
			adapter_text: adapter_text
		}
		if bench_phases {
			eprintln('fastc-phase native.parts ${sw.elapsed().microseconds()}us')
		}
		mut full_marker := ''
		if sliceable && region.len > 0 {
			lean := v3_fastc_native_lean_bridge(prefs, flags, sdk, &parts, before, region, after, mut validation)
			if lean.built {
				return lean.bridge
			}
			full_marker = lean.full_marker
		}
		bridge := v3_fastc_native_bridge_head(prefs, flags, sdk, &parts, head, mut validation)!
		if full_marker != '' {
			// Clang needs declarations that the lean owner could not name: remember to
			// give this program all of them.
			os.write_file(full_marker, 'full\n') or {}
		}
		return bridge
	}

	// V3FastcNativeOwnerParts are the inputs of a native owner other than the V
	// declarations at its head.
	struct V3FastcNativeOwnerParts {
		dir          string
		functions    []string
		adapters     []fastc.FastcNativeArgsAdapter
		sources      []string
		bindings     string
		adapter_text string
	}

	struct V3FastcNativeLeanResult {
		bridge V3FastcNativeBridge
		built  bool
		// full_marker names the file to write once the full head compiles: Clang
		// rejected the lean owner for a reason no declaration of the program resolves.
		full_marker string
	}

	// v3_fastc_native_lean_bridge builds the owner with only the V types that the
	// generated bindings mention, and those an earlier Clang run reported missing.
	// A native source can name any type of the program, but V cannot know which
	// without reading it; an owner with all of them is recompiled after every edit of
	// an unrelated struct. Clang is the judge instead: it rejects a native source that
	// names a declaration the lean owner lacks, the missing names are added, and a
	// program whose native code needs more than that keeps the full head. The names
	// are a hint kept beside the cache for the program's native sources and flags, so a
	// new C binding does not lose them; a wrong hint only costs a rejected compilation.
	// Every constant of the program stays, as a macro: a native source that tests one
	// with `#ifdef` compiles without it too. A changed constant is settled by the text
	// Clang preprocesses the owner to, like any other macro.
	fn v3_fastc_native_lean_bridge(prefs &pref.Preferences, flags []string, sdk string, parts &V3FastcNativeOwnerParts, before []string, region []string, after []string, mut validation V3FastcNativeValidation) V3FastcNativeLeanResult {
		mut key := c_hash_bytes(u64(1469598103934665603), 'v3-fastc-native-lean-v2'.bytes())
		key = v3_fastc_hash_pieces(before, key)
		key = v3_fastc_hash_pieces(after, key)
		key = contenthash.sum64_string(parts.bindings, key)
		key = contenthash.sum64_string(parts.adapter_text, key)
		key = c_hash_bytes(key, flags.join('\x00').bytes())
		full_marker := os.join_path(parts.dir, 'lean_full_${key.hex()}')
		if os.exists(full_marker) {
			return V3FastcNativeLeanResult{}
		}
		mut names_key := c_hash_bytes(u64(1469598103934665603), 'v3-fastc-native-lean-names-v1'.bytes())
		names_key = c_hash_bytes(names_key, parts.sources.join('\x00').bytes())
		names_key = c_hash_bytes(names_key, flags.join('\x00').bytes())
		names_path := os.join_path(parts.dir, 'lean_names_${names_key.hex()}')
		region_hash := v3_fastc_hash_pieces(region, key)
		for _ in 0 .. 4 {
			names_text := os.read_file(names_path) or { '' }
			names := names_text.split_into_lines()
			slice_path := os.join_path(parts.dir, 'lean_${contenthash.sum64_string(names_text, region_hash).hex()}.h')
			slice := os.read_file(slice_path) or {
				computed := v3_fastc_native_lean_declarations(region.join(''), [
					after.join(''),
					parts.bindings,
					parts.adapter_text,
				], names)
				staged := slice_path + '.tmp.' + tempname.unique_token()
				os.write_file(staged, computed) or {}
				os.mv(staged, slice_path) or { os.rm(staged) or {} }
				computed
			}
			mut head := before.clone()
			head << slice
			head << after
			bridge := v3_fastc_native_bridge_head(prefs, flags, sdk, parts, head, mut validation) or {
				// Only a compiler diagnostic can name a missing declaration.
				if !err.msg().contains('error:') {
					return V3FastcNativeLeanResult{}
				}
				missing := v3_fastc_native_missing_declarations(err.msg(), region.join(''), names)
				if missing.len == 0 {
					return V3FastcNativeLeanResult{ full_marker: full_marker }
				}
				mut extended := names.clone()
				extended << missing
				staged := names_path + '.tmp.' + tempname.unique_token()
				os.write_file(staged, extended.join('\n') + '\n') or {
					return V3FastcNativeLeanResult{}
				}
				os.mv(staged, names_path) or {
					os.rm(staged) or {}
					return V3FastcNativeLeanResult{}
				}
				continue
			}
			return V3FastcNativeLeanResult{ bridge: bridge, built: true }
		}
		return V3FastcNativeLeanResult{ full_marker: full_marker }
	}

	// v3_fastc_native_bridge_head returns the native library of an owner whose V
	// declarations are `head`, compiling it when no cached library matches.
	fn v3_fastc_native_bridge_head(prefs &pref.Preferences, flags []string, sdk string, parts &V3FastcNativeOwnerParts, head []string, mut validation V3FastcNativeValidation) !V3FastcNativeBridge {
		bench_phases := os.getenv('FASTC_BENCH_PHASES') != ''
		sw := time.new_stopwatch()
		dir := parts.dir
		bindings := parts.bindings
		adapter_text := parts.adapter_text
		adapters := parts.adapters
		sources := parts.sources
		// The owner is the head's declarations plus the bindings and adapters, keyed
		// below by its content hash. A reused head finds that hash beside its cached
		// declarations, so the owner is only assembled when its source is missing.
		owner_seed := c_hash_bytes(u64(1469598103934665603), 'v3-fastc-native-v3'.bytes())
		declarations_path := v3_fastc_native_declarations_path(dir, head)
		owner_hash_path := if declarations_path == '' {
			''
		} else {
			mut parts_hash := contenthash.sum64_string(bindings, u64(1469598103934665603))
			parts_hash = contenthash.sum64_string(adapter_text, parts_hash)
			declarations_path + '.owner-${parts_hash.hex()}'
		}
		mut owner := ''
		mut owner_hash := u64(0)
		if owner_hash_path != '' {
			owner_hash = (os.read_file(owner_hash_path) or { '' }).u64()
		}
		if owner_hash == 0 {
			owner = v3_fastc_native_declarations(dir, head) + bindings + adapter_text
			owner_hash = contenthash.sum64_string(owner, owner_seed)
			if owner_hash_path != '' {
				staged_hash := owner_hash_path + '.tmp.' + tempname.unique_token()
				os.write_file(staged_hash, owner_hash.str()) or {}
				os.mv(staged_hash, owner_hash_path) or { os.rm(staged_hash) or {} }
			}
		}
		if bench_phases {
			eprintln('fastc-phase native.owner ${sw.elapsed().microseconds()}us')
		}
		mut native_flags := v3_tcc_dylib_native_flags(flags)
		if sdk != '' { native_flags << ['-isysroot', sdk] }
		prepared := prepare_c_flags_for_link(native_flags, [], [], ['-O0'], true, false, '', [], prefs.target, '/usr/bin/clang', true, dir, mut validation.stats)!
		link_flags := c_dylib_link_flags(prepared)
		mut archives := c_dylib_named_static_archive_inputs(link_flags)
		for archive in c_dylib_force_loaded_static_archive_inputs(link_flags) {
			if archive !in archives { archives << archive }
		}
		// Invalidate owners whose manifests did not validate the preprocessing snapshot.
		// Archive paths also keep changed library searches from reusing an old owner.
		mut hash := owner_hash
		hash = c_hash_bytes(hash, prepared.join('\x00').bytes())
		hash = c_hash_bytes(hash, archives.join('\x00').bytes())
		key := hash.hex()
		path := os.join_path(dir, key + '.m')
		if !os.is_file(path) {
			if owner == '' {
				owner = v3_fastc_native_declarations(dir, head) + bindings + adapter_text
			}
			staged_source := path + '.tmp.' + tempname.unique_token()
			os.write_file(staged_source, owner)!
			publish_native_c_object(staged_source, path)!
		}
		mut args := ['-std=gnu11', '-O0', '-fwrapv', '-Wno-incompatible-function-pointer-types',
			'-Wno-typedef-redefinition']
		args << c_object_compile_flags(prepared)
		args << ['-x', 'objective-c']
		manifest := c_object_manifest_path(dir, path, '/usr/bin/clang', prepared, prefs.target, mut validation.stats)
		if cached := v3_fastc_native_read_manifest(manifest) {
			if validation.add(cached.dependencies, cached.signatures, [], fastc.fastc_tcc_job_count(prefs)) {
				if bench_phases {
					eprintln('fastc-phase native.validated ${sw.elapsed().microseconds()}us dependencies=${cached.dependencies.len} overlapped=${validation.overlap}')
				}
				return V3FastcNativeBridge{ functions: parts.functions, adapters: adapters, sources: sources, dependencies: cached.dependencies, library: cached.library }
			}
		}
		// V macros such as @BUILD_TIMESTAMP can change without affecting native code.
		// Only Clang may establish that equivalence: headers remain opaque to V.
		preprocessed := v3_fastc_native_preprocessed_source(dir, path, args, archives, '/usr/bin/clang', mut validation.stats)!
		if bench_phases {
			eprintln('fastc-phase native.preprocessed ${sw.elapsed().microseconds()}us')
		}
		preprocessed_manifest := c_object_manifest_path(dir, preprocessed.path, '/usr/bin/clang', prepared, prefs.target, mut validation.stats)
		if cached := v3_fastc_native_cached_library(preprocessed_manifest, fastc.fastc_tcc_job_count(prefs)) {
			if bench_phases {
				eprintln('fastc-phase native.preprocessed_reuse ${sw.elapsed().microseconds()}us')
			}
			// This owner expands to the text of the library with the inputs just
			// validated. Record that under the owner as well: the next build of the same
			// program then finds the library without asking Clang to preprocess it again.
			if text := os.read_file(preprocessed_manifest) {
				staged_manifest := manifest + '.tmp.' + tempname.unique_token()
				os.write_file(staged_manifest, text) or {}
				os.mv(staged_manifest, manifest) or { os.rm(staged_manifest) or {} }
			}
			return V3FastcNativeBridge{ functions: parts.functions, adapters: adapters, sources: sources, dependencies: cached.dependencies, library: cached.library }
		}
		mut dependencies := preprocessed.dependencies.clone()
		dependencies << preprocessed.path
		mut listed := map[string]bool{}
		for dependency in dependencies {
			listed[dependency] = true
		}
		for archive in archives {
			if !listed[archive] {
				listed[archive] = true
				dependencies << archive
			}
		}
		// A program can link a hundred C sources (mbedtls). Each has its own headers,
		// so ask Clang for them side by side; the list keeps the order of the flags.
		source_inputs := prepared.filter(c_flag_is_existing_file(it) && c_flag_is_c_source_file(it))
		source_headers := v3_fastc_native_source_dependencies(source_inputs, c_object_compile_flags(prepared),
			fastc.fastc_tcc_job_count(prefs))
		mut source_index := 0
		for input in prepared {
			if !c_flag_is_existing_file(input) { continue }
			if !listed[input] {
				listed[input] = true
				dependencies << input
			}
			if c_flag_is_c_source_file(input) {
				inputs := source_headers[source_index]
				source_index++
				if inputs.used_fallback {
					return error('FastC native dependency manifest is unavailable: ' + input)
				}
				for dependency in inputs.files {
					if !listed[dependency] {
						listed[dependency] = true
						dependencies << dependency
					}
				}
			}
		}
		mut signatures := []string{}
		mut preprocessing_signatures := map[string]string{}
		for i, dependency in preprocessed.dependencies {
			preprocessing_signatures[dependency] = preprocessed.signatures[i]
		}
		for dependency in dependencies {
			// Preserve the hashes of the inputs Clang actually preprocessed, even
			// when a header changes before this later collection of native inputs.
			signature := preprocessing_signatures[dependency] or { v3_fastc_native_file_signature(dependency, false, mut validation.stats) }
			if signature == '' { return error('cannot sign native input: ' + dependency) }
			signatures << signature
			hash = c_hash_bytes(hash, (dependency + '\x00' + signature).bytes())
		}
		hash = c_hash_bytes(hash, v3_shared_c_compiler_identity('/usr/bin/clang').bytes())
		if bench_phases {
			eprintln('fastc-phase native.signed ${sw.elapsed().microseconds()}us dependencies=${dependencies.len}')
		}
		library := os.join_path(dir, hash.hex() + '.dylib')
		staged := library + '.tmp.' + tempname.unique_token()
		defer { os.rm(staged) or {} }
		if !os.is_file(library) {
			// Compile exactly the text that established the secondary cache key,
			// including Clang's expansion of time-dependent native macros.
			args << ['-dynamiclib', '-undefined', 'dynamic_lookup', '-Wl,-install_name,' + library,
				'-x', 'objective-c-cpp-output', preprocessed.path, '-o', staged, '-x', 'none']
			args << prepared.filter(c_flag_is_c_source_file(it))
			args << link_flags
			result := cmdexec.run('/usr/bin/clang', args)
			if result.exit_code != 0 { return error(result.output) }
			if bench_phases {
				eprintln('fastc-phase native.compiled ${sw.elapsed().microseconds()}us')
			}
		}
		v3_fastc_native_publish_library(staged, library, [manifest, preprocessed_manifest], dependencies, signatures, mut validation.stats)!
		return V3FastcNativeBridge{ functions: parts.functions, adapters: adapters, sources: sources, dependencies: dependencies, library: library }
	}

	// v3_fastc_native_source_dependencies returns the files Clang opens for each
	// source, in the order of `sources`, running up to `jobs` Clang processes at a time.
	fn v3_fastc_native_source_dependencies(sources []string, compile_flags []string, jobs int) []CObjectDependencies {
		mut results := []CObjectDependencies{cap: sources.len}
		batch := if jobs < 1 { 1 } else { jobs }
		mut start := 0
		for start < sources.len {
			end := if start + batch < sources.len { start + batch } else { sources.len }
			mut workers := []thread CObjectDependencies{cap: end - start}
			for source in sources[start..end] {
				workers << spawn c_object_dependencies('/usr/bin/clang', compile_flags, source)
			}
			for worker in workers {
				results << worker.wait()
			}
			start = end
		}
		return results
	}

	struct V3FastcNativePreprocessedSource {
		path         string
		dependencies []string
		signatures   []string
	}

	fn v3_fastc_native_validate_inputs(dependencies []string, signatures []string, phase string, mut stats CObjectCacheStats) ! {
		if dependencies.len != signatures.len {
			return error('FastC native dependency signatures are incomplete')
		}
		for i, dependency in dependencies {
			if signatures[i] == '' || v3_fastc_native_file_signature(dependency, true, mut stats) != signatures[i] {
				return error('FastC native input changed during ${phase}: ' + dependency)
			}
		}
	}

	// v3_fastc_native_publish_library validates the captured inputs before publishing
	// either the library or its manifests. Serialize those hashes without sampling again.
	fn v3_fastc_native_publish_library(staged string, library string, manifests []string, dependencies []string, signatures []string, mut stats CObjectCacheStats) ! {
		v3_fastc_native_validate_inputs(dependencies, signatures, 'compilation', mut stats)!
		if os.is_file(staged) { publish_native_c_object(staged, library)! }
		if !os.is_file(library) {
			return error('FastC native library was not produced: ' + library)
		}
		mut out := strings.new_builder(128 + dependencies.len * 96)
		out.writeln('format=v3-c-object-manifest-v1')
		out.writeln('object=${library}')
		for i, dependency in dependencies {
			out.writeln('dependency=${dependency}\t${signatures[i]}')
		}
		text := out.str()
		for manifest in manifests {
			temporary := manifest + '.tmp.' + tempname.unique_token()
			os.write_file(temporary, text) or {
				os.rm(temporary) or {}
				continue
			}
			os.mv(temporary, manifest) or { os.rm(temporary) or {} }
		}
	}

	// v3_fastc_native_preprocessed_source keys a second lookup by Clang's expanded
	// source and dependency paths. Unused macros disappear; macros used by a native
	// header, including in conditional includes, still affect the result. The original
	// dependency contents and native inputs are validated before either lookup reuses a library.
	fn v3_fastc_native_preprocessed_source(dir string, source string, compile_args []string, archives []string, compiler string, mut stats CObjectCacheStats) !V3FastcNativePreprocessedSource {
		staged := os.join_path(dir, 'preprocessed.tmp.' + tempname.unique_token())
		depfile := staged + '.d'
		defer {
			os.rm(staged) or {}
			os.rm(depfile) or {}
		}
		before := c_object_dependencies(compiler, compile_args, source)
		if before.used_fallback {
			return error('FastC native preprocessing dependency manifest is unavailable: ' + source)
		}
		mut signatures := []string{cap: before.files.len}
		for dependency in before.files {
			signature := v3_fastc_native_file_signature(dependency, true, mut stats)
			if signature == '' { return error('cannot sign native input: ' + dependency) }
			signatures << signature
		}
		mut args := compile_args.clone()
		args << ['-E', '-P', '-MD', '-MF', depfile, '-MT', 'v3cache', source, '-o', staged]
		result := cmdexec.run(compiler, args)
		if result.exit_code != 0 { return error(result.output) }
		deps := c_object_dependencies_from_output(os.read_file(depfile)!, source)
		if deps.used_fallback {
			return error('FastC native preprocessing dependency manifest is unavailable: ' + source)
		}
		if deps.files != before.files {
			return error('FastC native dependencies changed during preprocessing: ' + source)
		}
		v3_fastc_native_validate_inputs(before.files, signatures, 'preprocessing', mut stats)!
		// Clang leaves up to eight empty lines where directives stood, so a macro that
		// nothing uses would still change the text by one line. Without line markers
		// (`-P`) the empty lines carry nothing.
		expanded := os.read_file(staged)!
		compact := v3_fastc_native_without_empty_lines(expanded)
		if compact.len != expanded.len {
			os.write_file(staged, compact)!
		}
		signature := v3_fastc_native_file_signature(staged, false, mut stats)
		if signature == '' { return error('cannot sign preprocessed native input: ' + source) }
		canonical_source := os.real_path(source)
		mut hash := c_hash_bytes(u64(1469598103934665603), 'v3-fastc-preprocessed-v3'.bytes())
		hash = c_hash_bytes(hash, signature.bytes())
		hash = c_hash_bytes(hash, deps.files.filter(it != source && it != canonical_source).join('\x00').bytes())
		hash = c_hash_bytes(hash, archives.join('\x00').bytes())
		path := os.join_path(dir, 'preprocessed_' + hash.hex() + '.mi')
		if !os.is_file(path) { publish_native_c_object(staged, path)! }
		return V3FastcNativePreprocessedSource{ path: path, dependencies: before.files, signatures: signatures }
	}

	// v3_fastc_native_without_empty_lines drops the lines of preprocessed text that
	// hold nothing but spaces.
	fn v3_fastc_native_without_empty_lines(text string) string {
		mut out := strings.new_builder(text.len)
		mut start := 0
		for start < text.len {
			mut end := start
			mut empty := true
			for end < text.len && text[end] != `\n` {
				if text[end] !in [` `, `\t`, `\r`] {
					empty = false
				}
				end++
			}
			if !empty {
				out.write_string(text[start..end])
				out.write_u8(`\n`)
			}
			start = end + 1
		}
		return out.str()
	}

	// v3_fastc_native_declarations derives the native owner's declarations from the program
	// head. It is a function of that text and of this compiler, so the result is kept beside
	// the native cache under both; recomputing it dominated reuse of a warm native library.
	// v3_fastc_native_declarations_path names the cached declarations of a head for this
	// compiler, or is empty when the compiler's file identity is unavailable. The head
	// is hashed piece by piece.
	fn v3_fastc_native_declarations_path(dir string, head []string) string {
		executable := os.executable()
		identity := modulecache.file_metadata_signature(executable)
		if identity == '' {
			return ''
		}
		mut hash := c_hash_bytes(u64(1469598103934665603), 'v3-fastc-native-declarations-v2'.bytes())
		hash = c_hash_bytes(hash, '${executable}\x00${identity}\x00'.bytes())
		hash = v3_fastc_hash_pieces(head, hash)
		return os.join_path(dir, 'declarations_${hash.hex()}.h')
	}

	// The head's text is joined only when the declarations are not cached yet.
	fn v3_fastc_native_declarations(dir string, head []string) string {
		path := v3_fastc_native_declarations_path(dir, head)
		if path == '' {
			return v3_fastc_native_owner_declarations(modulecache.declaration_header(head.join('')))
		}
		if cached := os.read_file(path) {
			return cached
		}
		declarations := v3_fastc_native_owner_declarations(modulecache.declaration_header(head.join('')))
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

	// Hash fixed-sized blocks with the word hash used by the compiler's maps.
	// Filling each block makes signatures independent of short reads. The prefix
	// separates these signatures from the older byte-at-a-time native manifests.
	fn v3_fastc_native_file_signature(path string, refresh bool, mut stats CObjectCacheStats) string {
		canonical := os.real_path(path)
		key := 'fastc-content:' + canonical
		if refresh { stats.file_signatures.delete(key) }
		if signature := stats.file_signatures[key] { return signature }
		signature := v3_fastc_native_read_signature(canonical)
		if signature == '' { return '' }
		stats.file_signatures[key] = signature
		stats.dependency_file_reads++
		return signature
	}

	// Manifest validation reads each named input once. Opening that path already
	// follows symlinks; it needs neither another realpath walk nor a memo table.
	fn v3_fastc_native_read_signature(path string) string {
		mut file := os.open(path) or { return '' }
		defer { file.close() }
		mut buffer := [64 * 1024]u8{}
		mut buffered := 0
		mut digest := u64(1469598103934665603)
		for {
			read := file.read_into_ptr(&buffer[buffered], buffer.len - buffered) or {
				if err is os.Eof { break }
				return ''
			}
			if read == 0 { break }
			buffered += read
			if buffered == buffer.len {
				digest = contenthash.sum64(buffer[..], digest)
				buffered = 0
			}
		}
		if buffered > 0 { digest = contenthash.sum64(buffer[..buffered], digest) }
		return 'word-v1:' + digest.hex()
	}

	// Validate opaque native inputs concurrently. SDK manifests contain thousands
	// of files; every byte is still hashed on each reuse, without reading headers
	// to infer declarations or relying on timestamps for cache correctness.
	fn v3_fastc_native_cached_library(manifest string, jobs int) ?V3FastcNativeBridge {
		cached := v3_fastc_native_read_manifest(manifest)?
		if !v3_fastc_native_signatures_match(cached.dependencies, cached.signatures, [], jobs) {
			return none
		}
		return V3FastcNativeBridge{ library: cached.library, dependencies: cached.dependencies }
	}

	struct V3FastcNativeManifest {
		library      string
		dependencies []string
		signatures   []string
		// reported holds the paths the C compiler reported for the dependencies.
		reported []string
	}

	// v3_fastc_native_read_manifest reads a native library manifest whose library
	// still exists. Its dependencies are not validated here.
	fn v3_fastc_native_read_manifest(manifest string) ?V3FastcNativeManifest {
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
		return V3FastcNativeManifest{ library: library, dependencies: dependencies, signatures: signatures }
	}

	// v3_fastc_native_signatures_match hashes every dependency again, concurrently.
	// With `reported` paths, each must also still resolve to its dependency.
	fn v3_fastc_native_signatures_match(dependencies []string, signatures []string, reported []string, jobs int) bool {
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
			pending << spawn v3_fastc_native_dependencies_match(dependencies[start..end], signatures[start..end],
				if reported.len == 0 { []string{} } else { reported[start..end] })
		}
		mut valid := true
		for worker in pending { if !worker.wait() { valid = false } }
		return valid
	}

	fn v3_fastc_native_dependencies_match(dependencies []string, signatures []string, reported []string) bool {
		for i, dependency in dependencies {
			if reported.len > 0 && os.real_path(reported[i]) != dependency {
				return false
			}
			if v3_fastc_native_read_signature(dependency) != signatures[i] {
				return false
			}
		}
		return true
	}

	fn v3_fastc_native_program_dependencies(pieces []string, units fastc.FastcUnitLayout, flags []string, sdk string, target pref.Target, jobs int, mut validation V3FastcNativeValidation) ![]string {
		mut externs := map[int]string{}
		for i, index in units.extern_indexes {
			externs[index] = units.extern_texts[i]
		}
		mut head := []string{cap: units.head_end}
		for i in 0 .. units.head_end {
			head << externs[i] or { pieces[i] }
		}
		dir := os.join_path(os.vtmp_dir(), 'v3_fastc_native')
		path := os.join_path(dir, 'consumer_v2_' + v3_fastc_hash_pieces(head, u64(1469598103934665603)).hex() + '.c')
		// The directives key is a function of the consumer text, which its path names.
		// It is kept under that name, so a reused head is neither joined nor scanned.
		// Rename the file when v3_fastc_preprocessor_lines changes.
		directives_path := path + '.directives-v2'
		mut directives_key := os.read_file(directives_path) or { '' }
		if directives_key == '' {
			directives_key = c_hash_bytes(u64(1469598103934665603), v3_fastc_preprocessor_lines(head.join('')).bytes()).hex()
			staged_key := directives_path + '.tmp.' + tempname.unique_token()
			os.write_file(staged_key, directives_key) or {}
			os.mv(staged_key, directives_path) or { os.rm(staged_key) or {} }
		}
		mut args := flags.clone()
		if sdk != '' { args << ['-isysroot', sdk] }
		// Declarations cannot change which headers the C compiler opens. Reuse the
		// compiler's previous answer for the same directives, compiler and flags while
		// every listed header keeps its content.
		manifest := c_object_manifest_path(dir, os.join_path(dir, 'consumer_directives_' + directives_key + '.c'), '/usr/bin/clang', args, target, mut validation.stats)
		if cached := v3_fastc_read_program_dependencies(manifest) {
			if validation.add(cached.dependencies, cached.signatures, cached.reported, jobs) {
				return cached.dependencies
			}
		}
		// Only Clang reads the consumer text, and only for directives it has not seen:
		// a head that differs in its declarations alone is not written.
		if !os.is_file(path) {
			staged := path + '.tmp.' + tempname.unique_token()
			os.write_file(staged, head.join(''))!
			publish_native_c_object(staged, path)!
		}
		canonical_path := os.real_path(path)
		dependencies := v3_tcc_dylib_program_dependencies('/usr/bin/clang', args, path, '-std=gnu11')!
		// The TinyCC framework include directory contains symlinks to SDK Headers.
		// Canonical manifest paths retain their owning .framework for link selection.
		canonical := dependencies.map(os.real_path(it))
		mut reported := []string{cap: dependencies.len}
		for i, dependency in dependencies {
			if canonical[i] != canonical_path { reported << dependency }
		}
		v3_fastc_write_program_dependencies(manifest, reported, mut validation.stats)
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
			signature := v3_fastc_native_file_signature(resolved, false, mut stats)
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
		cached := v3_fastc_read_program_dependencies(manifest)?
		if !v3_fastc_native_signatures_match(cached.dependencies, cached.signatures, cached.reported, jobs) {
			return none
		}
		return cached.dependencies
	}

	// v3_fastc_read_program_dependencies reads a program dependency manifest without
	// validating the files it names.
	fn v3_fastc_read_program_dependencies(manifest string) ?V3FastcNativeManifest {
		lines := (os.read_file(manifest) or { return none }).split_into_lines()
		if lines.len < 2 || lines[0] != v3_fastc_program_dependencies_format { return none }
		mut dependencies := []string{cap: lines.len - 1}
		mut signatures := []string{cap: lines.len - 1}
		mut reported := []string{cap: lines.len - 1}
		for line in lines[1..] {
			if !line.starts_with('dependency=') { return none }
			entry := line.all_after('dependency=')
			signature_tab := entry.last_index('\t') or { return none }
			paths := entry[..signature_tab]
			resolved_tab := paths.last_index('\t') or { return none }
			if resolved_tab == 0 || resolved_tab + 1 >= paths.len || signature_tab + 1 >= entry.len {
				return none
			}
			// The workers that hash the files also check that each reported path
			// still resolves to the recorded file.
			reported << paths[..resolved_tab]
			dependencies << paths[resolved_tab + 1..]
			signatures << entry[signature_tab + 1..]
		}
		return V3FastcNativeManifest{ dependencies: dependencies, signatures: signatures, reported: reported }
	}

	// v3_fastc_preprocessor_lines keeps the lines of generated C that can change what
	// the preprocessor opens: directives (also after a comment, or spelled `%:`), comment
	// delimiters, and continued lines. The other lines are ordinary declarations. Nothing
	// is opened after the last line that can include a file, so the lines after it (the
	// constants of the program, as macros) are left out as well.
	fn v3_fastc_preprocessor_lines(text string) string {
		mut out := strings.new_builder(text.len / 8)
		mut start := 0
		mut continued := false
		mut include_end := 0
		for start < text.len {
			mut end := start
			mut keep := continued
			mut includes := false
			// A line that begins with another directive holds no include: the `#` of
			// a macro body is text. A comment that ends on the line can hide that start.
			mut first := true
			mut other_directive := false
			mut closes_comment := false
			for end < text.len && text[end] != `\n` {
				c := text[end]
				is_directive := c == `#` || (c == `%` && end + 1 < text.len && text[end + 1] == `:`)
				if is_directive || (c == `/` && end + 1 < text.len && text[end + 1] == `*`) {
					keep = true
				}
				if c == `*` && end + 1 < text.len && text[end + 1] == `/` {
					keep = true
					closes_comment = true
				}
				if is_directive {
					// `#include`, `#import`, a comment before either, or a spliced line.
					mut word := if c == `#` { end + 1 } else { end + 2 }
					for word < text.len && text[word] in [` `, `\t`] {
						word++
					}
					rest := text[word..if word + 8 < text.len { word + 8 } else { text.len }]
					if rest.starts_with('include') || rest.starts_with('import') || rest.starts_with('/*')
						|| rest.starts_with('\\') || rest.len == 0 || rest[0] in [
						`\n`,
						`\r`,
					] {
						includes = true
					} else if first {
						other_directive = true
					}
				}
				if c !in [` `, `\t`] {
					first = false
				}
				end++
			}
			mut last := end - 1
			if last >= start && text[last] == `\r` {
				last--
			}
			if other_directive && !closes_comment {
				includes = false
			}
			includes = includes || continued
			continued = last >= start && text[last] == `\\`
			if keep || continued {
				out.write_string(text[start..end])
				out.write_u8(`\n`)
				if includes || continued {
					include_end = out.len
				}
			}
			start = end + 1
		}
		return out.str()[..include_end]
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
