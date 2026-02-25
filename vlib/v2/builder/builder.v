// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v2.ast
import v2.abi
import v2.gen.arm64
import v2.gen.c
import v2.gen.cleanc
import v2.gen.v as gen_v
import v2.gen.x64
import v2.insel
import v2.markused
import v2.mir
import v2.pref
import v2.ssa
import v2.ssa.optimize
import v2.token
import v2.transformer
import v2.types
import time

struct Builder {
	pref &pref.Preferences
mut:
	files               []ast.File
	user_files          []string // original user-provided files (for output name)
	file_set            &token.FileSet     = token.FileSet.new()
	env                 &types.Environment = unsafe { nil } // Type checker environment
	parsed_full_files_n int
	parsed_vh_files_n   int
	entry_v_lines_n     int
	parsed_v_lines_n    int
	parsed_full_files   []string
	parsed_vh_files     []string
	used_fn_keys        map[string]bool
}

pub fn new_builder(prefs &pref.Preferences) &Builder {
	unsafe {
		return &Builder{
			pref:         prefs
			used_fn_keys: map[string]bool{}
		}
	}
}

pub fn (mut b Builder) build(files []string) {
	b.user_files = files
	mut sw := time.new_stopwatch()
	$if parallel ? {
		b.files = if b.pref.no_parallel {
			b.parse_files(files)
		} else {
			b.parse_files_parallel(files)
		}
	} $else {
		b.files = b.parse_files(files)
	}
	parse_time := sw.elapsed()
	print_time('Scan & Parse', parse_time)
	b.update_parse_summary_counts()
	print_parse_summary(b.parsed_full_files_n, b.parsed_vh_files_n, b.entry_v_lines_n,
		b.parsed_v_lines_n, b.pref.stats, b.pref.print_parsed_files, b.parsed_full_files,
		b.parsed_vh_files)
	if b.pref.stats {
		// b.print_flat_ast_summary()
	}

	if b.pref.skip_type_check {
		b.env = types.Environment.new()
	} else {
		$if parallel ? {
			b.env = if b.pref.no_parallel {
				b.type_check_files()
			} else {
				b.type_check_files_parallel()
			}
		} $else {
			b.env = b.type_check_files()
		}
	}
	type_check_time := time.Duration(sw.elapsed() - parse_time)
	print_time('Type Check', type_check_time)

	// Transform AST (flag enum desugaring, etc.)
	transform_start := sw.elapsed()
	mut trans := transformer.Transformer.new_with_pref(b.files, b.env, b.pref)
	trans.set_file_set(b.file_set)
	b.files = trans.transform_files(b.files)
	transform_time := time.Duration(sw.elapsed() - transform_start)
	print_time('Transform', transform_time)

	// Mark used functions/methods for backend pruning.
	if b.pref.no_markused {
		b.used_fn_keys = map[string]bool{}
	} else {
		mark_used_start := sw.elapsed()
		b.used_fn_keys = markused.mark_used(b.files, b.env)
		mark_used_time := time.Duration(sw.elapsed() - mark_used_start)
		print_time('Mark Used', mark_used_time)
	}

	// Generate output based on backend
	match b.pref.backend {
		.v {
			if !b.pref.skip_genv {
				b.gen_v_files()
			}
		}
		.cleanc {
			b.gen_cleanc()
		}
		.c {
			b.gen_ssa_c()
		}
		.x64 {
			b.gen_native(.x64)
		}
		.arm64 {
			b.gen_native(.arm64)
		}
	}

	print_time('Total', sw.elapsed())
}

fn (mut b Builder) gen_v_files() {
	mut gen := gen_v.new_gen(b.pref)
	for file in b.files {
		gen.gen(file)
		if b.pref.debug {
			gen.print_output()
		}
	}
}

fn (mut b Builder) gen_cleanc() {
	// Clean C Backend (AST -> C)
	mut sw := time.new_stopwatch()

	// The cached-core split is currently unstable during cmd/v2 self-host
	// bootstrapping (v2 -> v3 -> v4). Force single-unit cleanc generation there.
	force_no_cache := b.is_cmd_v2_self_build()
	use_cache := !b.pref.no_cache && !force_no_cache

	// Determine output name
	output_name := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		os.file_name(b.user_files.last()).all_before_last('.v')
	} else {
		'out'
	}

	cc := os.getenv_opt('V2CC') or { default_cc(b.pref.vroot) }
	cc_flags := (os.getenv_opt('V2CFLAGS') or { '' }) + ' ' + tcc_flags(cc, b.pref.vroot)
	mut error_limit_flag := ''
	if !cc.contains('tcc') {
		version_res := os.execute('${cc} --version')
		if version_res.exit_code == 0 && version_res.output.contains('clang') {
			error_limit_flag = ' -ferror-limit=0'
		}
	}

	// If output ends with .c, just write the C file
	if output_name.ends_with('.c') {
		mut c_source := ''
		// For .c output, prefer the same cached-core split used by normal
		// build+link flow, when the cache is valid.
		if use_cache && !b.pref.skip_builtin && b.has_module('builtin') && b.has_module('strconv')
			&& b.can_use_cached_core_headers() {
			main_modules := b.collect_modules_excluding(core_cached_module_names)
			if main_modules.len > 0 {
				b.ensure_core_module_headers()
				c_source = b.gen_cleanc_source(main_modules)
			}
		}
		if c_source == '' {
			c_source = b.gen_cleanc_source([]string{})
		}
		print_time('C Gen', sw.elapsed())
		if c_source == '' {
			eprintln('error: cleanc backend is not fully functional (compiled with stubbed functions)')
			eprintln('hint: use v2 compiled with v1 for proper C code generation')
			return
		}
		os.write_file(output_name, c_source) or { panic(err) }
		println('[*] Wrote ${output_name}')
		return
	}

	// Fast path: cache one core object (builtin+strconv), compile/link only the rest.
	if use_cache && !b.pref.skip_builtin && b.has_module('builtin') && b.has_module('strconv') {
		if b.gen_cleanc_with_cached_core(output_name, cc, cc_flags, error_limit_flag, mut
			sw)
		{
			return
		}
	}

	// Fallback: compile one full C translation unit.
	c_source := b.gen_cleanc_source([]string{})
	print_time('C Gen', sw.elapsed())
	if c_source == '' {
		eprintln('error: cleanc backend is not fully functional (compiled with stubbed functions)')
		eprintln('hint: use v2 compiled with v1 for proper C code generation')
		return
	}

	c_file := output_name + '.c'
	os.write_file(c_file, c_source) or { panic(err) }
	println('[*] Wrote ${c_file}')

	cc_start := sw.elapsed()
	cc_cmd := '${cc} ${cc_flags} -w "${c_file}" -o "${output_name}"${error_limit_flag}'
	run_cc_cmd_or_exit(cc_cmd, 'C compilation', b.pref.show_cc)
	print_time('CC', time.Duration(sw.elapsed() - cc_start))

	println('[*] Compiled ${output_name}')
}

fn (b &Builder) is_cmd_v2_self_build() bool {
	if b.user_files.len != 1 {
		return false
	}
	// Avoid path normalization here: during bootstraps, some intermediate
	// compilers can still have unstable path helpers.
	path := b.user_files[0].replace('\\', '/')
	if path == 'v2.v' || path.ends_with('/v2.v') {
		return true
	}
	return path.ends_with('/cmd/v2/v2.v') || path.ends_with('cmd/v2/v2.v')
}

fn (mut b Builder) gen_ssa_c() {
	// SSA -> C backend.
	mut sw := time.new_stopwatch()

	mut mod := ssa.Module.new('main')
	if mod == unsafe { nil } {
		eprintln('error: ssa c backend not available (compiled with stubbed ssa module)')
		eprintln('hint: use v2 compiled with v1 for ssa c code generation')
		return
	}
	mut ssa_builder := ssa.Builder.new_with_env(mod, b.env)

	mut stage_start := sw.elapsed()
	ssa_builder.build_all(b.files)
	print_time('SSA Build', time.Duration(sw.elapsed() - stage_start))

	// TODO: re-enable SSA optimization once the new builder is mature
	// stage_start = sw.elapsed()
	// optimize.optimize(mut mod)
	// print_time('SSA Optimize', time.Duration(sw.elapsed() - stage_start))

	cc := os.getenv_opt('V2CC') or { default_cc(b.pref.vroot) }
	cc_flags := (os.getenv_opt('V2CFLAGS') or { '' }) + ' ' + tcc_flags(cc, b.pref.vroot)
	mut error_limit_flag := ''
	if !cc.contains('tcc') {
		version_res := os.execute('${cc} --version')
		if version_res.exit_code == 0 && version_res.output.contains('clang') {
			error_limit_flag = ' -ferror-limit=0'
		}
	}

	// Try to get pre-compiled builtin.o and vlib.o from the cleanc cache
	mut builtin_obj := ''
	mut vlib_obj := ''
	if !b.pref.skip_builtin && b.has_module('builtin') && b.has_module('strconv') {
		cache_dir := b.core_cache_dir()
		os.mkdir_all(cache_dir) or {}
		builtin_obj = b.ensure_cached_module_object(cache_dir, builtin_cache_name, builtin_cached_module_paths,
			builtin_cached_module_names, cc, cc_flags, error_limit_flag) or { '' }
		if builtin_obj.len > 0 && vlib_cached_module_paths.len > 0 {
			vlib_obj = b.ensure_cached_module_object(cache_dir, vlib_cache_name, vlib_cached_module_paths,
				vlib_cached_module_names, cc, cc_flags, error_limit_flag) or { '' }
		}
	}

	stage_start = sw.elapsed()
	mut gen := c.Gen.new(mod)
	gen.link_builtin = builtin_obj.len > 0
	c_source := gen.gen()
	print_time('C Gen', time.Duration(sw.elapsed() - stage_start))
	if c_source == '' {
		eprintln('error: ssa c backend failed to generate C source')
		return
	}

	output_name := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		os.file_name(b.user_files.last()).all_before_last('.v')
	} else {
		'out'
	}

	if output_name.ends_with('.c') {
		os.write_file(output_name, c_source) or { panic(err) }
		println('[*] Wrote ${output_name}')
		return
	}

	c_file := output_name + '.c'
	os.write_file(c_file, c_source) or { panic(err) }
	println('[*] Wrote ${c_file}')

	cc_start := sw.elapsed()
	mut cc_cmd := ''
	if builtin_obj.len > 0 {
		// Compile SSA main.c and link against pre-compiled builtin.o
		main_obj := output_name + '.main.o'
		compile_cmd := '${cc} ${cc_flags} -w -c "${c_file}" -o "${main_obj}"${error_limit_flag}'
		if b.pref.show_cc {
			println(compile_cmd)
		}
		compile_res := os.execute(compile_cmd)
		if compile_res.exit_code != 0 {
			eprintln('error: ssa c backend compilation failed')
			lines := compile_res.output.split_into_lines()
			limit := if lines.len < 20 { lines.len } else { 20 }
			for line in lines[..limit] {
				eprintln(line)
			}
			exit(1)
		}
		mut link_objects := '"${main_obj}" "${builtin_obj}"'
		if vlib_obj.len > 0 {
			link_objects += ' "${vlib_obj}"'
		}
		cc_cmd = '${cc} ${cc_flags} -w ${link_objects} -o "${output_name}"'
		if b.pref.show_cc {
			println(cc_cmd)
		}
		cc_res := os.execute(cc_cmd)
		if cc_res.exit_code != 0 {
			eprintln('error: ssa c backend linking failed')
			lines := cc_res.output.split_into_lines()
			limit := if lines.len < 20 { lines.len } else { 20 }
			for line in lines[..limit] {
				eprintln(line)
			}
			exit(1)
		}
		os.rm(main_obj) or {}
	} else {
		// Single-file compilation (no builtin linking)
		cc_cmd = '${cc} ${cc_flags} -w "${c_file}" -o "${output_name}"${error_limit_flag}'
		if b.pref.show_cc {
			println(cc_cmd)
		} else if os.getenv('V2VERBOSE') != '' {
			dump(cc_cmd)
		}
		cc_res := os.execute(cc_cmd)
		if cc_res.exit_code != 0 {
			eprintln('error: ssa c backend compilation failed')
			lines := cc_res.output.split_into_lines()
			limit := if lines.len < 20 { lines.len } else { 20 }
			for line in lines[..limit] {
				eprintln(line)
			}
			exit(1)
		}
	}
	print_time('CC', time.Duration(sw.elapsed() - cc_start))

	if !b.pref.keep_c {
		os.rm(c_file) or {}
	}
	println('[*] Compiled ${output_name}')
}

fn (mut b Builder) gen_cleanc_source(modules []string) string {
	return b.gen_cleanc_source_with_options(modules, false, '', []string{}, true)
}

fn (mut b Builder) gen_cleanc_source_for_cache(modules []string, cache_bundle_name string) string {
	return b.gen_cleanc_source_with_options(modules, true, cache_bundle_name, []string{},
		false)
}

fn (mut b Builder) gen_cleanc_source_with_cache_init_calls(modules []string, cached_init_calls []string) string {
	return b.gen_cleanc_source_with_options(modules, false, '', cached_init_calls, true)
}

fn (mut b Builder) gen_cleanc_source_with_options(modules []string, export_const_symbols bool, cache_bundle_name string, cached_init_calls []string, use_markused bool) string {
	mut gen := cleanc.Gen.new_with_env_and_pref(b.files, b.env, b.pref)
	if modules.len > 0 {
		gen.set_emit_modules(modules)
	}
	if use_markused && b.used_fn_keys.len > 0 {
		gen.set_used_fn_keys(b.used_fn_keys)
	}
	gen.set_export_const_symbols(export_const_symbols)
	if cache_bundle_name.len > 0 {
		gen.set_cache_bundle_name(cache_bundle_name)
	}
	if cached_init_calls.len > 0 {
		gen.set_cached_init_calls(cached_init_calls)
	}
	return gen.gen()
}

fn (mut b Builder) gen_cleanc_with_cached_core(output_name string, cc string, cc_flags string, error_limit_flag string, mut sw time.StopWatch) bool {
	main_modules := b.collect_modules_excluding(core_cached_module_names)
	if main_modules.len == 0 {
		return false
	}

	cache_dir := b.core_cache_dir()
	os.mkdir_all(cache_dir) or {
		// If we cannot create cache dir, fall back to full compilation.
		return false
	}

	builtin_obj := b.ensure_cached_module_object(cache_dir, builtin_cache_name, builtin_cached_module_paths,
		builtin_cached_module_names, cc, cc_flags, error_limit_flag) or { return false }
	mut vlib_obj := ''
	if vlib_cached_module_paths.len > 0 {
		vlib_obj = b.ensure_cached_module_object(cache_dir, vlib_cache_name, vlib_cached_module_paths,
			vlib_cached_module_names, cc, cc_flags, error_limit_flag) or { return false }
	}
	b.ensure_core_module_headers()

	// When TCC is the default compiler but fell back to cc for cache
	// compilation (e.g. due to TCC not supporting certain C constructs),
	// the cached .o files are Mach-O (from cc) while TCC would produce ELF.
	// Detect this mismatch and use cc for main compilation and linking too.
	mut main_cc := cc
	mut main_cc_flags := cc_flags
	if cc.contains('tcc') && os.exists(builtin_obj) {
		bytes := os.read_bytes(builtin_obj) or { []u8{} }
		is_elf := bytes.len >= 4 && bytes[0] == 0x7f && bytes[1] == 0x45 && bytes[2] == 0x4c
			&& bytes[3] == 0x46
		if !is_elf {
			// Cached .o was compiled by cc (via TCC fallback), not TCC.
			// Use cc for main compilation and linking to match formats.
			main_cc = 'cc'
			main_cc_flags = (os.getenv_opt('V2CFLAGS') or { '' })
		}
	}

	mut cached_init_calls := []string{}
	cached_init_calls << '__v2_cached_init_${builtin_cache_name}'
	if vlib_obj.len > 0 {
		cached_init_calls << '__v2_cached_init_${vlib_cache_name}'
	}
	main_source := b.gen_cleanc_source_with_cache_init_calls(main_modules, cached_init_calls)
	print_time('C Gen', sw.elapsed())
	if main_source == '' {
		return false
	}

	main_c_file := output_name + '.c'
	os.write_file(main_c_file, main_source) or { return false }
	println('[*] Wrote ${main_c_file}')

	cc_start := sw.elapsed()
	main_obj := output_name + '.main.o'
	compile_main_cmd := '${main_cc} ${main_cc_flags} -w -c "${main_c_file}" -o "${main_obj}"${error_limit_flag}'
	main_fell_back := run_cc_cmd_or_exit(compile_main_cmd, 'C compilation', b.pref.show_cc)
	if main_fell_back && main_cc.contains('tcc') {
		// TCC failed on main.c but cached .o files are ELF (from TCC).
		// Fallback produced Mach-O main.o — can't link with ELF cache.
		// Fall back to non-cached full compilation.
		os.rm(main_obj) or {}
		return false
	}
	mut link_cmd := '${main_cc} ${main_cc_flags} -w "${main_obj}" "${builtin_obj}"'
	if vlib_obj.len > 0 {
		link_cmd += ' "${vlib_obj}"'
	}
	link_cmd += ' -o "${output_name}"'
	run_cc_cmd_or_exit(link_cmd, 'Linking', b.pref.show_cc)
	print_time('CC', time.Duration(sw.elapsed() - cc_start))

	os.rm(main_obj) or {}
	println('[*] Compiled ${output_name}')
	return true
}

fn (mut b Builder) ensure_cached_module_object(cache_dir string, cache_name string, module_paths []string, emit_modules []string, cc string, cc_flags string, error_limit_flag string) !string {
	obj_path := os.join_path(cache_dir, '${cache_name}.o')
	stamp_path := os.join_path(cache_dir, '${cache_name}.stamp')
	c_path := os.join_path(cache_dir, '${cache_name}.c')
	expected_stamp := b.cache_stamp_for_modules(cache_name, module_paths, cc, cc_flags)
	if os.exists(obj_path) && os.exists(stamp_path) {
		if current_stamp := os.read_file(stamp_path) {
			if current_stamp == expected_stamp {
				if os.getenv('V2VERBOSE') != '' {
					println('[*] Reusing ${obj_path}')
				}
				return obj_path
			}
		}
	}

	module_source := b.gen_cleanc_source_for_cache(emit_modules, cache_name)
	if module_source == '' {
		return error('failed to generate C source for ${cache_name}')
	}
	os.write_file(c_path, module_source)!

	compile_cmd := '${cc} ${cc_flags} -w -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	os.write_file(stamp_path, expected_stamp)!
	return obj_path
}

fn (b &Builder) has_module(module_name string) bool {
	for file in b.files {
		if file_module_name(file) == module_name {
			return true
		}
	}
	return false
}

fn (b &Builder) collect_modules_excluding(excluded []string) []string {
	mut excluded_set := map[string]bool{}
	for module_name in excluded {
		excluded_set[module_name] = true
	}
	mut modules_set := map[string]bool{}
	for file in b.files {
		module_name := file_module_name(file)
		if module_name in excluded_set {
			continue
		}
		modules_set[module_name] = true
	}
	mut modules := modules_set.keys()
	modules.sort()
	return modules
}

fn file_module_name(file ast.File) string {
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			return stmt.name.replace('.', '_')
		}
	}
	return 'main'
}

fn default_cc(vroot string) string {
	// Try to use tcc by default, like v1 does.
	tcc_path := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	if os.exists(tcc_path) {
		return tcc_path
	}
	return 'cc'
}

fn tcc_flags(cc string, vroot string) string {
	if !cc.contains('tcc') {
		return ''
	}
	tcc_dir := os.join_path(vroot, 'thirdparty', 'tcc')
	return '-I "${os.join_path(tcc_dir, 'lib', 'include')}" -L "${os.join_path(tcc_dir,
		'lib')}"'
}

// run_cc_cmd_or_exit runs a C compiler command, falling back from tcc to cc
// if needed. Returns true if tcc fell back to cc.
fn run_cc_cmd_or_exit(cmd string, stage string, show_cc bool) bool {
	if show_cc {
		println(cmd)
	} else if os.getenv('V2VERBOSE') != '' {
		dump(cmd)
	}
	result := os.execute(cmd)
	if result.exit_code != 0 {
		// If tcc failed, fall back to cc.
		// Check only the compiler binary (before the first space), not the full
		// command string which contains tcc in include/library flag paths.
		cc_binary := cmd.all_before(' ')
		if cc_binary.contains('tcc') {
			eprintln('Failed to compile with tcc, falling back to cc')
			eprintln('tcc cmd: ${cmd}')
			eprintln(result.output)
			fallback_cmd := cmd.replace_once(cc_binary, 'cc')
			run_cc_cmd_or_exit(fallback_cmd, stage, show_cc)
			return true
		}
		eprintln('${stage} failed:')
		lines := result.output.split_into_lines()
		limit := if lines.len < 50 { lines.len } else { 50 }
		for line in lines[..limit] {
			eprintln(line)
		}
		mut error_count := 0
		mut warning_count := 0
		for line in lines {
			if line.contains(': error:') || line.contains(': fatal error:') {
				error_count += 1
			} else if line.contains(': warning:') {
				warning_count += 1
			}
		}
		if stage == 'C compilation' {
			eprintln('Total: ${warning_count} warnings and ${error_count} errors')
		}
		exit(1)
	}
	return false
}

fn (mut b Builder) gen_native(backend_arch pref.Arch) {
	arch := if backend_arch == .auto { b.pref.get_effective_arch() } else { backend_arch }

	// Build all files into a single SSA module
	mut mod := ssa.Module.new('main')
	if mod == unsafe { nil } {
		eprintln('error: native backend not available (compiled with stubbed ssa module)')
		eprintln('hint: use v2 compiled with v1 for native code generation')
		return
	}
	mut ssa_builder := ssa.Builder.new_with_env(mod, b.env)
	mut native_sw := time.new_stopwatch()

	// Build all files together with proper multi-file ordering
	mut stage_start := native_sw.elapsed()
	ssa_builder.build_all(b.files)
	print_time('SSA Build', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	optimize.optimize(mut mod)
	print_time('SSA Optimize', time.Duration(native_sw.elapsed() - stage_start))
	$if debug {
		optimize.verify_and_panic(mod, 'full optimization')
	}

	stage_start = native_sw.elapsed()
	mut mir_mod := mir.lower_from_ssa(mod)
	print_time('MIR Lower', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	abi.lower(mut mir_mod, arch)
	print_time('ABI Lower', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	insel.select(mut mir_mod, arch)
	print_time('InsSel', time.Duration(native_sw.elapsed() - stage_start))

	// Determine output binary name from the last user file
	output_binary := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		os.file_name(b.user_files.last()).all_before_last('.v')
	} else {
		'out'
	}

	if arch == .arm64 && os.user_os() == 'macos' {
		// Use built-in linker for ARM64 macOS
		mut gen := arm64.Gen.new(&mir_mod)
		gen.gen()
		gen.link_executable(output_binary)

		if b.pref.verbose {
			println('[*] Linked ${output_binary} (built-in linker)')
		}
	} else {
		// Generate object file and use external linker
		obj_file := 'main.o'

		if arch == .arm64 {
			mut gen := arm64.Gen.new(&mir_mod)
			gen.gen()
			gen.write_file(obj_file)
		} else {
			mut gen := x64.Gen.new(&mir_mod)
			gen.gen()
			gen.write_file(obj_file)
		}

		if b.pref.verbose {
			println('[*] Wrote ${obj_file}')
		}

		// Link the object file into an executable
		if os.user_os() == 'macos' {
			sdk_res := os.execute('xcrun -sdk macosx --show-sdk-path')
			sdk_path := sdk_res.output.trim_space()
			arch_flag := if arch == .arm64 { 'arm64' } else { 'x86_64' }
			link_cmd := 'ld -o ${output_binary} ${obj_file} -lSystem -syslibroot "${sdk_path}" -e _main -arch ${arch_flag} -platform_version macos 11.0.0 11.0.0'
			link_result := os.execute(link_cmd)
			if link_result.exit_code != 0 {
				eprintln('Link failed:')
				eprintln(link_result.output)
				exit(1)
			}
		} else {
			// Linux linking
			link_result := os.execute('cc ${obj_file} -o ${output_binary} -no-pie')
			if link_result.exit_code != 0 {
				eprintln('Link failed:')
				eprintln(link_result.output)
				exit(1)
			}
		}

		if b.pref.verbose {
			println('[*] Linked ${output_binary}')
		}

		// Clean up object file
		os.rm(obj_file) or {}
	}
}

fn print_time(title string, time_d time.Duration) {
	println(' * ${title}: ${time_d.milliseconds()}ms')
}

fn (mut b Builder) update_parse_summary_counts() {
	mut parsed_full_files_n := 0
	mut parsed_vh_files_n := 0
	mut parsed_full_files := []string{}
	mut parsed_vh_files := []string{}
	for file in b.files {
		if file.name.ends_with('.vh') {
			parsed_vh_files_n++
			parsed_vh_files << file.name
		} else {
			parsed_full_files_n++
			parsed_full_files << file.name
		}
	}
	b.parsed_full_files_n = parsed_full_files_n
	b.parsed_vh_files_n = parsed_vh_files_n
	b.parsed_full_files = parsed_full_files
	b.parsed_vh_files = parsed_vh_files
	if b.pref.stats {
		b.entry_v_lines_n = count_v_lines_for_paths(b.user_files)
		b.parsed_v_lines_n = b.count_parsed_v_lines()
	} else {
		b.entry_v_lines_n = 0
		b.parsed_v_lines_n = 0
	}
}

/*
fn (b &Builder) print_flat_ast_summary() {
	legacy_stats := ast.legacy_ast_stats(b.files)
	legacy_nodes := ast.count_legacy_nodes(b.files)
	flat := ast.flatten_files(b.files)
	flat_stats := flat.stats()
	mut mem_delta_pct := f64(0)
	if legacy_stats.bytes_estimate > 0 {
		mem_delta_pct = (f64(legacy_stats.bytes_estimate) - f64(flat_stats.bytes_estimate)) * 100.0 / f64(legacy_stats.bytes_estimate)
	}
	println(' * AST nodes: legacy=${legacy_nodes}, flat=${flat_stats.nodes}')
	println(' * AST memory est: legacy=${legacy_stats.bytes_estimate}B, flat=${flat_stats.bytes_estimate}B (${mem_delta_pct:.2f}% reduction)')
}
*/

fn count_v_lines_for_paths(paths []string) int {
	mut seen_paths := map[string]bool{}
	mut total_v_lines := 0
	for path in paths {
		norm_path := os.norm_path(path)
		if norm_path in seen_paths {
			continue
		}
		seen_paths[norm_path] = true
		lines := os.read_lines(norm_path) or { continue }
		total_v_lines += lines.len
	}
	return total_v_lines
}

fn (b &Builder) count_parsed_v_lines() int {
	mut parsed_paths := []string{}
	mut seen_files := map[string]bool{}
	for file in b.files {
		if file.name in seen_files {
			continue
		}
		seen_files[file.name] = true
		parsed_paths << file.name
	}
	return count_v_lines_for_paths(parsed_paths)
}

fn print_parse_summary(parsed_full_files_n int, parsed_vh_files_n int, entry_v_lines_n int, parsed_v_lines_n int, show_stats bool, print_parsed_files bool, parsed_full_files []string, parsed_vh_files []string) {
	println(' * Parsed files: fully parsed files: ${parsed_full_files_n}, parsed .vh files: ${parsed_vh_files_n}')
	if print_parsed_files {
		if parsed_full_files.len > 0 {
			println(' * Fully parsed files:')
			for path in parsed_full_files {
				println('   [full] ${path}')
			}
		}
		if parsed_vh_files.len > 0 {
			println(' * Parsed .vh files:')
			for path in parsed_vh_files {
				println('   [vh] ${path}')
			}
		}
	}
	if show_stats {
		println(' * Parsed V LOC (entry files): ${entry_v_lines_n}')
		println(' * Parsed V LOC (all parsed sources): ${parsed_v_lines_n}')
	}
}
