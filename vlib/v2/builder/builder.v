// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v2.ast
import v2.abi
import v2.eval
import v2.gen.arm64
import v2.gen.c
import v2.gen.cleanc
import v2.gen.v as gen_v
import v2.gen.x64
import v2.insel
import v2.markused
import v2.parser
import v2.mir
import v2.pref
import v2.ssa
import v2.ssa.optimize as ssa_optimize
import v2.token
import v2.transformer
import v2.types
import time
import runtime

const staged_c_file = '/tmp/v2_codegen.tmp.c'
const staged_main_obj_file = '/tmp/v2_codegen.tmp.main.o'

struct Builder {
	pref &pref.Preferences
mut:
	files                     []ast.File
	user_files                []string // original user-provided files (for output name)
	file_set                  &token.FileSet     = token.FileSet.new()
	env                       &types.Environment = unsafe { nil } // Type checker environment
	parsed_full_files_n       int
	parsed_vh_files_n         int
	entry_v_lines_n           int
	parsed_v_lines_n          int
	parsed_full_files         []string
	parsed_vh_files           []string
	used_fn_keys              map[string]bool
	cached_called_fn_names    map[string]bool
	used_vh_for_parse         bool
	used_import_vh_for_parse  bool
	used_virtual_vh_for_parse bool
	flat_roundtrip_enabled    bool // V2_FLAT_ROUNDTRIP=1: route parses through streaming + to_files()
	flat_check_enabled        bool // V2_CHECK_FLAT=1: route type-check through Checker.check_flat
	markused_flat_enabled     bool // V2_MARKUSED_FLAT=1: route markused through mark_used_flat shim
	// flat caches the FlatAst representation of b.files. When
	// flat_check_enabled is set, parse_batch streams directly into
	// flat_builder so b.flat is built incrementally during parsing rather
	// than via a redundant flatten_files() pass afterwards.
	flat         ast.FlatAst
	flat_builder ast.FlatBuilder
	// flat_builder_inited tracks whether flat_builder has been seeded with
	// pre-sized arenas. We can only size after we know the input set, so
	// the first parse_batch call lazily initializes it.
	flat_builder_inited bool
}

pub fn new_builder(prefs &pref.Preferences) &Builder {
	unsafe {
		return &Builder{
			pref:                   prefs
			used_fn_keys:           map[string]bool{}
			cached_called_fn_names: map[string]bool{}
			flat_roundtrip_enabled: os.getenv('V2_FLAT_ROUNDTRIP') != ''
			flat_check_enabled:     os.getenv('V2_CHECK_FLAT') != ''
			markused_flat_enabled:  os.getenv('V2_MARKUSED_FLAT') != ''
		}
	}
}

// exec_build_c_file returns the staging C file path for executable builds.
// A stable temp path avoids output-name-derived string construction in
// self-hosted arm64 binaries, where that path is currently unstable.
fn (b &Builder) exec_build_c_file(output_name string) string {
	if b.pref.keep_c {
		return output_name + '.c'
	}
	return staged_c_file
}

fn (mut b Builder) compile_cleanc_executable(output_name string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, mut sw time.StopWatch) {
	cc_start := sw.elapsed()
	if b.pref.is_shared_lib {
		// Shared library: compile with -shared -fPIC -undefined dynamic_lookup
		// Use -fvisibility=hidden so only explicitly exported (impl_live_*) symbols
		// are visible. All other functions become hidden, causing the dylib to
		// resolve them from the host executable at load time.
		mut cc_cmd := '${cc} ${cc_flags} -shared -fPIC -fvisibility=hidden -undefined dynamic_lookup -w -Wno-incompatible-function-pointer-types "${staged_c_file}"'
		if cc_link_flags.len > 0 {
			cc_cmd += ' -x none ${cc_link_flags}'
		}
		cc_cmd += ' -o "${output_name}"${error_limit_flag}'
		run_cc_cmd_or_exit(cc_cmd, 'shared lib compilation', b.pref.show_cc)
		print_time('CC (shared)', time.Duration(sw.elapsed() - cc_start))
		println('[*] Compiled shared library ${output_name}')
		return
	}
	// Non-cached path: compile and link in one step.
	// Place link flags (which may include .o files) AFTER the source file.
	// Use `-x none` to reset the language before .o files, since -x objective-c
	// would cause cc to treat .o files as source code.
	mut cc_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types "${staged_c_file}"'
	if cc_link_flags.len > 0 {
		cc_cmd += ' -x none ${cc_link_flags}'
	}
	cc_cmd += ' -o "${output_name}"${error_limit_flag}'
	run_cc_cmd_or_exit(cc_cmd, 'C compilation', b.pref.show_cc)
	print_time('CC', time.Duration(sw.elapsed() - cc_start))

	println('[*] Compiled ${output_name}')
}

// print_rss prints process RSS in MB at the given phase boundary.
// Gated on V2_MEM=1.
//
// CAVEAT: v2 is force-built with `-gc none` (see is_v2_compiler_target in
// vlib/v/pref/default.v), so allocations are never reclaimed. RSS is
// dominated by the OS's decision of which pages to keep resident, not by
// what the program is actually using. Run-to-run variance is huge
// (observed 500 MB to 4.8 GB on identical runs). Treat these numbers as
// a coarse signal, not a precision metric.
//
// For reliable peak-memory comparisons between optimizations, use
// `/usr/bin/time -l <cmd>` and read `peak memory footprint` — that
// metric is stable to within ~0.1% across runs.
fn print_rss(stage string) {
	if os.getenv('V2_MEM') == '' {
		return
	}
	bytes := runtime.used_memory() or { 0 }
	eprintln('  [mem] ${stage}: ${bytes / (1024 * 1024)} MB')
}

// print_heap reports retained heap size after a forced GC, in MB. Unlike
// print_rss, this would give a stable measurement of memory the program
// is actually holding alive — but ONLY when built with a real GC.
//
// CURRENTLY A NO-OP FOR v2 SELF-HOST: v2 is forced to `-gc none`, where
// `gc_collect()` is a NOP and `gc_memory_use()` always returns 0. If
// you build v2 with an explicit GC (bypassing is_v2_compiler_target) this
// becomes useful. Until then, use `/usr/bin/time -l` for peak readings.
fn print_heap(stage string) {
	if os.getenv('V2_HEAP') == '' {
		return
	}
	gc_collect()
	bytes := gc_memory_use()
	eprintln('  [heap] ${stage}: ${bytes / (1024 * 1024)} MB')
}

pub fn (mut b Builder) build(files []string) {
	b.user_files = files
	mut sw := time.new_stopwatch()
	print_rss('start')
	print_heap('start')
	$if parallel ? {
		if b.flat_roundtrip_enabled && !b.pref.no_parallel {
			eprintln('warning: V2_FLAT_ROUNDTRIP=1 only routes through the serial parser; pass --no-parallel to exercise it')
		}
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
	print_rss('after parse')
	print_heap('after parse')
	if b.flat_check_enabled {
		// FlatBuilder is the canonical parse output; both parse paths stream
		// into it. parse_files / parse_files_parallel return [] in flat
		// mode, so b.flat is the live source of truth from here on. The
		// rehydration to legacy []ast.File is deferred until the transformer
		// (the first consumer that still needs it) actually starts —
		// keeping ~120MB of legacy AST out of memory during the type check
		// phase, which is the current memory peak.
		b.flat = b.flat_builder.flat
	}
	b.update_parse_summary_counts()
	print_parse_summary(b.parsed_full_files_n, b.parsed_vh_files_n, b.entry_v_lines_n,
		b.parsed_v_lines_n, b.pref.stats, b.pref.print_parsed_files, b.parsed_full_files,
		b.parsed_vh_files)
	if b.pref.stats {
		b.print_flat_ast_summary()
	}

	if b.pref.skip_type_check {
		b.env = types.Environment.new()
	} else {
		b.env = if b.pref.no_parallel {
			b.type_check_files()
		} else {
			b.type_check_files_parallel()
		}
	}
	type_check_time := time.Duration(sw.elapsed() - parse_time)
	print_time('Type Check', type_check_time)
	print_rss('after type check')
	print_heap('after type check')

	// Transform AST (flag enum desugaring, etc.)
	transform_start := sw.elapsed()
	mut trans := transformer.Transformer.new_with_pref(b.env, b.pref)
	trans.set_file_set(b.file_set)
	sequential_transform := b.pref.no_parallel_transform || b.pref.ownership
	use_flat_markused := b.markused_flat_enabled && b.flat_check_enabled
	// Both paths can now consume flat directly: sequential streams via
	// transform_files_from_flat, parallel streams per-worker via
	// to_files_range. No up-front full rehydration needed in either case.
	//
	// When V2_MARKUSED_FLAT is also enabled, both transform paths route
	// through their *_to_flat wedge so the post-transform flatten lives
	// inside the transformer call (one round-trip), avoiding a separate
	// flatten_files() pass before mark_used_flat.
	mut flat_populated_by_transform := false
	if sequential_transform {
		if use_flat_markused {
			new_flat, files_out := trans.transform_files_to_flat(&b.flat, b.files)
			b.flat = new_flat
			b.files = files_out
			flat_populated_by_transform = true
		} else if b.flat_check_enabled {
			b.files = trans.transform_files_from_flat(&b.flat, b.files)
		} else {
			b.files = trans.transform_files(b.files)
		}
	} else {
		if use_flat_markused {
			new_flat, files_out := b.transform_files_parallel_to_flat(mut trans)
			b.flat = new_flat
			b.files = files_out
			flat_populated_by_transform = true
		} else if b.flat_check_enabled {
			b.files = b.transform_files_parallel_from_flat(mut trans)
		} else {
			b.files = b.transform_files_parallel(mut trans)
		}
	}
	transform_time := time.Duration(sw.elapsed() - transform_start)
	print_time('Transform', transform_time)
	print_rss('after transform')
	print_heap('after transform')

	// Mark used functions/methods for backend pruning.
	if b.pref.no_markused {
		b.used_fn_keys = map[string]bool{}
	} else {
		mark_used_start := sw.elapsed()
		// V2_MARKUSED_FLAT only takes effect when V2_CHECK_FLAT is also on,
		// since b.flat is only populated when flat_check_enabled streams
		// parses into flat_builder. Without that, b.flat is empty and the
		// shim would walk nothing.
		//
		// The transformer mutates b.files but does not write back into
		// b.flat. Both sequential and parallel paths now populate b.flat
		// as part of their *_to_flat wedge when V2_MARKUSED_FLAT is on,
		// so the separate flatten_files() pass is gone. The branch below
		// remains as a defensive fallback for any future code path that
		// reaches markused without having set flat_populated_by_transform.
		if use_flat_markused && !flat_populated_by_transform {
			b.flat = ast.flatten_files(b.files)
		}
		if b.uses_minimal_windows_x64_runtime() {
			opts := markused.MarkUsedOptions{
				minimal_runtime_roots: true
			}
			b.used_fn_keys = if use_flat_markused {
				markused.mark_used_flat_with_options(&b.flat, b.env, opts)
			} else {
				markused.mark_used_with_options(b.files, b.env, opts)
			}
		} else {
			b.used_fn_keys = if use_flat_markused {
				markused.mark_used_flat(&b.flat, b.env)
			} else {
				markused.mark_used(b.files, b.env)
			}
		}
		mark_used_time := time.Duration(sw.elapsed() - mark_used_start)
		print_time('Mark Used', mark_used_time)
		// b.flat is unused by the codegen path; drop the arenas so a GC build
		// can reclaim them. Under -gc none this is a no-op for peak memory,
		// but it documents the lifetime correctly for the eventual GC switch.
		b.flat = ast.FlatAst{}
		print_rss('after markused')
		print_heap('after markused')
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
		.eval {
			mut runner := eval.new(b.pref)
			runner.run_files(b.files) or {
				eprintln('error: ${err.msg()}')
				exit(1)
			}
		}
	}

	print_time('Total', sw.elapsed())
	print_rss('after codegen (peak)')
	print_heap('after codegen')
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

	// The cached-core split is currently unstable in some module builds
	// (including cmd/v2 self-host and directory-style user module builds).
	// Force single-unit cleanc generation there.
	force_no_cache := b.should_disable_cleanc_cache()
	use_cache := !b.pref.no_cache && !force_no_cache
	if os.getenv('V2_TRACE_CACHE') != '' {
		eprintln('TRACE_CACHE use_cache=${use_cache} no_cache=${b.pref.no_cache} force_no_cache=${force_no_cache} self_build=${b.is_cmd_v2_self_build()} files=${b.user_files}')
	}

	// Determine output name
	output_name := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		b.default_output_name()
	} else {
		'out'
	}

	mut cc := if b.pref.ccompiler.len > 0 {
		b.pref.ccompiler
	} else {
		configured_cc(b.pref.vroot)
	}
	// -prod requires a real optimizing compiler — TCC cannot handle -O3/-flto.
	// Switch to system cc (gcc/clang) when the default compiler is TCC.
	if b.pref.is_prod && cc.contains('tcc') {
		cc = 'cc'
	}
	directive_flags := b.collect_cflags_from_sources()
	$if macos {
		if cc.contains('tcc') && cflags_need_objc_mode(directive_flags) {
			cc = 'cc'
		}
	}
	// Separate directive flags into compile-only and link-only flags.
	// -framework, -l, -L, .o/.a/.so/.dylib are linker flags and must NOT
	// be passed during -c compilation (they can trigger unwanted header
	// processing, e.g. MetalKit SIMD errors on macOS).
	directive_compile_flags, directive_link_flags := split_compile_and_link_flags(directive_flags)
	mut cc_flag_parts := []string{}
	mut cc_link_parts := []string{}
	env_flags := configured_cflags()
	if env_flags.trim_space() != '' {
		cc_flag_parts << env_flags.trim_space()
	}
	if cc.contains('tcc') && directive_flags.contains('thirdparty/sqlite/sqlite3.c')
		&& !directive_compile_flags.contains('SQLITE_DISABLE_INTRINSIC') {
		cc_flag_parts << '-DSQLITE_DISABLE_INTRINSIC'
	}
	if directive_compile_flags.trim_space() != '' {
		cc_flag_parts << directive_compile_flags.trim_space()
	}
	if directive_link_flags.trim_space() != '' {
		cc_link_parts << directive_link_flags.trim_space()
	}
	tcc_extra := tcc_flags(cc, b.pref.vroot)
	if tcc_extra.trim_space() != '' {
		cc_flag_parts << tcc_extra.trim_space()
	}
	// macOS code can include Objective-C (.m) files via #include directives.
	// Tell the C compiler to treat the source as Objective-C only when needed.
	$if macos {
		if cflags_need_objc_mode(directive_flags) {
			cc_flag_parts << '-x objective-c'
		}
	}
	cc_flag_parts << '-std=gnu11'
	cc_flag_parts << '-fwrapv'

	// Detect compiler type for optimization flags and error limit.
	is_tcc := cc.contains('tcc')
	mut is_clang := false
	if !is_tcc {
		version_res := os.execute('${cc} --version')
		if version_res.exit_code == 0 && version_res.output.contains('clang') {
			is_clang = true
		}
	}

	// -prod: add -O3, -flto, -DNDEBUG for gcc/clang
	if b.pref.is_prod {
		cc_flag_parts << '-O3'
		cc_flag_parts << '-DNDEBUG'
		if !b.pref.is_shared_lib {
			$if !windows {
				cc_flag_parts << '-flto'
			}
		}
		if !is_clang {
			cc_flag_parts << '-fno-strict-aliasing'
		}
	}

	cc_flags := cc_flag_parts.join(' ')
	cc_link_flags := cc_link_parts.join(' ')
	mut error_limit_flag := ''
	if is_clang {
		error_limit_flag = ' -ferror-limit=0'
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
		if b.gen_cleanc_with_cached_core(output_name, cc, cc_flags, cc_link_flags,
			error_limit_flag, mut sw)
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
	os.write_file(staged_c_file, c_source) or { panic(err) }
	println('[*] Wrote ${staged_c_file}')
	b.compile_cleanc_executable(output_name, cc, cc_flags, cc_link_flags, error_limit_flag, mut sw)
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

fn (b &Builder) should_disable_cleanc_cache() bool {
	// ARM64 cache previously disabled due to runtime helpers being emitted
	// as static inline, which dropped them at cached-core boundaries.
	// Fixed: __v2_array_eq is now a regular function with its body in the
	// builtin cache unit and a forward declaration in the main TU.
	for raw_input in b.user_files {
		input := raw_input.trim_right('/\\')
		if input.len == 0 {
			continue
		}
		if input.ends_with('.v') || input.ends_with('.vv') || input.ends_with('.vsh')
			|| input.ends_with('.vh') {
			continue
		}
	}
	return false
}

fn (b &Builder) default_output_name() string {
	if b.user_files.len == 0 {
		return 'out'
	}
	last_input := b.user_files[b.user_files.len - 1].trim_right('/\\')
	if last_input.len == 0 || last_input == '.' {
		cwd := os.getwd()
		base := os.file_name(cwd)
		return if base.len > 0 { base } else { 'out' }
	}
	if os.is_dir(last_input) {
		base := os.file_name(last_input)
		return if base.len > 0 { base } else { 'out' }
	}
	base := os.file_name(last_input).all_before_last('.v')
	return if base.len > 0 { base } else { os.file_name(last_input) }
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
	ssa_builder.target_os = b.pref.target_os_or_host()

	mut stage_start := sw.elapsed()
	ssa_builder.build_all(b.files)
	print_time('SSA Build', time.Duration(sw.elapsed() - stage_start))

	// TODO: re-enable SSA optimization once the new builder is mature
	// stage_start = sw.elapsed()
	// optimize.optimize(mut mod)
	// print_time('SSA Optimize', time.Duration(sw.elapsed() - stage_start))

	cc := if b.pref.ccompiler.len > 0 { b.pref.ccompiler } else { configured_cc(b.pref.vroot) }
	directive_flags := b.collect_cflags_from_sources()
	mut cc_flag_parts := []string{}
	env_flags := configured_cflags()
	if env_flags.trim_space() != '' {
		cc_flag_parts << env_flags.trim_space()
	}
	if cc.contains('tcc') && directive_flags.contains('thirdparty/sqlite/sqlite3.c')
		&& !directive_flags.contains('SQLITE_DISABLE_INTRINSIC') {
		cc_flag_parts << '-DSQLITE_DISABLE_INTRINSIC'
	}
	if directive_flags.trim_space() != '' {
		cc_flag_parts << directive_flags.trim_space()
	}
	tcc_extra := tcc_flags(cc, b.pref.vroot)
	if tcc_extra.trim_space() != '' {
		cc_flag_parts << tcc_extra.trim_space()
	}
	cc_flags := cc_flag_parts.join(' ')
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
	if !b.pref.skip_builtin && b.has_module('builtin') && b.has_module('strconv')
		&& b.ensure_core_cache_dir() {
		cache_dir := b.core_cache_dir()
		builtin_obj = b.ensure_cached_module_object(cache_dir, builtin_cache_name,
			builtin_cached_module_paths, builtin_cached_module_names, cc, cc_flags, '',
			error_limit_flag, false) or { '' }
		if builtin_obj.len > 0 && vlib_cached_module_paths.len > 0 {
			vlib_obj = b.ensure_cached_module_object(cache_dir, vlib_cache_name,
				vlib_cached_module_paths, vlib_cached_module_names, cc, cc_flags, '',
				error_limit_flag, false) or { '' }
		}
	}

	stage_start = sw.elapsed()
	mut gen := c.new_gen(mod)
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
		b.default_output_name()
	} else {
		'out'
	}

	if output_name.ends_with('.c') {
		os.write_file(output_name, c_source) or { panic(err) }
		println('[*] Wrote ${output_name}')
		return
	}

	c_file := b.exec_build_c_file(output_name)
	os.write_file(c_file, c_source) or { panic(err) }
	if c_file != staged_c_file {
		os.write_file(staged_c_file, c_source) or { panic(err) }
	}
	println('[*] Wrote ${c_file}')

	cc_start := sw.elapsed()
	mut cc_cmd := ''
	if builtin_obj.len > 0 {
		// Compile SSA main.c and link against pre-compiled builtin.o
		main_obj := staged_main_obj_file
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
		if !b.pref.keep_c {
			os.rm(main_obj) or {}
		}
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
	return b.gen_cleanc_source_with_options(modules, []string{}, false, '', []string{}, true,
		[]string{})
}

fn (mut b Builder) gen_cleanc_source_for_cache(modules []string, cache_bundle_name string, use_markused bool) string {
	return b.gen_cleanc_source_with_options(modules, []string{}, true, cache_bundle_name,
		[]string{}, use_markused, []string{})
}

fn (mut b Builder) gen_cleanc_source_for_cache_files(modules []string, emit_files []string, cache_bundle_name string, use_markused bool) string {
	return b.gen_cleanc_source_with_options(modules, emit_files, true, cache_bundle_name,
		[]string{}, use_markused, []string{})
}

fn (mut b Builder) gen_cleanc_source_with_cache_init_calls(modules []string, cached_init_calls []string) string {
	return b.gen_cleanc_source_with_options(modules, []string{}, false, '', cached_init_calls,
		true, []string{})
}

fn (mut b Builder) gen_cleanc_source_with_cache_init_calls_and_files(modules []string, emit_files []string, cached_init_calls []string, use_markused bool) string {
	return b.gen_cleanc_source_with_options(modules, emit_files, false, '', cached_init_calls,
		use_markused, []string{})
}

fn (mut b Builder) gen_cleanc_source_with_cache_init_calls_files_force(modules []string, emit_files []string, cached_init_calls []string, use_markused bool, force_emit_fn_names []string) string {
	return b.gen_cleanc_source_with_options(modules, emit_files, false, '', cached_init_calls,
		use_markused, force_emit_fn_names)
}

fn (mut b Builder) gen_cleanc_source_with_options(modules []string, emit_files []string, export_const_symbols bool, cache_bundle_name string, cached_init_calls []string, use_markused bool, force_emit_fn_names []string) string {
	type_modules := if cache_bundle_name.len > 0 {
		b.expand_type_modules_with_imports(cache_type_module_names(cache_bundle_name, modules))
	} else {
		cache_type_module_names(cache_bundle_name, modules)
	}
	mut type_module_names := map[string]bool{}
	restrict_to_cache_modules := cache_bundle_name.len > 0 && type_modules.len > 0
	if restrict_to_cache_modules {
		for module_name in type_modules {
			if module_name != '' {
				type_module_names[module_name] = true
			}
		}
	}
	mut gen_files := []ast.File{cap: b.files.len}
	for file in b.files {
		if restrict_to_cache_modules && ast_file_module_name(file) !in type_module_names {
			continue
		}
		gen_files << file
	}
	if cached_init_calls.len > 0 && b.used_vh_for_parse {
		mut p := parser.Parser.new(b.pref)
		header_files := p.parse_files(b.core_cached_parse_paths(), mut b.file_set)
		for header_file in header_files {
			gen_files << header_file
		}
	}
	mut gen := cleanc.Gen.new_with_env_and_pref(gen_files, b.env, b.pref)
	if modules.len > 0 {
		gen.set_emit_modules(modules)
	}
	if type_modules.len > 0 {
		gen.set_type_modules(type_modules)
	}
	if emit_files.len > 0 {
		gen.set_emit_files(emit_files)
	}
	if use_markused && b.used_fn_keys.len > 0 {
		gen.set_used_fn_keys(b.used_fn_keys)
	}
	if force_emit_fn_names.len > 0 {
		gen.set_force_emit_fn_names(force_emit_fn_names)
	}
	gen.set_export_const_symbols(export_const_symbols)
	if cache_bundle_name.len > 0 {
		gen.set_cache_bundle_name(cache_bundle_name)
	}
	if cached_init_calls.len > 0 {
		gen.set_cached_init_calls(cached_init_calls)
	}
	if cache_bundle_name.len == 0 && cached_init_calls.len > 0 && b.cached_called_fn_names.len > 0 {
		gen.add_called_fn_names(b.cached_called_fn_name_list())
	}
	use_parallel := b.pref != unsafe { nil } && !b.pref.no_parallel
	if use_parallel {
		gen.gen_passes_1_to_4()
		b.gen_cleanc_parallel(mut gen)
		source := gen.gen_finalize()
		if cache_bundle_name.len > 0 {
			b.add_cached_called_fn_names(gen.external_called_fn_names())
		}
		if os.getenv('V2TRACE_CLEANC') != '' {
			eprintln('TRACE_CLEANC builder_files=${b.files.len} gen_files=${gen_files.len} source_len=${source.len}')
		}
		return source
	}
	source := gen.gen()
	if cache_bundle_name.len > 0 {
		b.add_cached_called_fn_names(gen.external_called_fn_names())
	}
	if os.getenv('V2TRACE_CLEANC') != '' {
		eprintln('TRACE_CLEANC builder_files=${b.files.len} gen_files=${gen_files.len} source_len=${source.len}')
	}
	return source
}

fn (mut b Builder) add_cached_called_fn_names(names []string) {
	for name in names {
		if name.len > 0 {
			b.cached_called_fn_names[name] = true
		}
	}
}

fn (b &Builder) cached_called_fn_name_list() []string {
	mut names := b.cached_called_fn_names.keys()
	names.sort()
	return names
}

fn cache_type_module_names(cache_bundle_name string, emit_modules []string) []string {
	if cache_bundle_name == '' {
		return emit_modules
	}
	mut names := []string{cap: core_cached_module_names.len + emit_modules.len}
	if cache_bundle_name == builtin_cache_name {
		names << emit_modules
	} else if cache_bundle_name == vlib_cache_name {
		names << builtin_cached_module_names
		names << emit_modules
	} else {
		names << core_cached_module_names
		names << emit_modules
	}
	return unique_sorted_strings(names)
}

fn (b &Builder) expand_type_modules_with_imports(modules []string) []string {
	mut type_modules := map[string]bool{}
	for module_name in modules {
		if module_name != '' {
			type_modules[module_name] = true
		}
	}
	mut changed := true
	for changed {
		changed = false
		for file in b.files {
			if ast_file_module_name(file) !in type_modules {
				continue
			}
			for import_stmt in file.imports {
				import_module := import_module_name(import_stmt.name)
				if import_module == '' || import_module in type_modules {
					continue
				}
				type_modules[import_module] = true
				changed = true
			}
		}
	}
	return unique_sorted_strings(type_modules.keys())
}

fn import_module_name(name string) string {
	return name.all_after_last('.').replace('.', '_')
}

fn unique_sorted_strings(items []string) []string {
	mut seen := map[string]bool{}
	mut out := []string{cap: items.len}
	for item in items {
		if item == '' || item in seen {
			continue
		}
		seen[item] = true
		out << item
	}
	out.sort()
	return out
}

fn (mut b Builder) gen_cleanc_with_cached_core(output_name string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, mut sw time.StopWatch) bool {
	cache_dir := b.core_cache_dir()
	if !b.ensure_core_cache_dir() {
		// If we cannot create a readable/writable cache dir, fall back to full compilation.
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=cache_dir_unusable')
		}
		return false
	}

	builtin_obj := b.ensure_cached_module_object(cache_dir, builtin_cache_name,
		builtin_cached_module_paths, builtin_cached_module_names, cc, cc_flags, '',
		error_limit_flag, false) or {
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=builtin_obj_failed')
		}
		return false
	}
	b.print_cached_bundle_modules(builtin_cache_name, builtin_cached_module_names)
	mut vlib_obj := ''
	if vlib_cached_module_paths.len > 0 {
		vlib_obj = b.ensure_cached_module_object(cache_dir, vlib_cache_name,
			vlib_cached_module_paths, vlib_cached_module_names, cc, cc_flags, '', error_limit_flag,
			false) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE cached_core=false reason=vlib_obj_failed')
			}
			return false
		}
		if vlib_obj.len > 0 {
			b.print_cached_bundle_modules(vlib_cache_name, vlib_cached_module_names)
		}
	}
	mut optional_cached_objs := []string{}
	mut optional_cached_cache_names := []string{}
	mut optional_cached_module_names := []string{}
	if veb_cached_module_paths.len > 0 && b.has_module('veb') {
		veb_obj := b.ensure_cached_module_object(cache_dir, veb_cache_name,
			veb_cached_module_paths, veb_cached_module_names, cc, cc_flags, cc_link_flags,
			error_limit_flag, true) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE optional_cache=veb reason=${err}')
			}
			''
		}
		if veb_obj.len > 0 {
			b.print_cached_bundle_modules(veb_cache_name, veb_cached_module_names)
			optional_cached_objs << veb_obj
			optional_cached_cache_names << veb_cache_name
			optional_cached_module_names << veb_cached_module_names
		}
	}
	mut v2compiler_obj := ''
	if v2compiler_cached_module_paths.len > 0 && b.is_cmd_v2_self_build() {
		v2compiler_obj = b.ensure_cached_module_object(cache_dir, v2compiler_cache_name,
			v2compiler_cached_module_paths, v2compiler_cached_module_names, cc, cc_flags, '',
			error_limit_flag, false) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE cached_core=false reason=v2compiler_obj_failed')
			}
			return false
		}
		if v2compiler_obj.len > 0 {
			b.print_cached_bundle_modules(v2compiler_cache_name, v2compiler_cached_module_names)
		}
	}
	mut dynamic_excluded := core_cached_module_names.clone()
	for module_name in optional_cached_module_names {
		dynamic_excluded << module_name
	}
	entry_module_names := b.user_entry_module_names()
	dynamic_excluded << entry_module_names
	if b.is_cmd_v2_self_build() {
		dynamic_excluded << v2compiler_cached_module_names
	}
	dynamic_excluded << 'main'
	dynamic_cached_module_names := b.collect_modules_excluding(dynamic_excluded)
	if dynamic_cached_module_names.len > 0 {
		mut import_dependency_cache_names := [builtin_cache_name]
		if vlib_obj.len > 0 {
			import_dependency_cache_names << vlib_cache_name
		}
		if v2compiler_obj.len > 0 {
			import_dependency_cache_names << v2compiler_cache_name
		}
		for cache_name in optional_cached_cache_names {
			import_dependency_cache_names << cache_name
		}
		import_dependency_compile_flags, import_dependency_link_flags :=
			b.cached_module_stamp_flags(import_dependency_cache_names)
		imports_cc_flags := join_flag_strings(cc_flags, import_dependency_compile_flags)
		imports_cc_link_flags := join_flag_strings(cc_link_flags, import_dependency_link_flags)
		imports_obj := b.ensure_cached_parsed_module_object(cache_dir, imports_cache_name,
			dynamic_cached_module_names, import_dependency_cache_names, cc, imports_cc_flags,
			imports_cc_link_flags, error_limit_flag, true) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE optional_cache=imports reason=${err}')
			}
			if b.used_import_vh_for_parse {
				return false
			}
			''
		}
		if imports_obj.len > 0 {
			b.print_cached_bundle_modules(imports_cache_name, dynamic_cached_module_names)
			optional_cached_objs << imports_obj
			optional_cached_cache_names << imports_cache_name
			optional_cached_module_names << dynamic_cached_module_names
		}
	}
	mut virtual_groups := if b.used_virtual_vh_for_parse {
		b.cached_virtual_manifest()
	} else {
		b.collect_virtual_main_modules()
	}
	mut virtual_source_files := []string{}
	mut virtuals_obj := ''
	if virtual_groups.len > 0 {
		mut virtual_dependency_cache_names := [builtin_cache_name]
		if vlib_obj.len > 0 {
			virtual_dependency_cache_names << vlib_cache_name
		}
		if v2compiler_obj.len > 0 {
			virtual_dependency_cache_names << v2compiler_cache_name
		}
		for cache_name in optional_cached_cache_names {
			virtual_dependency_cache_names << cache_name
		}
		virtual_dependency_compile_flags, virtual_dependency_link_flags :=
			b.cached_module_stamp_flags(virtual_dependency_cache_names)
		virtual_cc_flags := join_flag_strings(cc_flags, virtual_dependency_compile_flags)
		virtual_cc_link_flags := join_flag_strings(cc_link_flags, virtual_dependency_link_flags)
		virtuals_obj = b.ensure_cached_virtual_module_object(cache_dir, virtual_groups,
			virtual_dependency_cache_names, cc, virtual_cc_flags, virtual_cc_link_flags,
			error_limit_flag, true) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE optional_cache=${virtuals_cache_name} reason=${err}')
			}
			if b.used_virtual_vh_for_parse {
				return false
			}
			virtual_groups = []CachedVirtualModule{}
			''
		}
		if virtuals_obj.len > 0 {
			virtual_source_files = virtual_module_source_files(virtual_groups)
			b.print_cached_bundle_modules(virtuals_cache_name, virtual_module_names(virtual_groups))
		}
	}
	b.ensure_core_module_headers()
	b.ensure_import_module_headers(dynamic_cached_module_names)
	b.ensure_virtual_module_headers(virtual_groups)
	mut excluded := core_cached_module_names.clone()
	for module_name in optional_cached_module_names {
		excluded << module_name
	}
	if b.is_cmd_v2_self_build() {
		excluded << v2compiler_cached_module_names
	}
	main_modules := b.collect_modules_excluding(excluded)
	if main_modules.len == 0 {
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=no_main_modules')
		}
		return false
	}
	mut linked_cache_names := [builtin_cache_name]
	if vlib_obj.len > 0 {
		linked_cache_names << vlib_cache_name
	}
	if v2compiler_obj.len > 0 {
		linked_cache_names << v2compiler_cache_name
	}
	for cache_name in optional_cached_cache_names {
		linked_cache_names << cache_name
	}
	cached_compile_flags, cached_link_flags := b.cached_module_stamp_flags(linked_cache_names)

	// When TCC is the default compiler but fell back to cc for cache
	// compilation (e.g. due to TCC not supporting certain C constructs),
	// the cached .o files are Mach-O (from cc) while TCC would produce ELF.
	// Detect this mismatch and use cc for main compilation and linking too.
	// Also, -prod builds with -flto require gcc/clang for linking — TCC
	// cannot link LTO object files.
	mut main_cc := cc
	mut main_cc_flags := join_flag_strings(cc_flags, cached_compile_flags)
	main_cc_link_flags := join_flag_strings(cc_link_flags, cached_link_flags)
	if cc.contains('tcc') && os.exists(builtin_obj) {
		bytes := os.read_bytes(builtin_obj) or { []u8{} }
		is_elf := bytes.len >= 4 && bytes[0] == 0x7f && bytes[1] == 0x45 && bytes[2] == 0x4c
			&& bytes[3] == 0x46
		if !is_elf {
			// Cached .o was compiled by cc (via TCC fallback), not TCC.
			// Use cc for main compilation and linking to match formats.
			// Keep all flags (including directive -I paths) but strip
			// TCC-specific -I/-L paths that would conflict with system headers.
			main_cc = 'cc'
			tcc_dir2 := cc.all_before_last('/tcc')
			if tcc_dir2.len > 0 {
				mut parts := main_cc_flags.fields()
				mut filtered := []string{cap: parts.len}
				mut j := 0
				for j < parts.len {
					p := parts[j]
					if (p == '-I' || p == '-L') && j + 1 < parts.len && parts[j + 1].contains('tcc') {
						j += 2
						continue
					}
					if (p.starts_with('-I') || p.starts_with('-L')) && p.contains('tcc') {
						j++
						continue
					}
					// Strip quoted -I/-L containing tcc
					if (p.starts_with('-I"') || p.starts_with('-L"')
						|| p.starts_with("-I'") || p.starts_with("-L'")) && p.contains('tcc') {
						j++
						continue
					}
					filtered << p
					j++
				}
				main_cc_flags = filtered.join(' ')
			}
		}
	}

	mut cached_init_calls := []string{}
	cached_init_calls << '__v2_cached_init_${builtin_cache_name}'
	if vlib_obj.len > 0 {
		cached_init_calls << '__v2_cached_init_${vlib_cache_name}'
	}
	if v2compiler_obj.len > 0 {
		cached_init_calls << '__v2_cached_init_${v2compiler_cache_name}'
	}
	for cache_name in optional_cached_cache_names {
		cached_init_calls << '__v2_cached_init_${cache_name}'
	}
	all_main_emit_files := if virtual_source_files.len > 0 {
		filter_out_source_files(b.module_source_files(main_modules), virtual_source_files)
	} else {
		[]string{}
	}
	mut main_source := if all_main_emit_files.len > 0 {
		b.gen_cleanc_source_with_cache_init_calls_files_force(main_modules, all_main_emit_files,
			cached_init_calls, true, []string{})
	} else {
		b.gen_cleanc_source_with_cache_init_calls(main_modules, cached_init_calls)
	}
	print_time('C Gen', sw.elapsed())
	if main_source == '' {
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=empty_main_source')
		}
		return false
	}

	main_c_file := b.exec_build_c_file(output_name)
	os.write_file(main_c_file, main_source) or { return false }
	if main_c_file != staged_c_file {
		os.write_file(staged_c_file, main_source) or { return false }
	}
	println('[*] Wrote ${main_c_file}')

	cc_start := sw.elapsed()
	main_obj := staged_main_obj_file
	compile_main_cmd := '${main_cc} ${main_cc_flags} -w -Wno-incompatible-function-pointer-types -c "${main_c_file}" -o "${main_obj}"${error_limit_flag}'
	main_fell_back := run_cc_cmd_or_exit(compile_main_cmd, 'C compilation', b.pref.show_cc)
	if main_fell_back && main_cc.contains('tcc') {
		// TCC failed on main.c but cached .o files are ELF (from TCC).
		// Fallback produced Mach-O main.o — can't link with ELF cache.
		// Fall back to non-cached full compilation.
		os.rm(main_obj) or {}
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=main_compile_fell_back')
		}
		return false
	}
	// Strip -c and -x flags from link command since we're linking, not compiling.
	// -x objective-c would cause cc to treat .o files as source code.
	mut link_flags :=
		main_cc_flags.replace('-x objective-c', '').replace('-x c', '').replace(' -c ', ' ')
	mut link_cmd := '${main_cc} ${link_flags} -w "${main_obj}" "${builtin_obj}"'
	if vlib_obj.len > 0 {
		link_cmd += ' "${vlib_obj}"'
	}
	if v2compiler_obj.len > 0 {
		link_cmd += ' "${v2compiler_obj}"'
	}
	for obj in optional_cached_objs {
		link_cmd += ' "${obj}"'
	}
	if virtuals_obj.len > 0 {
		link_cmd += ' "${virtuals_obj}"'
	}
	link_cmd += ' -o "${output_name}"'
	if main_cc_link_flags.len > 0 {
		link_cmd += ' ${main_cc_link_flags}'
	}
	run_cc_cmd_or_exit(link_cmd, 'Linking', b.pref.show_cc)
	print_time('CC', time.Duration(sw.elapsed() - cc_start))

	if !b.pref.keep_c {
		os.rm(main_obj) or {}
		os.rm(main_c_file) or {}
	}
	if os.getenv('V2_TRACE_CACHE') != '' {
		eprintln('TRACE_CACHE cached_core=true')
	}
	println('[*] Compiled ${output_name}')
	return true
}

fn (b &Builder) cached_module_stamp_flags(cache_names []string) (string, string) {
	cache_dir := b.core_cache_dir()
	mut compile_flags := ''
	mut link_flags := ''
	for cache_name in cache_names {
		stamp_path := os.join_path(cache_dir, '${cache_name}.stamp')
		stamp := os.read_file(stamp_path) or { continue }
		for line in stamp.split_into_lines() {
			if line.starts_with('cc_flags=') {
				compile_flags = join_flag_strings(compile_flags, line['cc_flags='.len..])
			} else if line.starts_with('cc_link_flags=') {
				link_flags = join_flag_strings(link_flags, line['cc_link_flags='.len..])
			}
		}
	}
	return compile_flags, link_flags
}

fn join_flag_strings(a string, b string) string {
	mut joined := []string{}
	for flags in [a, b] {
		for item in flag_string_items(flags) {
			if item !in joined {
				joined << item
			}
		}
	}
	return joined.join(' ')
}

fn flag_string_items(flags string) []string {
	tokens := flags.fields()
	mut items := []string{}
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if tok in ['-I', '-L', '-l', '-F', '-framework', '-isystem', '-include']
			&& i + 1 < tokens.len {
			items << '${tok} ${tokens[i + 1]}'
			i += 2
			continue
		}
		items << tok
		i++
	}
	return items
}

fn (mut b Builder) ensure_cached_module_object(cache_dir string, cache_name string, module_paths []string, emit_modules []string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, use_markused bool) !string {
	obj_path := cache_path_join(cache_dir, '${cache_name}.o')
	stamp_path := cache_path_join(cache_dir, '${cache_name}.stamp')
	c_path := cache_path_join(cache_dir, '${cache_name}.c')
	expected_stamp := b.cache_stamp_for_modules(cache_name, module_paths, cc, cc_flags,
		cc_link_flags, use_markused)
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
	if b.used_vh_for_parse {
		if os.exists(obj_path) && os.exists(stamp_path) {
			return obj_path
		}
		return error('missing cached ${cache_name} object for .vh parse')
	}

	module_source := b.gen_cleanc_source_for_cache(emit_modules, cache_name, use_markused)
	if module_source == '' {
		return error('failed to generate C source for ${cache_name}')
	}
	os.write_file(c_path, module_source)!

	compile_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	os.write_file(stamp_path, expected_stamp)!
	return obj_path
}

fn (mut b Builder) ensure_cached_parsed_module_object(cache_dir string, cache_name string, module_names []string, dependency_cache_names []string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, use_markused bool) !string {
	obj_path := cache_path_join(cache_dir, '${cache_name}.o')
	stamp_path := cache_path_join(cache_dir, '${cache_name}.stamp')
	c_path := cache_path_join(cache_dir, '${cache_name}.c')
	expected_stamp := b.cache_stamp_for_parsed_modules(cache_name, module_names,
		dependency_cache_names, cc, cc_flags, cc_link_flags, use_markused)
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
	if b.used_import_vh_for_parse {
		if os.exists(obj_path) && os.exists(stamp_path) {
			return obj_path
		}
		return error('missing cached ${cache_name} object for .vh parse')
	}

	mut module_source := b.gen_cleanc_source_for_cache(module_names, cache_name, use_markused)
	if module_source == '' {
		return error('failed to generate C source for ${cache_name}')
	}
	os.write_file(c_path, module_source)!

	compile_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	os.write_file(stamp_path, expected_stamp)!
	return obj_path
}

fn (mut b Builder) ensure_cached_virtual_module_object(cache_dir string, groups []CachedVirtualModule, dependency_cache_names []string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, use_markused bool) !string {
	obj_path := cache_path_join(cache_dir, '${virtuals_cache_name}.o')
	stamp_path := cache_path_join(cache_dir, '${virtuals_cache_name}.stamp')
	c_path := cache_path_join(cache_dir, '${virtuals_cache_name}.c')
	expected_stamp := b.cache_stamp_for_virtual_modules(groups, dependency_cache_names, cc,
		cc_flags, cc_link_flags, use_markused)
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
	if b.used_virtual_vh_for_parse {
		if os.exists(obj_path) && os.exists(stamp_path) {
			return obj_path
		}
		return error('missing cached ${virtuals_cache_name} object for .vh parse')
	}

	emit_files := virtual_module_source_files(groups)
	mut module_source := b.gen_cleanc_source_for_cache_files(['main'], emit_files,
		virtuals_cache_name, use_markused)
	if module_source == '' {
		return error('failed to generate C source for ${virtuals_cache_name}')
	}
	os.write_file(c_path, module_source)!

	compile_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	os.write_file(stamp_path, expected_stamp)!
	return obj_path
}

fn (b &Builder) has_module(module_name string) bool {
	for file in b.files {
		if ast_file_module_name(file) == module_name {
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
		module_name := ast_file_module_name(file)
		if module_name in excluded_set {
			continue
		}
		modules_set[module_name] = true
	}
	mut modules := modules_set.keys()
	modules.sort()
	return modules
}

fn (b &Builder) user_entry_module_names() []string {
	mut entry_files := map[string]bool{}
	for file in b.user_entry_stamp_files() {
		entry_files[os.norm_path(file)] = true
		entry_files[os.norm_path(os.abs_path(file))] = true
	}
	mut modules_set := map[string]bool{}
	for file in b.files {
		if file.name == '' || file.name.ends_with('.vh') {
			continue
		}
		norm_name := os.norm_path(file.name)
		abs_name := os.norm_path(os.abs_path(file.name))
		if norm_name !in entry_files && abs_name !in entry_files {
			continue
		}
		module_name := ast_file_module_name(file)
		if module_name.len > 0 {
			modules_set[module_name] = true
		}
	}
	mut modules := modules_set.keys()
	modules.sort()
	return modules
}

fn (b &Builder) print_cached_bundle_modules(cache_name string, module_names []string) {
	if module_names.len == 0 {
		return
	}
	stats_enabled := b.pref != unsafe { nil } && b.pref.stats
	show_cc_enabled := b.pref != unsafe { nil } && b.pref.show_cc
	if !stats_enabled && !show_cc_enabled && os.getenv('V2_TRACE_CACHE') == '' {
		return
	}
	if stats_enabled {
		println(' * Cached ${cache_name} modules: ${module_names.len}')
		for module_name in module_names {
			println('   [cache ${cache_name}] ${module_name}')
		}
		return
	}
	println('[*] Cached ${cache_name} modules: ${module_names.join(', ')}')
}

fn ast_file_module_name(file ast.File) string {
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			return stmt.name.replace('.', '_')
		}
	}
	return 'main'
}

fn normalize_target_os_name(target_os string) string {
	return match target_os.to_lower() {
		'darwin', 'mac' { 'macos' }
		else { target_os.to_lower() }
	}
}

fn is_windows_x64_native_target(arch pref.Arch, target_os string) bool {
	return arch == .x64 && normalize_target_os_name(target_os) == 'windows'
}

fn (b &Builder) uses_minimal_windows_x64_runtime() bool {
	arch := b.pref.get_effective_arch()
	return b.pref.backend == .x64 && is_windows_x64_native_target(arch, b.pref.target_os_or_host())
}

fn is_macos_native_target(target_os string) bool {
	return normalize_target_os_name(target_os) == 'macos'
}

fn native_x64_object_format_for_os(target_os string) x64.ObjectFormat {
	return match normalize_target_os_name(target_os) {
		'macos' { x64.ObjectFormat.macho }
		'windows' { x64.ObjectFormat.coff }
		else { x64.ObjectFormat.elf }
	}
}

fn native_x64_codegen_abi_for_os(target_os string) x64.X64Abi {
	return match normalize_target_os_name(target_os) {
		'windows' { x64.X64Abi.windows }
		else { x64.X64Abi.sysv }
	}
}

fn native_x64_lowering_abi_for_os(target_os string) abi.X64Abi {
	return match normalize_target_os_name(target_os) {
		'windows' { abi.X64Abi.windows }
		else { abi.X64Abi.sysv }
	}
}

fn flag_os_matches(cond string, target_os string) bool {
	current := normalize_target_os_name(target_os)
	return match cond.to_lower() {
		'darwin', 'macos', 'mac' { current == 'macos' || current == 'darwin' }
		'linux' { current == 'linux' }
		'windows' { current == 'windows' }
		'bsd' { current in ['macos', 'freebsd', 'openbsd', 'netbsd', 'dragonfly'] }
		'freebsd' { current == 'freebsd' }
		'openbsd' { current == 'openbsd' }
		'netbsd' { current == 'netbsd' }
		'dragonfly' { current == 'dragonfly' }
		'android' { current == 'android' }
		else { false }
	}
}

fn find_vmod_root_for_file(file_path string) string {
	mut dir := os.dir(file_path)
	for _ in 0 .. 12 {
		if os.exists(os.join_path(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir || parent == '' {
			break
		}
		dir = parent
	}
	return os.dir(file_path)
}

fn resolve_flag_path(path string, file_dir string, vmod_root string) string {
	mut resolved := path.replace('@VMODROOT', vmod_root)
	if os.is_abs_path(resolved) {
		return resolved
	}
	// Resolve any relative path (including bare relative like 'r/qrcodegen')
	// relative to the source file's directory, matching V1 behavior
	return os.norm_path(os.join_path(file_dir, resolved))
}

fn expand_existing_path_macros(flag_value string) ?string {
	mut out := ''
	mut i := 0
	for i < flag_value.len {
		if flag_value[i] == `$` {
			remainder := flag_value[i..]
			mut literal := ''
			if remainder.starts_with(r'$when_first_existing') {
				literal = r'$when_first_existing'
			} else if remainder.starts_with(r'$first_existing') {
				literal = r'$first_existing'
			}
			if literal != '' {
				if remainder.len <= literal.len || remainder[literal.len] != `(` {
					out += flag_value[i].ascii_str()
					i++
					continue
				}
				params_part := remainder[literal.len + 1..]
				params := params_part.all_before(')')
				if params == params_part {
					return none
				}
				paths := params.replace(',', '\n').split_into_lines().map(it.trim('\t \'"'))
				mut found := ''
				for path in paths {
					if path != '' && os.exists(path) {
						found = path
						break
					}
				}
				if found == '' {
					return none
				}
				out += found
				i += literal.len + 1 + params.len + 1
				continue
			}
		}
		out += flag_value[i].ascii_str()
		i++
	}
	return out
}

fn normalize_flag_value_for_file(flag_value string, file_path string) string {
	file_dir := os.dir(os.real_path(file_path))
	vmod_root := find_vmod_root_for_file(file_path)
	expanded_flag_value := expand_existing_path_macros(flag_value) or { return '' }
	mut tokens := expanded_flag_value.fields()
	mut out := []string{}
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if tok in ['-I', '-L', '-F'] && i + 1 < tokens.len {
			out << tok
			out << resolve_flag_path(tokens[i + 1], file_dir, vmod_root)
			i += 2
			continue
		}
		if tok.starts_with('-I') && tok.len > 2 {
			out << '-I' + resolve_flag_path(tok[2..], file_dir, vmod_root)
			i++
			continue
		}
		if tok.starts_with('-L') && tok.len > 2 {
			out << '-L' + resolve_flag_path(tok[2..], file_dir, vmod_root)
			i++
			continue
		}
		if tok.starts_with('-F') && tok.len > 2 {
			out << '-F' + resolve_flag_path(tok[2..], file_dir, vmod_root)
			i++
			continue
		}
		if tok.contains('@VMODROOT') || tok.contains('@VEXEROOT') || tok.starts_with('./')
			|| tok.starts_with('../') || tok.ends_with('.c') || tok.ends_with('.m')
			|| tok.ends_with('.o') {
			out << resolve_flag_path(tok, file_dir, vmod_root)
			i++
			continue
		}
		out << tok
		i++
	}
	return out.join(' ')
}

fn parse_flag_directive_line(line string, file_path string, target_os string) ?string {
	trimmed := line.trim_space()
	if !trimmed.starts_with('#flag') {
		return none
	}
	mut rest := trimmed['#flag'.len..].trim_space()
	if rest == '' {
		return none
	}
	if comment_idx := rest.index('//') {
		rest = rest[..comment_idx].trim_space()
		if rest == '' {
			return none
		}
	}
	parts := rest.fields()
	if parts.len == 0 {
		return none
	}
	if !parts[0].starts_with('-') && !parts[0].starts_with('@') && parts.len > 1 {
		if !flag_os_matches(parts[0], target_os) {
			return none
		}
		rest = rest[parts[0].len..].trim_space()
	}
	if rest == '' {
		return none
	}
	return normalize_flag_value_for_file(rest, file_path)
}

fn flag_references_missing_file(flag string, include_flags []string) bool {
	for tok in flag.fields() {
		clean := tok.trim('"').trim("'")
		if clean.len == 0 {
			continue
		}
		if clean.ends_with('.o') || clean.ends_with('.a') || clean.ends_with('.so')
			|| clean.ends_with('.dylib') || clean.ends_with('.m') || clean.ends_with('.c') {
			if os.is_abs_path(clean) || clean.starts_with('./') || clean.starts_with('../') {
				if !os.exists(clean) {
					// For .o files, try to build from corresponding .c file
					if clean.ends_with('.o') {
						c_file := clean[..clean.len - 2] + '.c'
						if os.exists(c_file) {
							inc_flags := include_flags.join(' ')
							compile_cmd := 'cc -c -w -O2 ${inc_flags} "${c_file}" -o "${clean}"'
							res := os.execute(compile_cmd)
							if res.exit_code == 0 {
								continue // successfully compiled, not missing
							}
						}
					}
					return true
				}
			}
		}
	}
	return false
}

fn (b &Builder) collect_cflags_from_sources() string {
	mut flags := []string{}
	mut seen := map[string]bool{}
	mut scanned_files := map[string]bool{}
	// Collect source file paths to scan.  When .vh headers were used for
	// parsing, b.files references the .vh summaries which lack #flag
	// directives.  Always include the original core module source files
	// so that directive flags (e.g. -I paths) are never lost.
	mut scan_paths := []string{}
	for file in b.files {
		if file.name != '' {
			scan_paths << file.name
		}
	}
	if !b.pref.skip_builtin {
		target_os := b.pref.target_os_or_host()
		for module_path in core_cached_module_paths {
			vlib_path := b.pref.get_vlib_module_path(module_path)
			module_files := get_v_files_from_dir(vlib_path, b.pref.user_defines, target_os)
			for mf in module_files {
				if mf !in scanned_files {
					scan_paths << mf
				}
			}
		}
	}
	scan_paths.sort()
	target_os := b.pref.target_os_or_host()
	for scan_path in scan_paths {
		if scan_path == '' || scan_path in scanned_files {
			continue
		}
		scanned_files[scan_path] = true
		lines := os.read_lines(scan_path) or { continue }
		// Track $if nesting to skip flags inside non-matching comptime blocks.
		// skip_depth > 0 means we are inside a non-matching $if block.
		// chain_matched[i] tracks whether the i-th enclosing $if chain has
		// already matched some branch (so subsequent $else / $else $if at the
		// same level should be skipped even if their cond would otherwise match).
		mut skip_depth := 0
		mut chain_matched := []bool{}
		for line in lines {
			trimmed := line.trim_space()
			// Strip a leading "} " so chained patterns like "} $else $if X {" parse
			// the same way as "$else $if X {" on their own line.
			rest := if trimmed.starts_with('} ') { trimmed[2..].trim_space() } else { trimmed }
			leading_close := rest != trimmed
			// $else $if cond { (a chain continuation)
			if rest.starts_with(r'$else $if ') {
				new_cond := rest[10..].trim_right('?{ ').trim_space()
				if skip_depth > 1 {
					// nested skipping; just continue without touching outer state
					continue
				}
				cur := chain_matched.len - 1
				if cur < 0 {
					continue
				}
				if chain_matched[cur] {
					skip_depth = 1
				} else if comptime_cond_matches(new_cond, target_os) {
					chain_matched[cur] = true
					skip_depth = 0
				} else {
					skip_depth = 1
				}
				continue
			}
			// plain $else { (chain terminator)
			if rest.starts_with(r'$else') {
				if skip_depth > 1 {
					continue
				}
				cur := chain_matched.len - 1
				if cur < 0 {
					continue
				}
				if chain_matched[cur] {
					skip_depth = 1
				} else {
					chain_matched[cur] = true
					skip_depth = 0
				}
				continue
			}
			// $if cond { (chain opener)
			if rest.starts_with(r'$if ') {
				cond := rest[4..].trim_right('?{ ').trim_space()
				matched := comptime_cond_matches(cond, target_os)
				chain_matched << matched
				if skip_depth > 0 {
					skip_depth++
				} else if !matched {
					skip_depth = 1
				}
				continue
			}
			// closing }
			if trimmed == '}' {
				if skip_depth > 0 {
					skip_depth--
				}
				if chain_matched.len > 0 {
					chain_matched.delete_last()
				}
				continue
			}
			// "} something" where the leading } closed a chain but the rest is
			// unrecognized: treat the } as a chain close.
			if leading_close && rest == '' {
				if skip_depth > 0 {
					skip_depth--
				}
				if chain_matched.len > 0 {
					chain_matched.delete_last()
				}
				continue
			}
			if skip_depth > 0 {
				continue
			}
			// Replace @VEXEROOT before parsing so path normalization sees absolute paths
			resolved_line := line.replace('@VEXEROOT', b.pref.vroot).replace('VEXEROOT',
				b.pref.vroot)
			mut flag := parse_flag_directive_line(resolved_line, scan_path, target_os) or {
				continue
			}
			// Build include flags from already-collected flags for compiling missing .o files
			mut inc_flags := []string{}
			for f in flags {
				if f.starts_with('-I') {
					inc_flags << f
				}
			}
			if flag_references_missing_file(flag, inc_flags) {
				continue
			}
			if flag == '' || flag in seen {
				continue
			}
			seen[flag] = true
			flags << flag
		}
	}
	return flags.join(' ')
}

// split_compile_and_link_flags separates a flags string into compiler-only
// flags (for -c compilation) and linker-only flags (for the link step).
// Linker flags include: -l*, -L*, -framework, C source files and
// prebuilt object/library files.  Source files from #flag directives must not
// be passed to per-module `-c -o module.o` cache compilations.
fn split_compile_and_link_flags(flags string) (string, string) {
	tokens := flags.fields()
	mut compile := []string{}
	mut link := []string{}
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if tok == '-framework' {
			// -framework Name: two tokens, linker only
			link << tok
			if i + 1 < tokens.len {
				i++
				link << tokens[i]
			}
		} else if tok.starts_with('-l') || tok.starts_with('-L') {
			link << tok
			// -L or -l alone (space-separated from its argument): grab the next token
			if (tok == '-L' || tok == '-l') && i + 1 < tokens.len {
				i++
				link << tokens[i]
			}
		} else if tok.ends_with('.c') || tok.ends_with('.cc') || tok.ends_with('.cpp')
			|| tok.ends_with('.cxx') || tok.ends_with('.m') || tok.ends_with('.mm')
			|| tok.ends_with('.o') || tok.ends_with('.obj') || tok.ends_with('.a')
			|| tok.ends_with('.so') || tok.ends_with('.dylib') {
			link << tok
		} else if tok == '-I' && i + 1 < tokens.len {
			// -I alone (space-separated from its argument): grab the next token
			compile << tok
			i++
			compile << tokens[i]
		} else {
			compile << tok
		}
		i++
	}
	return compile.join(' '), link.join(' ')
}

fn comptime_cond_matches(cond string, target_os string) bool {
	// Handle negation: $if !platform
	if cond.starts_with('!') {
		return !comptime_cond_matches(cond[1..], target_os)
	}
	// Handle && conjunction
	if and_idx := cond.index('&&') {
		left := cond[..and_idx].trim_space()
		right := cond[and_idx + 2..].trim_space()
		return comptime_cond_matches(left, target_os) && comptime_cond_matches(right, target_os)
	}
	if or_idx := cond.index('||') {
		left := cond[..or_idx].trim_space()
		right := cond[or_idx + 2..].trim_space()
		return comptime_cond_matches(left, target_os) || comptime_cond_matches(right, target_os)
	}
	current := normalize_target_os_name(target_os)
	return match cond.to_lower() {
		'macos', 'darwin', 'mac' { current == 'macos' || current == 'darwin' }
		'linux' { current == 'linux' }
		'windows' { current == 'windows' }
		'bsd' { current in ['macos', 'freebsd', 'openbsd', 'netbsd', 'dragonfly'] }
		'freebsd' { current == 'freebsd' }
		'openbsd' { current == 'openbsd' }
		'netbsd' { current == 'netbsd' }
		'dragonfly' { current == 'dragonfly' }
		'android' { current == 'android' }
		'native' { false }
		'emscripten' { false }
		'ios' { false }
		else { false } // unknown user-defined flags default to false
	}
}

fn default_cc(vroot string) string {
	// Try to use tcc by default, like v1 does.
	tcc_path := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	if os.exists(tcc_path) {
		return tcc_path
	}
	return 'cc'
}

fn configured_cc(vroot string) string {
	cc := (os.getenv_opt('V2CC') or { '' }).trim_space()
	if cc != '' {
		return cc
	}
	return default_cc(vroot)
}

fn configured_cflags() string {
	return (os.getenv_opt('V2CFLAGS') or { '' }).trim_space()
}

fn tcc_flags(cc string, vroot string) string {
	if !cc.contains('tcc') {
		return ''
	}
	tcc_dir := os.join_path(vroot, 'thirdparty', 'tcc')
	return '-I "${os.join_path(tcc_dir, 'lib', 'include')}" -L "${os.join_path(tcc_dir, 'lib')}"'
}

fn cflags_need_objc_mode(flags string) bool {
	lower_flags := flags.to_lower()
	for tok in lower_flags.fields() {
		clean := tok.trim('"\'')
		if clean.ends_with('.m') || clean.ends_with('.mm') {
			return true
		}
	}
	return lower_flags.contains('-framework cocoa') || lower_flags.contains('-framework appkit')
		|| lower_flags.contains('-framework foundation') || lower_flags.contains('-framework uikit')
		|| lower_flags.contains('-framework metal') || lower_flags.contains('-framework metalkit')
		|| lower_flags.contains('-framework quartzcore')
}

fn cc_recompile_flags_from_cmd(cmd string) string {
	parts := cmd.fields()
	mut flags := []string{}
	mut i := 1 // skip compiler
	for i < parts.len {
		p := parts[i]
		if p == '-o' {
			i += 2
			continue
		}
		if p == '-x' {
			if i + 1 < parts.len && parts[i + 1] != 'none' {
				flags << p
				flags << parts[i + 1]
			}
			i += 2
			continue
		}
		if p in ['-I', '-D', '-U', '-F', '-include', '-isystem', '-idirafter'] {
			flags << p
			if i + 1 < parts.len {
				i++
				flags << parts[i]
			}
			i++
			continue
		}
		if p.starts_with('-I') || p.starts_with('-D') || p.starts_with('-U') || p.starts_with('-F')
			|| p.starts_with('-std=') || p.starts_with('-W') || p.starts_with('-f')
			|| p.starts_with('-m') || p == '-pthread' {
			flags << p
		}
		i++
	}
	return flags.join(' ')
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
			// Replace TCC binary with cc and strip TCC-specific include/lib
			// paths. TCC's tgmath.h conflicts with macOS system headers,
			// causing SIMD ambiguity errors in MetalKit when compiling as
			// Objective-C.
			mut fallback_cmd := cmd.replace_once(cc_binary, 'cc')
			tcc_dir := cc_binary.all_before_last('/tcc')
			if tcc_dir.len > 0 {
				// Remove -I and -L flags pointing into the TCC directory.
				mut parts := fallback_cmd.fields()
				mut filtered := []string{cap: parts.len}
				mut i2 := 0
				for i2 < parts.len {
					p := parts[i2]
					if (p == '-I' || p == '-L') && i2 + 1 < parts.len
						&& parts[i2 + 1].contains('tcc') {
						i2 += 2
						continue
					}
					if (p.starts_with('-I') || p.starts_with('-L')) && p.contains('tcc') {
						i2++
						continue
					}
					filtered << p
					i2++
				}
				fallback_cmd = filtered.join(' ')
			}
			// cc cannot read .o files produced by tcc on macOS arm64. Recompile
			// any cached .o files referenced in the command from their .c siblings
			// using cc before retrying the link.
			recompile_flags := cc_recompile_flags_from_cmd(fallback_cmd)
			for tok in fallback_cmd.fields() {
				clean_tok := tok.trim('"')
				if !clean_tok.ends_with('.o') {
					continue
				}
				stem := clean_tok.all_before_last('.o')
				mut c_sibling := stem + '.c'
				if !os.exists(c_sibling) && stem.ends_with('.main') {
					c_sibling = stem.all_before_last('.main') + '.c'
				}
				if !os.exists(c_sibling) {
					continue
				}
				recompile_cmd := 'cc ${recompile_flags} -w -Wno-incompatible-function-pointer-types -c "${c_sibling}" -o "${clean_tok}"'
				if show_cc {
					println(recompile_cmd)
				}
				rr := os.execute(recompile_cmd)
				if rr.exit_code != 0 {
					eprintln('cc recompile failed for ${c_sibling}:')
					eprintln(rr.output)
				}
				// Invalidate stamp so future builds rebuild from .c too.
				stamp_path := stem + '.stamp'
				if os.exists(stamp_path) {
					os.rm(stamp_path) or {}
				}
			}
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
	target_os := b.pref.target_os_or_host()

	// Build all files into a single SSA module
	mut mod := ssa.Module.new('main')
	if mod == unsafe { nil } {
		eprintln('error: native backend not available (compiled with stubbed ssa module)')
		eprintln('hint: use v2 compiled with v1 for native code generation')
		return
	}
	mut ssa_builder := ssa.Builder.new_with_env(mod, b.env)
	ssa_builder.guard_invalid_type_payloads = true
	ssa_builder.target_os = target_os
	ssa_builder.minimal_runtime_roots = b.uses_minimal_windows_x64_runtime()
	mut native_sw := time.new_stopwatch()

	// Pass markused data for dead code elimination. The ARM64 backend has its own
	// relocation-based dead stripping; using markused before SSA makes self-hosted
	// compiler builds fragile when the markused set is under-collected.
	if b.used_fn_keys.len > 0 && arch != .arm64 {
		ssa_builder.used_fn_keys = b.used_fn_keys.clone()
	}

	// --single-backend: strip unused backend modules from the binary
	if b.pref.single_backend {
		all_backends := ['cleanc', 'eval', 'c', 'x64', 'arm64']
		own := match b.pref.backend {
			.arm64 { 'arm64' }
			.x64 { 'x64' }
			.cleanc { 'cleanc' }
			.c { 'c' }
			.eval { 'eval' }
			else { '' }
		}

		for backend_mod in all_backends {
			if backend_mod != own {
				ssa_builder.skip_modules[backend_mod] = true
			}
		}
	}

	// In hot_fn mode, only build the target function body (skip all others)
	if b.pref.hot_fn.len > 0 {
		ssa_builder.hot_fn = b.pref.hot_fn
	}

	// Build all files together with proper multi-file ordering
	mut stage_start := native_sw.elapsed()
	if b.pref.no_parallel || b.pref.hot_fn.len > 0 {
		ssa_builder.build_all(b.files)
	} else {
		// Phases 1-3 sequential, Phase 4 parallel, Phase 5 sequential
		ssa_builder.skip_fn_bodies = true
		ssa_builder.build_all(b.files)
		ssa_builder.skip_fn_bodies = false
		b.ssa_build_parallel(mut ssa_builder, b.files)
		ssa_builder.generate_vinit()
	}
	print_time('SSA Build', time.Duration(native_sw.elapsed() - stage_start))

	// SSA build has consumed b.files into `mod`; the rest of the native
	// pipeline (optimize, MIR lower, ABI lower, insel, codegen, link)
	// operates on `mod`/`mir_mod` only. Drop the ~120MB legacy AST so it
	// can be reclaimed before codegen's working-set grows.
	b.files = []ast.File{}

	stage_start = native_sw.elapsed()
	if b.pref.no_optimize {
		eprintln('  opt: skipped (-O0)')
	} else {
		ssa_optimize.optimize(mut mod)
	}
	print_time('SSA Optimize', time.Duration(native_sw.elapsed() - stage_start))
	$if debug {
		// Post-opt SSA verification is useful while debugging the optimizer, but it
		// is currently noisy enough to block normal self-host builds. Keep it
		// opt-in so `test_all.sh` and manual self-hosting can still complete.
		if !b.pref.no_optimize && os.getenv('V2_VERIFY') != '' {
			ssa_optimize.verify_and_panic(mod, 'full optimization')
		}
	}

	// Post-optimization SSA dump for debugging
	dump_fn_name := os.getenv('V2_DUMP_OPT_SSA')
	if dump_fn_name.len > 0 {
		for func in mod.funcs {
			if func.name == dump_fn_name {
				eprintln('=== POST-OPT SSA DUMP: ${func.name} ===')
				eprintln('  params: ${func.params}')
				for pi, pid in func.params {
					pval := mod.values[pid]
					eprintln('  param[${pi}]: v${pid} kind=${pval.kind} name=`${pval.name}` typ=${pval.typ}')
				}
				for blk_id in func.blocks {
					blk := mod.blocks[blk_id]
					eprintln('  block ${blk_id} (${blk.name}):')
					for dval_id in blk.instrs {
						dval := mod.values[dval_id]
						if dval.kind != .instruction {
							continue
						}
						dinstr := mod.instrs[dval.index]
						mut ops_str := ''
						for oi, op_id in dinstr.operands {
							op_v := mod.values[op_id]
							ops_str += 'v${op_id}(${op_v.kind}:${op_v.name})'
							if oi < dinstr.operands.len - 1 {
								ops_str += ', '
							}
						}
						eprintln('    v${dval_id}: ${dinstr.op} [${ops_str}] typ=${dval.typ}')
					}
				}
				eprintln('=== END POST-OPT SSA DUMP ===')
			}
		}
	}

	stage_start = native_sw.elapsed()
	mut mir_mod := mir.lower_from_ssa(mod)
	print_time('MIR Lower', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	if is_windows_x64_native_target(arch, target_os) {
		abi.lower_with_x64_abi(mut mir_mod, arch, native_x64_lowering_abi_for_os(target_os))
	} else {
		abi.lower(mut mir_mod, arch)
	}
	print_time('ABI Lower', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	insel.select(mut mir_mod, arch)
	print_time('InsSel', time.Duration(native_sw.elapsed() - stage_start))

	// Determine output binary name from the last user file
	output_binary := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		b.default_output_name()
	} else {
		'out'
	}

	if arch == .arm64 && is_macos_native_target(target_os) {
		// Use built-in linker for ARM64 macOS
		stage_start = native_sw.elapsed()
		mut gen := arm64.Gen.new(&mir_mod)
		if b.pref.no_parallel {
			gen.gen()
		} else {
			b.gen_arm64_parallel(mut gen)
		}
		print_time('ARM64 Gen', time.Duration(native_sw.elapsed() - stage_start))

		if b.pref.hot_fn.len > 0 {
			// Hot code reloading: extract raw machine code for a single function
			code := gen.extract_function(b.pref.hot_fn)
			if code.len > 0 {
				os.write_file_array(output_binary, code) or { panic(err) }
				println('hot-fn: wrote ${code.len} bytes for "${b.pref.hot_fn}" to ${output_binary}')
			}
			return
		}

		gen.link_executable(output_binary)

		if b.pref.verbose {
			println('[*] Linked ${output_binary} (built-in linker)')
		}
	} else {
		// Generate object file and use external linker
		obj_file := 'main.o'

		if arch == .arm64 {
			mut gen := arm64.Gen.new(&mir_mod)
			if b.pref.no_parallel {
				gen.gen()
			} else {
				b.gen_arm64_parallel(mut gen)
			}
			gen.write_file(obj_file)
		} else {
			obj_format := native_x64_object_format_for_os(target_os)
			codegen_abi := native_x64_codegen_abi_for_os(target_os)
			mut gen := x64.Gen.new_with_format_and_abi(&mir_mod, obj_format, codegen_abi)
			gen.gen()
			if is_windows_x64_native_target(arch, target_os) {
				gen.link_executable(output_binary) or {
					eprintln('Link failed:')
					eprintln(err.msg())
					exit(1)
				}
				if b.pref.verbose {
					println('[*] Linked ${output_binary} (built-in PE linker)')
				}
				return
			}
			gen.write_file(obj_file)
		}

		if b.pref.verbose {
			println('[*] Wrote ${obj_file}')
		}

		// Link the object file into an executable
		if is_macos_native_target(target_os) {
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
		if !b.pref.keep_c {
			os.rm(obj_file) or {}
		}
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
	if b.flat_check_enabled {
		for ff in b.flat.files {
			name := b.flat.file_name(ff)
			if name.ends_with('.vh') {
				parsed_vh_files_n++
				parsed_vh_files << name
			} else {
				parsed_full_files_n++
				parsed_full_files << name
			}
		}
	} else {
		for file in b.files {
			if file.name.ends_with('.vh') {
				parsed_vh_files_n++
				parsed_vh_files << file.name
			} else {
				parsed_full_files_n++
				parsed_full_files << file.name
			}
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

fn (b &Builder) print_flat_ast_summary() {
	legacy_stats := ast.legacy_ast_stats(b.files)
	legacy_nodes := ast.count_legacy_nodes(b.files)
	flat := if b.flat_check_enabled { b.flat } else { ast.flatten_files(b.files) }
	flat_stats := flat.stats()
	mut mem_delta_pct := f64(0)
	if legacy_stats.bytes_estimate > 0 {
		mem_delta_pct = (f64(legacy_stats.bytes_estimate) - f64(flat_stats.bytes_estimate)) * 100.0 / f64(legacy_stats.bytes_estimate)
	}
	// Flat AST uses 4 arenas (files, nodes, edges, strings) regardless of payload
	// count; each is one allocation amortised across millions of cells.
	flat_allocs := u64(4)
	mut alloc_delta_pct := f64(0)
	if legacy_stats.allocs > 0 {
		alloc_delta_pct = (f64(legacy_stats.allocs) - f64(flat_allocs)) * 100.0 / f64(legacy_stats.allocs)
	}
	println(' * AST nodes:      legacy=${legacy_nodes}, flat=${flat_stats.nodes} (edges=${flat_stats.edges}, strings=${flat_stats.strings})')
	println(' * AST memory est: legacy=${legacy_stats.bytes_estimate}B, flat=${flat_stats.bytes_estimate}B (${mem_delta_pct:.2f}% reduction)')
	println(' * AST allocs:     legacy=${legacy_stats.allocs}, flat=${flat_allocs} (${alloc_delta_pct:.2f}% reduction)')
	if os.getenv('V2_FLAT_HIST') != '' {
		hist := flat.count_nodes_by_kind()
		mut keys := hist.keys()
		keys.sort_with_compare(fn [hist] (a &string, b &string) int {
			return hist[*b] - hist[*a]
		})
		println(' * AST per-kind histogram (top 20):')
		for i, k in keys {
			if i >= 20 {
				break
			}
			println('     ${k:-30s} ${hist[k]}')
		}
	}
}

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
	if b.flat_check_enabled {
		for ff in b.flat.files {
			name := b.flat.file_name(ff)
			if name in seen_files {
				continue
			}
			seen_files[name] = true
			parsed_paths << name
		}
	} else {
		for file in b.files {
			if file.name in seen_files {
				continue
			}
			seen_files[file.name] = true
			parsed_paths << file.name
		}
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
	}
	if (show_stats || print_parsed_files) && parsed_vh_files.len > 0 {
		println(' * Parsed .vh files:')
		for path in parsed_vh_files {
			println('   [vh] ${path}')
		}
	}
	if show_stats {
		println(' * Parsed V LOC (entry files): ${entry_v_lines_n}')
		println(' * Parsed V LOC (all parsed sources): ${parsed_v_lines_n}')
	}
}
