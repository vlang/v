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
	files                                 []ast.File
	user_files                            []string // original user-provided files (for output name)
	file_set                              &token.FileSet     = token.FileSet.new()
	env                                   &types.Environment = unsafe { nil } // Type checker environment
	parsed_full_files_n                   int
	parsed_vh_files_n                     int
	entry_v_lines_n                       int
	parsed_v_lines_n                      int
	parsed_full_files                     []string
	parsed_vh_files                       []string
	used_fn_keys                          map[string]bool
	cached_called_fn_names                map[string]bool
	v2compiler_generic_setup_snapshot     cleanc.GenericSetupSnapshot
	has_v2compiler_generic_setup_snapshot bool
	used_vh_for_parse                     bool
	used_import_vh_for_parse              bool
	used_virtual_vh_for_parse             bool
	// flat is the canonical parse output. parse_batch streams directly into
	// flat_builder so b.flat is built incrementally during parsing rather than
	// via a redundant flatten_files() pass afterwards.
	flat         ast.FlatAst
	flat_builder ast.FlatBuilder
	// flat_builder_inited tracks whether flat_builder has been seeded with
	// pre-sized arenas. We can only size after we know the input set, so
	// the first parse_batch call lazily initializes it.
	flat_builder_inited bool
	// native_flat_pipeline_enabled means the transform phase produced a
	// post-transform FlatAst and intentionally did not materialize b.files.
	native_flat_pipeline_enabled bool
	// Source snapshot used only for an isolated macOS tiny candidate graph.
	// The normal hosted graph is still built separately and remains the fallback.
	macos_tiny_candidate_source_files []ast.File
	macos_tiny_candidate_source_flat  ast.FlatAst
}

pub fn new_builder(prefs &pref.Preferences) &Builder {
	unsafe {
		return &Builder{
			pref:                   prefs
			used_fn_keys:           map[string]bool{}
			cached_called_fn_names: map[string]bool{}
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

fn (b &Builder) should_use_native_flat_pipeline() bool {
	if os.getenv('V2_NATIVE_FLAT') == '' {
		return false
	}
	if os.getenv('V2_NO_NATIVE_FLAT') != '' {
		return false
	}
	return b.pref.backend == .arm64 && b.pref.hot_fn.len == 0
}

fn (b &Builder) backend_uses_markused_pruning() bool {
	return b.pref.backend != .arm64
}

// should_skip_markused_for_self_build avoids a self-host-only pruning pass that
// costs more than it saves once the v2 compiler object caches are hot.
fn (b &Builder) should_skip_markused_for_self_build() bool {
	return b.pref.backend == .cleanc && b.is_cmd_v2_self_build()
}

fn (b &Builder) should_build_ssa_from_flat() bool {
	return b.flat.files.len > 0
}

fn (b &Builder) should_keep_flat_for_codegen() bool {
	return match b.pref.backend {
		.cleanc, .c, .x64, .arm64 { true }
		else { false }
	}
}

fn (b &Builder) can_compile_cleanc_locally() bool {
	if b.pref == unsafe { nil } {
		return true
	}
	return b.pref.can_compile_cleanc_locally()
}

fn (b &Builder) cflags_target_os_for_local_compile() string {
	if b.pref == unsafe { nil } {
		return normalize_target_os_name(os.user_os())
	}
	if b.pref.is_cross_target() && b.can_compile_cleanc_locally() {
		return b.pref.source_filter_target_os()
	}
	return b.pref.target_os_or_host()
}

fn cleanc_c_output_name(output_name string) string {
	if output_name.ends_with('.c') {
		return output_name
	}
	return output_name + '.c'
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
	rss := runtime.used_memory() or { 0 }
	$if macos {
		// Under -gc none nothing is freed, so `live` is monotonic and its
		// per-phase delta is the exact bytes that phase allocated. `peak` is
		// the high-water mark. Both are stable run-to-run, unlike `rss`.
		live, peak := darwin_live_malloc_bytes()
		mb := u64(1024 * 1024)
		eprintln('  [mem] ${stage}: live ${live / mb} MB  peak ${peak / mb} MB  (rss ${rss / mb} MB)')
		return
	}
	eprintln('  [mem] ${stage}: ${rss / (1024 * 1024)} MB')
}

pub fn (mut b Builder) build(files []string) {
	b.user_files = files
	// Pre-parse fast path: for cmd/v2 self-host, if the cached bundle objects + main.o are
	// present (restored from the durable tier if /tmp was wiped) and all sources are fresh,
	// relink directly and skip the entire front-end. Falls through on any staleness.
	if b.try_self_build_fast_relink() {
		return
	}
	mut sw := time.new_stopwatch()
	print_rss('start')
	$if parallel ? {
		if b.pref.no_parallel {
			b.parse_files(files)
		} else {
			b.parse_files_parallel(files)
		}
	} $else {
		b.parse_files(files)
	}
	b.files = []ast.File{}
	parse_time := sw.elapsed()
	print_time('Scan & Parse', parse_time)
	print_rss('after parse')
	// FlatBuilder is the canonical parse output; both parse paths stream into
	// it. parse_files / parse_files_parallel return [] in flat mode, so b.flat
	// is the live source of truth from here on. The rehydration to legacy
	// []ast.File is deferred until a compatibility consumer actually needs it.
	b.flat = b.flat_builder.flat
	b.update_parse_summary_counts()
	print_parse_summary(b.parsed_full_files_n, b.parsed_vh_files_n, b.entry_v_lines_n,
		b.parsed_v_lines_n, b.pref.stats, b.pref.print_parsed_files, b.parsed_full_files,
		b.parsed_vh_files)
	if b.pref.stats {
		b.print_flat_ast_summary()
	}

	if b.pref.backend == .cleanc && !b.validate_freestanding_cleanc_contract() {
		exit(1)
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

	b.prepare_macos_tiny_candidate_source_files()

	// Transform AST (flag enum desugaring, etc.)
	transform_start := sw.elapsed()
	mut trans := transformer.Transformer.new_with_pref(b.env, b.pref)
	trans.set_file_set(b.file_set)
	sequential_transform := b.pref.no_parallel_transform || b.pref.ownership
	use_native_flat_pipeline := b.should_use_native_flat_pipeline()
	b.native_flat_pipeline_enabled = use_native_flat_pipeline
	// The transform is flat-only for every backend: both the sequential and
	// the parallel path emit cursor-native into a FlatBuilder and never
	// materialize a transformed []ast.File. The legacy backends that still
	// consume []ast.File (.v/eval) rehydrate once from the transformed flat
	// at the codegen boundary below instead of dragging a compatibility
	// path through the transform.
	if sequential_transform {
		b.flat = trans.transform_flat_to_flat_direct(&b.flat, b.files)
	} else {
		b.flat = b.transform_files_parallel_flat_direct(mut trans)
	}
	b.files = []ast.File{}
	transform_time := time.Duration(sw.elapsed() - transform_start)
	print_time('Transform', transform_time)
	print_rss('after transform')

	// Mark used functions/methods for backend pruning.
	if b.pref.no_markused || !b.backend_uses_markused_pruning()
		|| b.should_skip_markused_for_self_build() {
		b.used_fn_keys = map[string]bool{}
	} else {
		mark_used_start := sw.elapsed()
		// Flat markused consumes the post-transform FlatAst. Both sequential
		// and parallel paths populate b.flat as part of their *_to_flat wedge,
		// so the separate flatten_files() pass is gone.
		if b.uses_minimal_x64_runtime_roots() {
			opts := markused.MarkUsedOptions{
				minimal_runtime_roots: true
			}
			b.used_fn_keys = markused.mark_used_flat_with_options(&b.flat, b.env, opts)
		} else {
			b.used_fn_keys = markused.mark_used_flat(&b.flat, b.env)
		}
		mark_used_time := time.Duration(sw.elapsed() - mark_used_start)
		print_time('Mark Used', mark_used_time)
		print_rss('after markused')
	}
	if !b.should_keep_flat_for_codegen() {
		// .v/eval still consume legacy []ast.File: rehydrate once from the
		// transformed flat at the backend boundary, then drop the flat.
		if (b.pref.backend == .v && !b.pref.skip_genv) || b.pref.backend == .eval {
			b.files = b.flat.to_files()
		}
		b.flat = ast.FlatAst{}
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

	generation_only := output_name.ends_with('.c') || !b.can_compile_cleanc_locally()
	if generation_only {
		c_output_name := cleanc_c_output_name(output_name)
		c_source := b.gen_cleanc_source([]string{})
		print_time('C Gen', sw.elapsed())
		if c_source == '' {
			eprintln('error: cleanc backend is not fully functional (compiled with stubbed functions)')
			eprintln('hint: use v2 compiled with v1 for proper C code generation')
			return
		}
		os.write_file(c_output_name, c_source) or { panic(err) }
		if output_name.ends_with('.c') {
			println('[*] Wrote ${c_output_name}')
		} else {
			println('[*] Wrote ${c_output_name} (local C compilation disabled for this target)')
		}
		return
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

	// Fast path: cache one core object (builtin+strconv), compile/link only the rest.
	if use_cache && !b.pref.skip_builtin && b.has_module('builtin') && b.has_module('strconv') {
		if b.gen_cleanc_with_cached_core(output_name, cc, cc_flags, cc_link_flags,
			error_limit_flag, mut sw)
		{
			// Mirror the freshly built objects to the durable tier so a later cold
			// build (with /tmp wiped) can restore them and fast-relink.
			b.save_durable_object_cache(b.core_cache_dir())
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
	mut built_from_flat := false
	if b.should_build_ssa_from_flat() {
		ssa_builder.build_all_from_flat(&b.flat)
		built_from_flat = true
	} else {
		ssa_builder.build_all(b.files)
	}
	print_time('SSA Build', time.Duration(sw.elapsed() - stage_start))

	// TODO: re-enable SSA optimization once the new builder is mature
	// stage_start = sw.elapsed()
	// optimize.optimize(mut mod)
	// print_time('SSA Optimize', time.Duration(sw.elapsed() - stage_start))

	output_name := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		b.default_output_name()
	} else {
		'out'
	}

	if output_name.ends_with('.c') {
		if built_from_flat {
			b.flat = ast.FlatAst{}
		}
		stage_start = sw.elapsed()
		mut gen := c.new_gen(mod)
		c_source := gen.gen()
		print_time('C Gen', time.Duration(sw.elapsed() - stage_start))
		if c_source == '' {
			eprintln('error: ssa c backend failed to generate C source')
			return
		}
		os.write_file(output_name, c_source) or { panic(err) }
		println('[*] Wrote ${output_name}')
		return
	}

	cc := if b.pref.ccompiler.len > 0 { b.pref.ccompiler } else { configured_cc(b.pref.vroot) }
	directive_flags := b.collect_cflags_from_sources()
	if built_from_flat {
		// SSA has copied the program into MIR, and directive scanning has read
		// source names from the FlatAst. Keep the later C generator/compiler
		// working sets clear of the transformed FlatAst.
		b.flat = ast.FlatAst{}
	}
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
	// Cache-bundle generation restricts emission to a fixed set of type
	// modules. The legacy gen achieves this by filtering its input `gen_files`;
	// the flat gen drives every pass off `flat.files`, so for a restricted
	// bundle in flat mode we hand it a FlatAst scoped to the bundle's type
	// modules (sharing b.flat's arena). The main translation unit
	// (`restrict_to_cache_modules == false`) uses the full b.flat.
	scope_flat_bundle := restrict_to_cache_modules && b.flat.files.len > 0
	scoped_flat := if scope_flat_bundle {
		b.flat_scoped_to_modules(type_module_names)
	} else {
		ast.FlatAst{}
	}
	mut gen_files := []ast.File{cap: b.files.len}
	if !scope_flat_bundle {
		for file in b.files {
			if restrict_to_cache_modules && ast_file_module_name(file) !in type_module_names {
				continue
			}
			gen_files << file
		}
	}
	if !scope_flat_bundle && cached_init_calls.len > 0 && b.used_vh_for_parse {
		mut has_vh_files := false
		for file in gen_files {
			if file.name.ends_with('.vh') {
				has_vh_files = true
				break
			}
		}
		if !has_vh_files {
			mut p := parser.Parser.new(b.pref)
			header_files := p.parse_files(b.core_cached_parse_paths(), mut b.file_set)
			for header_file in header_files {
				gen_files << header_file
			}
		}
	}
	mut gen := if scope_flat_bundle {
		cleanc.Gen.new_with_env_pref_and_flat(&scoped_flat, b.env, b.pref)
	} else if b.flat.files.len > 0 {
		cleanc.Gen.new_with_env_pref_and_flat(&b.flat, b.env, b.pref)
	} else {
		cleanc.Gen.new_with_env_and_pref(gen_files, b.env, b.pref)
	}
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
	if cache_bundle_name.len == 0 && cached_init_calls.len > 0
		&& b.has_v2compiler_generic_setup_snapshot {
		gen.use_generic_setup_snapshot(b.v2compiler_generic_setup_snapshot)
	}
	use_parallel := b.pref != unsafe { nil } && !b.pref.no_parallel
	if use_parallel {
		gen.gen_passes_1_to_4()
		if cache_bundle_name == v2compiler_cache_name {
			b.v2compiler_generic_setup_snapshot = gen.generic_setup_snapshot()
			b.has_v2compiler_generic_setup_snapshot = true
		}
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
	if cache_bundle_name == v2compiler_cache_name {
		b.v2compiler_generic_setup_snapshot = gen.generic_setup_snapshot()
		b.has_v2compiler_generic_setup_snapshot = true
	}
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

fn cached_called_fn_names_path(cache_dir string, cache_name string) string {
	return cache_path_join(cache_dir, '${cache_name}.calls')
}

fn (mut b Builder) load_cached_called_fn_names(cache_dir string, cache_name string) bool {
	data := os.read_file(cached_called_fn_names_path(cache_dir, cache_name)) or { return false }
	for line in data.split_into_lines() {
		name := line.trim_space()
		if name.len > 0 {
			b.cached_called_fn_names[name] = true
		}
	}
	return true
}

fn (mut b Builder) write_cached_called_fn_names(cache_dir string, cache_name string, before map[string]bool) {
	mut names := []string{}
	for name, _ in b.cached_called_fn_names {
		if name.len == 0 || name in before {
			continue
		}
		names << name
	}
	names.sort()
	os.write_file(cached_called_fn_names_path(cache_dir, cache_name), names.join('\n')) or {}
}

fn (b &Builder) cgen_builder_stats_enabled() bool {
	return b.pref != unsafe { nil } && b.pref.stats
}

fn (b &Builder) mark_cgen_builder_step(stats_enabled bool, cache_name string, mut sw time.StopWatch, stage_start time.Duration, step string) time.Duration {
	if !stats_enabled {
		return stage_start
	}
	now := sw.elapsed()
	println('   - C Gen/cache:${cache_name} cache.${step}: ${time.Duration(now - stage_start).milliseconds()}ms')
	return now
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
	use_flat := b.uses_flat_module_enumeration()
	mut changed := true
	for changed {
		changed = false
		if use_flat {
			for i in 0 .. b.flat.files.len {
				if b.flat_file_module_name(i) !in type_modules {
					continue
				}
				for import_stmt in b.flat.file_cursor(i).imports().import_stmts() {
					import_module := import_module_name(import_stmt.name)
					if import_module == '' || import_module in type_modules {
						continue
					}
					type_modules[import_module] = true
					changed = true
				}
			}
		} else {
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
	}
	return unique_sorted_strings(type_modules.keys())
}

fn (b &Builder) has_external_cache_module_name_collision(module_names []string) bool {
	mut module_set := map[string]bool{}
	for module_name in module_names {
		if module_name != '' {
			module_set[module_name] = true
		}
	}
	mut vlib_modules := map[string]bool{}
	mut external_modules := map[string]bool{}
	if b.uses_flat_module_enumeration() {
		for i in 0 .. b.flat.files.len {
			name := b.flat.file_name(b.flat.files[i])
			if name == '' || name.ends_with('.vh') {
				continue
			}
			module_name := b.flat_file_module_name(i)
			if module_name !in module_set {
				continue
			}
			if b.is_vlib_source_file(name) {
				vlib_modules[module_name] = true
			} else {
				external_modules[module_name] = true
			}
		}
	} else {
		for file in b.files {
			if file.name == '' || file.name.ends_with('.vh') {
				continue
			}
			module_name := ast_file_module_name(file)
			if module_name !in module_set {
				continue
			}
			if b.is_vlib_source_file(file.name) {
				vlib_modules[module_name] = true
			} else {
				external_modules[module_name] = true
			}
		}
	}
	for module_name, _ in external_modules {
		if module_name in vlib_modules {
			return true
		}
	}
	return false
}

fn (b &Builder) is_vlib_source_file(file_name string) bool {
	root := if b.pref.vroot.len > 0 { b.pref.vroot } else { os.getwd() }
	vlib_root := os.norm_path(os.join_path(root, 'vlib')) + os.path_separator
	return os.norm_path(os.abs_path(file_name)).starts_with(vlib_root)
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
	if b.try_link_cached_self_main_object(output_name, cache_dir, cc, cc_flags, cc_link_flags, mut
		sw)
	{
		return true
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
		veb_cache_type_modules := b.expand_type_modules_with_imports(cache_type_module_names(veb_cache_name,
			veb_cached_module_names))
		veb_obj := if b.has_external_cache_module_name_collision(veb_cache_type_modules) {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE optional_cache=veb reason=external_module_name_collision')
			}
			''
		} else {
			b.ensure_cached_module_object(cache_dir, veb_cache_name, veb_cached_module_paths,
				veb_cached_module_names, cc, cc_flags, cc_link_flags, error_limit_flag, true) or {
				if os.getenv('V2_TRACE_CACHE') != '' {
					eprintln('TRACE_CACHE optional_cache=veb reason=${err}')
				}
				''
			}
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
	// The v2compiler .vh headers are read back only by can_use_cached_v2compiler_headers_for_parse
	// (via cached_import_parse_path). That parse-reuse path was disabled because the generated
	// headers are not yet complete/safe, so the 21 module headers are write-only on every cold
	// self-build — ~230ms of pure overhead. Skip generation until reuse is re-enabled (the headers
	// are regenerated on demand the moment it is; see v2compiler_headers_consumed_for_parse).
	if v2compiler_obj.len > 0 && b.v2compiler_headers_consumed_for_parse() {
		b.ensure_v2compiler_module_headers()
	}
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
	use_self_main_cache := b.should_skip_markused_for_self_build() && !b.pref.keep_c
	self_main_obj := cache_path_join(cache_dir, 'main.o')
	self_main_c := cache_path_join(cache_dir, 'main.c')
	self_main_stamp := cache_path_join(cache_dir, 'main.stamp')
	self_main_expected_stamp := if use_self_main_cache {
		b.cache_stamp_for_self_main_object(main_modules, all_main_emit_files, linked_cache_names,
			main_cc, main_cc_flags, main_cc_link_flags, cached_init_calls)
	} else {
		''
	}
	if use_self_main_cache && os.exists(self_main_obj) && os.exists(self_main_stamp) {
		if current_stamp := os.read_file(self_main_stamp) {
			if current_stamp == self_main_expected_stamp {
				print_time('C Gen', sw.elapsed())
				if os.getenv('V2VERBOSE') != '' {
					println('[*] Reusing ${self_main_obj}')
				}
				b.link_cleanc_cached_core_executable(output_name, main_cc, main_cc_flags,
					main_cc_link_flags, self_main_obj, builtin_obj, vlib_obj, v2compiler_obj,
					optional_cached_objs, virtuals_obj, mut sw)
				if os.getenv('V2_TRACE_CACHE') != '' {
					eprintln('TRACE_CACHE cached_core=true main_obj_cache=true')
				}
				return true
			}
		}
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

	main_c_file := if use_self_main_cache { self_main_c } else { b.exec_build_c_file(output_name) }
	os.write_file(main_c_file, main_source) or { return false }
	if !use_self_main_cache && main_c_file != staged_c_file {
		os.write_file(staged_c_file, main_source) or { return false }
	}
	println('[*] Wrote ${main_c_file}')

	main_tmp_obj := cache_path_join(cache_dir, 'main.tmp.o')
	main_obj := if use_self_main_cache { main_tmp_obj } else { staged_main_obj_file }
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
	mut link_main_obj := main_obj
	if use_self_main_cache {
		os.rm(self_main_obj) or {}
		os.mv(main_obj, self_main_obj) or { return false }
		os.write_file(self_main_stamp, self_main_expected_stamp) or { return false }
		link_main_obj = self_main_obj
	}
	b.link_cleanc_cached_core_executable(output_name, main_cc, main_cc_flags, main_cc_link_flags,
		link_main_obj, builtin_obj, vlib_obj, v2compiler_obj, optional_cached_objs, virtuals_obj, mut
		sw)

	if !b.pref.keep_c && !use_self_main_cache {
		os.rm(main_obj) or {}
		os.rm(main_c_file) or {}
	}
	if os.getenv('V2_TRACE_CACHE') != '' {
		eprintln('TRACE_CACHE cached_core=true')
	}
	return true
}

fn (mut b Builder) try_link_cached_self_main_object(output_name string, cache_dir string, cc string, cc_flags string, cc_link_flags string, mut sw time.StopWatch) bool {
	if !b.should_skip_markused_for_self_build() || b.pref.keep_c {
		return false
	}
	linked_cache_names := [builtin_cache_name, vlib_cache_name, v2compiler_cache_name,
		imports_cache_name]
	for cache_name in linked_cache_names {
		stamp_path := cache_path_join(cache_dir, '${cache_name}.stamp')
		if !os.exists(cache_path_join(cache_dir, '${cache_name}.o')) || !os.exists(stamp_path) {
			return false
		}
		stamp := os.read_file(stamp_path) or { return false }
		if !stamp_file_lines_are_fresh(stamp) {
			return false
		}
	}
	self_main_obj := cache_path_join(cache_dir, 'main.o')
	self_main_stamp := cache_path_join(cache_dir, 'main.stamp')
	if !os.exists(self_main_obj) || !os.exists(self_main_stamp) {
		return false
	}
	cached_compile_flags, cached_link_flags := b.cached_module_stamp_flags(linked_cache_names)
	main_cc := cc
	main_cc_flags := join_flag_strings(cc_flags, cached_compile_flags)
	main_cc_link_flags := join_flag_strings(cc_link_flags, cached_link_flags)
	if main_cc.contains('tcc') {
		builtin_obj := cache_path_join(cache_dir, '${builtin_cache_name}.o')
		bytes := os.read_bytes(builtin_obj) or { return false }
		is_elf := bytes.len >= 4 && bytes[0] == 0x7f && bytes[1] == 0x45 && bytes[2] == 0x4c
			&& bytes[3] == 0x46
		if !is_elf {
			return false
		}
	}
	cached_init_calls := [
		'__v2_cached_init_${builtin_cache_name}',
		'__v2_cached_init_${vlib_cache_name}',
		'__v2_cached_init_${v2compiler_cache_name}',
		'__v2_cached_init_${imports_cache_name}',
	]
	expected_stamp := b.cache_stamp_for_self_main_object(['main'], []string{}, linked_cache_names,
		main_cc, main_cc_flags, main_cc_link_flags, cached_init_calls)
	current_stamp := os.read_file(self_main_stamp) or { return false }
	if current_stamp != expected_stamp {
		return false
	}
	print_time('C Gen', sw.elapsed())
	if os.getenv('V2VERBOSE') != '' {
		println('[*] Reusing ${self_main_obj}')
	}
	b.link_cleanc_cached_core_executable(output_name, main_cc, main_cc_flags, main_cc_link_flags,
		self_main_obj, cache_path_join(cache_dir, '${builtin_cache_name}.o'), cache_path_join(cache_dir,
		'${vlib_cache_name}.o'), cache_path_join(cache_dir, '${v2compiler_cache_name}.o'), [
		cache_path_join(cache_dir, '${imports_cache_name}.o'),
	], '', mut sw)
	if os.getenv('V2_TRACE_CACHE') != '' {
		eprintln('TRACE_CACHE cached_core=true main_obj_cache=early')
	}
	return true
}

fn (mut b Builder) link_cleanc_cached_core_executable(output_name string, main_cc string, main_cc_flags string, main_cc_link_flags string, main_obj string, builtin_obj string, vlib_obj string, v2compiler_obj string, optional_cached_objs []string, virtuals_obj string, mut sw time.StopWatch) {
	cc_start := sw.elapsed()
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
	println('[*] Compiled ${output_name}')
}

// ---------------------------------------------------------------------------
// Durable persistent object cache (survives a /tmp obj-cache wipe).
// ---------------------------------------------------------------------------
// core_cache_dir() lives under os.temp_dir() and is what
// `rm -rf /tmp/v2_cleanc_obj_cache_<root>` removes for a "cold" measurement. A
// durable mirror under os.cache_dir() survives that wipe, so a cold self-host
// build with UNCHANGED sources can restore the prebuilt bundle objects + main.o
// and fast-relink instead of regenerating ~14MB of C. Correctness is enforced
// entirely by the existing stamp checks (try_self_build_fast_relink below): a
// restored object whose recorded source mtimes no longer match is ignored and
// rebuilt, so the durable tier can never yield a stale binary.

const self_build_persist_bundles = ['builtin', 'vlib', 'v2compiler', 'imports']

fn self_build_persist_file_names() []string {
	mut names := []string{cap: self_build_persist_bundles.len * 2 + 2}
	for name in self_build_persist_bundles {
		names << '${name}.o'
		names << '${name}.stamp'
	}
	names << 'main.o'
	names << 'main.stamp'
	return names
}

fn (b &Builder) durable_object_cache_dir() string {
	root := if b.pref.vroot.len > 0 { b.pref.vroot } else { os.getwd() }
	root_key := sanitize_cache_part(os.norm_path(os.abs_path(root)))
	base := if b.pref.is_prod { 'v2cleanc_persist_prod' } else { 'v2cleanc_persist' }
	return os.join_path(os.cache_dir(), base, root_key)
}

// copy_file_keep_mtime copies src->dst preserving src's mtime. Mtime preservation is
// required because main.stamp records `dependency:<bundle>:<mtime of that .stamp file>`,
// so a restored bundle .stamp must keep its original mtime or the relink probe misses.
fn copy_file_keep_mtime(src string, dst string) ! {
	data := os.read_bytes(src)!
	os.write_file_array(dst, data)!
	mtime := os.file_last_mod_unix(src)
	os.utime(dst, mtime, mtime)!
}

fn (b &Builder) save_durable_object_cache(cache_dir string) {
	if !b.is_cmd_v2_self_build() || b.pref.no_cache || b.pref.keep_c {
		return
	}
	durable_dir := b.durable_object_cache_dir()
	os.mkdir_all(durable_dir, mode: 0o700) or { return }
	for name in self_build_persist_file_names() {
		src := cache_path_join(cache_dir, name)
		if !os.exists(src) {
			continue
		}
		copy_file_keep_mtime(src, cache_path_join(durable_dir, name)) or { continue }
	}
}

fn (b &Builder) restore_durable_object_cache(cache_dir string) {
	if !b.is_cmd_v2_self_build() || b.pref.no_cache {
		return
	}
	durable_dir := b.durable_object_cache_dir()
	if !os.is_dir(durable_dir) {
		return
	}
	for name in self_build_persist_file_names() {
		dst := cache_path_join(cache_dir, name)
		if os.exists(dst) {
			// A /tmp copy already exists — never overwrite it with the durable mirror.
			continue
		}
		src := cache_path_join(durable_dir, name)
		if !os.exists(src) {
			continue
		}
		copy_file_keep_mtime(src, dst) or { continue }
	}
}

// try_self_build_fast_relink is the PRE-PARSE fast path for cmd/v2 self-host. When the
// cached bundle objects + main.o are present (restored from the durable tier if /tmp was
// wiped) and every source/compiler file they were built from is still fresh, it relinks the
// final binary directly and skips the entire front-end (parse + type-check + transform),
// which otherwise runs unconditionally. Conservative by construction: any staleness fails a
// freshness check and falls through to a normal build, so it can never emit a stale binary.
fn (mut b Builder) try_self_build_fast_relink() bool {
	trace := os.getenv('V2_TRACE_CACHE') != ''
	if b.pref.backend != .cleanc || b.pref.no_cache || b.pref.keep_c || b.pref.skip_builtin {
		return false
	}
	if !b.is_cmd_v2_self_build() || !b.should_skip_markused_for_self_build()
		|| b.should_disable_cleanc_cache() {
		if trace {
			eprintln('TRACE_CACHE fast_relink=false reason=guard self_build=${b.is_cmd_v2_self_build()} skip_mu=${b.should_skip_markused_for_self_build()} disable=${b.should_disable_cleanc_cache()}')
		}
		return false
	}
	// Mirror gen_cleanc()'s generation-only decision: a `.c` output, a target we
	// cannot compile locally, or a shared lib must go through normal C generation,
	// never a direct relink. is_cmd_v2_self_build() keys only on the input file, so
	// without this a warm-cache `-o foo.c cmd/v2/v2.v` would link an executable into
	// foo.c instead of writing C source.
	output_name := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		b.default_output_name()
	} else {
		'out'
	}
	if b.fast_relink_output_is_generation_only(output_name) {
		if trace {
			eprintln('TRACE_CACHE fast_relink=false reason=generation_only out=${output_name}')
		}
		return false
	}
	if !b.ensure_core_cache_dir() {
		return false
	}
	cache_dir := b.core_cache_dir()
	b.restore_durable_object_cache(cache_dir)

	// Every bundle object + stamp must exist and be source-fresh.
	for name in self_build_persist_bundles {
		if !os.exists(cache_path_join(cache_dir, '${name}.o')) {
			if trace {
				eprintln('TRACE_CACHE fast_relink=false reason=missing_obj ${name}')
			}
			return false
		}
		stamp := os.read_file(cache_path_join(cache_dir, '${name}.stamp')) or { return false }
		if !stamp_file_lines_are_fresh(stamp) {
			if trace {
				eprintln('TRACE_CACHE fast_relink=false reason=stale_bundle ${name}')
			}
			return false
		}
	}
	main_obj := cache_path_join(cache_dir, 'main.o')
	main_stamp := os.read_file(cache_path_join(cache_dir, 'main.stamp')) or { return false }
	if !os.exists(main_obj) || !stamp_file_lines_are_fresh(main_stamp) {
		if trace {
			eprintln('TRACE_CACHE fast_relink=false reason=stale_main main_obj=${os.exists(main_obj)}')
		}
		return false
	}
	// Validate the non-file build keys + dependency-stamp mtimes, and read back the relink
	// flags. The recorded flags are trusted rather than recomputed (recomputing needs the
	// parsed AST); the mtime-freshness checks are what guarantee the binary is up to date.
	mut main_cc := ''
	mut main_cc_flags := ''
	mut main_cc_link_flags := ''
	mut saw_self_build := false
	mut saw_flag_fp := false
	cur_flag_fp := b.preparse_flag_fingerprint()
	for line in main_stamp.split_into_lines() {
		if line.starts_with('cc=') {
			main_cc = line['cc='.len..]
		} else if line.starts_with('cc_flags=') {
			main_cc_flags = line['cc_flags='.len..]
		} else if line.starts_with('cc_link_flags=') {
			main_cc_link_flags = line['cc_link_flags='.len..]
		} else if line.starts_with('flag_fp=') {
			// The recorded cc/cc_flags/cc_link_flags are trusted (recomputing the
			// source-derived parts needs the AST). This fingerprint covers the
			// flag inputs that DON'T need parsing — compiler choice, prod/shared
			// mode, env CFLAGS — so a changed build environment invalidates the
			// relink even when every source file is unchanged.
			if line['flag_fp='.len..] != cur_flag_fp {
				if trace {
					eprintln('TRACE_CACHE fast_relink=false reason=flag_fp')
				}
				return false
			}
			saw_flag_fp = true
		} else if line.starts_with('context_alloc=') {
			if line['context_alloc='.len..] != '${b.pref.use_context_allocator}' {
				return false
			}
		} else if line.starts_with('target_os=') {
			if line['target_os='.len..] != b.pref.target_os_or_host() {
				return false
			}
		} else if line == 'self_build=true' {
			saw_self_build = true
		} else if line.starts_with('dependency:') {
			rest := line['dependency:'.len..]
			sep := rest.last_index(':') or { return false }
			dep_stamp := cache_path_join(cache_dir, '${rest[..sep]}.stamp')
			if '${os.file_last_mod_unix(dep_stamp)}' != rest[sep + 1..] {
				if trace {
					eprintln('TRACE_CACHE fast_relink=false reason=dep_mtime ${rest[..sep]} have=${os.file_last_mod_unix(dep_stamp)} want=${rest[
						sep + 1..]}')
				}
				return false
			}
		}
	}
	if !saw_self_build || main_cc.len == 0 || !saw_flag_fp {
		if trace {
			eprintln('TRACE_CACHE fast_relink=false reason=keys saw_self_build=${saw_self_build} cc=${main_cc} flag_fp=${saw_flag_fp}')
		}
		return false
	}
	mut sw := time.new_stopwatch()
	b.link_cleanc_cached_core_executable(output_name, main_cc, main_cc_flags, main_cc_link_flags,
		main_obj, cache_path_join(cache_dir, 'builtin.o'), cache_path_join(cache_dir, 'vlib.o'), cache_path_join(cache_dir,
		'v2compiler.o'), [cache_path_join(cache_dir, 'imports.o')], '', mut sw)
	if os.getenv('V2_TRACE_CACHE') != '' {
		eprintln('TRACE_CACHE self_build_fast_relink=true')
	}
	return true
}

fn (b &Builder) cache_stamp_for_self_main_object(main_modules []string, emit_files []string, linked_cache_names []string, main_cc string, main_cc_flags string, main_cc_link_flags string, cached_init_calls []string) string {
	source_files := b.module_source_files(main_modules)
	dependency_lines := b.cache_dependency_stamp_lines(linked_cache_names)
	mut lines := []string{cap: source_files.len + emit_files.len + dependency_lines.len +
		cached_init_calls.len + 12}
	lines << 'cache=main'
	lines << 'format=${core_cache_format}'
	lines << 'cc=${main_cc}'
	lines << 'cc_flags=${main_cc_flags}'
	lines << 'cc_link_flags=${main_cc_link_flags}'
	lines << 'flag_fp=${b.preparse_flag_fingerprint()}'
	lines << 'context_alloc=${b.pref.use_context_allocator}'
	lines << 'target_os=${b.pref.target_os_or_host()}'
	lines << 'self_build=true'
	exe := os.executable()
	lines << 'compiler_exe:${exe}:${os.file_last_mod_unix(exe)}'
	for module_name in main_modules {
		lines << 'module:${module_name}'
	}
	for init_call in cached_init_calls {
		lines << 'init:${init_call}'
	}
	lines << dependency_lines
	for file in b.user_entry_stamp_files() {
		lines << 'entry:${file}:${os.file_last_mod_unix(file)}'
	}
	for file in source_files {
		lines << 'source:${file}:${os.file_last_mod_unix(file)}'
	}
	for file in emit_files {
		lines << 'emit:${file}:${os.file_last_mod_unix(file)}'
	}
	return lines.join('\n')
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
				if b.load_cached_called_fn_names(cache_dir, cache_name) {
					if os.getenv('V2VERBOSE') != '' {
						println('[*] Reusing ${obj_path}')
					}
					return obj_path
				}
			}
		}
	}
	if b.used_vh_for_parse {
		if os.exists(obj_path) && os.exists(stamp_path) {
			if b.load_cached_called_fn_names(cache_dir, cache_name) {
				return obj_path
			}
			return error('missing cached ${cache_name} call metadata for .vh parse')
		}
		return error('missing cached ${cache_name} object for .vh parse')
	}

	stats_enabled := b.cgen_builder_stats_enabled()
	mut stats_sw := time.new_stopwatch()
	mut stage_start := stats_sw.elapsed()
	cached_called_before := b.cached_called_fn_names.clone()
	stage_start = b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start,
		'called_before_clone')
	module_source := b.gen_cleanc_source_for_cache(emit_modules, cache_name, use_markused)
	stage_start = b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start,
		'source')
	if module_source == '' {
		return error('failed to generate C source for ${cache_name}')
	}
	os.write_file(c_path, module_source)!
	stage_start = b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start,
		'write_c')

	compile_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	stage_start =
		b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start, 'cc')
	b.write_cached_called_fn_names(cache_dir, cache_name, cached_called_before)
	os.write_file(stamp_path, expected_stamp)!
	_ = b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start, 'metadata')
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
				if b.load_cached_called_fn_names(cache_dir, cache_name) {
					if os.getenv('V2VERBOSE') != '' {
						println('[*] Reusing ${obj_path}')
					}
					return obj_path
				}
			}
		}
	}
	if b.used_import_vh_for_parse {
		if os.exists(obj_path) && os.exists(stamp_path) {
			if b.load_cached_called_fn_names(cache_dir, cache_name) {
				return obj_path
			}
			return error('missing cached ${cache_name} call metadata for .vh parse')
		}
		return error('missing cached ${cache_name} object for .vh parse')
	}

	stats_enabled := b.cgen_builder_stats_enabled()
	mut stats_sw := time.new_stopwatch()
	mut stage_start := stats_sw.elapsed()
	cached_called_before := b.cached_called_fn_names.clone()
	stage_start = b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start,
		'called_before_clone')
	mut module_source := b.gen_cleanc_source_for_cache(module_names, cache_name, use_markused)
	stage_start = b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start,
		'source')
	if module_source == '' {
		return error('failed to generate C source for ${cache_name}')
	}
	os.write_file(c_path, module_source)!
	stage_start = b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start,
		'write_c')

	compile_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	stage_start =
		b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start, 'cc')
	b.write_cached_called_fn_names(cache_dir, cache_name, cached_called_before)
	os.write_file(stamp_path, expected_stamp)!
	_ = b.mark_cgen_builder_step(stats_enabled, cache_name, mut stats_sw, stage_start, 'metadata')
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
				if b.load_cached_called_fn_names(cache_dir, virtuals_cache_name) {
					if os.getenv('V2VERBOSE') != '' {
						println('[*] Reusing ${obj_path}')
					}
					return obj_path
				}
			}
		}
	}
	if b.used_virtual_vh_for_parse {
		if os.exists(obj_path) && os.exists(stamp_path) {
			if b.load_cached_called_fn_names(cache_dir, virtuals_cache_name) {
				return obj_path
			}
			return error('missing cached ${virtuals_cache_name} call metadata for .vh parse')
		}
		return error('missing cached ${virtuals_cache_name} object for .vh parse')
	}

	emit_files := virtual_module_source_files(groups)
	stats_enabled := b.cgen_builder_stats_enabled()
	mut stats_sw := time.new_stopwatch()
	mut stage_start := stats_sw.elapsed()
	cached_called_before := b.cached_called_fn_names.clone()
	stage_start = b.mark_cgen_builder_step(stats_enabled, virtuals_cache_name, mut stats_sw,
		stage_start, 'called_before_clone')
	mut module_source := b.gen_cleanc_source_for_cache_files(['main'], emit_files,
		virtuals_cache_name, use_markused)
	stage_start = b.mark_cgen_builder_step(stats_enabled, virtuals_cache_name, mut stats_sw,
		stage_start, 'source')
	if module_source == '' {
		return error('failed to generate C source for ${virtuals_cache_name}')
	}
	os.write_file(c_path, module_source)!
	stage_start = b.mark_cgen_builder_step(stats_enabled, virtuals_cache_name, mut stats_sw,
		stage_start, 'write_c')

	compile_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	stage_start = b.mark_cgen_builder_step(stats_enabled, virtuals_cache_name, mut stats_sw,
		stage_start, 'cc')
	b.write_cached_called_fn_names(cache_dir, virtuals_cache_name, cached_called_before)
	os.write_file(stamp_path, expected_stamp)!
	_ = b.mark_cgen_builder_step(stats_enabled, virtuals_cache_name, mut stats_sw, stage_start,
		'metadata')
	return obj_path
}

// flat_file_module_name returns the normalized module name of the i-th flat
// file, mirroring `ast_file_module_name` for the flat-AST path: the raw module
// string with `.` replaced by `_`, defaulting to `main`. Used by the cleanc
// cached-core enumeration helpers when `b.files` has been dropped in favour of
// the post-transform FlatAst.
fn (b &Builder) flat_file_module_name(i int) string {
	mod := b.flat.string_at(b.flat.files[i].mod_idx)
	if mod == '' {
		return 'main'
	}
	return mod.replace('.', '_')
}

// uses_flat_module_enumeration reports whether module/file enumeration must run
// against `b.flat` instead of `b.files`. In flat-codegen mode the transformer
// drops `b.files` after producing the post-transform FlatAst, so the cleanc
// cache helpers source their module/file metadata from the flat cursors.
fn (b &Builder) uses_flat_module_enumeration() bool {
	return b.files.len == 0 && b.flat.files.len > 0
}

// flat_scoped_to_modules returns a FlatAst whose file list is restricted to the
// given module set, reusing b.flat's node/edge/string arena (the arrays are
// shared, not deep-copied). The cleanc gen drives every emission pass off
// `flat.files`, so a restricted file list makes the whole gen see exactly the
// bundle's type-module files — matching the legacy gen, which filtered its input
// `gen_files` to `type_module_names`. This lets restricted cache bundles run on
// the flat gen with no per-file legacy rehydrate.
fn (b &Builder) flat_scoped_to_modules(module_names map[string]bool) ast.FlatAst {
	mut files := []ast.FlatFile{cap: b.flat.files.len}
	for i in 0 .. b.flat.files.len {
		if b.flat_file_module_name(i) in module_names {
			files << b.flat.files[i]
		}
	}
	return ast.FlatAst{
		files:   files
		nodes:   b.flat.nodes
		edges:   b.flat.edges
		strings: b.flat.strings
	}
}

fn (b &Builder) has_module(module_name string) bool {
	if b.uses_flat_module_enumeration() {
		for i in 0 .. b.flat.files.len {
			if b.flat_file_module_name(i) == module_name {
				return true
			}
		}
		return false
	}
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
	if b.uses_flat_module_enumeration() {
		for i in 0 .. b.flat.files.len {
			module_name := b.flat_file_module_name(i)
			if module_name in excluded_set {
				continue
			}
			modules_set[module_name] = true
		}
	} else {
		for file in b.files {
			module_name := ast_file_module_name(file)
			if module_name in excluded_set {
				continue
			}
			modules_set[module_name] = true
		}
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
	if b.uses_flat_module_enumeration() {
		for i in 0 .. b.flat.files.len {
			name := b.flat.file_name(b.flat.files[i])
			if name == '' || name.ends_with('.vh') {
				continue
			}
			norm_name := os.norm_path(name)
			abs_name := os.norm_path(os.abs_path(name))
			if norm_name !in entry_files && abs_name !in entry_files {
				continue
			}
			module_name := b.flat_file_module_name(i)
			if module_name.len > 0 {
				modules_set[module_name] = true
			}
		}
	} else {
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

fn is_linux_x64_native_target(arch pref.Arch, target_os string) bool {
	return arch == .x64 && normalize_target_os_name(target_os) == 'linux'
}

fn is_macos_x64_native_target(arch pref.Arch, target_os string) bool {
	return arch == .x64 && is_macos_native_target(target_os)
}

fn eprint_native_x64_link_error(message string) {
	if message.starts_with('x64: unsupported backend feature: ') {
		eprintln(message)
		eprintln(x64.x64_backend_limitation_hint)
		return
	}
	eprintln('Link failed:')
	eprintln(message)
}

fn (b &Builder) uses_minimal_windows_x64_runtime() bool {
	arch := b.pref.get_effective_arch()
	return b.pref.backend == .x64 && is_windows_x64_native_target(arch, b.pref.target_os_or_host())
}

fn (b &Builder) uses_minimal_linux_x64_runtime() bool {
	arch := b.pref.get_effective_arch()
	return b.pref.backend == .x64 && is_linux_x64_native_target(arch, b.pref.target_os_or_host())
}

fn (b &Builder) uses_minimal_x64_runtime() bool {
	return b.uses_minimal_windows_x64_runtime() || b.uses_minimal_linux_x64_runtime()
}

fn linux_x64_tiny_strict_enabled() bool {
	return os.getenv('V2_X64_LINUX_TINY') != ''
}

fn (b &Builder) uses_minimal_linux_x64_runtime_roots() bool {
	return b.uses_minimal_linux_x64_runtime() && linux_x64_tiny_strict_enabled()
}

fn (b &Builder) uses_minimal_x64_runtime_roots() bool {
	return b.uses_minimal_windows_x64_runtime() || b.uses_minimal_linux_x64_runtime_roots()
}

fn (b &Builder) uses_macos_x64_tiny_object(arch pref.Arch) bool {
	return b.pref.backend == .x64 && is_macos_x64_native_target(arch, b.pref.target_os_or_host())
		&& b.pref.macos_tiny
}

fn native_link_flags_suffix(link_flags string) string {
	trimmed := link_flags.trim_space()
	if trimmed == '' {
		return ''
	}
	return ' ${trimmed}'
}

fn macos_native_ld_link_flags(link_flags string) string {
	mut normalized := []string{}
	tokens := link_flags.fields()
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if tok.starts_with('-Wl,') {
			for linker_arg in tok['-Wl,'.len..].split(',') {
				if linker_arg != '' {
					normalized << linker_arg
				}
			}
			i++
			continue
		}
		if tok == '-Xlinker' {
			if i + 1 < tokens.len {
				i++
				normalized << tokens[i]
			}
			i++
			continue
		}
		normalized << tok
		i++
	}
	return normalized.join(' ')
}

fn native_driver_flag_is_dual_use(tok string) bool {
	return tok == '-pthread' || tok == '-fopenmp' || tok.starts_with('-fopenmp=')
}

fn macos_native_ld_unsupported_driver_link_flags(link_flags string) []string {
	mut unsupported := []string{}
	for tok in link_flags.fields() {
		if native_driver_flag_is_dual_use(tok) {
			unsupported << tok
		}
	}
	return unsupported
}

fn validate_macos_native_ld_link_flags(link_flags string) ! {
	unsupported := macos_native_ld_unsupported_driver_link_flags(link_flags)
	if unsupported.len > 0 {
		return error('x64: unsupported backend feature: macOS native ld cannot consume driver linker flags: ${unsupported.join(' ')}')
	}
}

fn native_linux_tiny_allows_system_lib(lib string) bool {
	return lib in ['pthread', 'm', 'dl', 'c']
}

fn native_internal_link_flags_allow_builtin_linux_tiny(link_flags string) bool {
	tokens := link_flags.trim_space().fields()
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if tok == '-l' {
			if i + 1 >= tokens.len {
				return false
			}
			i++
			if !native_linux_tiny_allows_system_lib(tokens[i]) {
				return false
			}
		} else if tok.starts_with('-l') {
			if !native_linux_tiny_allows_system_lib(tok['-l'.len..]) {
				return false
			}
		} else {
			return false
		}
		i++
	}
	return true
}

fn native_link_flags_allow_builtin_linux_tiny(link_flags string, user_link_flags string) bool {
	return user_link_flags.trim_space() == ''
		&& native_internal_link_flags_allow_builtin_linux_tiny(link_flags)
}

fn (b &Builder) native_link_flags_from_sources() string {
	_, directive_link_flags := split_compile_and_link_flags(b.collect_cflags_from_sources())
	return directive_link_flags.trim_space()
}

fn (b &Builder) native_compile_and_link_flags_from_sources() (string, string) {
	directive_compile_flags, directive_link_flags :=
		split_compile_and_link_flags(b.collect_cflags_from_sources())
	return directive_compile_flags.trim_space(), directive_link_flags.trim_space()
}

fn (b &Builder) native_user_compile_and_link_flags_from_sources() (string, string) {
	directive_compile_flags, directive_link_flags :=
		split_compile_and_link_flags(b.collect_user_cflags_from_sources())
	return directive_compile_flags.trim_space(), directive_link_flags.trim_space()
}

struct NativeExternalLinkInputs {
	link_flags   string
	source_files []string
	object_files []string
}

fn native_external_source_clean_token(tok string) string {
	return tok.trim('"').trim("'")
}

fn native_external_source_is_supported(tok string) bool {
	lower := native_external_source_clean_token(tok).to_lower()
	return lower.ends_with('.c') || lower.ends_with('.m')
}

fn native_external_source_is_unsupported(tok string) bool {
	lower := native_external_source_clean_token(tok).to_lower()
	return lower.ends_with('.cc') || lower.ends_with('.cpp') || lower.ends_with('.cxx')
		|| lower.ends_with('.mm')
}

fn native_external_source_unsupported_message(tok string) string {
	clean := native_external_source_clean_token(tok)
	if clean.to_lower().ends_with('.mm') {
		return 'x64: unsupported backend feature: Objective-C++ #flag source `${clean}`'
	}
	return 'x64: unsupported backend feature: C++ #flag source `${clean}`'
}

fn native_external_source_object_file(output_binary string, source_index int) string {
	output_name := if output_binary == '' { 'out' } else { os.file_name(output_binary) }
	clean_name := output_name.replace('/', '_').replace('\\', '_').replace('.', '_')
	return os.join_path(os.vtmp_dir(), 'v2_native_${os.getpid()}_${clean_name}_${source_index}.o')
}

fn native_external_link_inputs(link_flags string, output_binary string) !NativeExternalLinkInputs {
	mut rewritten := []string{}
	mut source_files := []string{}
	mut object_files := []string{}
	for tok in link_flags.fields() {
		clean := native_external_source_clean_token(tok)
		if native_external_source_is_unsupported(clean) {
			return error(native_external_source_unsupported_message(clean))
		}
		if native_external_source_is_supported(clean) {
			if tok.contains('"') || tok.contains("'") {
				return error('x64: unsupported backend feature: quoted #flag source path `${tok}`')
			}
			obj_file := native_external_source_object_file(output_binary, source_files.len)
			source_files << clean
			object_files << obj_file
			rewritten << os.quoted_path(obj_file)
			continue
		}
		rewritten << tok
	}
	return NativeExternalLinkInputs{
		link_flags:   rewritten.join(' ')
		source_files: source_files
		object_files: object_files
	}
}

fn native_external_source_compile_command(cc string, source_file string, object_file string, compile_flags string, sdk_path string, arch_flag string, target_os string) string {
	mut parts := [cc, '-c']
	if is_macos_native_target(target_os) {
		if sdk_path != '' {
			parts << '-isysroot'
			parts << os.quoted_path(sdk_path)
		}
		if arch_flag != '' {
			parts << '-arch'
			parts << arch_flag
		}
	}
	if compile_flags != '' {
		parts << compile_flags
	}
	parts << os.quoted_path(source_file)
	parts << '-o'
	parts << os.quoted_path(object_file)
	return parts.join(' ')
}

fn (b &Builder) native_external_source_compiler(target_os string) string {
	if b.pref.ccompiler.len > 0 {
		return b.pref.ccompiler
	}
	v2cc := (os.getenv_opt('V2CC') or { '' }).trim_space()
	if v2cc != '' {
		return v2cc
	}
	if is_macos_native_target(target_os) {
		return 'cc'
	}
	return configured_cc(b.pref.vroot)
}

fn (b &Builder) native_linux_hosted_link_compiler() string {
	if b.pref.ccompiler.len > 0 {
		return b.pref.ccompiler
	}
	v2cc := (os.getenv_opt('V2CC') or { '' }).trim_space()
	if v2cc != '' {
		return v2cc
	}
	return 'cc'
}

fn (b &Builder) compile_native_external_sources(inputs NativeExternalLinkInputs, compile_flags string, target_os string, sdk_path string, arch_flag string) ! {
	if inputs.source_files.len == 0 {
		return
	}
	cc := b.native_external_source_compiler(target_os)
	for i, source_file in inputs.source_files {
		cmd := native_external_source_compile_command(cc, source_file, inputs.object_files[i],
			compile_flags, sdk_path, arch_flag, target_os)
		if b.pref.show_cc {
			println(cmd)
		}
		res := os.execute(cmd)
		if res.exit_code != 0 {
			return error('native external source compilation failed:\n${cmd}\n${res.output}')
		}
	}
}

fn cleanup_native_external_objects(inputs NativeExternalLinkInputs) {
	for obj_file in inputs.object_files {
		os.rm(obj_file) or {}
	}
}

fn macos_native_link_command(output_binary string, obj_file string, sdk_path string, arch_flag string, tiny_object bool, link_flags string) string {
	ld_link_flags := macos_native_ld_link_flags(link_flags)
	normal_link_cmd := 'ld -o ${os.quoted_path(output_binary)} ${os.quoted_path(obj_file)}${native_link_flags_suffix(ld_link_flags)} -lSystem -syslibroot ${os.quoted_path(sdk_path)} -e _main -arch ${arch_flag} -platform_version macos 11.0.0 11.0.0'
	if tiny_object {
		return '${normal_link_cmd} -dead_strip -x -S'
	}
	return normal_link_cmd
}

fn macos_sdk_path_from_xcrun_output(output string) !string {
	mut sdk_path := ''
	for raw_line in output.split_into_lines() {
		line := raw_line.trim(' \t\r')
		if line == '' {
			continue
		}
		if is_clean_macos_sdk_path_line(line) {
			sdk_path = line
		}
	}
	if sdk_path == '' {
		return error('could not find a clean macOS SDK path in xcrun output')
	}
	return sdk_path
}

fn is_clean_macos_sdk_path_line(path string) bool {
	if !os.is_abs_path(path) {
		return false
	}
	base := os.file_name(path)
	return base.starts_with('MacOSX') && base.ends_with('.sdk')
}

fn validate_macos_sdk_path_for_native_link(sdk_path string) ! {
	if !os.is_dir(sdk_path) {
		return error('macOS SDK path does not exist: ${sdk_path}')
	}
	libsystem_tbd := os.join_path(sdk_path, 'usr', 'lib', 'libSystem.tbd')
	libsystem_dylib := os.join_path(sdk_path, 'usr', 'lib', 'libSystem.dylib')
	if !os.exists(libsystem_tbd) && !os.exists(libsystem_dylib) {
		return error('macOS SDK path is missing usr/lib/libSystem.tbd or usr/lib/libSystem.dylib: ${sdk_path}')
	}
}

fn linux_native_link_command(cc string, output_binary string, obj_file string, link_flags string) string {
	return '${cc} ${os.quoted_path(obj_file)} -o ${os.quoted_path(output_binary)} -no-pie${native_link_flags_suffix(link_flags)}'
}

fn native_external_object_file(output_binary string, target_os string) string {
	if !is_macos_native_target(target_os) {
		return 'main.o'
	}
	output_path := if os.is_abs_path(output_binary) {
		output_binary
	} else {
		os.abs_path(output_binary)
	}
	output_dir := os.dir(output_path)
	output_name := os.file_name(output_path)
	if output_name == '' {
		return os.join_path(output_dir, '.v_native_${os.getpid()}.o')
	}
	return os.join_path(output_dir, '.${output_name}.${os.getpid()}.o')
}

fn (mut b Builder) prepare_macos_tiny_candidate_source_files() {
	b.macos_tiny_candidate_source_files = []ast.File{}
	b.macos_tiny_candidate_source_flat = ast.FlatAst{}
	if !b.uses_macos_x64_tiny_object(.x64) {
		return
	}
	if b.flat.files.len > 0 {
		b.macos_tiny_candidate_source_flat = b.flat
		return
	}
	b.macos_tiny_candidate_source_files = b.files.clone()
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

fn native_x64_mir_unsupported_external_symbol_message(mir_mod &mir.Module, obj_format x64.ObjectFormat) ?string {
	for val in mir_mod.values {
		if val.kind != .func_ref {
			continue
		}
		if msg := x64.unsupported_external_symbol_message_for_name(obj_format, val.name,
			'needed while preparing native x64 output')
		{
			return msg
		}
	}
	return none
}

fn (b &Builder) native_backend_requires_ssa_optimization(arch pref.Arch) bool {
	return arch == .x64
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
		'termux' { current == 'termux' }
		'ios' { current == 'ios' }
		'solaris' { current == 'solaris' }
		'qnx' { current == 'qnx' }
		'serenity' { current == 'serenity' }
		'plan9' { current == 'plan9' }
		'vinix' { current == 'vinix' }
		'cross' { current == 'cross' }
		'none' { current == 'none' }
		else { false }
	}
}

fn flag_pref_matches(cond string, prefs &pref.Preferences) bool {
	if prefs == unsafe { nil } {
		return false
	}
	lower := cond.to_lower()
	if pref.comptime_flag_value(prefs, lower) {
		return true
	}
	return flag_os_matches(lower, prefs.target_os_or_host())
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
	return parse_flag_directive_line_with_context(line, file_path, target_os, unsafe {
		&pref.Preferences(nil)
	})
}

fn parse_flag_directive_line_with_pref(line string, file_path string, prefs &pref.Preferences) ?string {
	target_os := if prefs == unsafe { nil } { '' } else { prefs.target_os_or_host() }
	return parse_flag_directive_line_with_context(line, file_path, target_os, prefs)
}

fn parse_flag_directive_line_with_context(line string, file_path string, target_os string, prefs &pref.Preferences) ?string {
	trimmed := line.trim_space()
	if trimmed.starts_with('#pkgconfig') {
		if prefs != unsafe { nil } && prefs.is_cross_target() {
			return none
		}
		rest := trimmed['#pkgconfig'.len..].trim_space()
		return resolve_pkgconfig_directive_flags(rest)
	}
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
		matches := if prefs == unsafe { nil } {
			flag_os_matches(parts[0], target_os)
		} else {
			flag_os_matches(parts[0], target_os) || flag_pref_matches(parts[0], prefs)
		}
		if !matches {
			return none
		}
		rest = rest[parts[0].len..].trim_space()
	}
	if rest == '' {
		return none
	}
	return normalize_flag_value_for_file(rest, file_path)
}

fn resolve_pkgconfig_directive_flags(value string) ?string {
	if value == '' {
		return none
	}
	args := if value.contains('--') {
		value.fields()
	} else {
		'--cflags --libs ${value}'.fields()
	}
	flags := pref.pkgconfig_result(args) or { return none }
	if flags == '' {
		return none
	}
	return flags
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
	for ff in b.flat.files {
		name := b.flat.file_name(ff)
		if name != '' {
			scan_paths << name
		}
	}
	cflags_target_os := b.cflags_target_os_for_local_compile()
	if !b.pref.skip_builtin {
		target_os := cflags_target_os
		for module_path in core_cached_module_paths {
			vlib_path := b.pref.get_vlib_module_path(module_path)
			module_files := get_v_files_from_dir(vlib_path, b.pref.user_defines, target_os)
			for mf in module_files {
				scan_paths << mf
			}
		}
	}
	return b.collect_cflags_from_scan_paths(scan_paths)
}

fn (b &Builder) source_path_is_internal_vlib(path string) bool {
	vlib_root := os.real_path(os.join_path(b.pref.vroot, 'vlib')).replace('\\', '/').trim_right('/')
	real_path := os.real_path(path).replace('\\', '/')
	return real_path == vlib_root || real_path.starts_with('${vlib_root}/')
}

fn (b &Builder) collect_user_cflags_from_sources() string {
	mut scan_paths := []string{}
	for path in b.user_files {
		if path != '' {
			scan_paths << path
		}
	}
	for file in b.files {
		if file.name != '' && !b.source_path_is_internal_vlib(file.name) {
			scan_paths << file.name
		}
	}
	for ff in b.flat.files {
		name := b.flat.file_name(ff)
		if name != '' && !b.source_path_is_internal_vlib(name) {
			scan_paths << name
		}
	}
	return b.collect_cflags_from_scan_paths(scan_paths)
}

fn (b &Builder) collect_cflags_from_scan_paths(paths []string) string {
	mut flags := []string{}
	mut seen := map[string]bool{}
	mut scanned_files := map[string]bool{}
	mut scan_paths := paths.clone()
	cflags_target_os := b.cflags_target_os_for_local_compile()
	scan_paths.sort()
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
				new_cond := rest[10..].trim_right('{ ').trim_space()
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
				} else if comptime_cond_matches_with_context(new_cond, cflags_target_os, b.pref) {
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
				cond := rest[4..].trim_right('{ ').trim_space()
				matched := comptime_cond_matches_with_context(cond, cflags_target_os, b.pref)
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
			mut flag := parse_flag_directive_line_with_context(resolved_line, scan_path,
				cflags_target_os, b.pref) or { continue }
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
// Linker flags include: -l*, -L*, -Wl,*, -Xlinker, -framework, C source files
// and prebuilt object/library files. Minimal dual-use driver flags, plus -F
// framework search paths, are kept in both outputs. Source files from #flag
// directives must not be passed to
// per-module `-c -o module.o` cache compilations.
fn split_compile_and_link_flags(flags string) (string, string) {
	tokens := flags.fields()
	mut compile := []string{}
	mut link := []string{}
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if native_driver_flag_is_dual_use(tok) {
			compile << tok
			link << tok
		} else if tok == '-F' {
			compile << tok
			link << tok
			if i + 1 < tokens.len {
				i++
				compile << tokens[i]
				link << tokens[i]
			}
		} else if tok.starts_with('-F') {
			compile << tok
			link << tok
		} else if tok == '-Xlinker' {
			link << tok
			if i + 1 < tokens.len {
				i++
				link << tokens[i]
			}
		} else if tok == '-framework' {
			// -framework Name: two tokens, linker only
			link << tok
			if i + 1 < tokens.len {
				i++
				link << tokens[i]
			}
		} else if tok.starts_with('-Wl,') || tok.starts_with('-l') || tok.starts_with('-L') {
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
	return comptime_cond_matches_with_context(cond, target_os, unsafe { &pref.Preferences(nil) })
}

fn comptime_cond_matches_with_pref(cond string, prefs &pref.Preferences) bool {
	target_os := if prefs == unsafe { nil } { '' } else { prefs.target_os_or_host() }
	return comptime_cond_matches_with_context(cond, target_os, prefs)
}

fn comptime_cond_matches_with_context(cond string, target_os string, prefs &pref.Preferences) bool {
	trimmed := cond.trim_space()
	if or_idx := top_level_bool_op_index(trimmed, '||') {
		left := trimmed[..or_idx].trim_space()
		right := trimmed[or_idx + 2..].trim_space()
		return comptime_cond_matches_with_context(left, target_os, prefs)
			|| comptime_cond_matches_with_context(right, target_os, prefs)
	}
	if and_idx := top_level_bool_op_index(trimmed, '&&') {
		left := trimmed[..and_idx].trim_space()
		right := trimmed[and_idx + 2..].trim_space()
		return comptime_cond_matches_with_context(left, target_os, prefs)
			&& comptime_cond_matches_with_context(right, target_os, prefs)
	}
	if trimmed.starts_with('!') {
		return !comptime_cond_matches_with_context(trimmed[1..], target_os, prefs)
	}
	if stripped := strip_outer_bool_parens(trimmed) {
		return comptime_cond_matches_with_context(stripped, target_os, prefs)
	}
	if optional_name := optional_user_ct_flag_name(trimmed) {
		if prefs == unsafe { nil } {
			return false
		}
		return pref.comptime_optional_flag_value(prefs, optional_name)
	}
	if pkg_name := pkgconfig_cond_name(trimmed) {
		if prefs != unsafe { nil } && prefs.is_cross_target() {
			return false
		}
		return pref.comptime_pkgconfig_value(pkg_name)
	}
	if prefs != unsafe { nil } {
		return flag_os_matches(trimmed, target_os) || flag_pref_matches(trimmed, prefs)
	}
	current := normalize_target_os_name(target_os)
	return match trimmed.to_lower() {
		'macos', 'darwin', 'mac' { current == 'macos' || current == 'darwin' }
		'linux' { current == 'linux' }
		'windows' { current == 'windows' }
		'bsd' { current in ['macos', 'freebsd', 'openbsd', 'netbsd', 'dragonfly'] }
		'freebsd' { current == 'freebsd' }
		'openbsd' { current == 'openbsd' }
		'netbsd' { current == 'netbsd' }
		'dragonfly' { current == 'dragonfly' }
		'android' { current == 'android' }
		'termux' { current == 'termux' }
		'ios' { current == 'ios' }
		'solaris' { current == 'solaris' }
		'qnx' { current == 'qnx' }
		'serenity' { current == 'serenity' }
		'plan9' { current == 'plan9' }
		'vinix' { current == 'vinix' }
		'cross' { current == 'cross' }
		'native' { false }
		'emscripten' { false }
		else { false } // unknown user-defined flags default to false
	}
}

fn pkgconfig_cond_name(cond string) ?string {
	trimmed := cond.trim_space()
	if !trimmed.starts_with(r'$pkgconfig(') || !trimmed.ends_with(')') {
		return none
	}
	arg := trimmed[r'$pkgconfig('.len..trimmed.len - 1].trim_space()
	if arg.len < 2 {
		return none
	}
	quote := arg[0]
	if (quote != `'` && quote != `"`) || arg[arg.len - 1] != quote {
		return none
	}
	return arg[1..arg.len - 1]
}

fn optional_user_ct_flag_name(cond string) ?string {
	trimmed := cond.trim_space()
	if !trimmed.ends_with('?') {
		return none
	}
	name := trimmed[..trimmed.len - 1].trim_space()
	if name == '' {
		return none
	}
	return name
}

fn top_level_bool_op_index(expr string, op string) ?int {
	mut depth := 0
	mut i := 0
	for i < expr.len {
		ch := expr[i]
		if ch == `(` {
			depth++
		} else if ch == `)` {
			if depth > 0 {
				depth--
			}
		} else if depth == 0 && i + op.len <= expr.len && expr[i..i + op.len] == op {
			return i
		}
		i++
	}
	return none
}

fn strip_outer_bool_parens(expr string) ?string {
	if expr.len < 2 || expr[0] != `(` || expr[expr.len - 1] != `)` {
		return none
	}
	mut depth := 0
	for i, ch in expr {
		if ch == `(` {
			depth++
		} else if ch == `)` {
			depth--
			if depth == 0 && i < expr.len - 1 {
				return none
			}
		}
	}
	if depth == 0 {
		return expr[1..expr.len - 1].trim_space()
	}
	return none
}

fn default_cc(vroot string) string {
	// Try to use tcc by default, like v1 does.
	tcc_path := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	if os.exists(tcc_path) {
		return tcc_path
	}
	return 'cc'
}

// fast_relink_output_is_generation_only mirrors gen_cleanc()'s generation-only
// decision: a `.c` output, a target we cannot compile locally, or a shared lib.
// Such a request must go through normal C generation, never the pre-parse relink
// (is_cmd_v2_self_build() keys only on the input file, so the relink path would
// otherwise link an executable into e.g. foo.c). Extracted so the decision is
// unit-testable without a warm object cache.
fn (b &Builder) fast_relink_output_is_generation_only(output_name string) bool {
	return output_name.ends_with('.c') || !b.can_compile_cleanc_locally() || b.pref.is_shared_lib
}

// preparse_flag_fingerprint captures the flag-affecting build inputs that are
// knowable WITHOUT parsing the sources: the C compiler choice (-cc / V2CC /
// default), prod/shared mode, and the env CFLAGS (V2CFLAGS). It is recorded in
// main.stamp and re-checked by the pre-parse fast relink so a changed compiler or
// CFLAGS environment invalidates a relink even when every source file is
// unchanged. Source-derived `#flag` directives are intentionally excluded — they
// change only when a source file changes, which the stamp freshness checks catch.
fn (b &Builder) preparse_flag_fingerprint() string {
	cc := if b.pref.ccompiler.len > 0 { b.pref.ccompiler } else { configured_cc(b.pref.vroot) }
	return 'cc=${cc}\x01ccpref=${b.pref.ccompiler}\x01prod=${b.pref.is_prod}\x01shared=${b.pref.is_shared_lib}\x01env=${configured_cflags()}'
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

fn native_graph_stage_title(label string, title string) string {
	if label == '' {
		return title
	}
	return '${label} ${title}'
}

const macos_tiny_candidate_graph_label = 'macOS Tiny Candidate'

fn (b &Builder) native_mir_build_sequential(label string) bool {
	return label == macos_tiny_candidate_graph_label || b.pref.no_parallel || b.pref.hot_fn.len > 0
}

fn (b &Builder) should_prune_native_backend_modules(arch pref.Arch) bool {
	return b.pref.single_backend || arch == .arm64
}

fn native_backend_module_file_fragment(backend_mod string) string {
	return match backend_mod {
		'eval' { '/vlib/v2/eval/' }
		else { '/vlib/v2/gen/${backend_mod}/' }
	}
}

fn (mut b Builder) build_native_mir_from_files(files []ast.File, arch pref.Arch, target_os string, minimal_runtime_roots bool, used_fn_keys map[string]bool, label string) mir.Module {
	mut mod := ssa.Module.new('main')
	if mod == unsafe { nil } {
		eprintln('error: native backend not available (compiled with stubbed ssa module)')
		eprintln('hint: use v2 compiled with v1 for native code generation')
		exit(1)
	}
	mut ssa_builder := ssa.Builder.new_with_env(mod, b.env)
	ssa_builder.guard_invalid_type_payloads = true
	ssa_builder.target_os = target_os
	ssa_builder.minimal_runtime_roots = minimal_runtime_roots
	ssa_builder.native_backend_bulk_zero_alloca = arch == .x64
	mut native_sw := time.new_stopwatch()

	// Pass markused data for dead code elimination. The ARM64 backend has its own
	// relocation-based dead stripping; using markused before SSA makes self-hosted
	// compiler builds fragile when the markused set is under-collected.
	if used_fn_keys.len > 0 && arch != .arm64 {
		ssa_builder.used_fn_keys = used_fn_keys.clone()
	}

	// Strip unused compiler backend modules before SSA. ARM64 already strips
	// these symbols after codegen, so avoid building MIR for them in the first place.
	if b.should_prune_native_backend_modules(arch) {
		all_backends := ['cleanc', 'eval', 'v', 'c', 'x64', 'arm64']
		own := match b.pref.backend {
			.arm64 { 'arm64' }
			.x64 { 'x64' }
			.cleanc { 'cleanc' }
			.v { 'v' }
			.c { 'c' }
			.eval { 'eval' }
		}

		for backend_mod in all_backends {
			if backend_mod != own {
				ssa_builder.skip_modules[backend_mod] = true
				ssa_builder.skip_module_file_fragments[backend_mod] =
					native_backend_module_file_fragment(backend_mod)
			}
		}
	}

	// In hot_fn mode, only build the target function body (skip all others)
	if b.pref.hot_fn.len > 0 {
		ssa_builder.hot_fn = b.pref.hot_fn
	}

	mut stage_start := native_sw.elapsed()
	// Route the whole SSA build through the cursor-native build_all_from_flat
	// on the post-transform b.flat (kept alive above). Sequential only
	// (build_all_from_flat builds fn bodies in-phase).
	//
	// b.flat is only POST-TRANSFORM when flat markused has routed transform
	// through transform_files_to_flat, or the direct native flat pipeline has
	// emitted transform output directly into FlatAst.
	build_from_flat := b.should_build_ssa_from_flat()
		|| (b.flat.files.len > 0 && b.native_flat_pipeline_enabled && label == '')
	if build_from_flat {
		ssa_builder.build_all_from_flat(&b.flat)
		// SSA has copied the program into MIR; keep the FlatAst lifetime out of
		// the later optimizer and machine-code generator working sets.
		b.flat = ast.FlatAst{}
	} else if b.native_mir_build_sequential(label) {
		ssa_builder.build_all(files)
	} else {
		// Phases 1-3 sequential, Phase 4 parallel, Phase 5 sequential
		ssa_builder.skip_fn_bodies = true
		ssa_builder.build_all(files)
		ssa_builder.skip_fn_bodies = false
		b.ssa_build_parallel(mut ssa_builder, files)
		ssa_builder.generate_vinit()
	}
	if arch == .arm64 {
		b.env.release_expr_type_cache_after_ssa()
	}
	print_time(native_graph_stage_title(label, 'SSA Build'),
		time.Duration(native_sw.elapsed() - stage_start))
	print_rss(native_graph_stage_title(label, 'after SSA build'))

	stage_start = native_sw.elapsed()
	ssa_optimization_required := b.native_backend_requires_ssa_optimization(arch)
	ssa_optimization_ran := !b.pref.no_optimize || ssa_optimization_required
	if ssa_optimization_required {
		if b.pref.no_optimize {
			eprintln('  opt: required for x64 backend (-O0 SSA is not supported yet)')
		}
		ssa_optimize.optimize(mut mod)
	} else if b.pref.no_optimize {
		eprintln('  opt: skipped (-O0)')
	} else {
		ssa_optimize.optimize(mut mod)
	}
	print_time(native_graph_stage_title(label, 'SSA Optimize'),
		time.Duration(native_sw.elapsed() - stage_start))
	print_rss(native_graph_stage_title(label, 'after SSA optimize'))
	$if debug {
		// Post-opt SSA verification is useful while debugging the optimizer, but it
		// is currently noisy enough to block normal self-host builds. Keep it
		// opt-in so `test_all.sh` and manual self-hosting can still complete.
		if ssa_optimization_ran && os.getenv('V2_VERIFY') != '' {
			ssa_optimize.verify_and_panic(mod, 'full optimization')
		}
	}

	// Post-optimization SSA dump for debugging
	dump_fn_name := os.getenv('V2_DUMP_OPT_SSA')
	if dump_fn_name.len > 0 {
		for func in mod.funcs {
			if func.name == dump_fn_name {
				eprintln('=== POST-OPT SSA DUMP: ${func.name} ===')
				eprintln('  params_len: ${func.params.len}')
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
	print_time(native_graph_stage_title(label, 'MIR Lower'),
		time.Duration(native_sw.elapsed() - stage_start))
	mod.release_outer_arenas_after_mir_lower()
	print_rss(native_graph_stage_title(label, 'after MIR lower'))

	stage_start = native_sw.elapsed()
	if is_windows_x64_native_target(arch, target_os) {
		abi.lower_with_x64_abi(mut mir_mod, arch, native_x64_lowering_abi_for_os(target_os))
	} else {
		abi.lower(mut mir_mod, arch)
	}
	print_time(native_graph_stage_title(label, 'ABI Lower'),
		time.Duration(native_sw.elapsed() - stage_start))
	print_rss(native_graph_stage_title(label, 'after ABI lower'))

	if arch != .arm64 {
		stage_start = native_sw.elapsed()
		insel.select(mut mir_mod, arch)
		print_time(native_graph_stage_title(label, 'InsSel'),
			time.Duration(native_sw.elapsed() - stage_start))
		print_rss(native_graph_stage_title(label, 'after InsSel'))
	}
	return mir_mod
}

fn (mut b Builder) build_macos_tiny_candidate_mir(arch pref.Arch, target_os string) mir.Module {
	if b.macos_tiny_candidate_source_flat.files.len == 0
		&& b.macos_tiny_candidate_source_files.len == 0 {
		eprintln('internal error: macOS tiny candidate graph was not prepared')
		exit(1)
	}
	mut trans := transformer.Transformer.new_with_pref(b.env, b.pref)
	trans.set_file_set(b.file_set)
	trans.enable_macos_tiny_candidate_graph()
	opts := markused.MarkUsedOptions{
		minimal_runtime_roots: true
	}
	if b.macos_tiny_candidate_source_flat.files.len > 0 {
		candidate_flat :=
			trans.transform_flat_to_flat_direct(&b.macos_tiny_candidate_source_flat, [])
		candidate_used_fn_keys := markused.mark_used_flat_with_options(&candidate_flat, b.env, opts)
		old_flat := b.flat
		b.flat = candidate_flat
		candidate_mir := b.build_native_mir_from_files([], arch, target_os, true,
			candidate_used_fn_keys, macos_tiny_candidate_graph_label)
		b.flat = old_flat
		return candidate_mir
	}
	candidate_files := trans.transform_files(b.macos_tiny_candidate_source_files)
	candidate_used_fn_keys := markused.mark_used_with_options(candidate_files, b.env, opts)
	return b.build_native_mir_from_files(candidate_files, arch, target_os, true,
		candidate_used_fn_keys, macos_tiny_candidate_graph_label)
}

fn (mut b Builder) gen_native(backend_arch pref.Arch) {
	arch := if backend_arch == .auto { b.pref.get_effective_arch() } else { backend_arch }
	target_os := b.pref.target_os_or_host()
	native_compile_flags, native_link_flags := b.native_compile_and_link_flags_from_sources()
	_, native_user_link_flags := b.native_user_compile_and_link_flags_from_sources()
	mut native_external_inputs := NativeExternalLinkInputs{
		link_flags: native_link_flags
	}

	mut mir_mod := b.build_native_mir_from_files(b.files, arch, target_os,
		b.uses_minimal_x64_runtime_roots(), b.used_fn_keys, '')
	// The hosted SSA build has consumed b.files into `mir_mod`; the rest of the
	// normal native pipeline operates on MIR only. Drop the ~120MB legacy AST so
	// it can be reclaimed before codegen's working-set grows. The macOS tiny
	// candidate, when requested, uses its own saved source snapshot.
	b.files = []ast.File{}

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
		mut native_sw := time.new_stopwatch()
		stage_start := native_sw.elapsed()
		mut gen := arm64.Gen.new(&mir_mod)
		if b.pref.no_parallel {
			gen.gen()
		} else {
			b.gen_arm64_parallel(mut gen)
		}
		gen.release_scratch_after_gen()
		mir_mod.release_after_native_codegen()
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
		obj_file := native_external_object_file(output_binary, target_os)
		mut used_macos_tiny_object := false

		if arch == .arm64 {
			mut gen := arm64.Gen.new(&mir_mod)
			if b.pref.no_parallel {
				gen.gen()
			} else {
				b.gen_arm64_parallel(mut gen)
			}
			gen.release_scratch_after_gen()
			mir_mod.release_after_native_codegen()
			gen.write_file(obj_file)
		} else {
			obj_format := native_x64_object_format_for_os(target_os)
			codegen_abi := native_x64_codegen_abi_for_os(target_os)
			if msg := native_x64_mir_unsupported_external_symbol_message(&mir_mod, obj_format) {
				eprint_native_x64_link_error(msg)
				exit(1)
			}
			if is_windows_x64_native_target(arch, target_os) {
				mut windows_gen := x64.Gen.new_with_format_and_abi(&mir_mod, obj_format,
					codegen_abi)
				windows_gen.gen()
				windows_gen.link_executable(output_binary) or {
					eprint_native_x64_link_error(err.msg())
					exit(1)
				}
				if b.pref.verbose {
					println('[*] Linked ${output_binary} (built-in PE linker)')
				}
				return
			}
			if is_linux_x64_native_target(arch, target_os) {
				mut linux_gen := x64.Gen.new_with_format_and_abi(&mir_mod, obj_format, codegen_abi)
				linux_gen.gen()
				if native_link_flags_allow_builtin_linux_tiny(native_link_flags,
					native_user_link_flags)
				{
					if os.exists(output_binary) {
						os.rm(output_binary) or {}
					}
					linux_gen.link_linux_tiny_executable(output_binary) or {
						msg := err.msg()
						if linux_x64_tiny_strict_enabled()
							|| !msg.starts_with(x64.linux_tiny_not_eligible_prefix) {
							eprint_native_x64_link_error(msg)
							exit(1)
						}
					}
					if os.exists(output_binary) {
						if b.pref.verbose {
							println('[*] Linked ${output_binary} (built-in Linux tiny linker)')
						}
						return
					}
				}
				linux_gen.write_file(obj_file)
			} else if b.uses_macos_x64_tiny_object(arch) {
				if os.exists(obj_file) {
					os.rm(obj_file) or {}
				}
				if b.pref.verbose {
					println('[*] macOS tiny object candidate enabled')
				}
				mut candidate_mir := b.build_macos_tiny_candidate_mir(arch, target_os)
				mut candidate_gen := x64.Gen.new_with_format_and_abi(&candidate_mir, obj_format,
					codegen_abi)
				candidate_gen.gen()
				candidate_gen.write_macos_tiny_object(obj_file) or {
					msg := err.msg()
					if !msg.starts_with(x64.macos_tiny_not_eligible_prefix) {
						eprint_native_x64_link_error(msg)
						exit(1)
					}
					if b.pref.verbose {
						println('[*] macOS tiny object not eligible; falling back to normal Mach-O object: ${msg}')
					}
				}
				if os.exists(obj_file) {
					used_macos_tiny_object = true
				} else {
					if b.pref.verbose {
						println('[*] macOS tiny object fallback: writing normal Mach-O object')
					}
					mut macos_fallback_gen := x64.Gen.new_with_format_and_abi(&mir_mod, obj_format,
						codegen_abi)
					macos_fallback_gen.gen()
					macos_fallback_gen.write_file(obj_file)
				}
			} else {
				mut normal_x64_gen := x64.Gen.new_with_format_and_abi(&mir_mod, obj_format,
					codegen_abi)
				normal_x64_gen.gen()
				normal_x64_gen.write_file(obj_file)
			}
		}

		if b.pref.verbose {
			if used_macos_tiny_object {
				println('[*] Wrote ${obj_file} (macOS tiny candidate object)')
			} else {
				println('[*] Wrote ${obj_file}')
			}
		}

		// Link the object file into an executable
		if is_macos_native_target(target_os) {
			sdk_res := os.execute('xcrun -sdk macosx --show-sdk-path')
			if sdk_res.exit_code != 0 {
				eprintln('Link failed:')
				eprintln('failed to resolve macOS SDK path with xcrun:')
				eprintln(sdk_res.output)
				exit(1)
			}
			sdk_path := macos_sdk_path_from_xcrun_output(sdk_res.output) or {
				eprintln('Link failed:')
				eprintln(err.msg())
				eprintln('xcrun output:')
				eprintln(sdk_res.output)
				exit(1)
			}
			validate_macos_sdk_path_for_native_link(sdk_path) or {
				eprintln('Link failed:')
				eprintln(err.msg())
				exit(1)
			}
			arch_flag := if arch == .arm64 { 'arm64' } else { 'x86_64' }
			native_external_inputs = native_external_link_inputs(native_link_flags, output_binary) or {
				eprint_native_x64_link_error(err.msg())
				exit(1)
			}
			validate_macos_native_ld_link_flags(native_external_inputs.link_flags) or {
				eprint_native_x64_link_error(err.msg())
				exit(1)
			}
			b.compile_native_external_sources(native_external_inputs, native_compile_flags,
				target_os, sdk_path, arch_flag) or {
				cleanup_native_external_objects(native_external_inputs)
				eprint_native_x64_link_error(err.msg())
				exit(1)
			}
			normal_link_cmd := macos_native_link_command(output_binary, obj_file, sdk_path,
				arch_flag, false, native_external_inputs.link_flags)
			link_cmd := macos_native_link_command(output_binary, obj_file, sdk_path, arch_flag,
				used_macos_tiny_object, native_external_inputs.link_flags)
			mut link_result := os.execute(link_cmd)
			if link_result.exit_code != 0 && used_macos_tiny_object {
				if b.pref.verbose {
					println('[*] macOS tiny object link failed; retrying with normal Mach-O object')
					println('[*] macOS tiny object link exit code: ${link_result.exit_code}')
					println('[*] macOS tiny object link output:')
					if link_result.output.len > 0 {
						print(link_result.output)
						if !link_result.output.ends_with('\n') {
							println('')
						}
					} else {
						println('<empty>')
					}
				}
				if os.exists(output_binary) {
					os.rm(output_binary) or {}
				}
				obj_format := native_x64_object_format_for_os(target_os)
				codegen_abi := native_x64_codegen_abi_for_os(target_os)
				mut fallback_gen := x64.Gen.new_with_format_and_abi(&mir_mod, obj_format,
					codegen_abi)
				fallback_gen.gen()
				fallback_gen.write_file(obj_file)
				used_macos_tiny_object = false
				if b.pref.verbose {
					println('[*] Wrote ${obj_file} (normal Mach-O fallback after macOS tiny link failure)')
				}
				link_result = os.execute(normal_link_cmd)
			}
			if link_result.exit_code != 0 {
				eprintln('Link failed:')
				eprintln(link_result.output)
				cleanup_native_external_objects(native_external_inputs)
				exit(1)
			}
			if b.pref.verbose && used_macos_tiny_object {
				println('[*] Linked ${output_binary} (built-in macOS tiny object)')
			}
		} else {
			// Linux linking
			native_external_inputs = native_external_link_inputs(native_link_flags, output_binary) or {
				eprint_native_x64_link_error(err.msg())
				exit(1)
			}
			b.compile_native_external_sources(native_external_inputs, native_compile_flags,
				target_os, '', '') or {
				cleanup_native_external_objects(native_external_inputs)
				eprint_native_x64_link_error(err.msg())
				exit(1)
			}
			link_result := os.execute(linux_native_link_command(b.native_linux_hosted_link_compiler(),
				output_binary, obj_file, native_external_inputs.link_flags))
			if link_result.exit_code != 0 {
				eprintln('Link failed:')
				eprintln(link_result.output)
				cleanup_native_external_objects(native_external_inputs)
				exit(1)
			}
		}

		if b.pref.verbose {
			println('[*] Linked ${output_binary}')
		}

		// Clean up object file
		if !b.pref.keep_c {
			cleanup_native_external_objects(native_external_inputs)
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
	flat_stats := b.flat.stats()
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
		hist := b.flat.count_nodes_by_kind()
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
	for ff in b.flat.files {
		name := b.flat.file_name(ff)
		if name in seen_files {
			continue
		}
		seen_files[name] = true
		parsed_paths << name
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
