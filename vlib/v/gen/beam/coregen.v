module beam

import v.ast
import v.pref
import strings
import os

pub struct CoreGen {
	table &ast.Table
	prefs &pref.Preferences
mut:
	out            strings.Builder
	indent         int
	cur_mod        string
	cur_file       string
	out_dir        string
	temp_counter   int
	fn_infos       []FnInfo // reuse FnInfo from beamgen.v
	cur_fn         &ast.FnDecl = unsafe { nil }
	var_map        map[string]string // V variable name -> Core Erlang variable name
	comptime_stack []ComptimeEnv
	module_sources []string // collected Core Erlang sources for batch compilation
	etf_mode       bool     // true = emit Erlang term text (AST), false = Core Erlang text
	erl_mode       bool     // true = transpile Core Erlang to Erlang source, compile via erlc
	fn_count       int      // track function definitions for separator
	match_cond_var string   // temp var for let-bound match condition (prevents re-evaluation)
}

pub fn core_gen(files []&ast.File, mut table ast.Table, out_name string, prefs_ &pref.Preferences) {
	// Check for native target override (VBEAM_TARGET=arm64|x86_64)
	native_target := native_target_from_env()
	if native_target != .none {
		native_gen(files, table, out_name, native_target)
		return
	}

	// ETF mode is the default pipeline (faster: bypasses core_scan/core_parse).
	// Set VBEAM_TEXT=1 to use the legacy Core Erlang text pipeline instead.
	// VBEAM_ETF=1 is still accepted for backward compat (no-op since ETF is default).
	// Set VBEAM_ERL=1 to transpile Core Erlang → Erlang source → compile via erlc.
	erl_mode := os.getenv('VBEAM_ERL') == '1'
	text_mode := os.getenv('VBEAM_TEXT') == '1' || erl_mode // ERL mode needs .core files
	etf := !text_mode
	mut g := CoreGen{
		table: &table
		prefs: prefs_
		out: strings.new_builder(4096)
		out_dir: if out_name.len > 0 { out_name } else { 'beam_output' }
		etf_mode: etf
		erl_mode: erl_mode
	}

	// Create output directory
	if !os.exists(g.out_dir) {
		os.mkdir_all(g.out_dir) or {}
	}

	// Group files by module name (same as Erlang backend)
	mut module_files := map[string][]&ast.File{}
	for file in files {
		mod_name := file.mod.name
		if mod_name in module_files {
			module_files[mod_name] << file
		} else {
			module_files[mod_name] = [file]
		}
	}

	// Generate one .core per module
	for mod_name, mod_files in module_files {
		g.core_gen_module(mod_name, mod_files)
	}

	// Tier 3: Compile .core → .beam using compile:forms/2
	g.compile_to_beam()
}

fn (mut g CoreGen) core_gen_module(v_mod string, files []&ast.File) {
	erl_mod := g.core_v_mod_to_erl_mod(v_mod)
	g.cur_mod = v_mod

	// Reset state
	g.out.clear()
	g.fn_infos.clear()
	g.var_map.clear()
	g.temp_counter = 0

	// First pass: collect function info from ALL files
	for file in files {
		g.cur_file = file.path
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				g.core_collect_fn_info(stmt)
			}
		}
	}

	// Detect behaviours (explicit attributes first, then auto-detect with warning)
	behaviours := g.detect_behaviours(files)

	// Generate module header with detected behaviours
	g.core_module_header(erl_mod, behaviours)

	// Second pass: generate code from ALL files
	for file in files {
		g.cur_file = file.path
		g.var_map.clear()
		g.temp_counter = 0
		for stmt in file.stmts {
			g.core_stmt(stmt)
		}
	}

	// Generate stubs for missing behaviour callbacks
	g.core_behaviour_stubs(behaviours)

	// Generate module_info boilerplate
	g.core_module_info(erl_mod)

	// Close module
	g.emit_module_footer()

	core_source := g.out.str()

	// Collect for batch compilation (Tier 3 pipe mode)
	g.module_sources << core_source

	// Also write debug file (.core for text mode, .etf for ETF mode)
	ext := if g.etf_mode { 'etf' } else { 'core' }
	debug_filename := '${g.out_dir}/${erl_mod}.${ext}'
	os.write_file(debug_filename, core_source) or {
		eprintln('Error writing ${debug_filename}: ${err}')
	}
}

fn (mut g CoreGen) core_collect_fn_info(node ast.FnDecl) {
	// Skip methods on interface types — they are abstract and have no
	// concrete implementation. Generating them produces functions that
	// call undefined interface methods (e.g. IError.msg/1), which
	// core_lint rightfully warns about.
	if node.is_method {
		rec_type := node.receiver.typ
		type_sym := g.table.sym(rec_type)
		if type_sym.kind == .interface {
			return
		}
	}
	name := g.core_fn_name(node)
	arity := node.params.len
	is_pub := node.is_pub || node.short_name == 'main' || node.is_main || !node.is_anon
	g.fn_infos << FnInfo{
		name: name
		arity: arity
		is_pub: is_pub
	}
}

fn (mut g CoreGen) core_module_header(erl_mod string, behaviours []string) {
	// Module declaration with exports — format depends on etf_mode
	mut exports := []string{}
	for info in g.fn_infos {
		if info.is_pub {
			if g.etf_mode {
				exports << "{c_var,[],{'${info.name}',${info.arity}}}"
			} else {
				exports << "'${info.name}'/${info.arity}"
			}
		}
	}
	// Export behaviour callbacks if detected
	for b in behaviours {
		for cb in g.behaviour_callbacks(b) {
			export := if g.etf_mode {
				"{c_var,[],{'${cb.name}',${cb.arity}}}"
			} else {
				"'${cb.name}'/${cb.arity}"
			}
			if export !in exports {
				exports << export
			}
		}
	}
	// Always export module_info
	if g.etf_mode {
		exports << "{c_var,[],{'module_info',0}}"
		exports << "{c_var,[],{'module_info',1}}"
	} else {
		exports << "'module_info'/0"
		exports << "'module_info'/1"
	}

	g.fn_count = 0
	g.emit_module_header(erl_mod, exports, behaviours)
}

// Collect explicit @[beam_behaviour('...')] attributes from AST
fn (mut g CoreGen) collect_explicit_behaviours(files []&ast.File) []string {
	mut explicit := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				for attr in stmt.attrs {
					if attr.name == 'beam_behaviour' && attr.has_arg && attr.arg !in explicit {
						explicit << attr.arg
					}
				}
			} else if stmt is ast.StructDecl {
				for attr in stmt.attrs {
					if attr.name == 'beam_behaviour' && attr.has_arg && attr.arg !in explicit {
						explicit << attr.arg
					}
				}
			}
		}
	}
	return explicit
}

// Auto-detect OTP behaviours from function signatures
fn (mut g CoreGen) autodetect_behaviours_from_sigs() []string {
	mut fn_sigs := map[string]bool{}
	for info in g.fn_infos {
		fn_sigs['${info.name}/${info.arity}'] = true
	}

	mut behaviours := []string{}

	// gen_server: init/1 + handle_call/3 + handle_cast/2
	if 'init/1' in fn_sigs && 'handle_call/3' in fn_sigs && 'handle_cast/2' in fn_sigs {
		behaviours << 'gen_server'
	}

	// supervisor: init/1 (only if no gen_server and has supervisor-specific functions)
	if 'init/1' in fn_sigs && 'handle_call/3' !in fn_sigs && 'start_link/0' in fn_sigs {
		if 'child_spec/1' in fn_sigs || 'which_children/0' in fn_sigs {
			behaviours << 'supervisor'
		}
	}

	// gen_event: init/1 + handle_event/2
	if 'init/1' in fn_sigs && 'handle_event/2' in fn_sigs {
		behaviours << 'gen_event'
	}

	// gen_statem: init/1 + callback_mode/0
	if 'init/1' in fn_sigs && 'callback_mode/0' in fn_sigs {
		behaviours << 'gen_statem'
	}

	// application: start/2 + stop/1
	if 'start/2' in fn_sigs && 'stop/1' in fn_sigs {
		behaviours << 'application'
	}

	return behaviours
}

// Detect OTP behaviours: explicit @[beam_behaviour('...')] attributes first,
// then auto-detect from function signatures as fallback (with warning).
fn (mut g CoreGen) detect_behaviours(files []&ast.File) []string {
	// Phase 1: Check for explicit attributes
	explicit := g.collect_explicit_behaviours(files)
	if explicit.len > 0 {
		return explicit
	}

	// Phase 2: Auto-detect from function signatures (with warnings)
	behaviours := g.autodetect_behaviours_from_sigs()
	for behaviour in behaviours {
		eprintln('Warning: Auto-detected ${behaviour} behaviour in module ${g.cur_mod}. Consider adding @[beam_behaviour("${behaviour}")] attribute.')
	}
	return behaviours
}

// Return required callback functions for a behaviour
fn (mut g CoreGen) behaviour_callbacks(behaviour string) []FnInfo {
	return match behaviour {
		'gen_server' {
			[
				FnInfo{'init', 1, true},
				FnInfo{'handle_call', 3, true},
				FnInfo{'handle_cast', 2, true},
				FnInfo{'handle_info', 2, true},
				FnInfo{'terminate', 2, true},
				FnInfo{'code_change', 3, true},
			]
		}
		'supervisor' {
			[FnInfo{'init', 1, true}]
		}
		'gen_event' {
			[
				FnInfo{'init', 1, true},
				FnInfo{'handle_event', 2, true},
				FnInfo{'handle_call', 2, true},
				FnInfo{'handle_info', 2, true},
				FnInfo{'terminate', 2, true},
			]
		}
		'gen_statem' {
			[
				FnInfo{'init', 1, true},
				FnInfo{'callback_mode', 0, true},
				FnInfo{'handle_event', 4, true},
			]
		}
		'application' {
			[
				FnInfo{'start', 2, true},
				FnInfo{'stop', 1, true},
			]
		}
		else {
			[]FnInfo{}
		}
	}
}

// Generate default stubs for missing behaviour callbacks.
// For example, if gen_server is detected but code_change/3 is not defined by the user,
// emit a default implementation: code_change(_OldVsn, State, _Extra) -> {ok, State}.
fn (mut g CoreGen) core_behaviour_stubs(behaviours []string) {
	// Build set of user-defined function signatures
	mut fn_sigs := map[string]bool{}
	for info in g.fn_infos {
		fn_sigs['${info.name}/${info.arity}'] = true
	}

	for behaviour in behaviours {
		callbacks := g.behaviour_callbacks(behaviour)
		for cb in callbacks {
			sig := '${cb.name}/${cb.arity}'
			if sig !in fn_sigs {
				// Generate a default stub for this missing callback
				g.core_default_callback_stub(cb.name, cb.arity)
				// Also register it in fn_infos so exports are consistent
				g.fn_infos << FnInfo{
					name: cb.name
					arity: cb.arity
					is_pub: true
				}
			}
		}
	}
}

// Generate gen_server callback stubs in ETF mode
fn (mut g CoreGen) core_gen_server_stubs_etf(name string, arity int) {
	match name {
		'code_change' {
			g.begin_tuple()
			g.emit_atom('ok')
			g.write_core(', ')
			g.emit_var('_1')
			g.end_tuple()
		}
		'terminate' {
			g.emit_atom('ok')
		}
		'handle_info' {
			if arity == 2 {
				g.begin_tuple()
				g.emit_atom('noreply')
				g.write_core(', ')
				g.emit_var('_1')
				g.end_tuple()
			} else {
				g.emit_atom('ok')
			}
		}
		'handle_call' {
			if arity == 3 {
				g.begin_tuple()
				g.emit_atom('reply')
				g.write_core(', ')
				g.emit_atom('ok')
				g.write_core(', ')
				g.emit_var('_2')
				g.end_tuple()
			} else if arity == 2 {
				g.begin_tuple()
				g.emit_atom('ok')
				g.write_core(', ')
				g.emit_atom('ok')
				g.write_core(', ')
				g.emit_var('_1')
				g.end_tuple()
			} else {
				g.emit_atom('ok')
			}
		}
		'handle_cast' {
			g.begin_tuple()
			g.emit_atom('noreply')
			g.write_core(', ')
			g.emit_var('_1')
			g.end_tuple()
		}
		else {
			g.emit_atom('ok')
		}
	}
}

// Generate gen_event/gen_statem callback stubs in ETF mode
fn (mut g CoreGen) core_event_statem_stubs_etf(name string, arity int) {
	if name == 'handle_event' {
		if arity == 2 {
			g.begin_tuple()
			g.emit_atom('ok')
			g.write_core(', ')
			g.emit_var('_1')
			g.end_tuple()
		} else if arity == 4 {
			g.emit_atom('keep_state_and_data')
		} else {
			g.emit_atom('ok')
		}
	} else {
		g.emit_atom('ok')
	}
}

// Generate gen_server callback stubs in text mode
fn (mut g CoreGen) core_gen_server_stubs_text(name string, arity int) {
	g.write_indent_core()
	match name {
		'code_change' {
			g.out.writeln("{'ok', _1}")
		}
		'terminate' {
			g.out.writeln("'ok'")
		}
		'handle_info' {
			if arity == 2 {
				g.out.writeln("{'noreply', _1}")
			} else {
				g.out.writeln("'ok'")
			}
		}
		'handle_call' {
			if arity == 3 {
				g.out.writeln("{'reply', 'ok', _2}")
			} else if arity == 2 {
				g.out.writeln("{'ok', 'ok', _1}")
			} else {
				g.out.writeln("'ok'")
			}
		}
		'handle_cast' {
			g.out.writeln("{'noreply', _1}")
		}
		else {
			g.out.writeln("'ok'")
		}
	}
}

// Generate gen_event/gen_statem callback stubs in text mode
fn (mut g CoreGen) core_event_statem_stubs_text(name string, arity int) {
	g.write_indent_core()
	if name == 'handle_event' {
		if arity == 2 {
			g.out.writeln("{'ok', _1}")
		} else if arity == 4 {
			g.out.writeln("'keep_state_and_data'")
		} else {
			g.out.writeln("'ok'")
		}
	} else {
		g.out.writeln("'ok'")
	}
}

// Generate a default Core Erlang stub for a missing OTP callback.
// Each callback gets a sensible default implementation.
fn (mut g CoreGen) core_default_callback_stub(name string, arity int) {
	// Build parameter names: _0, _1, _2, ...
	mut params := []string{}
	for i in 0 .. arity {
		params << '_${i}'
	}

	if g.etf_mode {
		// ETF mode: use helpers
		if g.fn_count > 0 {
			g.fndef_sep()
		}
		g.fn_count++
		g.begin_fndef(name, arity)
		g.begin_fun(params)

		// Dispatch to appropriate helper based on callback type
		if name in ['code_change', 'terminate', 'handle_info', 'handle_call', 'handle_cast'] {
			g.core_gen_server_stubs_etf(name, arity)
		} else if name == 'handle_event' {
			g.core_event_statem_stubs_etf(name, arity)
		} else {
			g.emit_atom('ok')
		}

		g.end_fun()
		g.end_fndef()
	} else {
		// Text mode: manual indentation
		g.writeln_core("'${name}'/${arity} =")
		g.indent++
		g.write_indent_core()
		g.out.write_string('fun (')
		g.out.write_string(params.join(', '))
		g.out.writeln(') ->')
		g.indent++

		// Dispatch to appropriate helper based on callback type
		if name in ['code_change', 'terminate', 'handle_info', 'handle_call', 'handle_cast'] {
			g.core_gen_server_stubs_text(name, arity)
		} else if name == 'handle_event' {
			g.core_event_statem_stubs_text(name, arity)
		} else {
			g.write_indent_core()
			g.out.writeln("'ok'")
		}

		g.indent -= 2
	}
}

fn (mut g CoreGen) core_module_info(erl_mod string) {
	if g.etf_mode {
		// ETF mode: module_info/0
		if g.fn_count > 0 {
			g.fndef_sep()
		}
		g.fn_count++
		g.begin_fndef('module_info', 0)
		g.begin_fun([]string{})
		g.begin_call('erlang', 'get_module_info')
		g.emit_atom(erl_mod)
		g.end_call()
		g.end_fun()
		g.end_fndef()

		// ETF mode: module_info/1
		g.fndef_sep()
		g.fn_count++
		g.begin_fndef('module_info', 1)
		g.begin_fun(['_0'])
		g.begin_call('erlang', 'get_module_info')
		g.emit_atom(erl_mod)
		g.write_core(', ')
		g.emit_var('_0')
		g.end_call()
		g.end_fun()
		g.end_fndef()
	} else {
		// Text mode: module_info/0
		g.writeln_core("'module_info'/0 =")
		g.writeln_core('    fun () ->')
		g.writeln_core("        call 'erlang':'get_module_info'")
		g.writeln_core("            ('${erl_mod}')")

		// Text mode: module_info/1
		g.writeln_core("'module_info'/1 =")
		g.writeln_core('    fun (_0) ->')
		g.writeln_core("        call 'erlang':'get_module_info'")
		g.writeln_core("            ('${erl_mod}', _0)")
	}
}

fn (mut g CoreGen) core_fn_name(node ast.FnDecl) string {
	if node.is_method {
		rec_type := node.receiver.typ
		type_sym := g.table.sym(rec_type)
		short_type := type_sym.name.all_after_last('.')
		return '${short_type}.${node.short_name}'
	}
	return node.short_name
}

fn (mut g CoreGen) core_v_mod_to_erl_mod(v_mod string) string {
	return 'v.${v_mod}'
}

// Output helpers
fn (mut g CoreGen) writeln_core(s string) {
	g.write_indent_core()
	g.out.writeln(s)
}

fn (mut g CoreGen) write_core(s string) {
	g.out.write_string(s)
}

fn (mut g CoreGen) write_indent_core() {
	for _ in 0 .. g.indent {
		g.out.write_string('    ')
	}
}

// Tier 3: Compile .core files to .beam using compile:forms/2
// Uses batch mode: writes all Core Erlang modules to a single temp file
// with null-byte delimiters, then the compiler processes them all in one
// VM invocation. This minimizes process spawns and file I/O.
fn (mut g CoreGen) compile_to_beam() {
	// Locate runtime directory (contains vbeam_compiler.erl and runtime modules)
	runtime_dir := os.join_path(@VEXEROOT, 'vlib', 'v', 'gen', 'beam', 'runtime')

	// Copy runtime .erl files to output directory
	if os.exists(runtime_dir) {
		erl_files := os.glob('${runtime_dir}/*.erl') or { []string{} }
		for erl_file in erl_files {
			dest := os.join_path(g.out_dir, os.file_name(erl_file))
			os.cp(erl_file, dest) or {}
		}
	}

	// Locate the compiler escript
	compiler_path := os.join_path(runtime_dir, 'vbeam_compiler.erl')

	if os.exists(compiler_path) {
		if g.erl_mode {
			// Erlang source mode: transpile each .core → .erl, then compile
			g.compile_via_erl_source(compiler_path)
		} else if g.module_sources.len > 0 {
			// Tier 3 batch mode: write all modules to single temp file
			batch_content := g.module_sources.join('\x00\x00\x00')
			batch_file := os.join_path(g.out_dir, '.vbeam_batch.tmp')
			os.write_file(batch_file, batch_content) or {
				eprintln('Error writing batch file: ${err}')
				g.compile_with_dir_mode(compiler_path)
				return
			}

			batch_flag := if g.etf_mode { '--batch-etf' } else { '--batch' }
			result := os.execute('escript "${compiler_path}" ${batch_flag} "${batch_file}" "${g.out_dir}"')
			os.rm(batch_file) or {}

			if result.output.len > 0 {
				for line in result.output.split_into_lines() {
					if line.len > 0 {
						println(line)
					}
				}
			}

			// Also compile .erl runtime files via --dir (they aren't in the batch)
			g.compile_erl_runtime(compiler_path)
		} else {
			g.compile_with_dir_mode(compiler_path)
		}
	} else {
		// Fallback: use erlc directly
		g.compile_with_erlc()
	}
}

// Compile using --dir mode (reads .core files from disk)
fn (mut g CoreGen) compile_with_dir_mode(compiler_path string) {
	result := os.execute('escript "${compiler_path}" --dir "${g.out_dir}"')
	if result.output.len > 0 {
		for line in result.output.split_into_lines() {
			if line.len > 0 {
				println(line)
			}
		}
	}
}

// Compile runtime .erl files that aren't in the batch
fn (mut g CoreGen) compile_erl_runtime(compiler_path string) {
	erl_files := os.glob('${g.out_dir}/*.erl') or { return }
	for erl_file in erl_files {
		base := os.file_name(erl_file).all_before_last('.')
		beam_file := os.join_path(g.out_dir, '${base}.beam')
		if !os.exists(beam_file) {
			result := os.execute('erlc -o "${g.out_dir}" "${erl_file}"')
			if result.exit_code != 0 {
				eprintln('erlc error for ${erl_file}: ${result.output}')
			}
		}
	}
}

// Compile via Erlang source: .core → .erl → erlc → .beam
// Uses vbeam_core_to_erl transpiler for Core Erlang → Erlang source.
// Triggered by VBEAM_ERL=1 environment variable.
fn (mut g CoreGen) compile_via_erl_source(compiler_path string) {
	runtime_dir := os.dir(compiler_path)

	// Compile the transpiler module first (needed by vbeam_compiler)
	transpiler_erl := os.join_path(runtime_dir, 'vbeam_core_to_erl.erl')
	transpiler_beam := os.join_path(g.out_dir, 'vbeam_core_to_erl.beam')
	if os.exists(transpiler_erl) && !os.exists(transpiler_beam) {
		os.execute('erlc -o "${g.out_dir}" "${transpiler_erl}"')
	}

	// Transpile each .core file to .erl, then compile via erl
	core_files := os.glob('${g.out_dir}/*.core') or { return }
	for core_file in core_files {
		result := os.execute('erl -noshell -pa "${g.out_dir}" -eval "' +
			"ok = vbeam_core_to_erl:transpile_file(\\\"${core_file}\\\", " +
			"\\\"${core_file.all_before_last('.')}.erl\\\"), " +
			'init:stop()" 2>&1')
		if result.exit_code != 0 {
			eprintln('core-to-erl error for ${core_file}: ${result.output}')
			continue
		}
		// Compile the generated .erl file
		erl_file := '${core_file.all_before_last(".")}.erl'
		compile_result := os.execute('erlc -o "${g.out_dir}" "${erl_file}"')
		if compile_result.exit_code != 0 {
			eprintln('erlc error for ${erl_file}: ${compile_result.output}')
		}
	}
	println('Compiled ${core_files.len} module(s) via Erlang source')

	// Also compile runtime .erl files
	g.compile_erl_runtime(compiler_path)
}

// Fallback: compile using erlc (Tier 2 behavior)
fn (mut g CoreGen) compile_with_erlc() {
	core_files := os.glob('${g.out_dir}/*.core') or { return }
	for core_file in core_files {
		result := os.execute('erlc -o "${g.out_dir}" "${core_file}"')
		if result.exit_code != 0 {
			eprintln('erlc error for ${core_file}: ${result.output}')
		}
	}
	// Compile runtime .erl files (only where no .beam exists yet)
	erl_files := os.glob('${g.out_dir}/*.erl') or { return }
	for erl_file in erl_files {
		base := os.file_name(erl_file).all_before_last('.')
		beam_file := os.join_path(g.out_dir, '${base}.beam')
		if !os.exists(beam_file) {
			result := os.execute('erlc -o "${g.out_dir}" "${erl_file}"')
			if result.exit_code != 0 {
				eprintln('erlc error for ${erl_file}: ${result.output}')
			}
		}
	}
}
