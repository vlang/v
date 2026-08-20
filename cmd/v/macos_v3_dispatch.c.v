module main

// This is the default-compiler dispatcher for the platforms where the V3
// compiler (vlib/v3) is embedded and runs in-process: macOS and Linux. On those
// targets `v` hands ordinary compile commands to V3 by default, and silently
// falls back to the stable V1 compiler when V3 cannot build a program yet. The
// file carries no `_darwin`/`_linux` suffix so a single implementation compiles
// everywhere; the actual dispatch is gated at runtime by
// `macos_v3_driver_is_available()`, which is only true on macOS and Linux (see
// macos_v3_driver_notd_cross.v). On Windows/BSD the driver is absent, so this
// dispatcher only rejects an explicit `-new-compiler` and otherwise stays out of
// the way. The `macos_v3_` name is kept for continuity with the original macOS
// rollout even though the behavior now also covers Linux.
import os
import v.pref
import v.util
import v.builder

const macos_v3_fallback_file_env = 'V_MACOS_V3_FALLBACK_FILE'
const macos_v3_c_error_dir_env = 'V_MACOS_V3_C_ERROR_DIR'
const macos_v3_vhash_env = 'V_MACOS_V3_VHASH'
const macos_v3_vcurrent_hash_env = 'V_MACOS_V3_VCURRENT_HASH'
const macos_v3_caller_vexe_env = 'V_MACOS_V3_CALLER_VEXE'
const macos_v3_caller_vexe_present_env = 'V_MACOS_V3_CALLER_VEXE_PRESENT'
const macos_v3_caller_vchild_env = 'V_MACOS_V3_CALLER_VCHILD'
const macos_v3_caller_vchild_present_env = 'V_MACOS_V3_CALLER_VCHILD_PRESENT'
const macos_v3_embedded_env = 'V_MACOS_V3_EMBEDDED'
const macos_v3_retry_env = 'V_MACOS_V3_RETRY'
const macos_v3_no_fallback_env = 'V_MACOS_V3_NO_FALLBACK'
const macos_v3_inline_asm_fallback = 'inline_asm'
const macos_v3_compiler_error_fallback = 'compiler_error'
const macos_v3_c_error_fallback = 'c_compilation_error'
const macos_v3_c_error_compiler_file = 'compiler'
const macos_v3_c_error_output_file = 'output'
const macos_v3_c_error_source_name_file = 'source_name'
// optional marker file inside a staged report dir; when it holds
// `macos_v3_compiler_error_fallback` the report describes a V3 internal compiler
// error (V source only) instead of a generated-C compilation error.
const macos_v3_c_error_kind_file = 'kind'
// the base diagnostic uploaded for a V3 internal compiler error (starts with `error:` so
// the receiver's c_error_string parser stores a nonempty, groupable diagnostic).
const macos_v3_compiler_error_message_base = 'error: the experimental V3 compiler hit an internal compiler error building this program'

fn macos_v3_compiler_error_message(stage string) string {
	stage_suffix := if stage == '' { '' } else { ' during ${stage}' }
	return '${macos_v3_compiler_error_message_base}${stage_suffix} (the stable V compiler built it successfully)'
}

fn macos_v3_fallback_reason_and_stage(payload string) (string, string) {
	if !payload.contains('\n') {
		return payload, ''
	}
	return payload.all_before('\n'), payload.all_after_first('\n').trim_space()
}

fn maybe_delegate_to_macos_v3(command string, prefs &pref.Preferences) ?MacosV3CErrorReport {
	if os.getenv(macos_v3_retry_env) == '1' {
		os.unsetenv(macos_v3_retry_env)
		return take_macos_v3_report_content()
	}
	if prefs.old_compiler {
		return take_macos_v3_report_content()
	}
	if !macos_v3_driver_is_available() {
		if prefs.new_compiler {
			eprintln('`-new-compiler` requires a build that embeds the V3 compiler, which this one does not.')
			exit(1)
		}
		return take_macos_v3_report_content()
	}
	all_args := util.join_env_vflags_and_os_args()
	forwarded_args := all_args[1..]
	if macos_v3_has_v1_only_leading_option(forwarded_args, command) {
		if prefs.new_compiler {
			eprintln('`-new-compiler` cannot be combined with a V1-only option; remove it or drop `-new-compiler`.')
			exit(1)
		}
		return none
	}
	if macos_v3_force_requested(command, prefs) {
		// An explicit `-new-compiler` is honored regardless of the executable's name.
		return launch_macos_v3_compiler(prefs, forwarded_args)
	}
	// Implicit default dispatch is limited to the canonical `v`/`vnew` executables, so
	// a freshly built or renamed compiler (v2, vstrict1, ... during self-hosting, or a
	// binary installed as e.g. `vlang`) stays on V1 unless `-new-compiler` is passed.
	if !is_macos_v3_default_executable(os.executable()) {
		trace_macos_v3_skip('non-default compiler executable `${os.executable()}`')
		return none
	}
	if !is_macos_v3_relevant_command(command, prefs) {
		return none
	}
	return launch_macos_v3_compiler(prefs, forwarded_args)
}

fn trace_macos_v3_skip(reason string) {
	if os.getenv('V3_CACHE_TRACE') != '' {
		eprintln('  macOS V3 dispatch skipped: ${reason}')
	}
}

fn is_macos_v3_default_executable(vexe string) bool {
	return os.base(vexe) in ['v', 'v.exe', 'vnew', 'vnew.exe']
}

fn is_macos_v3_relevant_command(command string, prefs &pref.Preferences) bool {
	if prefs.old_compiler {
		return false
	}
	if v3_has_v1_only_preferences(prefs) || (prefs.gc_set_by_flag && prefs.gc_mode != .no_gc) {
		// V1 still owns compiler modes whose runtime or C toolchain support has not
		// been implemented by V3 yet. The implicit GC default is resolved before
		// dispatch, but it must not prevent V3 from being the default compiler.
		return false
	}
	if prefs.autofree {
		// V1 owns autofree here. On macOS a direct `v -autofree` build is delegated
		// to the ownership-enabled compiler before this point (so only autofree
		// `run` arrives). On Linux autofree is NOT delegated and the ordinary
		// embedded V3 has no `ownership` support — it would exit with "ownership
		// support is not compiled into this v3 executable" — so keep every autofree
		// build and run on V1 rather than dispatching it to a V3 that cannot honor it.
		return false
	}
	if command == 'test' {
		// Keep discovery, per-file isolation, build constraints, and result
		// aggregation in vtest. Each _test.v file is compiled by this executable
		// again, so user test code still uses V3 by default.
		return false
	}
	if prefs.path == '' {
		return false
	}
	normalized_path := prefs.path.replace('\\', '/').trim_right('/')
	// cmd/v remains the command dispatcher. All other user compilation and test
	// modes use V3 by default.
	if normalized_path == 'cmd/v' || normalized_path.starts_with('cmd/v/')
		|| normalized_path.contains('/cmd/v/') || normalized_path.ends_with('/cmd/v')
		|| normalized_path == 'vlib/v3/v3.v' || normalized_path.ends_with('/vlib/v3/v3.v')
		|| is_macos_v3_internal_tool_bootstrap(normalized_path, os.getenv('VCHILD') == 'true') {
		return false
	}
	if command in external_tools {
		return false
	}
	if macos_v3_non_compilation_command(command) {
		return false
	}
	return command in ['run', 'build', 'test'] || prefs.is_script || os.is_dir(prefs.path)
		|| normalized_path.ends_with('.v') || normalized_path.ends_with('.vsh')
}

fn is_macos_v3_internal_tool_bootstrap(normalized_path string, is_vchild bool) bool {
	return is_vchild
		&& (normalized_path.starts_with('cmd/tools/') || normalized_path.contains('/cmd/tools/'))
}

fn launch_macos_v3_compiler(prefs &pref.Preferences, raw_args []string) ?MacosV3CErrorReport {
	caller_environment := os.environ()
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	util.set_vroot_folder(vroot)
	forwarded_args := macos_v3_forwarded_args(prefs, raw_args)
	if prefs.is_verbose {
		println('Running macOS V3 compiler in process: ${util.args_quote_paths(forwarded_args)}')
	}
	fallback_file := macos_v3_fallback_file_for_pid()
	os.rm(fallback_file) or {}
	c_error_dir := macos_v3_c_error_report_dir(fallback_file)
	os.rmdir_all(c_error_dir) or {}
	mut environment := macos_v3_child_environment(vexe, fallback_file, caller_environment)
	if prefs.new_compiler {
		// The user explicitly asked for V3, so a V3 failure must be reported
		// instead of silently retrying the build with the V1 compiler.
		environment[macos_v3_no_fallback_env] = '1'
	}
	replace_macos_v3_process_environment(environment)
	is_verbose := prefs.is_verbose
	// The input path is captured before V3 runs so the compatibility fallback can
	// stage it for a bug report if V3 fails with an internal compiler error.
	input_path := prefs.path
	retry_args := os.args[1..].clone()
	at_exit(fn [caller_environment, fallback_file, c_error_dir, retry_args, is_verbose, input_path] () {
		retry_macos_v3_with_old_compiler(caller_environment, fallback_file, c_error_dir,
			retry_args, is_verbose, input_path)
	}) or {
		eprintln('cannot register the V3 compatibility fallback: ${err}')
		exit(1)
	}
	macos_v3_driver_run(forwarded_args)
	os.rm(fallback_file) or {}
	os.rmdir_all(c_error_dir) or {}
	exit(0)
}

fn replace_macos_v3_process_environment(environment map[string]string) {
	current := os.environ()
	for name, _ in current {
		if name !in environment {
			os.unsetenv(name)
		}
	}
	for name, value in environment {
		os.setenv(name, value, true)
	}
}

fn retry_macos_v3_with_old_compiler(caller_environment map[string]string, fallback_file string, c_error_dir string, retry_args []string, is_verbose bool, input_path string) {
	fallback_payload := os.read_file(fallback_file) or { return }
	fallback_reason, fallback_stage := macos_v3_fallback_reason_and_stage(fallback_payload)
	os.rm(fallback_file) or {}
	if fallback_reason !in [macos_v3_inline_asm_fallback, macos_v3_compiler_error_fallback,
		macos_v3_c_error_fallback] {
		os.rmdir_all(c_error_dir) or {}
		return
	}
	if os.getenv(macos_v3_no_fallback_env) == '1' {
		if fallback_reason == macos_v3_c_error_fallback {
			if report := read_macos_v3_c_error_report(c_error_dir) {
				if report.c_output != '' {
					eprintln(report.c_output.trim_right('\r\n'))
				}
			}
		}
		os.rmdir_all(c_error_dir) or {}
		eprintln('V3 compatibility fallback disabled; requested reason: ${fallback_reason}')
		return
	}
	replace_macos_v3_process_environment(caller_environment)
	should_report := is_verbose || os.getenv('V3_CACHE_TRACE') != ''
	if fallback_reason == macos_v3_c_error_fallback {
		// The C-error report was staged by this process's own V3 run (generated C + C
		// compiler output), so it is trusted. Extract the bounded source snippet HERE,
		// forward it to the V1 retry as content, and remove the staged directory. The
		// retry files a bug once its build succeeds (see cmd/v rebuild ->
		// compile_with_external_c_error_report) without reading any path or deleting any
		// directory named by the (inheritable, forgeable) environment.
		report := read_macos_v3_c_error_report(c_error_dir) or {
			os.rmdir_all(c_error_dir) or {}
			eprintln('V3 requested a C-error fallback, but its diagnostics could not be read')
			return
		}
		export_macos_v3_report_content(report.kind, report.ccompiler, report.c_output,
			report.c_file)
		os.rmdir_all(c_error_dir) or {}
		if should_report {
			eprintln('V3 C compilation failed; retrying with `-old-compiler`.')
		}
	} else if fallback_reason == macos_v3_compiler_error_fallback {
		// V3 hit an internal compiler error (parser/checker/codegen) that V1 may still
		// handle. Bound the input V source HERE (trusted) and forward it to the retry as
		// content, so once the retry's build succeeds it prints the fallback notice and
		// (for a single-file build) files a bug with the source. A directory build (`v .`)
		// or non-V input yields no source, so the report stays metadata-only but the
		// fallback is never silent.
		export_macos_v3_report_content(macos_v3_compiler_error_fallback, 'v3',
			macos_v3_compiler_error_message(fallback_stage),
			macos_v3_compiler_error_input_source(input_path))
		os.rmdir_all(c_error_dir) or {}
		if should_report {
			eprintln('V3 compilation failed; retrying with `-old-compiler`.')
		}
	} else {
		// Inline assembly is a known, expected V3 limitation, not a bug: fall back without
		// filing a report. Still forward a notice-only marker so that once the stable
		// build succeeds the user sees the documented fallback notice (doc/docs.md) rather
		// than a silent switch that is indistinguishable from a direct V3 success.
		export_macos_v3_report_content(macos_v3_inline_asm_fallback, 'v3', '', '')
		os.rmdir_all(c_error_dir) or {}
		if should_report {
			println('V3 requested the compatibility compiler for inline assembly')
		}
	}
	os.setenv(macos_v3_retry_env, '1', true)
	executable := os.executable()
	os.execvp(executable, retry_args) or {
		os.rmdir_all(c_error_dir) or {}
		eprintln('failed to launch the compatibility compiler `${executable}`: ${err}')
	}
}

// take_macos_v3_report_content reads the content-only fallback report the owning process
// forwarded through the environment, or none when there is none. The report carries no
// path to read and no directory to delete, so a stale, inherited, or hostile handoff can
// at most make the retry upload attacker-supplied text — never disclose a file from, or
// recursively delete, an arbitrary directory. That is what makes authentication
// unnecessary here; across an execvp that an attacker can wrap (it retains the pid and
// inherits VTMP and every env var), authenticating a path handoff is impossible anyway.
fn take_macos_v3_report_content() ?MacosV3CErrorReport {
	report := builder.take_external_v3_report_from_env()?
	return MacosV3CErrorReport{
		kind:      report.kind
		ccompiler: report.ccompiler
		c_output:  report.c_output
		v_file:    report.v_file
		v_source:  report.v_source
	}
}

// export_macos_v3_report_content bounds the fallback source in THIS process — which staged
// the report and therefore trusts `c_file` — and forwards only that content to the V1
// retry through the environment.
fn export_macos_v3_report_content(kind string, ccompiler string, c_output string, c_file string) {
	v_file, v_source := builder.bounded_v3_fallback_source(kind, c_output, c_file)
	builder.export_external_v3_report_to_env(builder.ExternalCErrorBugReport{
		kind:          kind
		ccompiler:     ccompiler
		c_output:      c_output
		v_file:        v_file
		v_source:      v_source
		source_inline: true
		tag:           'V3'
	})
}

// macos_v3_compiler_error_input_source returns `input_path` when it is a single V source
// file whose bounded contents can be uploaded, or '' for a directory / non-V input (which
// keeps the internal-error report metadata-only).
fn macos_v3_compiler_error_input_source(input_path string) string {
	if input_path != '' && os.is_file(input_path)
		&& (input_path.ends_with('.v') || input_path.ends_with('.vsh')
		|| input_path.ends_with('.vv')) {
		return input_path
	}
	return ''
}

fn macos_v3_c_error_report_dir(fallback_file string) string {
	return fallback_file + '.c_error'
}

// macos_v3_fallback_file_for_pid names this process's scratch staging file under V's temp
// dir. It is only a working path for this process's own V3 run and retry; it is not used
// to authenticate anything (see take_macos_v3_report_content).
fn macos_v3_fallback_file_for_pid() string {
	return os.join_path(os.vtmp_dir(), 'macos_v3_fallback_${os.getpid()}')
}

// MacosV3StagedReport is the on-disk report a V3 run staged in this process's own scratch
// directory. Only the owning process reads it (to bound its source into content); it is
// never reconstructed from an environment-named directory.
struct MacosV3StagedReport {
	kind       string
	ccompiler  string
	c_output   string
	c_file     string
	report_dir string
}

fn read_macos_v3_c_error_report(report_dir string) ?MacosV3StagedReport {
	ccompiler := os.read_file(os.join_path(report_dir, macos_v3_c_error_compiler_file)) or {
		return none
	}
	c_output := os.read_file(os.join_path(report_dir, macos_v3_c_error_output_file)) or {
		return none
	}
	// The `kind` marker is absent (empty string -> generated-C compilation error) for the
	// C-error reports the V3 driver stages; only those are read from disk here.
	kind :=
		(os.read_file(os.join_path(report_dir, macos_v3_c_error_kind_file)) or { '' }).trim_space()
	clean_source_name := (os.read_file(os.join_path(report_dir, macos_v3_c_error_source_name_file)) or {
		''
	}).trim_space()
	mut c_file := ''
	if clean_source_name != '' {
		if os.base(clean_source_name) != clean_source_name {
			return none
		}
		candidate := os.join_path(report_dir, clean_source_name)
		if !os.is_file(candidate) {
			return none
		}
		c_file = candidate
	} else if kind != macos_v3_compiler_error_fallback {
		// A generated-C error report must always carry its source; only a
		// compiler-error report may be notice-only (a directory / non-file build).
		return none
	}
	return MacosV3StagedReport{
		kind:       kind
		ccompiler:  ccompiler.trim_space()
		c_output:   c_output
		c_file:     c_file
		report_dir: report_dir
	}
}

fn macos_v3_child_environment(vexe string, fallback_file string, caller_environment map[string]string) map[string]string {
	mut environment := caller_environment.clone()
	preserve_macos_v3_caller_environment_value(mut environment, caller_environment, 'VEXE',
		macos_v3_caller_vexe_env, macos_v3_caller_vexe_present_env)
	preserve_macos_v3_caller_environment_value(mut environment, caller_environment, 'VCHILD',
		macos_v3_caller_vchild_env, macos_v3_caller_vchild_present_env)
	for private_name in ['V_MACOS_V3_FALLBACK_FILE', 'V_MACOS_V3_C_ERROR_DIR', 'V_MACOS_V3_RETRY'] {
		environment.delete(private_name)
	}
	environment['VCHILD'] = 'true'
	environment['VEXE'] = os.real_path(vexe)
	environment[macos_v3_fallback_file_env] = fallback_file
	environment[macos_v3_c_error_dir_env] = macos_v3_c_error_report_dir(fallback_file)
	environment[macos_v3_vhash_env] = @VHASH
	environment[macos_v3_vcurrent_hash_env] = @VCURRENTHASH
	environment[macos_v3_embedded_env] = '1'
	return environment
}

fn preserve_macos_v3_caller_environment_value(mut environment map[string]string, caller_environment map[string]string, name string, value_name string, present_name string) {
	if value := caller_environment[name] {
		environment[value_name] = value
		environment[present_name] = '1'
	} else {
		environment[value_name] = ''
		environment[present_name] = '0'
	}
}
