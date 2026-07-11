module builder

import os
import strings
import v.pref
import v.gen.c as cgen
import v.util.version

const default_c_error_bug_report_url = 'https://bugs.vlang.io/bug-report'
const c_error_bug_report_disabled_env = 'V_C_ERROR_BUG_REPORT_DISABLED'
const c_error_context_radius = 5
// how many V source lines to upload on each side of the failing line: a small chunk local to the
// error, wider than the pinpoint `c_error_context_radius` above, but deliberately not the whole
// file (the failing program can hold proprietary code / secrets that should not be auto-uploaded)
const c_error_v_source_radius = 40
// marker used when `v_source` itself is too large; a V comment so the kept source stays parseable
const c_error_v_source_truncation_notice = '// ... v_source truncated for the bug report ...'
const c_error_bug_report_max_body_bytes = 256 * 1024
const c_error_bug_report_max_v_source_bytes = 64 * 1024
const c_error_bug_report_truncation_notice = '\n... report truncated before upload ...\n'

struct CErrorReportLine {
pub:
	line int
	text string
}

struct CErrorReportLocation {
pub:
	file string
	line int
}

struct CErrorBugReport {
pub:
	kind           string
	v_version      string
	target_os      string
	target_backend string
	arch           string
	ccompiler      string
	build_options  string // the codegen-affecting `v` flags (autofree, gc mode, -g, -prod, ...)
	c_error        string
	c_file         string
	c_line         int
	c_context      []CErrorReportLine
	v_file         string
	v_line         int
	v_context      []CErrorReportLine
	v_source       string // a small chunk of V source around the failing line (bounded), never the whole file
}

fn (mut v Builder) submit_c_error_bug_report(ccompiler string, c_output string) {
	if !should_submit_c_error_bug_report(v.pref.c_error_bug_report_url) {
		return
	}
	// Snapshot the user's real flags now: the vlines fallback below temporarily flips
	// `pref.is_vlines`, so computing this after it would misreport `vlines` for plain builds.
	build_options := codegen_build_options(v.pref)
	mut raw_report := v.new_c_error_bug_report(ccompiler, c_output)
	if raw_report.v_file == '' {
		// The default `.tmp.c` has no `#line` directives, so the C error could not be
		// traced back to a V line. Regenerate the C with `#line` info (as `-g` would),
		// recompile, and reuse the richer report when it does map to a V source line.
		if vlines_report := v.new_c_error_bug_report_with_vlines(ccompiler) {
			raw_report = vlines_report
		}
	}
	raw_report = CErrorBugReport{
		...raw_report
		build_options: build_options
	}
	report := bounded_c_error_bug_report(raw_report, c_error_bug_report_max_body_bytes)
	report_url := c_error_bug_report_url(v.pref.c_error_bug_report_url)
	tool_output := send_c_error_bug_report(report, report_url) or {
		eprintln('C compiler bug report was not sent to ${report_url}: ${err}')
		return
	}
	println('================== C compiler bug report ==============')
	if tool_output != '' {
		println(tool_output)
	}
	println('V ${report.v_version}, ${report.target_os}/${report.arch}, cc: ${report.ccompiler}, build options: ${report.build_options}')
	print_c_error_bug_report_context(report)
	println('='.repeat('================== C compiler bug report =============='.len))
}

fn (mut v Builder) new_c_error_bug_report(ccompiler string, c_output string) CErrorBugReport {
	c_source := os.read_file(v.out_name_c) or { '' }
	c_lines := c_source.split_into_lines()
	mut c_file := v.out_name_c
	mut c_line := 0
	mut v_file := ''
	mut v_line := 0
	if c_loc := c_error_location_for_generated_c(c_output, v.out_name_c) {
		c_file = c_loc.file
		c_line = c_loc.line
		if v_loc := v_source_location_for_c_line(c_lines, c_line, v.out_name_c) {
			v_file = v_loc.file
			v_line = v_loc.line
		}
	} else if source_loc := first_error_source_location(c_output) {
		v_file = source_loc.file
		v_line = source_loc.line
		if found_c_line := generated_c_line_for_source_location(c_lines, source_loc, v.out_name_c) {
			c_line = found_c_line
		}
	}
	// `v_context` shows the lines of whatever file the C error maps to (which can be an
	// included header, not V source).
	mapped_source := if v_file != '' { os.read_file(v_file) or { '' } } else { '' }
	v_chunk := selected_v_source(v_file, mapped_source.split_into_lines(), v_line)
	return CErrorBugReport{
		kind:           'v-c-compiler-error'
		v_version:      version.full_v_version(true)
		target_os:      v.pref.os.str()
		target_backend: v.pref.backend.str()
		arch:           v.pref.arch.str()
		ccompiler:      ccompiler
		build_options:  codegen_build_options(v.pref)
		c_error:        c_output
		c_file:         c_file
		c_line:         c_line
		c_context:      numbered_context_lines(c_lines, c_line, c_error_context_radius)
		v_file:         v_file
		v_line:         v_line
		v_context:      numbered_context_lines(mapped_source.split_into_lines(), v_line,
			c_error_context_radius)
		v_source:       bounded_v_source(v_chunk.text, c_error_bug_report_max_v_source_bytes,
			v_chunk.focus)
	}
}

// codegen_build_options returns a compact, space-separated list of the `v` flags that
// affect code generation (and therefore reproduction), e.g. `autofree gc:boehm -g skip_unused`.
fn codegen_build_options(p &pref.Preferences) string {
	mut opts := []string{}
	if p.autofree {
		opts << 'autofree'
	}
	opts << 'gc:${p.gc_mode}'
	if p.is_prod {
		opts << 'prod'
	}
	if p.no_prod_options {
		// suppresses the default -O3/-flto prod C flags (cc.v), changing the C compiler command.
		opts << 'no_prod_options'
	}
	if p.is_debug {
		// `-g` sets is_vlines (V `#line` output), `-cg` does not (C-line debug mode);
		// they produce different generated C, so distinguish them for reproduction.
		opts << if p.is_vlines { '-g' } else { '-cg' }
	}
	if p.skip_unused {
		opts << 'skip_unused'
	} else if p.backend == .c && p.build_mode != .build_module && !p.output_cross_c {
		// skip_unused defaults back to true for a normal C build, so a false value here means
		// `-no-skip-unused` was passed; without recording it, replay would drop unused code and
		// could compile a smaller C program that no longer hits the error. (`-build-module` and
		// `-cross` also force it off, but on their own; they are reported separately.)
		opts << 'no_skip_unused'
	}
	if p.output_cross_c {
		// `-cross` / `-os cross` compiles all platform files under C guards (and forces
		// skip_unused and the GC off), so the generated C differs from a host build.
		opts << 'cross'
	}
	if p.use_coroutines {
		opts << 'use_coroutines'
	}
	if p.parallel_cc {
		opts << 'parallel_cc'
	}
	if p.is_livemain {
		opts << 'live'
	}
	// `-sharedlive` also sets is_shared; report it as the live mode, not plain `shared`.
	if p.is_liveshared {
		opts << 'sharedlive'
	} else if p.is_shared {
		opts << 'shared'
	}
	if p.is_o {
		opts << 'obj'
	}
	if p.is_cstrict {
		opts << 'cstrict'
	}
	if p.sanitize {
		opts << 'sanitize'
	}
	if p.no_bounds_checking {
		opts << 'no_bounds_checking'
	}
	if p.force_bounds_checking {
		// keeps array bounds checks even inside `@[direct_array_access]` functions, so the
		// generated C differs from a replay that honors the attribute again.
		opts << 'force_bounds_checking'
	}
	if p.div_by_zero_is_zero {
		// cgen emits different safe div/mod helpers (`x / 0 == 0` instead of the panic path).
		opts << 'div_by_zero_is_zero'
	}
	if p.is_check_overflow {
		// cgen inserts runtime integer-overflow-check paths, changing the generated C.
		opts << 'check_overflow'
	}
	if !p.relaxed_gcc14 {
		// `-no-relaxed-gcc14` drops the gcc-14 diagnostic-relaxing pragmas (default on), so
		// gcc 14+ can turn the original errors into warnings on replay without it.
		opts << 'no_relaxed_gcc14'
	}
	if p.translated {
		opts << 'translated'
	}
	if p.enable_globals {
		// the checker rejects `__global` without this, so replaying the report would stop at
		// the checker instead of reaching the C compiler error.
		opts << 'enable_globals'
	}
	if p.experimental {
		// gates checker constructs allowed only under `-experimental` and changes autofree C,
		// so replay without it can stop in the checker or generate different C.
		opts << 'experimental'
	}
	if p.use_cache {
		opts << 'usecache'
	}
	if p.nofloat {
		opts << 'nofloat'
	}
	if p.fast_math {
		// appends `-ffast-math` / `/fp:fast` to the C compiler command, changing its invocation.
		opts << 'fast_math'
	}
	if p.no_std {
		// drops the default `-std=c99` / `-D_DEFAULT_SOURCE` C flags, changing the C compiler command.
		opts << 'no_std'
	}
	if p.no_rsp {
		// passes C backend options directly on the command line instead of via a `.rsp` response
		// file (should_use_rsp), so the C compiler is invoked differently.
		opts << 'no_rsp'
	}
	if p.prealloc {
		opts << 'prealloc'
	}
	if p.is_bare {
		opts << 'freestanding'
	}
	if p.no_builtin {
		opts << 'no_builtin'
	}
	if p.no_preludes {
		opts << 'no_preludes'
	}
	if p.is_prof {
		// the profile output path is embedded in the generated C (the `fopen(...)` call).
		opts << 'profile:${p.profile_file}'
	}
	if p.profile_no_inline {
		opts << 'profile_no_inline'
	}
	if p.profile_fns.len > 0 {
		// cgen only instruments the selected functions, so the set changes the generated C.
		opts << 'profile_fns:${p.profile_fns.join(',')}'
	}
	if p.trace_calls {
		opts << 'trace_calls'
	}
	if p.trace_fns.len > 0 {
		// the transformer only injects tracing into the matching functions.
		opts << 'trace_fns:${p.trace_fns.join(',')}'
	}
	if p.is_coverage {
		// coverage adds instrumentation and stores output under coverage_dir.
		opts << 'coverage:${p.coverage_dir}'
	}
	if p.cmain != '' {
		// `-cmain Foo` makes cgen emit `int Foo(...)` as the entry point instead of the normal
		// one, so the generated C differs.
		opts << 'cmain:${p.cmain}'
	}
	if p.assert_failure_mode != .default {
		// `-assert aborts|backtraces|continues` makes cgen emit a different post-failure path
		// (abort(), print_backtrace(), or none), so the generated C differs.
		opts << 'assert:${p.assert_failure_mode}'
	}
	if p.subsystem != .auto {
		// `-subsystem windows|console` changes the generated main function (cgen) and the
		// linker command on Windows, so the generated/linked C differs.
		opts << 'subsystem:${p.subsystem}'
	}
	if p.is_ios_simulator {
		// `-os ios -simulator` makes cc.v emit `-miphonesimulator-version-min` (simulator SDK)
		// instead of `-miphoneos-version-min`, so a device replay compiles differently.
		opts << 'ios_simulator'
	}
	if p.thread_stack_size_set_by_flag {
		// `spawn`/`go` embed this value in the CreateThread / pthread_attr_setstacksize call,
		// so it changes the generated C. Only recorded when set by flag, since the default
		// varies by target architecture.
		opts << 'thread_stack_size:${p.thread_stack_size}'
	}
	if p.build_mode != .default_mode {
		opts << 'build_mode:${p.build_mode}'
	}
	// Options reused verbatim from the recorded build options so they are preserved exactly.
	// Value-carrying ones (kept by prefix):
	//   -d              defines (`-d foo`, `-d pad=7`, empty `-d header=`) select source/codegen
	//                   via `$if foo ?` / `$d()`
	//   -cflags         passed to the C compiler, can decide whether the error reproduces (`-Werror`)
	//   -ldflags        passed to the C compiler/linker after every other option
	//   -custom-prelude replaces the generated prelude written into the C headers
	//   -bare-builtin-dir selects the freestanding builtin implementation
	//   -macosx-version-min passed to clang as `-mmacosx-version-min=...`, selects the SDK target
	//   -path           custom module lookup path, decides which imported module is resolved
	// Bare flags (kept by exact match), only present when explicitly passed (host-detected libc
	// defaults are not recorded, so these capture the user's explicit choice):
	//   -musl/-glibc    force the linked libc; `-musl` also enables `$if musl` and changes libgc flags
	//   -m32/-m64       select the target machine width, appended to the C compiler command via cflags
	verbatim_prefixes := ['-d ', '-cflags ', '-ldflags ', '-custom-prelude ', '-bare-builtin-dir ',
		'-macosx-version-min ', '-path ']
	verbatim_flags := ['-musl', '-glibc', '-m32', '-m64']
	for opt in p.build_options {
		if opt in verbatim_flags || verbatim_prefixes.any(opt.starts_with(it)) {
			opts << opt
		}
	}
	return opts.join(' ')
}

// VSourceChunk is the V source selected for a report, plus `focus` — the 1-based line within
// `text` that holds the failing line (0 when it is not known). The focus lets bounding keep a
// window around the failing line instead of dropping the middle.
struct VSourceChunk {
	text  string
	focus int
}

// selected_v_source picks the V source to upload. Only a small chunk around the mapped failing
// line is ever sent (never the whole file), so a C compiler error does not auto-upload unrelated
// or proprietary source. It returns an empty chunk when the C error does not map to a V source
// line (a header error, or no `#line` mapping at all).
fn selected_v_source(v_file string, mapped_lines []string, v_line int) VSourceChunk {
	if is_v_source_file(v_file) {
		return v_source_for_report(mapped_lines, v_line, c_error_v_source_radius)
	}
	return VSourceChunk{}
}

// v_source_for_report returns a small window of the mapped V file: `radius` lines on each side of
// the failing line, clamped to the file bounds. It is deliberately local to the error and never
// the whole file, so unrelated source is not uploaded. It returns an empty chunk when there is no
// mapped V line. The returned `focus` is the failing line's position within the window, so bounding
// can keep it if the window still exceeds the byte budget.
fn v_source_for_report(lines []string, center int, radius int) VSourceChunk {
	if center <= 0 || lines.len == 0 {
		return VSourceChunk{}
	}
	start := if center - radius < 1 { 1 } else { center - radius }
	end := if center + radius > lines.len { lines.len } else { center + radius }
	return VSourceChunk{
		text:  lines[start - 1..end].join('\n')
		focus: center - start + 1
	}
}

// bounded_v_source keeps the V source under `max_bytes`. The stored source is meant to be replayed
// as V, so it cuts on line boundaries and drops the marker in as a V comment on its own line. When
// `focus_line` is known (1-based), it keeps a window of whole lines around that line, so the exact
// failing line is never dropped even inside a declaration larger than `max_bytes`. Otherwise it
// keeps the start (declarations/imports) and the end (usually where the failing code lives).
fn bounded_v_source(source string, max_bytes int, focus_line int) string {
	if max_bytes <= 0 || source.len <= max_bytes {
		return source
	}
	marker := '\n${c_error_v_source_truncation_notice}\n'
	if max_bytes <= marker.len {
		// no room for both content and the marker: fall back to a hard prefix cut
		return source[..max_bytes]
	}
	if focus_line <= 0 {
		kept_bytes := max_bytes - marker.len
		head_budget := kept_bytes / 2
		tail_budget := kept_bytes - head_budget
		// end the head on a whole line, so a partial statement is not left before the marker
		mut head_end := source[..head_budget].last_index_u8(`\n`)
		if head_end <= 0 {
			head_end = head_budget
		}
		// begin the tail on a whole line, so it does not start in the middle of a statement
		tail_region_start := source.len - tail_budget
		next_nl := source[tail_region_start..].index_u8(`\n`)
		tail_start := if next_nl >= 0 { tail_region_start + next_nl + 1 } else { source.len }
		return source[..head_end] + marker + source[tail_start..]
	}
	// keep a window of whole lines centered on the failing line, growing outward until the budget
	// (minus room for a marker on each dropped side) is exhausted.
	lines := source.split_into_lines()
	fi := if focus_line > lines.len { lines.len - 1 } else { focus_line - 1 }
	reserve := 2 * marker.len
	mut lo := fi
	mut hi := fi
	mut used := lines[fi].len
	for {
		mut progressed := false
		if hi + 1 < lines.len && used + 1 + lines[hi + 1].len + reserve <= max_bytes {
			used += 1 + lines[hi + 1].len
			hi++
			progressed = true
		}
		if lo > 0 && used + 1 + lines[lo - 1].len + reserve <= max_bytes {
			used += 1 + lines[lo - 1].len
			lo--
			progressed = true
		}
		if !progressed {
			break
		}
	}
	mut parts := []string{}
	if lo > 0 {
		parts << c_error_v_source_truncation_notice
	}
	parts << lines[lo..hi + 1].join('\n')
	if hi + 1 < lines.len {
		parts << c_error_v_source_truncation_notice
	}
	result := parts.join('\n')
	// safety clamp in case a single very long line still exceeds the budget
	return if result.len > max_bytes { result[..max_bytes] } else { result }
}

// new_c_error_bug_report_with_vlines regenerates the program's C source with `#line`
// directives enabled (the same information `-g` would add), recompiles it with the
// previously used C compiler command, and builds a report from the recompiled output.
// Because the regenerated C carries `#line` annotations, the C error can be mapped back
// to the exact V source line that produced it. It returns none when the V mapping still
// cannot be produced, so the caller keeps the original, C-only report.
fn (mut v Builder) new_c_error_bug_report_with_vlines(ccompiler string) ?CErrorBugReport {
	if v.pref.is_vlines || v.pref.parallel_cc || v.pref.generate_c_project != ''
		|| v.last_cc_cmd == '' || v.parsed_files.len == 0 || v.out_name_c == '' {
		return none
	}
	old_is_vlines := v.pref.is_vlines
	v.pref.is_vlines = true
	defer {
		v.pref.is_vlines = old_is_vlines
	}
	// Regenerate the C source, now with `#line` directives, into the same `.tmp.c` file,
	// so that the recorded compiler command recompiles exactly the annotated source.
	// Keep the original `.tmp.c` so that it can be restored afterwards (e.g. for `-keepc`).
	original_c := os.read_file(v.out_name_c) or { return none }
	goutput := cgen.gen(v.parsed_files, mut v.table, v.pref)
	mut c_builder := goutput.res_builder
	c_builder = cgen.fix_reset_dbg_line(c_builder, v.out_name_c)
	os.write_file_array(v.out_name_c, c_builder) or { return none }
	vdir := os.dir(pref.vexe_path())
	original_pwd := os.getwd()
	os.chdir(vdir) or {}
	recompiled := os.execute(v.last_cc_cmd)
	os.chdir(original_pwd) or {}
	report := v.new_c_error_bug_report(ccompiler, recompiled.output)
	// Restore the C source that the user actually compiled, now that the report is built.
	os.write_file(v.out_name_c, original_c) or {}
	if report.v_file == '' {
		return none
	}
	return report
}

fn c_error_bug_report_url(flag_url string) string {
	trimmed_flag_url := flag_url.trim_space()
	if trimmed_flag_url != '' {
		return trimmed_flag_url.trim_right('/')
	}
	env_url := os.getenv('V_C_ERROR_BUG_REPORT_URL').trim_space()
	if env_url != '' {
		return env_url.trim_right('/')
	}
	return default_c_error_bug_report_url
}

fn should_submit_c_error_bug_report(flag_url string) bool {
	if c_error_bug_reports_disabled() {
		return false
	}
	if running_in_github_ci() {
		return c_error_bug_report_url(flag_url) != default_c_error_bug_report_url
	}
	return true
}

fn c_error_bug_reports_disabled() bool {
	return os.getenv(c_error_bug_report_disabled_env).trim_space().to_lower() in ['1', 'true',
		'yes', 'on']
}

fn disable_c_error_bug_reports() {
	os.setenv(c_error_bug_report_disabled_env, '1', true)
}

fn running_in_github_ci() bool {
	return os.getenv('GITHUB_ACTIONS') == 'true' || os.getenv('GITHUB_JOB') != ''
}

fn send_c_error_bug_report(report CErrorBugReport, report_url string) !string {
	report_path := os.join_path(os.vtmp_dir(), 'v-c-error-report-${os.getpid()}.json')
	os.write_file(report_path, c_error_bug_report_json(report))!
	defer {
		os.rm(report_path) or {}
	}
	cmd := '${os.quoted_path(pref.vexe_path())} bug-report-send --url ${os.quoted_path(report_url)} --file ${os.quoted_path(report_path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		return error(res.output.trim_space())
	}
	return res.output.trim_right('\r\n')
}

fn c_error_bug_report_json(report CErrorBugReport) string {
	mut b := strings.new_builder(1024 + report.c_error.len)
	b.write_u8(`{`)
	write_json_string_field(mut b, 'kind', report.kind, false)
	write_json_string_field(mut b, 'v_version', report.v_version, true)
	write_json_string_field(mut b, 'target_os', report.target_os, true)
	write_json_string_field(mut b, 'target_backend', report.target_backend, true)
	write_json_string_field(mut b, 'arch', report.arch, true)
	write_json_string_field(mut b, 'ccompiler', report.ccompiler, true)
	write_json_string_field(mut b, 'build_options', report.build_options, true)
	write_json_string_field(mut b, 'c_error', report.c_error, true)
	write_json_string_field(mut b, 'c_file', report.c_file, true)
	write_json_int_field(mut b, 'c_line', report.c_line, true)
	write_json_report_lines_field(mut b, 'c_context', report.c_context, true)
	write_json_string_field(mut b, 'v_file', report.v_file, true)
	write_json_int_field(mut b, 'v_line', report.v_line, true)
	write_json_report_lines_field(mut b, 'v_context', report.v_context, true)
	write_json_string_field(mut b, 'v_source', report.v_source, true)
	b.write_u8(`}`)
	return b.str()
}

fn write_json_string_field(mut b strings.Builder, name string, value string, needs_comma bool) {
	write_json_field_name(mut b, name, needs_comma)
	write_json_string(mut b, value)
}

fn write_json_int_field(mut b strings.Builder, name string, value int, needs_comma bool) {
	write_json_field_name(mut b, name, needs_comma)
	b.write_string(value.str())
}

fn write_json_report_lines_field(mut b strings.Builder, name string, lines []CErrorReportLine, needs_comma bool) {
	write_json_field_name(mut b, name, needs_comma)
	b.write_u8(`[`)
	for idx, line in lines {
		if idx > 0 {
			b.write_u8(`,`)
		}
		b.write_u8(`{`)
		write_json_int_field(mut b, 'line', line.line, false)
		write_json_string_field(mut b, 'text', line.text, true)
		b.write_u8(`}`)
	}
	b.write_u8(`]`)
}

fn write_json_field_name(mut b strings.Builder, name string, needs_comma bool) {
	if needs_comma {
		b.write_u8(`,`)
	}
	write_json_string(mut b, name)
	b.write_u8(`:`)
}

fn write_json_string(mut b strings.Builder, value string) {
	b.write_u8(`"`)
	for ch in value.bytes() {
		match ch {
			`"` {
				b.write_string('\\"')
			}
			`\\` {
				b.write_string('\\\\')
			}
			`\b` {
				b.write_string('\\b')
			}
			`\f` {
				b.write_string('\\f')
			}
			`\n` {
				b.write_string('\\n')
			}
			`\r` {
				b.write_string('\\r')
			}
			`\t` {
				b.write_string('\\t')
			}
			else {
				if ch < 0x20 {
					write_json_control_escape(mut b, ch)
				} else {
					b.write_u8(ch)
				}
			}
		}
	}
	b.write_u8(`"`)
}

fn write_json_control_escape(mut b strings.Builder, ch u8) {
	hex := '0123456789abcdef'
	b.write_string('\\u00')
	b.write_u8(hex[ch >> 4])
	b.write_u8(hex[ch & 0x0f])
}

fn bounded_c_error_bug_report(report CErrorBugReport, max_body_bytes int) CErrorBugReport {
	if max_body_bytes <= 0 || c_error_bug_report_json(report).len <= max_body_bytes {
		return report
	}
	if bounded := report_with_bounded_c_error(report, max_body_bytes, report.c_context,
		report.v_context)
	{
		return bounded
	}
	for context_text_bytes in [4096, 1024, 256, 80, 0] {
		c_context := bounded_report_lines(report.c_context, context_text_bytes)
		v_context := bounded_report_lines(report.v_context, context_text_bytes)
		if bounded := report_with_bounded_c_error(report, max_body_bytes, c_context, v_context) {
			return bounded
		}
	}
	return CErrorBugReport{
		...report
		c_error:   truncated_report_text(report.c_error, 0)
		c_context: []CErrorReportLine{}
		v_context: []CErrorReportLine{}
	}
}

fn report_with_bounded_c_error(report CErrorBugReport, max_body_bytes int, c_context []CErrorReportLine, v_context []CErrorReportLine) ?CErrorBugReport {
	min_report := CErrorBugReport{
		...report
		c_error:   truncated_report_text(report.c_error, 0)
		c_context: c_context
		v_context: v_context
	}
	if c_error_bug_report_json(min_report).len > max_body_bytes {
		return none
	}
	mut low := 0
	mut high := report.c_error.len
	mut best := min_report
	for low <= high {
		mid := (low + high) / 2
		candidate := CErrorBugReport{
			...report
			c_error:   truncated_report_text(report.c_error, mid)
			c_context: c_context
			v_context: v_context
		}
		if c_error_bug_report_json(candidate).len <= max_body_bytes {
			best = candidate
			low = mid + 1
		} else {
			high = mid - 1
		}
	}
	return best
}

fn bounded_report_lines(lines []CErrorReportLine, max_text_bytes int) []CErrorReportLine {
	mut bounded := []CErrorReportLine{cap: lines.len}
	for report_line in lines {
		bounded << CErrorReportLine{
			line: report_line.line
			text: truncated_report_text(report_line.text, max_text_bytes)
		}
	}
	return bounded
}

fn truncated_report_text(text string, max_bytes int) string {
	if max_bytes <= 0 {
		return ''
	}
	if text.len <= max_bytes {
		return text
	}
	if max_bytes <= c_error_bug_report_truncation_notice.len {
		return text[..max_bytes]
	}
	kept_bytes := max_bytes - c_error_bug_report_truncation_notice.len
	head_bytes := kept_bytes / 2
	tail_bytes := kept_bytes - head_bytes
	return text[..head_bytes] + c_error_bug_report_truncation_notice + text[text.len - tail_bytes..]
}

fn print_c_error_bug_report_context(report CErrorBugReport) {
	println('Generated C lines sent from ${report.c_file}:${report.c_line}:')
	print_report_lines(report.c_context, report.c_line)
	if report.v_file != '' {
		println('Corresponding V lines sent from ${report.v_file}:${report.v_line}:')
		print_report_lines(report.v_context, report.v_line)
	} else {
		println('Corresponding V lines sent: no V source mapping was available.')
	}
}

fn print_report_lines(lines []CErrorReportLine, center int) {
	if lines.len == 0 {
		println('  (no source lines available)')
		return
	}
	for line in lines {
		prefix := if line.line == center { '>' } else { ' ' }
		println('${prefix} ${line.line:6} | ${line.text}')
	}
}

fn numbered_context_lines(lines []string, center int, radius int) []CErrorReportLine {
	if center <= 0 || lines.len == 0 {
		return []CErrorReportLine{}
	}
	mut start := center - radius
	if start < 1 {
		start = 1
	}
	mut end := center + radius
	if end > lines.len {
		end = lines.len
	}
	mut context := []CErrorReportLine{cap: end - start + 1}
	for line_nr in start .. end + 1 {
		context << CErrorReportLine{
			line: line_nr
			text: lines[line_nr - 1]
		}
	}
	return context
}

fn c_error_location_for_generated_c(c_output string, generated_c_file string) ?CErrorReportLocation {
	needles := c_error_generated_c_needles(generated_c_file)
	for output_line in c_output.split_into_lines() {
		if !output_line.to_lower_ascii().contains('error') {
			continue
		}
		for needle in needles {
			if loc := parse_error_location_after_needle(output_line, needle) {
				return loc
			}
		}
	}
	return none
}

fn c_error_generated_c_needles(generated_c_file string) []string {
	mut needles := []string{}
	for candidate in [generated_c_file, os.real_path(generated_c_file),
		os.file_name(generated_c_file)] {
		if candidate != '' && candidate !in needles {
			needles << candidate
		}
		normalized := candidate.replace('\\', '/')
		if normalized != '' && normalized !in needles {
			needles << normalized
		}
	}
	return needles
}

fn parse_error_location_after_needle(output_line string, needle string) ?CErrorReportLocation {
	idx := output_line.index(needle) or { return none }
	after := output_line[idx + needle.len..]
	if after.starts_with(':') {
		line_nr := leading_int(after[1..])
		if line_nr > 0 {
			return CErrorReportLocation{
				file: needle
				line: line_nr
			}
		}
	}
	if after.starts_with('(') {
		line_nr := leading_int(after[1..])
		if line_nr > 0 {
			return CErrorReportLocation{
				file: needle
				line: line_nr
			}
		}
	}
	return none
}

fn first_error_source_location(c_output string) ?CErrorReportLocation {
	for output_line in c_output.split_into_lines() {
		if !output_line.to_lower_ascii().contains('error') {
			continue
		}
		if loc := parse_colon_error_location(output_line) {
			return loc
		}
		if loc := parse_msvc_error_location(output_line) {
			return loc
		}
	}
	return none
}

fn parse_colon_error_location(output_line string) ?CErrorReportLocation {
	parts := output_line.split(':')
	if parts.len < 2 {
		return none
	}
	for idx := 1; idx < parts.len; idx++ {
		line_nr := parts[idx].int()
		if line_nr <= 0 {
			continue
		}
		file := parts[..idx].join(':')
		if file == '' {
			continue
		}
		return CErrorReportLocation{
			file: file
			line: line_nr
		}
	}
	return none
}

fn parse_msvc_error_location(output_line string) ?CErrorReportLocation {
	open_idx := output_line.index('(') or { return none }
	close_rel_idx := output_line[open_idx + 1..].index(')') or { return none }
	line_nr := leading_int(output_line[open_idx + 1..open_idx + 1 + close_rel_idx])
	if line_nr <= 0 {
		return none
	}
	return CErrorReportLocation{
		file: output_line[..open_idx]
		line: line_nr
	}
}

fn v_source_location_for_c_line(c_lines []string, c_line int, generated_c_file string) ?CErrorReportLocation {
	if c_line <= 0 || c_lines.len == 0 {
		return none
	}
	mut current := CErrorReportLocation{}
	last_line := if c_line <= c_lines.len { c_line } else { c_lines.len }
	for idx in 0 .. last_line {
		if directive := parse_line_directive(c_lines[idx]) {
			current = directive
			continue
		}
		if idx + 1 == c_line && is_v_source_file(current.file)
			&& !same_path(current.file, generated_c_file) {
			return current
		}
		if current.file != '' {
			current = CErrorReportLocation{
				file: current.file
				line: current.line + 1
			}
		}
	}
	return none
}

fn generated_c_line_for_source_location(c_lines []string, source CErrorReportLocation, generated_c_file string) ?int {
	if source.file == '' || source.line <= 0 {
		return none
	}
	mut current := CErrorReportLocation{}
	mut fallback_line := 0
	for idx, line in c_lines {
		if directive := parse_line_directive(line) {
			current = directive
			continue
		}
		if is_v_source_file(current.file) && !same_path(current.file, generated_c_file)
			&& same_path(current.file, source.file) && current.line == source.line {
			if fallback_line == 0 {
				fallback_line = idx + 1
			}
			if line.trim_space() != '' {
				return idx + 1
			}
		}
		if current.file != '' {
			current = CErrorReportLocation{
				file: current.file
				line: current.line + 1
			}
		}
	}
	if fallback_line > 0 {
		return fallback_line
	}
	return none
}

fn parse_line_directive(line string) ?CErrorReportLocation {
	trimmed := line.trim_space()
	if !trimmed.starts_with('#line ') {
		return none
	}
	rest := trimmed['#line '.len..].trim_space()
	line_nr := leading_int(rest)
	if line_nr <= 0 {
		return none
	}
	first_quote_idx := rest.index('"') or { return none }
	remaining := rest[first_quote_idx + 1..]
	second_quote_idx := remaining.index('"') or { return none }
	return CErrorReportLocation{
		file: remaining[..second_quote_idx]
		line: line_nr
	}
}

fn leading_int(s string) int {
	mut end := 0
	for end < s.len && s[end].is_digit() {
		end++
	}
	if end == 0 {
		return 0
	}
	return s[..end].int()
}

fn is_v_source_file(path string) bool {
	return path.ends_with('.v') || path.ends_with('.vv') || path.ends_with('.vsh')
}

fn same_path(a string, b string) bool {
	if a == b {
		return true
	}
	normalized_a := a.replace('\\', '/')
	normalized_b := b.replace('\\', '/')
	return normalized_a == normalized_b
		|| os.real_path(a).replace('\\', '/') == os.real_path(b).replace('\\', '/')
}
