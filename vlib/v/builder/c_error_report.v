module builder

import os
import strings
import crypto.sha256
import encoding.base64
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
// The largest C diagnostic forwarded through a single V_MACOS_V3_REPORT_* environment
// variable during the V3->V1 handoff. A single exec argument/environment string is capped
// near 128 KiB on Linux (MAX_ARG_STRLEN), so a huge template/generated-code diagnostic
// must be truncated here or the os.execvp retry fails with E2BIG and defeats the
// compatibility fallback. The final upload is separately bounded by
// c_error_bug_report_max_body_bytes.
const c_error_bug_report_max_env_c_output_bytes = 64 * 1024
// Keep the complete V/native input manifest below Linux's per-environment-string
// exec limit. If a very large project exceeds this bound, the retry still runs,
// but its fallback report is conservatively suppressed because exact equivalence
// cannot be proved.
const v3_report_max_env_input_digests_bytes = 64 * 1024
// Bound the complete handoff, not just each value. Linux guarantees at least 128 KiB
// for argv+environment and macOS allows more, so use that lower bound while accounting
// for the caller's existing argv/environment and retaining extra implementation slack.
const v3_report_conservative_exec_bytes = 128 * 1024
const v3_report_exec_reserve_bytes = 16 * 1024
const v3_report_max_env_payload_bytes = 64 * 1024
// `_wputenv` receives NAME=VALUE as one wide string. Keep each handoff value
// below Win32's 32,767-character user-defined environment-variable limit.
const v3_report_windows_max_env_value_bytes = 30 * 1024
const v3_fallback_native_input_prefix = '@native-input:'
const v3_fallback_native_manifest_key = '@native-input-manifest:v1'
const v3_fallback_native_manifest_value = 'v3-native-input-manifest-v1'

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
	v_source       string // full mapped source for a V3 fallback, or a bounded excerpt for a direct stable-compiler report
	// v_source_truncated records, explicitly, whether v_source is a bounded excerpt rather than
	// the complete file, so the fallback notice states accurately how much source was uploaded
	// without having to sniff for the in-band truncation marker.
	v_source_truncated bool
}

// external_v3_compiler_error_kind marks an ExternalCErrorBugReport that describes a
// V3 internal compiler error (parser/checker/codegen) rather than a generated-C
// compilation error. It matches `macos_v3_compiler_error_fallback` in cmd/v.
const external_v3_compiler_error_kind = 'compiler_error'

// external_v3_notice_only_kind marks a fallback for a known, expected V3 limitation
// (e.g. inline assembly) rather than a defect: the standard notice is printed once the
// stable build succeeds, but no bug report is filed. It matches
// `macos_v3_inline_asm_fallback` in cmd/v.
const external_v3_notice_only_kind = 'inline_asm'

// external_v3_transport_limited_kind preserves the successful-fallback notice when the
// complete report cannot fit safely through execve. No report is filed in this case.
const external_v3_transport_limited_kind = 'transport_limit'

// ExternalCErrorBugReport describes a failure produced by another compiler
// implementation and confirmed by a successful build with the established compiler.
// `kind` is empty for a generated-C compilation error (the default), identifies an
// internal V3 failure, or marks a notice-only fallback; for internal failures `c_file`
// is the input V source and `c_output` is a short description.
pub struct ExternalCErrorBugReport {
pub:
	kind        string
	ccompiler   string
	c_output    string
	c_file      string
	tag         string
	cleanup_dir string
	// The fields below carry a report to an external builder as self-contained CONTENT
	// (see export_external_v3_report_to_env), instead of as a filesystem path to read or
	// a directory to delete — both of which would be forgeable by an inherited or hostile
	// environment. When source_inline is set, v_file/v_source are used verbatim and no
	// path from the report is ever read or removed.
	v_file   string // informational base filename of the failing source (no directory)
	v_source string // the full failing file, or an already bounded internal-error snapshot
	// v_source_focus is the 1-based failing line within v_source (0 for none). export applies or
	// tightens the byte bound centered on it, so a generated-C failure keeps the exact failing code.
	v_source_focus int
	// v_source_truncated records, explicitly, whether v_source is already a bounded excerpt.
	// Export preserves it and also sets it when tightening the byte bound; it rides through the
	// environment so the retry states truncation accurately without sniffing source content.
	v_source_truncated bool
	source_inline      bool // true: use v_file/v_source as-is and touch no filesystem path
	// Untagged path/digest pairs below describe source bytes V3 actually parsed. The
	// stable parser compares them only with its own parsed-file paths and scanner
	// digests. Tagged native inputs describe resolved #include/#insert dependencies;
	// after a successful build the stable compiler reopens those regular files only to
	// verify their SHA-256 digests before it can submit the fallback report.
	input_digests          map[string]string
	input_digests_complete bool
}

enum V3FallbackInputStatus {
	unavailable
	unchanged
	changed
}

@[unsafe]
fn external_c_error_report_cleanup_dir(dir string, update bool) string {
	mut static pending_dir := ''
	if update {
		pending_dir = dir
	}
	return pending_dir
}

fn register_external_c_error_report_cleanup(dir string) {
	if dir == '' {
		return
	}
	unsafe {
		external_c_error_report_cleanup_dir(dir, true)
	}
	at_exit(cleanup_pending_external_c_error_report) or {}
}

fn cleanup_external_c_error_report(dir string) {
	if dir == '' {
		return
	}
	os.rmdir_all(dir) or { return }
	pending_dir := unsafe { external_c_error_report_cleanup_dir('', false) }
	if pending_dir == dir {
		unsafe {
			external_c_error_report_cleanup_dir('', true)
		}
	}
}

fn cleanup_pending_external_c_error_report() {
	pending_dir := unsafe { external_c_error_report_cleanup_dir('', false) }
	cleanup_external_c_error_report(pending_dir)
}

fn (mut v Builder) submit_c_error_bug_report(ccompiler string, c_output string) {
	v.submit_c_error_bug_report_with_tag(ccompiler, c_output, '', true)
}

fn (mut v Builder) submit_c_error_bug_report_with_tag(ccompiler string, c_output string, tag string, retry_with_vlines bool) {
	// A non-empty tag marks an external report: another compiler (V3) produced C
	// that failed to build, and the stable compiler has since confirmed the program
	// is buildable. That is exactly the V3->V1 fallback the user should be told about.
	is_v3_fallback := tag != ''
	if !should_submit_c_error_bug_report(v.pref.c_error_bug_report_url) {
		if is_v3_fallback {
			print_v3_fallback_notice('', false, false, false)
		}
		return
	}
	// Snapshot the user's real flags now: the vlines fallback below temporarily flips
	// `pref.is_vlines`, so computing this after it would misreport `vlines` for plain builds.
	build_options := c_error_report_build_options(v.pref, tag)
	mut raw_report := v.new_c_error_bug_report(ccompiler, c_output, is_v3_fallback)
	if retry_with_vlines && raw_report.v_file == '' {
		// The default `.tmp.c` has no `#line` directives, so the C error could not be
		// traced back to a V line. Regenerate the C with `#line` info (as `-g` would),
		// recompile, and reuse the richer report when it does map to a V source line.
		if vlines_report := v.new_c_error_bug_report_with_vlines(ccompiler, is_v3_fallback) {
			raw_report = vlines_report
		}
	}
	v.send_prepared_c_error_bug_report(CErrorBugReport{
		...raw_report
		build_options: build_options
	}, tag)
}

// send_prepared_c_error_bug_report bounds and submits an already-built C-compiler bug
// report, then — for a V3 fallback (non-empty tag) — prints the fallback notice relative
// to the send outcome. Callers must have already confirmed should_submit_c_error_bug_report.
fn (mut v Builder) send_prepared_c_error_bug_report(raw_report CErrorBugReport, tag string) {
	is_v3_fallback := tag != ''
	report := bounded_c_error_bug_report(raw_report, c_error_bug_report_max_body_bytes)
	report_url := c_error_bug_report_url(v.pref.c_error_bug_report_url)
	tool_output := send_c_error_bug_report(report, report_url) or {
		eprintln('C compiler bug report was not sent to ${report_url}: ${err}')
		if is_v3_fallback {
			print_v3_fallback_notice('', false, false, false)
		}
		return
	}
	// Report diagnostics go to stderr, never stdout: with `v -o - source.v` the generated
	// C is already on stdout, so appending this banner there would corrupt the documented
	// `-o -` output for exactly the programs that needed the fallback.
	eprintln('================== C compiler bug report ==============')
	if is_v3_fallback {
		// A report can carry source as the full `v_source` OR only a few `v_context` lines (a
		// context-only upload for a mapped non-V file / legacy external report). Only a nonempty,
		// non-truncated `v_source` is the complete file; context-only is a bounded excerpt.
		print_v3_fallback_notice(report_url, true, report_includes_v_source(report),
			report_uploaded_complete_source(report))
	}
	if tool_output != '' {
		eprintln(tool_output)
	}
	eprintln('V ${report.v_version}, ${report.target_os}/${report.arch}, cc: ${report.ccompiler}, build options: ${report.build_options}')
	print_c_error_bug_report_context(report)
	eprintln('='.repeat('================== C compiler bug report =============='.len))
}

// submit_external_c_error_bug_report submits C diagnostics and generated source produced by
// another compiler implementation after the established compiler has confirmed the build.
pub fn submit_external_c_error_bug_report(prefs &pref.Preferences, ccompiler string, c_output string, c_file string, tag string) {
	if !c_error_should_send_bug_report(c_output) {
		// These diagnostics are not eligible for automatic submission (e.g. a
		// missing library or missing libatomic), but V3 still fell back to the
		// stable compiler, so the user must be told about the fallback regardless.
		// `submit_c_error_bug_report_with_tag` would normally emit this, but the
		// filter returns before reaching it. A non-empty tag marks the V3 fallback.
		if tag != '' {
			print_v3_fallback_notice('', false, false, false)
		}
		return
	}
	mut b := new_builder(prefs)
	b.out_name_c = c_file
	b.submit_c_error_bug_report_with_tag(ccompiler, c_output, tag, false)
}

fn consume_external_c_error_bug_report(prefs &pref.Preferences, report ExternalCErrorBugReport) {
	defer {
		// Empty for an inline (content) report: it names no directory to delete, so a
		// forged handoff can never trigger os.rmdir_all on a caller path.
		cleanup_external_c_error_report(report.cleanup_dir)
	}
	if report.source_inline {
		// The report carries its already-bounded source as content — so submission never
		// reads a filesystem path or deletes a directory named by the (inheritable,
		// forgeable) environment; a forged handoff can therefore at worst upload
		// attacker-supplied text. Dispatch on kind so a generated-C fallback keeps its
		// `v-c-compiler-error` classification and its missing-library filter instead of
		// being reported as an internal V3 error.
		if report.kind in [external_v3_notice_only_kind, external_v3_transport_limited_kind] {
			// A known V3 limitation or a report omitted to keep the retry transport safe:
			// the stable build has just succeeded, so tell the user V3 fell back — matching
			// the documented notice (doc/docs.md) — but file no bug report.
			print_v3_fallback_notice('', false, false, false)
			return
		}
		if report.kind == external_v3_compiler_error_kind {
			submit_inline_v3_compiler_error_bug_report(prefs, report.ccompiler, report.c_output,
				report.v_file, report.v_source, report.v_source_truncated, report.tag)
		} else {
			submit_inline_c_error_bug_report(prefs, report.ccompiler, report.c_output,
				report.v_file, report.v_source, report.v_source_truncated, report.tag)
		}
		return
	}
	// Trusted in-process path (the report never crossed the environment): read the files.
	if report.kind == external_v3_compiler_error_kind {
		submit_external_v3_compiler_error_bug_report(prefs, report.ccompiler, report.c_output,
			report.c_file, report.tag)
		return
	}
	submit_external_c_error_bug_report(prefs, report.ccompiler, report.c_output, report.c_file,
		report.tag)
}

// v3_report_env_prefix names the environment variables used to hand a staged V3->V1
// fallback report to an external builder tool (e.g. the wasm builder) launched via
// os.execvp, which replaces this process. The tool reads them back with
// take_external_v3_report_from_env and, via compile_with_external_c_error_report,
// submits the report and prints the notice only after its own build succeeds.
const v3_report_env_prefix = 'V_MACOS_V3_REPORT_'

// v3_report_env_suffixes lists every V_MACOS_V3_REPORT_* variable, so both the export
// and the take paths agree on exactly what to set and clear. Source snippets are content
// only and no directory path is accepted. Tagged native paths can only be hashed after
// the receiver constrains them to roots selected by its own successful build.
const v3_report_env_suffixes = ['PRESENT', 'KIND', 'CCOMPILER', 'COUTPUT', 'TAG', 'VFILE', 'VSOURCE',
	'VSOURCE_TRUNCATED', 'VSOURCE_FOCUS', 'INPUT_DIGESTS', 'INPUT_DIGESTS_COMPLETE']

// encode_v3_report_input_digests serializes arbitrary input paths without allowing a
// newline or separator in a filename to corrupt the manifest. Untagged paths are matched
// to stable-parser inputs; tagged native paths are constrained before digest verification.
fn encode_v3_report_input_digests(input_digests map[string]string) ?string {
	mut paths := input_digests.keys()
	paths.sort()
	mut encoded := strings.new_builder(paths.len * 128)
	for path in paths {
		digest := input_digests[path]
		if digest.len != sha256.size * 2 {
			return none
		}
		encoded.write_string(base64.encode_str(path))
		encoded.write_u8(` `)
		encoded.write_string(digest)
		encoded.write_u8(`\n`)
		if encoded.len > v3_report_max_env_input_digests_bytes {
			return none
		}
	}
	return encoded.str()
}

fn decode_v3_report_input_digests(encoded string) ?map[string]string {
	mut input_digests := map[string]string{}
	for line in encoded.split_into_lines() {
		separator := line.index_u8(` `)
		if separator <= 0 || separator + 1 >= line.len {
			return none
		}
		encoded_path := line[..separator]
		path := base64.decode_str(encoded_path)
		digest := line[separator + 1..]
		if path == '' || base64.encode_str(path) != encoded_path || digest.len != sha256.size * 2 {
			return none
		}
		input_digests[path] = digest
	}
	return input_digests
}

fn v3_report_env_entry_bytes(suffix string, value string) int {
	// NAME=VALUE\0 plus the environment-vector pointer used by execve.
	return v3_report_env_prefix.len + suffix.len + 1 + value.len + 1 + int(sizeof(voidptr))
}

fn v3_report_env_payload_bytes(payload map[string]string) int {
	mut total := 0
	for suffix, value in payload {
		total += v3_report_env_entry_bytes(suffix, value)
	}
	return total
}

fn v3_report_env_value_limit() int {
	$if windows {
		return v3_report_windows_max_env_value_bytes
	} $else {
		return v3_report_max_env_payload_bytes
	}
}

fn v3_report_env_values_fit(payload map[string]string, max_value_bytes int) bool {
	for _, value in payload {
		if value.len > max_value_bytes {
			return false
		}
	}
	return true
}

fn v3_report_env_budget(environment map[string]string, args []string) int {
	mut used := v3_report_exec_reserve_bytes
	mut base_environment_entries := 0
	for name, value in environment {
		// Existing handoff values are replaced below, so they consume no additional
		// capacity in the next process.
		if name.starts_with(v3_report_env_prefix) {
			continue
		}
		used += name.len + 1 + value.len + 1
		base_environment_entries++
	}
	for arg in args {
		used += arg.len + 1
	}
	used += (base_environment_entries + args.len + 2) * int(sizeof(voidptr))
	remaining := v3_report_conservative_exec_bytes - used
	if remaining <= 0 {
		return 0
	}
	return if remaining < v3_report_max_env_payload_bytes {
		remaining
	} else {
		v3_report_max_env_payload_bytes
	}
}

fn set_v3_report_env_payload(payload map[string]string) {
	for suffix in v3_report_env_suffixes {
		os.unsetenv('${v3_report_env_prefix}${suffix}')
	}
	for suffix, value in payload {
		os.setenv('${v3_report_env_prefix}${suffix}', value, true)
	}
}

fn v3_transport_limited_notice_payload() map[string]string {
	return {
		'PRESENT': '1'
		'KIND':    external_v3_transport_limited_kind
	}
}

// export_external_v3_report_to_env hands `report` to the next external builder (launched
// via os.execvp) as self-contained content. That builder cannot authenticate anything
// passed to it: it inherits the environment, and VTMP plus every V_MACOS_V3_REPORT_*
// value is caller-controlled, so a token or path validated against that same environment
// establishes no provenance (an inherited or hostile environment could forge all of it).
// The safe design therefore never gives the builder a capability it could be tricked
// into misusing: this function bounds the source snippet here — in the trusted parent
// that owns the staged directory — passes only that content, and then removes the staged
// directory itself. The builder never reads report-named source or deletes a directory;
// it only hashes tagged native dependencies after constraining them to roots selected by
// its own successful build. A forged handoff therefore cannot disclose a victim's file
// or recursively delete a victim's directory.
// env_c_output_budget caps the diagnostic's environment budget at both the per-variable value
// limit and the fixed maximum, so a single V_MACOS_V3_REPORT_COUTPUT value never risks E2BIG.
fn env_c_output_budget(available int, value_limit int) int {
	mut b := available
	if b > c_error_bug_report_max_env_c_output_bytes {
		b = c_error_bug_report_max_env_c_output_bytes
	}
	if b > value_limit {
		b = value_limit
	}
	return b
}

pub fn export_external_v3_report_to_env(report ExternalCErrorBugReport) {
	// `report` is already content-only: its source was captured by the process that owns the staged
	// directory, and that process deletes the directory itself. This function bounds and forwards
	// that content, so nothing here reads a path or deletes a directory named by the environment.
	mut encoded_digests := ''
	mut input_digests_complete := false
	if encoded := encode_v3_report_input_digests(report.input_digests) {
		encoded_digests = encoded
		input_digests_complete = report.input_digests_complete
	}
	mut payload := {
		'PRESENT':                '1'
		'KIND':                   report.kind
		'CCOMPILER':              report.ccompiler
		'COUTPUT':                ''
		'TAG':                    report.tag
		'VFILE':                  report.v_file
		'VSOURCE':                ''
		'VSOURCE_TRUNCATED':      '0'
		'VSOURCE_FOCUS':          '0'
		'INPUT_DIGESTS':          encoded_digests
		'INPUT_DIGESTS_COMPLETE': if input_digests_complete { '1' } else { '0' }
	}
	budget := v3_report_env_budget(os.environ(), os.args)
	value_limit := v3_report_env_value_limit()
	if v3_report_env_payload_bytes(payload) > budget
		|| !v3_report_env_values_fit(payload, value_limit) {
		// The exact-input manifest and metadata are indivisible. Preserve compatibility
		// and the post-success notice, but omit the report rather than risk E2BIG.
		payload = v3_transport_limited_notice_payload()
		if v3_report_env_payload_bytes(payload) > budget {
			payload = map[string]string{}
		}
		set_v3_report_env_payload(payload)
		return
	}
	// The aggregate capacity left after the base payload, split between the diagnostic and source.
	content_budget := budget - v3_report_env_payload_bytes(payload)
	plan := plan_env_report_content(report, content_budget, value_limit)
	payload['COUTPUT'] = plan.c_output
	payload['VSOURCE'] = plan.v_source
	// Carry the truncation state explicitly through the handoff, computed from the byte budget
	// rather than from (user-controlled) source content, so the retry's notice is accurate even
	// when a complete file legitimately contains the truncation marker text.
	payload['VSOURCE_TRUNCATED'] = if plan.v_source_truncated { '1' } else { '0' }
	// Carry the failing line's position within the forwarded excerpt so a later re-bound (the wasm
	// re-export) centers on it instead of dropping it with a head+tail window.
	payload['VSOURCE_FOCUS'] = plan.v_source_focus.str()
	set_v3_report_env_payload(payload)
}

struct EnvContentPlan {
	c_output           string
	v_source           string
	v_source_truncated bool
	// v_source_focus is the failing line's 1-based position within the produced v_source, carried
	// through the handoff so a further re-bound (e.g. the wasm re-export) keeps the failing line.
	v_source_focus int
}

// plan_env_report_content splits `content_budget` (the aggregate environment capacity left after
// the base payload) between the C diagnostic and the V source, each capped by `value_limit`. The
// env payload counts value bytes, so the diagnostic and source consume exactly their lengths.
//
// When both are present it halves the capacity so a short missing-library/libatomic diagnostic
// stays intact and the receiver's filter stays accurate. Source is bounded around the failing line,
// or omitted when the budget cannot hold the required marker(s) plus actual source content. When
// source is omitted, its half is reclaimed for the diagnostic so the linker failure remains
// recognizable rather than being misclassified as a compiler bug. Any truncation recorded upstream
// (e.g. the first handoff before a wasm re-export) is preserved; further bounding only sets it.
fn plan_env_report_content(report ExternalCErrorBugReport, content_budget int, value_limit int) EnvContentPlan {
	full_c_output_budget := env_c_output_budget(content_budget, value_limit)
	mut c_output_budget := content_budget
	if report.v_source != '' {
		c_output_budget /= 2
	}
	c_output_budget = env_c_output_budget(c_output_budget, value_limit)
	mut c_output := truncated_report_text(report.c_output, c_output_budget)
	remaining := content_budget - c_output.len
	v_source_budget := if remaining < value_limit { remaining } else { value_limit }
	mut v_source_truncated := report.v_source_truncated
	mut source_omitted := false
	mut v_source_focus := 0
	v_source := if v_source_budget <= 0 {
		source_omitted = report.v_source != ''
		''
	} else if report.v_source.len > v_source_budget {
		bounded, focus := bounded_v_source_with_focus(report.v_source, v_source_budget,
			report.v_source_focus)
		if bounded == '' {
			source_omitted = true
			''
		} else {
			v_source_truncated = true
			v_source_focus = focus
			bounded
		}
	} else {
		// Fits whole: the complete file is forwarded, with the failing line unchanged.
		v_source_focus = report.v_source_focus
		report.v_source
	}
	if source_omitted && c_output_budget < full_c_output_budget {
		// The half reserved for the omitted source is now free; give it back to the diagnostic.
		c_output = truncated_report_text(report.c_output, full_c_output_budget)
	}
	return EnvContentPlan{
		c_output:           c_output
		v_source:           v_source
		v_source_truncated: v_source_truncated
		v_source_focus:     v_source_focus
	}
}

// bounded_v3_fallback_source extracts the bounded V source snippet to upload for a V3->V1
// fallback, reading ONLY files the caller already trusts — it must be invoked by the
// process that staged the report, never by one that merely inherited a report path from
// the environment. `c_file` is the staged generated C for a generated-C compilation
// error. Internal compiler errors deliberately return no source here: their input must
// be snapshotted before V3 starts and passed to bounded_v3_internal_fallback_source, so
// a later editor/build-watcher rewrite cannot be uploaded as if V3 had parsed it.
// `allowed_v_sources` maps every source file V3 actually parsed to the SHA-256 digest
// captured from those exact parser bytes. Generated-C mappings outside that set, or
// files whose current contents no longer match the captured digest, contribute no
// source. The returned source is the full mapped file plus the 1-based failing line to focus
// on; the byte bound is applied once, at the handoff, centered on that line, so it is never
// dropped by a second truncation. ('', '', 0) means no source is available, so the report
// stays metadata-only.
pub fn bounded_v3_fallback_source(kind string, c_output string, c_file string, allowed_v_sources map[string]string) (string, string, int) {
	if kind == external_v3_compiler_error_kind {
		return '', '', 0
	}
	if c_file == '' || !os.is_file(c_file) {
		return '', '', 0
	}
	return bounded_v_source_for_generated_c(c_output, c_file, allowed_v_sources)
}

// bounded_v3_internal_fallback_source returns the source CONTENT captured before V3 starts,
// with focus 0. It never opens `source_name`; the dispatcher separately verifies that the input
// still has the captured digest before forwarding this content to the stable retry. The snapshot
// is bounded here so a successful V3 compile does not retain an unbounded source copy until exit;
// export may tighten the bound further to fit the process environment.
pub fn bounded_v3_internal_fallback_source(source_name string, source string) (string, string, int) {
	if source_name == '' {
		return '', '', 0
	}
	return os.base(source_name), bounded_v_source(source, c_error_bug_report_max_v_source_bytes, 0), 0
}

// bounded_v_source_for_generated_c maps a generated-C compilation error back to the V
// source line it came from (via the #line directives in the trusted staged C) and returns
// the full mapped V file (bounded to a window around the failing line only when the file
// is larger than the byte budget), so the report is reproducible. The generated C was
// staged by this process's own V3 run, while its mapped V path is trusted only when it
// matches a parsed compiler input.
fn bounded_v_source_for_generated_c(c_output string, generated_c_file string, allowed_v_sources map[string]string) (string, string, int) {
	c_source := os.read_file(generated_c_file) or { return '', '', 0 }
	c_lines := c_source.split_into_lines()
	mut v_file := ''
	mut v_line := 0
	if c_loc := c_error_location_for_generated_c(c_output, generated_c_file) {
		if v_loc := v_source_location_for_c_line(c_lines, c_loc.line, generated_c_file) {
			v_file = v_loc.file
			v_line = v_loc.line
		}
	} else if source_loc := first_error_source_location(c_output) {
		v_file = source_loc.file
		v_line = source_loc.line
	}
	if v_file == '' || !os.is_file(v_file) {
		return '', '', 0
	}
	mut parsed_digest := ''
	for parsed_file, digest in allowed_v_sources {
		if same_path(parsed_file, v_file) {
			parsed_digest = digest
			break
		}
	}
	if parsed_digest == '' {
		return '', '', 0
	}
	mapped_source := os.read_file(v_file) or { return '', '', 0 }
	if sha256.hexhash(mapped_source) != parsed_digest {
		// An editor/build watcher rewrote this input after V3 parsed it. Keep the
		// report metadata-only rather than uploading bytes unrelated to the failure.
		return os.base(v_file), '', 0
	}
	// Return the full failing file plus the failing line so the report is reproducible. The
	// digest check above guarantees these are exactly the bytes V3 parsed, and only files V3
	// actually parsed (always V source, never a mapped header) reach this point. The byte bound
	// is applied once, at the handoff (export_external_v3_report_to_env), centered on this line,
	// so a file larger than the environment budget still keeps the exact failing code.
	if !is_v_source_file(v_file) {
		return os.base(v_file), '', 0
	}
	return os.base(v_file), mapped_source, v_line
}

// take_external_v3_report_from_env returns the content-only fallback report exported by
// export_external_v3_report_to_env, or none when none is present. It clears the variables
// unconditionally so neither a nested build nor a stale/poisoned environment can leak
// them into later work. The returned report carries source_inline = true and no c_file
// or cleanup_dir, so consume_external_c_error_bug_report reads no source path and deletes
// no directory. Tagged dependency paths are separately constrained and hashed after a
// successful build. Pass the result to compile_with_external_c_error_report so the notice
// and submission happen relative to the tool's own build outcome (only on success).
pub fn take_external_v3_report_from_env() ?ExternalCErrorBugReport {
	present := os.getenv('${v3_report_env_prefix}PRESENT')
	input_digests := decode_v3_report_input_digests(os.getenv('${v3_report_env_prefix}INPUT_DIGESTS')) or {
		map[string]string{}
	}
	report := ExternalCErrorBugReport{
		kind:                   os.getenv('${v3_report_env_prefix}KIND')
		ccompiler:              os.getenv('${v3_report_env_prefix}CCOMPILER')
		c_output:               os.getenv('${v3_report_env_prefix}COUTPUT')
		tag:                    os.getenv('${v3_report_env_prefix}TAG')
		v_file:                 os.getenv('${v3_report_env_prefix}VFILE')
		v_source:               os.getenv('${v3_report_env_prefix}VSOURCE')
		v_source_truncated:     os.getenv('${v3_report_env_prefix}VSOURCE_TRUNCATED') == '1'
		v_source_focus:         os.getenv('${v3_report_env_prefix}VSOURCE_FOCUS').int()
		source_inline:          true
		input_digests:          input_digests
		input_digests_complete: os.getenv('${v3_report_env_prefix}INPUT_DIGESTS_COMPLETE') == '1'
			&& input_digests.len > 0
		// c_file and cleanup_dir are intentionally left empty: the builder must not read
		// a file or delete a directory named by the environment.
	}
	for suffix in v3_report_env_suffixes {
		os.unsetenv('${v3_report_env_prefix}${suffix}')
	}
	if present != '1' {
		return none
	}
	return report
}

// v3_fallback_input_status compares the stable compiler's exact parser inputs with V3's
// staged manifest. An unavailable manifest means V3 failed before it could stage parser
// and resolved-native digests; that is distinct from a complete manifest whose input set
// or bytes changed. Untagged paths are compared only with canonical paths belonging to
// the stable parser's own AST files. Tagged native paths are hashed only after the stable
// build succeeded, so a changed header/source suppresses a false V3-only report.
fn (mut b Builder) v3_fallback_input_status(report ExternalCErrorBugReport) V3FallbackInputStatus {
	if report.kind in [external_v3_notice_only_kind, external_v3_transport_limited_kind] {
		return .unchanged
	}
	if !report.input_digests_complete || report.input_digests.len == 0 {
		return .unavailable
	}
	native_manifest_digest := report.input_digests[v3_fallback_native_manifest_key] or {
		return .unavailable
	}
	if native_manifest_digest != sha256.hexhash(v3_fallback_native_manifest_value) {
		return .changed
	}
	mut stable_digests := map[string]string{}
	mut report_source_count := 0
	trusted_native_roots := b.v3_fallback_trusted_native_roots()
	builtin_root :=
		os.real_path(os.join_path(b.pref.vroot, 'vlib', 'builtin')).trim_right(os.path_separator)
	for file in b.parsed_files {
		// Parser-generated helper ASTs reuse their originating source path but contain
		// synthesized text. They are derived inputs, not a second version of that file.
		if file.path == '' || file.source_digest == '' || file.is_parse_text {
			continue
		}
		path := os.real_path(file.path)
		// Mirror macos_v3_fallback_report_sources: internal builtin support selected
		// differently by V1 and V3 is not part of their shared source set.
		if v3_fallback_backend_specific_builtin_source(path, builtin_root) {
			continue
		}
		if previous := stable_digests[path] {
			if previous != file.source_digest {
				return .changed
			}
		}
		stable_digests[path] = file.source_digest
	}
	for path, v3_digest in report.input_digests {
		if path == v3_fallback_native_manifest_key {
			continue
		}
		if path.starts_with(v3_fallback_native_input_prefix) {
			native_path := path[v3_fallback_native_input_prefix.len..]
			if native_path == '' || !os.is_abs_path(native_path)
				|| os.real_path(native_path) != native_path || !os.is_file(native_path)
				|| !v3_fallback_native_path_is_trusted(native_path, trusted_native_roots) {
				return .changed
			}
			content := os.read_bytes(native_path) or { return .changed }
			if sha256.sum(content).hex() != v3_digest {
				return .changed
			}
			continue
		}
		report_source_count++
		if stable_digests[path] != v3_digest {
			return .changed
		}
	}
	if stable_digests.len != report_source_count {
		return .changed
	}
	return .unchanged
}

// matches_v3_fallback_inputs reports whether V3 supplied a complete manifest and the
// stable compiler parsed the same source bytes, or whether this is a notice-only fallback
// that never submits a V3 bug report.
fn (mut b Builder) matches_v3_fallback_inputs(report ExternalCErrorBugReport) bool {
	return b.v3_fallback_input_status(report) == .unchanged
}

// v3_fallback_trusted_native_roots derives an allowlist only from paths the stable
// compiler selected itself. This keeps the content-only environment handoff from
// becoming an arbitrary-file hashing oracle while still covering nested project/module
// headers discovered by V3.
fn (mut b Builder) v3_fallback_trusted_native_roots() []string {
	mut roots := map[string]bool{}
	for root in [b.compiled_dir, b.pref.vroot] {
		if root != '' {
			roots[os.real_path(root)] = true
		}
	}
	for root in b.pref.lookup_path {
		if root != '' {
			roots[os.real_path(root)] = true
		}
	}
	for root in b.module_search_paths {
		if root != '' {
			roots[os.real_path(root)] = true
		}
	}
	for cflag in b.get_os_cflags() {
		value := cflag.eval() or { continue }
		if cflag.name == '-I' && os.is_dir(value) {
			roots[os.real_path(value)] = true
		} else if os.is_file(value) {
			roots[os.real_path(os.dir(value))] = true
		}
	}
	// crun_dependency_files walks the stable AST's active local include/insert
	// directives and evaluated file-valued #flags. Only their containing trees become
	// eligible for digest verification; no report-provided path contributes a root.
	for path in b.crun_dependency_files() {
		roots[os.real_path(os.dir(path))] = true
	}
	mut result := roots.keys()
	result.sort()
	return result
}

fn v3_fallback_native_path_is_trusted(path string, roots []string) bool {
	normalized_path := path.replace('\\', '/').trim_right('/')
	for root in roots {
		normalized_root := root.replace('\\', '/').trim_right('/')
		if normalized_root != '' && (normalized_path == normalized_root
			|| normalized_path.starts_with(normalized_root + '/')) {
			return true
		}
	}
	return false
}

fn v3_fallback_backend_specific_builtin_source(path string, builtin_root string) bool {
	return (path == builtin_root || path.starts_with(builtin_root + os.path_separator))
		&& os.file_name(path) in ['ownership_interface_d_v3_backend.v', 'ownership_interface_notd_v3_backend.v', 'prealloc.c.v']
}

// submit_external_v3_compiler_error_bug_report reports metadata for a V3 internal
// compiler error after the stable compiler has confirmed the program is buildable.
// Source may only be supplied through the pre-V3 snapshot/content path, so this legacy
// path form deliberately does not reopen `v_file` after the failure.
pub fn submit_external_v3_compiler_error_bug_report(prefs &pref.Preferences, v3_stage string, v3_output string, v_file string, tag string) {
	mut b := new_builder(prefs)
	b.submit_v3_compiler_error_bug_report(v3_stage, v3_output, if v_file == '' {
		''
	} else {
		os.base(v_file)
	}, '', false, tag)
}

// submit_inline_v3_compiler_error_bug_report reports a V3 internal compiler error whose
// bounded source snippet is already supplied as `v_source` content and whose failing
// file is identified only by the base name `v_file_label`. It reads no filesystem path,
// so it is safe to drive from the environment handoff, where every value is caller-
// controlled (a forged handoff can therefore only submit attacker-supplied text).
fn submit_inline_v3_compiler_error_bug_report(prefs &pref.Preferences, v3_stage string, v3_output string, v_file_label string, v_source string, v_source_truncated bool, tag string) {
	mut b := new_builder(prefs)
	b.submit_v3_compiler_error_bug_report(v3_stage, v3_output, v_file_label, v_source,
		v_source_truncated, tag)
}

// build_inline_c_error_report constructs the generated-C fallback report to upload from
// already-bounded content, or none when the diagnostic is not eligible for automatic
// submission (an expected missing-library / missing-libatomic error). No generated C is
// available inline, so there is no C context or C/V line mapping — only the bounded V
// source snippet — but the `v-c-compiler-error` classification matches the in-process path.
fn build_inline_c_error_report(prefs &pref.Preferences, ccompiler string, c_output string, v_file string, v_source string, v_source_truncated bool, tag string) ?CErrorBugReport {
	if !c_error_should_send_bug_report(c_output) {
		return none
	}
	return CErrorBugReport{
		kind:               'v-c-compiler-error'
		v_version:          version.full_v_version(true)
		target_os:          prefs.os.str()
		target_backend:     prefs.backend.str()
		arch:               prefs.arch.str()
		ccompiler:          ccompiler
		build_options:      c_error_report_build_options(prefs, tag)
		c_error:            c_output
		v_file:             v_file
		v_source:           v_source
		v_source_truncated: v_source_truncated
	}
}

// submit_inline_c_error_bug_report reports a generated-C compilation error whose bounded V
// source snippet is already supplied as `v_source` content. Unlike the internal-V3-error
// submitter, it preserves the generated-C semantics of the trusted in-process path: the
// c_error_should_send_bug_report filter (so expected missing-library / missing-libatomic
// diagnostics are not uploaded) and the `v-c-compiler-error` classification. It reads no
// filesystem path, so it is safe to drive from the environment handoff.
fn submit_inline_c_error_bug_report(prefs &pref.Preferences, ccompiler string, c_output string, v_file_label string, v_source string, v_source_truncated bool, tag string) {
	is_v3_fallback := tag != ''
	raw_report := build_inline_c_error_report(prefs, ccompiler, c_output, v_file_label, v_source,
		v_source_truncated, tag) or {
		// Not eligible for automatic submission (e.g. a missing library), but V3 still
		// fell back to the stable compiler, so the user is told about the fallback.
		if is_v3_fallback {
			print_v3_fallback_notice('', false, false, false)
		}
		return
	}
	mut b := new_builder(prefs)
	if !should_submit_c_error_bug_report(b.pref.c_error_bug_report_url) {
		if is_v3_fallback {
			print_v3_fallback_notice('', false, false, false)
		}
		return
	}
	b.send_prepared_c_error_bug_report(raw_report, tag)
}

fn (mut v Builder) submit_v3_compiler_error_bug_report(v3_stage string, v3_output string, v_file string, v_source string, v_source_truncated bool, tag string) {
	if !should_submit_c_error_bug_report(v.pref.c_error_bug_report_url) {
		print_v3_fallback_notice('', false, false, false)
		return
	}
	build_options := c_error_report_build_options(v.pref, tag)
	// A V3 internal failure has no mapped failing line, so the whole captured input is uploaded
	// (bounded to a head+tail window at the handoff only when larger than the byte budget); the
	// dispatcher forwards it and its explicit truncation state through the environment. A
	// directory build (`v .`) stages an empty source and so contributes no source at all — but
	// the version/target/build-option metadata is still reported. See doc/docs.md.
	raw_report := CErrorBugReport{
		kind:               'v3-compiler-error'
		v_version:          version.full_v_version(true)
		target_os:          v.pref.os.str()
		target_backend:     v.pref.backend.str()
		arch:               v.pref.arch.str()
		ccompiler:          v3_stage
		build_options:      build_options
		c_error:            v3_output
		v_file:             v_file
		v_source:           v_source
		v_source_truncated: v_source_truncated
	}
	report := bounded_c_error_bug_report(raw_report, c_error_bug_report_max_body_bytes)
	report_url := c_error_bug_report_url(v.pref.c_error_bug_report_url)
	tool_output := send_c_error_bug_report(report, report_url) or {
		eprintln('V3 compiler bug report was not sent to ${report_url}: ${err}')
		print_v3_fallback_notice('', false, false, false)
		return
	}
	// Report diagnostics go to stderr, never stdout: with `v -o - source.v` the generated
	// C is already on stdout, so appending this banner there would corrupt the documented
	// `-o -` output for exactly the programs that needed the fallback.
	eprintln('================== V3 compiler bug report ==============')
	print_v3_fallback_notice(report_url, true, report.v_source != '',
		report_uploaded_complete_source(report))
	if tool_output != '' {
		eprintln(tool_output)
	}
	eprintln('V ${report.v_version}, ${report.target_os}/${report.arch}, build options: ${report.build_options}')
	eprintln('='.repeat('================== V3 compiler bug report =============='.len))
}

// v_source_is_whole_file reports whether the selected excerpt is the entire mapped
// file rather than a strict subset. A short mapped file makes the C-error window (or
// reproducer) cover everything, so this is used to enforce the doc/docs.md guarantee
// that automatic reports never upload a whole source file.
fn v_source_is_whole_file(selected string, full_source string) bool {
	return selected != '' && selected.split_into_lines() == full_source.split_into_lines()
}

// v_source_exposes_whole_file reports whether v_source contains the entire mapped
// file, including when the only omitted lines are blank and exact line-array
// equality therefore misses the substantive whole-file coverage.
fn v_source_exposes_whole_file(v_source string, mapped_source string, mapped_lines []string) bool {
	return v_source_is_whole_file(v_source, mapped_source)
		|| v_source_and_context_expose_whole_file(v_source, []CErrorReportLine{}, mapped_lines)
}

// v_context_covers_whole_file reports whether the numbered context window spans
// every line of the mapped file. numbered_context_lines returns a contiguous
// window, so covering the whole file means it holds as many lines as the file has.
// Used to keep the v_context payload within the doc/docs.md strict-subset guarantee.
fn v_context_covers_whole_file(context []CErrorReportLine, mapped_lines []string) bool {
	return mapped_lines.len > 0 && context.len == mapped_lines.len
}

// v_source_and_context_expose_whole_file reports whether the union of the uploaded
// v_source excerpt and the v_context window exposes every nonblank line of the
// mapped file. Each can be a strict subset on its own while together they
// reconstruct the complete source, so coverage must be checked across the combined
// payload. Blank lines and indentation are ignored (the reproducer normalizes them).
// Used to keep the combined upload within the doc/docs.md no-whole-file guarantee.
fn v_source_and_context_expose_whole_file(v_source string, context []CErrorReportLine, mapped_lines []string) bool {
	mut exposed := map[string]bool{}
	for line in v_source.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed != '' {
			exposed[trimmed] = true
		}
	}
	for c in context {
		trimmed := c.text.trim_space()
		if trimmed != '' {
			exposed[trimmed] = true
		}
	}
	mut nonblank := 0
	for line in mapped_lines {
		trimmed := line.trim_space()
		if trimmed == '' {
			continue
		}
		nonblank++
		if trimmed !in exposed {
			return false
		}
	}
	return nonblank > 0
}

// report_includes_v_source reports whether the uploaded report carries any of the
// user's V source. That is the bounded `v_source` excerpt OR the `v_context` lines
// around the failing line, since a short mapped file can have its whole-file
// `v_source` dropped while `v_context` remains a strict subset that is still
// uploaded. Used so the privacy notice describes what was actually sent; a
// metadata-only report carries neither.
fn report_includes_v_source(report CErrorBugReport) bool {
	return report.v_source != '' || report.v_context.len > 0
}

// report_uploaded_complete_source reports whether the report's uploaded source is the complete
// failing file, as opposed to a bounded excerpt or a context-only upload (v_source empty while
// v_context still carries a few mapped lines). Used to pick the disclosure notice so a
// context-only or truncated upload is never announced as the complete source.
fn report_uploaded_complete_source(report CErrorBugReport) bool {
	return report.v_source != '' && !report.v_source_truncated
}

// print_v3_fallback_notice explains, in plain language, that V3 could not build the
// program so the stable compiler was used instead. `submitted` selects whether a bug
// report was actually filed; `report_url` is where it went (empty when not filed).
// `complete_source` distinguishes a full-file upload from a bounded excerpt so users are
// told accurately how much source was disclosed.
fn print_v3_fallback_notice(report_url string, submitted bool, source_uploaded bool, complete_source bool) {
	eprintln('note: V3 could not build this program, so V used the stable compiler instead.')
	if !submitted {
		return
	}
	if source_uploaded {
		if complete_source {
			eprintln('A bug report with the complete failing V source was submitted to ${report_url} so this can be fixed.')
		} else {
			eprintln('A bug report with a bounded excerpt of the failing V source was submitted to ${report_url} so this can be fixed.')
		}
	} else {
		eprintln('A metadata-only bug report (no source) was submitted to ${report_url} so this can be fixed.')
	}
	eprintln('Set ${c_error_bug_report_disabled_env}=1 to opt out of these automatic reports.')
}

fn c_error_report_build_options(prefs &pref.Preferences, tag string) string {
	options := codegen_build_options(prefs)
	trimmed_tag := tag.trim_space()
	if trimmed_tag == '' {
		return options
	}
	if options == '' {
		return trimmed_tag
	}
	return '${trimmed_tag} ${options}'
}

fn (mut v Builder) new_c_error_bug_report(ccompiler string, c_output string, include_full_source bool) CErrorBugReport {
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
	mapped_lines := mapped_source.split_into_lines()
	// Tagged V3 fallbacks always upload the verified mapped file, matching the normal inline
	// handoff path. Direct stable-compiler reports retain their prior strict-subset selection:
	// prefer a self-contained reproducer, then fall back to a local source window.
	repro := if include_full_source {
		''
	} else {
		v.v_source_reproducer(v_file, v_line, c_error_bug_report_max_v_source_bytes)
	}
	mut v_source := if include_full_source && is_v_source_file(v_file) {
		bounded_v_source(mapped_source, c_error_bug_report_max_v_source_bytes, v_line)
	} else if repro != '' {
		repro
	} else {
		// Direct stable-compiler reports retain their established privacy boundary: select only
		// a local window, and below drop it if that window would expose the complete file.
		v_chunk := selected_v_source(v_file, mapped_lines, v_line)
		bounded_v_source(v_chunk.text, c_error_bug_report_max_v_source_bytes, v_chunk.focus)
	}
	if !include_full_source && v_source_exposes_whole_file(v_source, mapped_source, mapped_lines) {
		v_source = ''
	}
	// Direct stable-compiler payloads are always strict subsets. A tagged V3 source is truncated
	// only when the mapped file is larger than the byte budget.
	v_source_truncated := v_source != ''
		&& (!include_full_source || mapped_source.len > c_error_bug_report_max_v_source_bytes)
	// v_context is a separate uploaded payload; for a short mapped file its radius
	// window can also span every line, so apply the same strict-subset rule to it.
	mut v_context := numbered_context_lines(mapped_lines, v_line, c_error_context_radius)
	if v_context_covers_whole_file(v_context, mapped_lines) {
		v_context = []CErrorReportLine{}
	}
	if v_source_and_context_expose_whole_file(v_source, v_context, mapped_lines) {
		// Neither field covers the file on its own, but their union exposes every
		// nonblank source line and so reconstructs the whole file. Drop v_context (the
		// wider window) so the remaining v_source stays a strict subset (doc/docs.md).
		v_context = []CErrorReportLine{}
	}
	return CErrorBugReport{
		kind:               'v-c-compiler-error'
		v_version:          version.full_v_version(true)
		target_os:          v.pref.os.str()
		target_backend:     v.pref.backend.str()
		arch:               v.pref.arch.str()
		ccompiler:          ccompiler
		build_options:      codegen_build_options(v.pref)
		c_error:            c_output
		c_file:             c_file
		c_line:             c_line
		c_context:          numbered_context_lines(c_lines, c_line, c_error_context_radius)
		v_file:             v_file
		v_line:             v_line
		v_context:          v_context
		v_source:           v_source
		v_source_truncated: v_source_truncated
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
	result, _ := bounded_v_source_with_focus(source, max_bytes, focus_line)
	return result
}

// bounded_v_source_with_focus bounds `source` to `max_bytes` and also returns the 1-based line of
// `focus_line` within the produced excerpt (0 when there is no meaningful focus). A later handoff
// that must re-bound the excerpt (e.g. the wasm re-export with a smaller budget) uses that focus
// to keep centering on the failing line instead of dropping it with a head+tail window.
fn bounded_v_source_with_focus(source string, max_bytes int, focus_line int) (string, int) {
	if max_bytes <= 0 {
		return '', 0
	}
	if source.len <= max_bytes {
		return source, focus_line
	}
	marker := '\n${c_error_v_source_truncation_notice}\n'
	if focus_line <= 0 {
		// A head+tail excerpt needs the marker plus at least two retained bytes: the split can
		// assign one byte to each side, and line-boundary clamping may discard one side.
		if max_bytes < marker.len + 2 {
			return '', 0
		}
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
		return source[..head_end] + marker + source[tail_start..], 0
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
	// The failing line's 1-based position in the excerpt: a leading truncation-marker line (present
	// when the window does not reach the file start) shifts it down by one.
	leading_marker_lines := if lo > 0 { 1 } else { 0 }
	adjusted_focus := leading_marker_lines + (fi - lo) + 1
	mut parts := []string{}
	if lo > 0 {
		parts << c_error_v_source_truncation_notice
	}
	parts << lines[lo..hi + 1].join('\n')
	if hi + 1 < lines.len {
		parts << c_error_v_source_truncation_notice
	}
	result := parts.join('\n')
	if result.len <= max_bytes {
		return result, adjusted_focus
	}
	// A single focused line longer than the whole budget still overflows the window. Build the
	// clamp explicitly from its markers and line prefix, rather than slicing `result`: when there
	// is a leading marker, slicing can spend the entire budget on marker text and retain no source.
	leading_marker := if lo > 0 { '${c_error_v_source_truncation_notice}\n' } else { '' }
	marker_tail := '\n${c_error_v_source_truncation_notice}'
	clamp := max_bytes - leading_marker.len - marker_tail.len
	if clamp <= 0 {
		return '', 0
	}
	return leading_marker + lines[fi][..clamp] + marker_tail, adjusted_focus
}

// new_c_error_bug_report_with_vlines regenerates the program's C source with `#line`
// directives enabled (the same information `-g` would add), recompiles it with the
// previously used C compiler command, and builds a report from the recompiled output.
// Because the regenerated C carries `#line` annotations, the C error can be mapped back
// to the exact V source line that produced it. It returns none when the V mapping still
// cannot be produced, so the caller keeps the original, C-only report.
fn (mut v Builder) new_c_error_bug_report_with_vlines(ccompiler string, include_full_source bool) ?CErrorBugReport {
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
	report := v.new_c_error_bug_report(ccompiler, recompiled.output, include_full_source)
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

// print_c_error_bug_report_context prints the uploaded C/V context to stderr — never
// stdout — so it cannot corrupt a `v -o - source.v` generated-C stream.
fn print_c_error_bug_report_context(report CErrorBugReport) {
	eprintln('Generated C lines sent from ${report.c_file}:${report.c_line}:')
	print_report_lines(report.c_context, report.c_line)
	if report.v_file != '' {
		eprintln('Corresponding V lines sent from ${report.v_file}:${report.v_line}:')
		print_report_lines(report.v_context, report.v_line)
	} else {
		eprintln('Corresponding V lines sent: no V source mapping was available.')
	}
}

fn print_report_lines(lines []CErrorReportLine, center int) {
	if lines.len == 0 {
		eprintln('  (no source lines available)')
		return
	}
	for line in lines {
		prefix := if line.line == center { '>' } else { ' ' }
		eprintln('${prefix} ${line.line:6} | ${line.text}')
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
	mut needles := [cgen.generated_c_debug_path]
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
