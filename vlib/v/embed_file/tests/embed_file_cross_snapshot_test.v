// Portable `-os cross` C must carry the bytes of every `$embed_file`, and must not
// need a longer source line to do it.
//
// An ordinary debug build only records the file's absolute path and re-reads it the
// first time `.data()` is called, which keeps rebuilds cheap. A portable snapshot is
// compiled and run on a different machine, where that path names nothing, so the same
// trick turns into a startup panic:
//
//	V panic: EmbedFileData error: files "manual_stdlib_c_headers.h" and
//	"/home/runner/work/v/v/vlib/v/gen/c/manual_stdlib_c_headers.h" do not exist
//
// That is exactly how `vc/v.c` broke `make`: the compiler embeds its own C header
// prelude, and the published snapshot pointed at the CI runner's checkout.
//
// The assertions below are about what the snapshot *is*, not how it is spelled, so
// they hold whichever backend generated it.
import os

// c_min_logical_source_line is the shortest logical source line a conforming C
// implementation may refuse to go past (C99 5.2.4.1). MSVC stops at 16384, so
// staying under this covers it too.
const c_min_logical_source_line = 4095

// tricky_payload holds the bytes whose escaping a C compiler would otherwise
// misread: a quote ends the literal, a backslash starts an escape, `??!` is a
// trigraph, and neither a NUL nor a high byte survives verbatim.
const tricky_payload = 'quote:" backslash:\\ trigraph:??! nul:\x00 high:\xfe\xff\nsecond line\n'

// large_payload is long enough that writing it out in one piece would pass every
// implementation limit the generated C has to respect: the maximum length of one
// string literal and the maximum length of one logical source line. It opens with
// the tricky bytes, so one compiled snapshot covers both concerns.
fn large_payload() string {
	mut raw := []u8{cap: 64 * 1024}
	raw << tricky_payload.bytes()
	for i in 0 .. 48 * 1024 {
		// Interleave bytes that need escaping with bytes that do not, so the splits
		// have to land between escapes rather than at a fixed stride.
		raw << if i % 5 == 0 { u8(i % 256) } else { u8(`a` + i % 26) }
	}
	return raw.bytestr()
}

fn checksum(payload string) string {
	mut sum := u32(0)
	for b in payload.bytes() {
		sum = sum * 31 + u32(b)
	}
	return '${payload.len} ${sum}'
}

fn max_line_len(text string) int {
	mut longest := 0
	for line in text.split_into_lines() {
		if line.len > longest {
			longest = line.len
		}
	}
	return longest
}

// cross_compile_probe generates portable C for a program embedding `payload`, and
// returns its temporary directory and the generated C file.
fn cross_compile_probe(payload string) ?(string, string) {
	vexe := os.getenv('VEXE')
	if vexe == '' {
		return none
	}
	dir := os.join_path(os.vtmp_dir(), 'embed_file_cross_${os.getpid()}_${payload.len}')
	os.mkdir_all(dir) or { return none }
	os.write_file(os.join_path(dir, 'payload.bin'), payload) or { return none }
	src := os.join_path(dir, 'prog.v')
	os.write_file(src, "module main

const payload = \$embed_file('payload.bin').to_bytes()

fn main() {
	mut sum := u32(0)
	for b in payload {
		sum = sum * 31 + u32(b)
	}
	println('\${payload.len} \${sum}')
}
") or { return none }
	out := os.join_path(dir, 'prog.c')
	res := os.execute('${os.quoted_path(vexe)} -os cross -o ${os.quoted_path(out)} ${os.quoted_path(src)}')
	if res.exit_code != 0 {
		assert false, res.output
	}
	return dir, out
}

// cross_probe_texts returns the portable C of two programs that differ in nothing
// but the size of the file they embed, plus the larger of the two payloads.
fn cross_probe_texts() ?(string, string, string) {
	small_dir, small_out := cross_compile_probe(tricky_payload) or { return none }
	small := os.read_file(small_out) or { '' }
	os.rmdir_all(small_dir) or {}
	payload := large_payload()
	large_dir, large_out := cross_compile_probe(payload) or { return none }
	large := os.read_file(large_out) or { '' }
	os.rmdir_all(large_dir) or {}
	if small == '' || large == '' {
		return none
	}
	return small, large, payload
}

// test_cross_output_carries_the_embedded_bytes is the regression this file exists
// for. Recording only the path made the generated C the same size no matter how
// big the embedded file was.
fn test_cross_output_carries_the_embedded_bytes() {
	small, large, payload := cross_probe_texts() or { return }
	grew := large.len - small.len
	expected := payload.len - tricky_payload.len
	assert grew >= expected, 'the portable snapshot grew by ${grew} bytes for ${expected} more embedded bytes, so it cannot be carrying them'
}

// test_cross_output_keeps_source_lines_within_limits covers the second half of the
// problem: the payload has to be written across several source lines, because the
// length of one line is as limited as the length of one string literal.
fn test_cross_output_keeps_source_lines_within_limits() {
	small, large, payload := cross_probe_texts() or { return }
	// Everything except the payload is identical between the two programs, so any
	// growth in the longest line is the payload's doing.
	mut allowed := max_line_len(small)
	if allowed < c_min_logical_source_line {
		allowed = c_min_logical_source_line
	}
	longest := max_line_len(large)
	assert longest <= allowed, '${payload.len} embedded bytes stretched the longest source line to ${longest} characters, past the ${allowed} a C implementation has to accept'
}

fn test_cross_snapshot_runs_without_the_embedded_file() {
	$if windows {
		// The snapshot is built here with a plain `cc` invocation, which is not how
		// the Windows toolchains are driven.
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	payload := large_payload()
	dir, out := cross_compile_probe(payload) or { return }
	defer {
		os.rmdir_all(dir) or {}
	}
	exe := os.join_path(dir, 'prog')
	build := os.execute('${os.quoted_path(cc)} -std=gnu11 -w -o ${os.quoted_path(exe)} ${os.quoted_path(out)} -lm -lpthread')
	assert build.exit_code == 0, build.output
	// Stand in for "compiled on another machine": the file the snapshot was
	// generated from is gone by the time the program runs.
	os.rm(os.join_path(dir, 'payload.bin')) or {
		assert false, err.msg()
		return
	}
	res := os.execute(os.quoted_path(exe))
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == checksum(payload)
}
