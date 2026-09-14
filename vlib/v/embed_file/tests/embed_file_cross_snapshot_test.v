// Portable `-os cross` C must carry the bytes of every `$embed_file`, in a form
// the C compiler it is later handed will actually accept.
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

// c_max_concatenated_string_literal is where MSVC stops accepting a string
// literal, counted after adjacent literals have been joined. Splitting a payload
// into short literals does not get past this, because the pieces still join into
// one; a payload this long has to be written as an array object instead.
const c_max_concatenated_string_literal = 65535

// tricky_payload holds the bytes whose escaping a C compiler would otherwise
// misread: a quote ends the literal, a backslash starts an escape, `??!` is a
// trigraph, and neither a NUL nor a high byte survives verbatim.
const tricky_payload = 'quote:" backslash:\\ trigraph:??! nul:\x00 high:\xfe\xff\nsecond line\n'

// A payload short enough to still be spelled as a string literal, but long enough
// that the literal has to be continued across several of them.
const medium_payload_len = 16 * 1024

// A payload past what any string literal can hold, joined or not.
const large_payload_len = 96 * 1024

// payload_of returns `size` deterministic bytes, opening with the tricky ones so
// that every snapshot below also covers escaping.
fn payload_of(size int) string {
	mut raw := []u8{cap: size + tricky_payload.len}
	raw << tricky_payload.bytes()
	for i in 0 .. size {
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

// longest_concatenated_string_literal returns the length of the longest string a
// C compiler would build out of `text`, joining every run of adjacent literals as
// it goes. Comments and character literals are skipped so their quotes do not
// start a run of their own.
fn longest_concatenated_string_literal(text string) int {
	mut longest := 0
	mut run := 0
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `/` && i + 1 < text.len && text[i + 1] == `/` {
			for i < text.len && text[i] != `\n` {
				i++
			}
			continue
		}
		if c == `/` && i + 1 < text.len && text[i + 1] == `*` {
			i += 2
			for i + 1 < text.len && !(text[i] == `*` && text[i + 1] == `/`) {
				i++
			}
			i += 2
			continue
		}
		if c == `'` {
			i++
			for i < text.len && text[i] != `'` {
				i += if text[i] == `\\` { 2 } else { 1 }
			}
			i++
			continue
		}
		if c != `"` {
			i++
			continue
		}
		i++
		for i < text.len && text[i] != `"` {
			i += escape_width(text, i)
			run++
		}
		i++
		if run > longest {
			longest = run
		}
		// Only whitespace may separate two literals that the compiler joins.
		mut next := i
		for next < text.len && text[next] in [` `, `\t`, `\r`, `\n`] {
			next++
		}
		if next >= text.len || text[next] != `"` {
			run = 0
		}
	}
	return longest
}

// escape_width returns how many source characters the byte at `i` spans, so that
// an escape counts as the single byte it stands for.
fn escape_width(text string, i int) int {
	if text[i] != `\\` || i + 1 >= text.len {
		return 1
	}
	if text[i + 1] < `0` || text[i + 1] > `7` {
		return 2
	}
	mut width := 2
	for width < 4 && i + width < text.len && text[i + width] >= `0` && text[i + width] <= `7` {
		width++
	}
	return width
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

// cross_probe_text returns the portable C of a program embedding `payload`.
fn cross_probe_text(payload string) ?string {
	dir, out := cross_compile_probe(payload) or { return none }
	text := os.read_file(out) or { '' }
	os.rmdir_all(dir) or {}
	if text == '' {
		return none
	}
	return text
}

// test_cross_output_carries_the_embedded_bytes is the regression this file exists
// for. Recording only the path made the generated C the same size no matter how
// big the embedded file was.
fn test_cross_output_carries_the_embedded_bytes() {
	small := cross_probe_text(tricky_payload) or { return }
	payload := payload_of(large_payload_len)
	large := cross_probe_text(payload) or { return }
	grew := large.len - small.len
	expected := payload.len - tricky_payload.len
	assert grew >= expected, 'the portable snapshot grew by ${grew} bytes for ${expected} more embedded bytes, so it cannot be carrying them'
}

// test_cross_output_keeps_source_lines_within_limits covers the length of one
// source line, which is as limited as the length of one literal.
fn test_cross_output_keeps_source_lines_within_limits() {
	small := cross_probe_text(tricky_payload) or { return }
	// Everything except the payload is identical between the programs, so any
	// growth in the longest line is the payload's doing.
	mut allowed := max_line_len(small)
	if allowed < c_min_logical_source_line {
		allowed = c_min_logical_source_line
	}
	for size in [medium_payload_len, large_payload_len] {
		payload := payload_of(size)
		text := cross_probe_text(payload) or { return }
		longest := max_line_len(text)
		assert longest <= allowed, '${payload.len} embedded bytes stretched the longest source line to ${longest} characters, past the ${allowed} a C implementation has to accept'
	}
}

// test_cross_output_never_builds_an_over_long_string_literal covers the limit that
// splitting cannot answer: adjacent literals are joined back into one, so a
// payload past the joined maximum has to be written as an array object.
fn test_cross_output_never_builds_an_over_long_string_literal() {
	payload := payload_of(large_payload_len)
	// Otherwise the case this test is about would not arise.
	assert payload.len > c_max_concatenated_string_literal
	text := cross_probe_text(payload) or { return }
	longest := longest_concatenated_string_literal(text)
	assert longest <= c_max_concatenated_string_literal, '${payload.len} embedded bytes were spelled as string literals joining into ${longest} bytes, past the ${c_max_concatenated_string_literal} MSVC accepts'
}

fn test_cross_snapshot_runs_without_the_embedded_file() {
	$if windows {
		// The snapshot is built here with a plain `cc` invocation, which is not how
		// the Windows toolchains are driven.
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	// Both sizes: one written as string literals, one as an array object.
	for size in [medium_payload_len, large_payload_len] {
		payload := payload_of(size)
		dir, out := cross_compile_probe(payload) or { return }
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
		os.rmdir_all(dir) or {}
	}
}
