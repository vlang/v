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

// c_min_concatenated_string_literal is how much a conforming C implementation
// has to accept in a string literal once adjacent literals have been joined
// (C99 5.2.4.1). Splitting a payload into short literals does not get past this,
// because the pieces still join into one; a payload this long has to be written
// as an array object instead.
const c_min_concatenated_string_literal = 4095

// c_max_object_size is how many bytes a hosted C implementation has to accept in
// a single object (C99 5.2.4.1). A payload past this cannot be embedded as one
// array however it is spelled, so it has to be split and joined at runtime.
const c_max_object_size = 65535

// tricky_payload holds the bytes whose escaping a C compiler would otherwise
// misread: a quote ends the literal, a backslash starts an escape, `??!` is a
// trigraph, and neither a NUL nor a high byte survives verbatim.
const tricky_payload = 'quote:" backslash:\\ trigraph:??! nul:\x00 high:\xfe\xff\nsecond line\n'

// A payload short enough to still be spelled as a string literal, but long enough
// that the literal has to be continued across several of them.
const medium_payload_len = 3 * 1024

// Just past what C guarantees a literal can hold, and nowhere near what any one
// compiler allows. This is the range a cutoff taken from a particular compiler's
// figure gets wrong, so it has to be covered on its own.
const over_guarantee_payload_len = 8 * 1024

// Past what MSVC accepts either, joined or not.
const large_payload_len = 96 * 1024

// Every payload size above, smallest first.
const probed_payload_lens = [medium_payload_len, over_guarantee_payload_len, large_payload_len]

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

// largest_declared_array returns the biggest length that `text` declares an array
// with, counting only those that have an initializer. For the byte arrays an
// embedded payload is written as, that length is the size of the object. Arrays
// of wider elements are undercounted, which only makes this lenient.
fn largest_declared_array(text string) int {
	mut largest := 0
	mut i := 0
	for i < text.len {
		if text[i] != `[` {
			i++
			continue
		}
		mut end := i + 1
		for end < text.len && text[end].is_digit() {
			end++
		}
		if end == i + 1 || end >= text.len || text[end] != `]` {
			i++
			continue
		}
		mut after := end + 1
		for after < text.len && text[after] == ` ` {
			after++
		}
		if after >= text.len || text[after] != `=` {
			i = end + 1
			continue
		}
		length := text[i + 1..end].int()
		if length > largest {
			largest = length
		}
		i = end + 1
	}
	return largest
}

// cross_compile_probe generates portable C for a program embedding `payload`, and
// returns its temporary directory and the generated C file.
fn cross_compile_probe(payload string) ?(string, string) {
	return cross_compile_probe_with(payload, '')
}

// cross_compile_probe_with does the same with extra flags on the V command line.
fn cross_compile_probe_with(payload string, extra string) ?(string, string) {
	return cross_compile_probe_full(payload, extra, '')
}

// cross_compile_probe_full also puts `prelude` in front of the embedded constant,
// which moves everything after it in the AST.
fn cross_compile_probe_full(payload string, extra string, prelude string) ?(string, string) {
	vexe := os.getenv('VEXE')
	if vexe == '' {
		return none
	}
	tag := if extra == '' { '' } else { '_${extra.replace('-', '')}' }
	dir := os.join_path(os.vtmp_dir(), 'embed_file_cross_${os.getpid()}_${payload.len}${tag}_${prelude.len}')
	os.mkdir_all(dir) or { return none }
	os.write_file(os.join_path(dir, 'payload.bin'), payload) or { return none }
	src := os.join_path(dir, 'prog.v')
	// The embedded file is read from several threads, all of them for the first
	// time. A payload large enough to be split used to be materialized lazily on
	// that first read, which made this a race over one shared constant.
	os.write_file(src, "module main

${prelude}
const embedded = \$embed_file('payload.bin')

// The same file embedded a second time must not get a second copy of the bytes.
const embedded_again = \$embed_file('payload.bin')

fn checksum() string {
	payload := embedded.to_bytes()
	mut sum := u32(0)
	for b in payload {
		sum = sum * 31 + u32(b)
	}
	return '\${payload.len} \${sum}'
}

fn reader(id int, mut seen []string) {
	seen[id] = checksum()
}

// An `\$embed_file` inside a generic body is cloned once per specialization. The
// copy has to stay recognizable as an embedded payload, or the backend falls back
// to spelling it out as an ordinary literal, which is what the limits above are
// there to prevent.
fn evaluated_in_a_generic[T](x T) int {
	local := \$embed_file('payload.bin')
	return local.len
}

// An `\$embed_file` written inside a function, rather than kept in a constant,
// is evaluated again on every call. Its bytes are embedded once either way, so
// every evaluation has to arrive at the same pointer; building a fresh buffer
// per call would allocate a copy of the payload each time round a loop.
fn evaluated_in_a_call() voidptr {
	local := \$embed_file('payload.bin')
	return local.data()
}

fn main() {
	mut seen := []string{len: 4, init: ''}
	mut readers := []thread{}
	for i in 0 .. 4 {
		readers << spawn reader(i, mut seen)
	}
	readers.wait()
	for got in seen {
		if got != seen[0] {
			println('threads disagreed: \${got} != \${seen[0]}')
			exit(1)
		}
	}
	first := evaluated_in_a_call()
	for _ in 0 .. 3 {
		if evaluated_in_a_call() != first {
			println('the payload was materialized again on a later evaluation')
			exit(1)
		}
	}
	if voidptr(embedded.data()) != voidptr(embedded_again.data()) {
		println('the same file was embedded twice over')
		exit(1)
	}
	if evaluated_in_a_generic(1) != embedded.len || evaluated_in_a_generic('s') != embedded.len
		|| evaluated_in_a_generic(1.5) != embedded.len {
		println('a specialization did not see the whole payload')
		exit(1)
	}
	println(seen[0])
}
") or { return none }
	out := os.join_path(dir, 'prog.c')
	res := os.execute('${os.quoted_path(vexe)} ${extra} -os cross -o ${os.quoted_path(out)} ${os.quoted_path(src)}')
	if res.exit_code != 0 {
		assert false, res.output
	}
	return dir, out
}

// probe_splits_over_large_payloads reports whether the compiler being probed is
// one that knows how to split a payload. Unlike everything else asserted here,
// that is not a property every backend ever had: the V1 compatibility compiler a
// test run can fall back to emits one object of any size, and compiles its own
// standard library, where the chunk table does not exist. Asking that library is
// therefore the question, rather than which binary is in VEXE.
fn probe_splits_over_large_payloads() bool {
	vexe := os.getenv('VEXE')
	if vexe == '' {
		return false
	}
	runtime := os.join_path(os.dir(vexe), 'vlib', 'v', 'embed_file', 'embed_file.v')
	text := os.read_file(runtime) or { return false }
	return text.contains('EmbedFileChunk')
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
	payload := payload_of(over_guarantee_payload_len)
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
	for size in probed_payload_lens {
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
	small := cross_probe_text(tricky_payload) or { return }
	// Ordinary V string constants are interned into this same output and some of
	// them are already longer than C guarantees, so the payload is held to not
	// making the longest literal any longer than it already was.
	mut allowed := longest_concatenated_string_literal(small)
	if allowed < c_min_concatenated_string_literal {
		allowed = c_min_concatenated_string_literal
	}
	// One payload below the limit, which may still be spelled as literals, and two
	// above it, which are what this test is about.
	assert payload_of(medium_payload_len).len <= c_min_concatenated_string_literal
	assert payload_of(over_guarantee_payload_len).len > c_min_concatenated_string_literal
	for size in probed_payload_lens {
		payload := payload_of(size)
		text := cross_probe_text(payload) or { return }
		longest := longest_concatenated_string_literal(text)
		assert longest <= allowed, '${payload.len} embedded bytes were spelled as string literals joining into ${longest} bytes, past the ${allowed} a C implementation has to accept'
	}
}

// test_cross_output_never_declares_an_over_large_object covers the limit that the
// array representation runs into in turn: C only has to accept 65535 bytes in one
// object, so a payload past that is split and joined when it is first asked for.
fn test_cross_output_never_declares_an_over_large_object() {
	if !probe_splits_over_large_payloads() {
		return
	}
	small := cross_probe_text(tricky_payload) or { return }
	mut allowed := largest_declared_array(small)
	if allowed < c_max_object_size {
		allowed = c_max_object_size
	}
	// Otherwise the case this test is about would not arise.
	assert payload_of(large_payload_len).len > c_max_object_size
	for size in probed_payload_lens {
		payload := payload_of(size)
		text := cross_probe_text(payload) or { return }
		largest := largest_declared_array(text)
		assert largest <= allowed, '${payload.len} embedded bytes were declared as an object of ${largest} bytes, past the ${allowed} a C implementation has to accept'
	}
}

// test_cross_snapshot_runs_under_prealloc covers where the joined buffer is
// reserved. Under `-prealloc` it must not come out of the preallocator, because
// `_vinit` fills it before `prealloc_vinit()` has installed the first arena, and
// that installation would then orphan whatever the join had already taken.
// externally_linked_u8_buffers returns the names the generated C imports as
// `extern u8*`. A split payload is joined into one of those, and a cached module
// keeps that name in its object file, so the name has to come out the same in
// the next program that links against it.
fn externally_linked_u8_buffers(text string) []string {
	mut names := []string{}
	mut rest := text
	for {
		at := rest.index('extern u8* ') or { break }
		rest = rest[at + 'extern u8* '.len..]
		name := rest.all_before(';')
		if name.len > 0 && !name.contains(' ') && !name.contains('\n') {
			names << name
		}
	}
	names.sort()
	return names
}

// test_cross_output_names_joined_buffers_after_their_contents covers what a
// cached module needs from those names. Naming them after a position in the AST
// would give the same payload a different name in the next program, which either
// fails to link or, worse, finds something else.
fn test_cross_output_names_joined_buffers_after_their_contents() {
	if !probe_splits_over_large_payloads() {
		return
	}
	payload := payload_of(large_payload_len)
	_, plain := cross_compile_probe_full(payload, '', '') or { return }
	plain_text := os.read_file(plain) or { '' }
	os.rmdir_all(os.dir(plain)) or {}
	// The same payload, with everything around it moved.
	shifted_prelude := 'fn shifted_a() int { return 1 }
fn shifted_b(x int) int { return x + 2 }
struct Shifted { field int }
'
	_, shifted := cross_compile_probe_full(payload, '', shifted_prelude) or { return }
	shifted_text := os.read_file(shifted) or { '' }
	os.rmdir_all(os.dir(shifted)) or {}
	assert plain_text != '' && shifted_text != ''
	names := externally_linked_u8_buffers(plain_text)
	// Otherwise the case this test is about would not arise.
	assert names.len > 0
	assert names == externally_linked_u8_buffers(shifted_text), 'the joined buffer is named after where the payload sits rather than after what it holds'
}

fn test_cross_snapshot_runs_under_prealloc() {
	$if windows {
		return
	}
	if !probe_splits_over_large_payloads() {
		// Nothing to order against: a compiler that embeds the payload as one
		// object never reaches for the allocator during _vinit.
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	payload := payload_of(large_payload_len)
	dir, out := cross_compile_probe_with(payload, '-prealloc') or { return }
	defer {
		os.rmdir_all(dir) or {}
	}
	exe := os.join_path(dir, 'prog')
	build := os.execute('${os.quoted_path(cc)} -std=gnu11 -w -o ${os.quoted_path(exe)} ${os.quoted_path(out)} -lm -lpthread')
	assert build.exit_code == 0, build.output
	os.rm(os.join_path(dir, 'payload.bin')) or {
		assert false, err.msg()
		return
	}
	res := os.execute(os.quoted_path(exe))
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == checksum(payload)
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
