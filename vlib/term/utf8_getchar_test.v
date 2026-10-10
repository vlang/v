module term

import os

// utf8_getchars_from feeds `input` to utf8_getchar through a pipe standing in
// for stdin, and returns every rune it decodes before the input runs out.
fn utf8_getchars_from(input string) ![]rune {
	original := os.fd_dup(0)
	if original == -1 {
		return error('could not duplicate stdin')
	}
	defer {
		os.fd_dup2(original, 0)
		os.fd_close(original)
	}
	mut pipe := os.pipe()!
	pipe.write_string(input)!
	os.fd_close(pipe.write_fd)
	pipe.write_fd = -1
	if os.fd_dup2(pipe.read_fd, 0) == -1 {
		return error('could not redirect stdin')
	}
	os.fd_close(pipe.read_fd)
	mut runes := []rune{}
	for runes.len < 8 {
		r := utf8_getchar() or { break }
		runes << r
	}
	return runes
}

fn test_utf8_getchar_decodes_a_plain_byte() {
	assert utf8_getchars_from('a')! == [rune(97)]
	assert utf8_getchars_from('\x00a')! == [rune(0), rune(97)]
}

fn test_utf8_getchar_decodes_a_two_byte_sequence() {
	assert utf8_getchars_from('\xC3\xA4')! == [rune(0xE4)] // ä
}

fn test_utf8_getchar_decodes_a_three_byte_sequence() {
	assert utf8_getchars_from('\xE4\xB8\x96')! == [rune(0x4E16)] // 世
}

fn test_utf8_getchar_decodes_a_four_byte_sequence() {
	assert utf8_getchars_from('\xF0\x9D\x84\x9E')! == [rune(0x1D11E)] // 𝄞
}

fn test_utf8_getchar_decodes_a_mixed_stream() {
	assert utf8_getchars_from('a\xC3\xA4\xE4\xB8\x96\xF0\x9D\x84\x9Eb')! == [rune(97), rune(0xE4),
		rune(0x4E16), rune(0x1D11E), rune(98)]
}

fn test_utf8_getchar_returns_none_at_the_end_of_the_input() {
	assert utf8_getchars_from('')!.len == 0
}

// NOTE: a truncated sequence and a NUL byte both decode to 0, so a caller
// cannot tell an incomplete rune from U+0000.
fn test_utf8_getchar_returns_zero_for_a_truncated_sequence() {
	assert utf8_getchars_from('\xC3')! == [rune(0)]
	assert utf8_getchars_from('\xE4\xB8')! == [rune(0)]
	assert utf8_getchars_from('\xF0\x9D\x84')! == [rune(0)]
}

fn test_utf8_getchar_reports_an_invalid_continuation_byte() {
	// NOTE: `rune` is unsigned, so the -1 sentinel is held as 0xFFFFFFFF
	assert utf8_getchars_from('\xC3a')! == [rune(0xFFFFFFFF)]
	assert utf8_getchars_from('\x80')! == [rune(0xFFFFFFFF)]
}
