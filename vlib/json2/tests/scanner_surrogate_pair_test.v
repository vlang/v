import io
import json2

// The UTF-8 bytes of U+FFFD, the replacement character.
const replacement = 'efbfbd'

struct StringCase {
	json string // the JSON text of one string
	hex  string // the bytes that its token has to hold
}

const string_cases = [
	// A high surrogate escape followed by a low surrogate escape is one code point.
	StringCase{
		json: r'"\ud83d\ude00"'
		hex:  'f09f9880'
	},
	StringCase{
		json: r'"\uD83D\uDE00"'
		hex:  'f09f9880'
	},
	StringCase{
		json: r'"\ud800\udc00"' // the first pair, U+10000
		hex:  'f0908080'
	},
	StringCase{
		json: r'"\udbff\udfff"' // the last pair, U+10FFFF
		hex:  'f48fbfbf'
	},
	StringCase{
		json: r'"abc\ud83d\ude00"' // a pair at the end of the string
		hex:  '616263f09f9880'
	},
	StringCase{
		json: r'"\ud83d\ude00abc"'
		hex:  'f09f9880616263'
	},
	StringCase{
		json: r'"\ud83d\ude00\ud83d\ude01"' // two pairs
		hex:  'f09f9880f09f9881'
	},
	// A half without its partner is not a code point.
	StringCase{
		json: r'"\ud83d"' // a lone high surrogate
		hex:  replacement
	},
	StringCase{
		json: r'"\ude00"' // a lone low surrogate
		hex:  replacement
	},
	StringCase{
		json: r'"a\ud83db"'
		hex:  '61' + replacement + '62'
	},
	StringCase{
		json: r'"\ude00\ud83d"' // a low surrogate followed by a high one
		hex:  replacement + replacement
	},
	StringCase{
		json: r'"\ud83d\ud83d\ude00"' // a lone high surrogate, then a pair
		hex:  replacement + 'f09f9880'
	},
	StringCase{
		json: r'"\ud83d\ude00\ude00"' // a pair, then a lone low surrogate
		hex:  'f09f9880' + replacement
	},
	StringCase{
		json: r'"\ud83dx\ude00"' // the halves are not next to each other
		hex:  replacement + '78' + replacement
	},
	StringCase{
		json: r'"\ud83d\n\ude00"' // another escape is between the halves
		hex:  replacement + '0a' + replacement
	},
	StringCase{
		json: r'"\ud83d\u0041"'
		hex:  replacement + '41'
	},
	// Escapes of the Basic Multilingual Plane, and unescaped text, are unchanged.
	StringCase{
		json: r'"\u0041\u00e9\u20ac\ud7ff\ue000\uffff"'
		hex:  '41c3a9e282aced9fbfee8080efbfbf'
	},
	StringCase{
		json: '"plain é € 😀"'
		hex:  'plain é € 😀'.bytes().hex()
	},
]

// SlowReader hands out at most `chunk_size` bytes per read, so that an escape
// is split between reads.
struct SlowReader {
	data       []u8
	chunk_size int
mut:
	pos int
}

fn (mut r SlowReader) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return io.Eof{}
	}
	mut n := r.chunk_size
	if n > r.data.len - r.pos {
		n = r.data.len - r.pos
	}
	if n > buf.len {
		n = buf.len
	}
	read := copy(mut buf[..n], r.data[r.pos..r.pos + n])
	r.pos += read
	return read
}

fn test_scanner_joins_surrogate_pair_escapes() {
	for c in string_cases {
		mut scanner := json2.new_scanner(c.json)
		token := scanner.next()!
		assert token.kind == .str, c.json
		assert token.lit.hex() == c.hex, c.json
		assert scanner.next()!.is_eof(), c.json

		mut from_bytes := json2.new_scanner_from_bytes(c.json.bytes())
		assert from_bytes.next()!.lit.hex() == c.hex, c.json
	}
}

fn test_reader_scanner_joins_surrogate_pair_escapes() {
	for chunk_size in [1, 5, 4096] {
		for c in string_cases {
			mut reader := SlowReader{
				data:       c.json.bytes()
				chunk_size: chunk_size
			}
			mut scanner := json2.new_reader_scanner(reader: reader, buffer_size: chunk_size)
			token := scanner.next()!
			assert token.kind == .str, '${c.json} in chunks of ${chunk_size}'
			assert token.lit.hex() == c.hex, '${c.json} in chunks of ${chunk_size}'
			assert scanner.next()!.is_eof(), '${c.json} in chunks of ${chunk_size}'
			scanner.free()
		}
	}
}

const array_json = r'["\ud83d\ude00", 7, "\ud83d", true]'
const array_tokens = ['lsbr:', 'str:f09f9880', 'comma:', 'int:37', 'comma:', 'str:efbfbd', 'comma:',
	'bool:74727565', 'rsbr:', 'eof:']

fn test_scanner_goes_on_after_a_surrogate_pair() {
	mut scanner := json2.new_scanner(array_json)
	mut got := []string{}
	for {
		token := scanner.next()!
		got << '${token.kind}:${token.lit.hex()}'
		if token.is_eof() {
			break
		}
	}
	assert got == array_tokens
}

fn test_reader_scanner_goes_on_after_a_surrogate_pair() {
	mut reader := SlowReader{
		data:       array_json.bytes()
		chunk_size: 3
	}
	mut scanner := json2.new_reader_scanner(reader: reader, buffer_size: 3)
	defer {
		scanner.free()
	}
	mut got := []string{}
	for {
		token := scanner.next()!
		got << '${token.kind}:${token.lit.hex()}'
		if token.is_eof() {
			break
		}
	}
	assert got == array_tokens
}

// Malformed escapes are still errors, also right after a high surrogate.
const error_cases = {
	r'"\ud83d\ude0"':  'unicode escape must have 4 hex digits'
	r'"\ud83d\ude0x"': 'is not a hex digit'
	r'"\ud83d\uDE00':  'missing double quotes in string closing'
}

fn test_scanner_still_reports_malformed_escapes() {
	for text, message in error_cases {
		mut scanner := json2.new_scanner(text)
		if token := scanner.next() {
			assert false, '${text} gave the token ${token.lit.hex()}'
		} else {
			assert err.msg().contains(message), text
		}
	}
}

fn test_reader_scanner_still_reports_malformed_escapes() {
	for text, message in error_cases {
		mut reader := SlowReader{
			data:       text.bytes()
			chunk_size: 1
		}
		mut scanner := json2.new_reader_scanner(reader: reader, buffer_size: 1)
		if token := scanner.next() {
			assert false, '${text} gave the token ${token.lit.hex()}'
		} else {
			assert err.msg().contains(message), text
		}
		scanner.free()
	}
}
