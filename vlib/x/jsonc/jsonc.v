// Copyright (c) 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module jsonc

import os
import json2

// ParseOpts tunes how strictly a document is read.
pub struct ParseOpts {
pub:
	// allow_trailing_comma accepts a comma before a closing `}` or `]`. It is
	// false by default, which matches the JSONC parser that VS Code uses;
	// TypeScript's own configuration files are read with it set to true.
	allow_trailing_comma bool
}

// Any is the dynamic tree a parsed document produces, shared with json2.
pub type Any = json2.Any

// Doc is a parsed document with json2's path lookup and JSONC rendering.
pub type Doc = json2.Doc

// str renders the document as JSONC, using overflowing decimal numbers for infinity.
// NaN values in manually wrapped trees render as null.
@[manualfree]
pub fn (d Doc) str() string {
	mut encoded := []u8{cap: 2048}
	// No views of the scratch buffers escape, so growth can release old storage.
	unsafe { encoded.flags.set(.noslices) }
	defer { unsafe { encoded.free() } }
	json2.encode_append(d.root, mut encoded, json5: true)
	mut output := []u8{cap: encoded.len}
	unsafe { output.flags.set(.noslices) }
	defer { unsafe { output.free() } }
	mut i := 0
	for i < encoded.len {
		current := encoded[i]
		if current == `"` {
			output << current
			i++
			// Copy quoted keys and values intact, including escaped quotes and backslashes.
			for i < encoded.len {
				byte := encoded[i]
				output << byte
				i++
				if byte == `\\` && i < encoded.len {
					output << encoded[i]
					i++
				} else if byte == `"` {
					break
				}
			}
			continue
		}
		// With quoted keys, these are the only uppercase tokens the encoder emits.
		if current == `I` {
			for byte in '1e999' {
				output << byte
			}
			i += 'Infinity'.len
		} else if current == `N` {
			for byte in 'null' {
				output << byte
			}
			i += 'NaN'.len
		} else {
			output << current
			i++
		}
	}
	return output.bytestr()
}

// violation returns the first JSONC strictness violation in `text`, or none when
// the document is valid.
//
// This is a convenience for a caller that needs to know *where* the problem is:
// the payload is the `ParseError` itself, so its position is reachable directly.
// parse_text, decode and their variants return the same violation as their
// error, where `if err is jsonc.ParseError` reaches it as well:
//
// ```v ignore
// if v := jsonc.violation(text) {
//     println('${v.pos.line}:${v.pos.col} bytes ${v.pos.offset}..${v.pos.end_offset}')
// }
// ```
//
// Input that is not well formed JSON has no violation to report, because the
// JSON5 parser owns it; parse_text returns that error instead.
pub fn violation(text string) ?ParseError {
	return violation_opts(text, ParseOpts{})
}

// violation_opts returns the first JSONC strictness violation in `text` under
// `opts`, or none when the document is valid.
pub fn violation_opts(text string, opts ParseOpts) ?ParseError {
	_ = json2.parse_text(text, json5: true) or { return none }
	found := earliest_violation(text, opts)
	if found != nil {
		return as_value(found)
	}
	return none
}

// earliest_violation returns the violation in `text` that comes first in the
// document, or nil when the document is valid JSONC.
//
// The rune pass and the token pass each stop at the first problem they see, and
// each sees only its own kind of problem, so the first in the document is
// whichever of the two starts earlier.
fn earliest_violation(text string, opts ParseOpts) &ParseError {
	lexed := lex(text).violation
	strict := validate(text, opts)
	if lexed == nil {
		return strict
	}
	if strict == nil || lexed.pos.offset <= strict.pos.offset {
		return lexed
	}
	return strict
}

// as_value copies a reported violation into the payload shape that
// violation_opts hands back.
fn as_value(e &ParseError) ParseError {
	return ParseError{
		message: e.message
		pos:     e.pos
	}
}

// is_valid reports whether `text` is valid JSONC.
//
// A document that is not well formed JSON is not valid JSONC either, so this
// answers the question rather than reporting why. Use parse_text when the
// reason matters: it returns a ParseError for a JSON5 extension that JSONC does
// not allow, and the JSON5 parser's own error for malformed input.
pub fn is_valid(text string) bool {
	return is_valid_opts(text, ParseOpts{})
}

// is_valid_opts reports whether `text` is valid JSONC under `opts`.
pub fn is_valid_opts(text string, opts ParseOpts) bool {
	_ = json2.parse_text(text, json5: true) or { return false }
	lexed := lex(text)
	if lexed.violation != nil {
		return false
	}
	return validate(text, opts) == nil
}

// parse_text parses the JSONC document in `text` and returns a `Doc`.
pub fn parse_text(text string) !Doc {
	return parse_text_opts(text, ParseOpts{})
}

// parse_text_opts parses the JSONC document in `text` under `opts` and returns
// a `Doc`.
pub fn parse_text_opts(text string, opts ParseOpts) !Doc {
	doc := json2.parse_text(text, json5: true)!
	found := earliest_violation(text, opts)
	if found != nil {
		return found
	}
	return doc
}

// parse_file reads and parses the JSONC document at `path`.
pub fn parse_file(path string) !Doc {
	return parse_file_opts(path, ParseOpts{})
}

// parse_file_opts reads and parses the JSONC document at `path` under `opts`.
pub fn parse_file_opts(path string, opts ParseOpts) !Doc {
	return parse_text_opts(read(path)!, opts)
}

// parse parses the JSONC document in `text` and returns its root `Any`.
pub fn parse(text string) !Any {
	return parse_opts(text, ParseOpts{})
}

// parse_opts parses the JSONC document in `text` under `opts` and returns its
// root `Any`.
pub fn parse_opts(text string, opts ParseOpts) !Any {
	doc := parse_text_opts(text, opts)!
	return doc.to_any()
}

// decode parses the JSONC document in `text` and decodes it into `T`.
//
// The target rules are json2's: structs, embedded structs, enums, maps, arrays,
// options and scalar types. Fields use `@[json: 'name']` for renaming.
// Missing keys retain their defaults. Dialect validation precedes decoding.
pub fn decode[T](text string) !T {
	// Validate the dialect before decoding the original text, preserving integers
	// beyond the precision of the dynamic tree's f64 values.
	_ = parse_text(text)!
	return json2.decode[T](text, json5: true)
}

// decode_file reads and decodes the JSONC document at `path` into `T`.
pub fn decode_file[T](path string) !T {
	return decode[T](read(path)!)
}

// decode_any converts an already parsed tree into `T` using the json2 decoder.
// Decimal overflow retained as infinity is preserved during conversion.
pub fn decode_any[T](value Any) !T {
	return json2.decode_any[T](value, json5: true)
}

// read returns the contents of `path`, naming the file in any error.
fn read(path string) !string {
	return os.read_file(path) or {
		return error('jsonc: could not read `${path}`: ${err.msg()}')
	}
}
