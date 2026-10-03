// Copyright (c) 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module jsonc

import os
import x.json5

// ParseOpts tunes how strictly a document is read.
pub struct ParseOpts {
pub:
	// allow_trailing_comma accepts a comma before a closing `}` or `]`. It is
	// false by default, which matches the JSONC parser that VS Code uses;
	// TypeScript's own configuration files are read with it set to true.
	allow_trailing_comma bool
}

// Any is the dynamic tree a parsed document produces. It is the JSON5 tree,
// unchanged: JSONC documents are a subset of what JSON5 accepts.
pub type Any = json5.Any

// Doc is a parsed document with path lookup. It is the JSON5 `Doc`.
pub type Doc = json5.Doc

// violation returns the JSONC strictness violation in `text`, or none when the
// document is valid.
//
// This is the entry point for a caller that needs to know *where* the problem is.
// A function returning `!T` cannot hand back a concrete error type, so
// parse_text, decode and their variants report the same violation through their
// own error, where it arrives as an `IError` and only its message can be read.
// The payload here is a `ParseError`, so its position is reachable directly:
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

// violation_opts returns the JSONC strictness violation in `text` under `opts`,
// or none when the document is valid.
pub fn violation_opts(text string, opts ParseOpts) ?ParseError {
	_ = json5.parse_text(text) or { return none }
	lexed := lex(text)
	found := lexed.violation
	if found != nil {
		return as_value(found)
	}
	strict := validate(text, opts)
	if strict != nil {
		return as_value(strict)
	}
	return none
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
	_ = json5.parse_text(text) or { return false }
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
	doc := json5.parse_text(text)!
	lexed := lex(text)
	found := lexed.violation
	if found != nil {
		return found
	}
	strict := validate(text, opts)
	if strict != nil {
		return strict
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
// The target rules are the JSON5 decoder's, so a JSONC document decodes into
// anything a JSON5 document does: structs with embedded structs flattened into
// the parent, enums by name or by value, `map[string]T`, arrays, options and
// every scalar width. A field is matched by name or by an `@[json5: 'name']`
// attribute, because the JSON5 decoder owns the traversal.
//
// A missing key leaves a field at its default. An error about the shape of a
// value rather than the dialect comes from that decoder, and so carries its
// `json5:` prefix.
pub fn decode[T](text string) !T {
	parse_text_opts(text, ParseOpts{})!
	return json5.decode[T](text)
}

// decode_file reads and decodes the JSONC document at `path` into `T`.
pub fn decode_file[T](path string) !T {
	return decode[T](read(path)!)
}

// decode_any converts an already parsed tree into `T` using the JSON5 decoder.
pub fn decode_any[T](value Any) !T {
	return json5.decode_any[T](value)
}

// read returns the contents of `path`, naming the file in any error.
fn read(path string) !string {
	return os.read_file(path) or {
		return error('jsonc: could not read `${path}`: ${err.msg()}')
	}
}
