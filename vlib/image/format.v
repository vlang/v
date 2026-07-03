// Copyright (c) 2026 The V Authors. All rights reserved.
// Portions derived from Go's image package.
// Copyright 2010 The Go Authors. All rights reserved.
// Use of the original Go source is governed by Go's BSD-style license.
@[has_globals]
module image

import io

// err_format indicates that decoding encountered an unknown format.
pub const err_format = 'image: unknown format'

// DecodeFn decodes an image from a registered format.
pub type DecodeFn = fn (mut PeekReader) !Image

// DecodeConfigFn decodes an image configuration from a registered format.
pub type DecodeConfigFn = fn (mut PeekReader) !Config

struct Format {
	name          string
	magic         string
	decode        DecodeFn       @[required]
	decode_config DecodeConfigFn @[required]
}

__global (
	registered_formats []Format
)

// PeekReader is an io.Reader that can also peek ahead without consuming bytes.
pub interface PeekReader {
mut:
	read(mut buf []u8) !int
	peek(n int) ![]u8
}

struct BufferedPeekReader {
mut:
	reader io.Reader
	buf    []u8
	offset int
}

// register_format registers an image format for use by decode and decode_config.
pub fn register_format(name string, magic string, decode DecodeFn, decode_config DecodeConfigFn) {
	registered_formats << Format{
		name:          name
		magic:         magic
		decode:        decode
		decode_config: decode_config
	}
}

fn as_reader(reader io.Reader) PeekReader {
	return BufferedPeekReader{
		reader: reader
	}
}

// read implements io.Reader for BufferedPeekReader.
pub fn (mut r BufferedPeekReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	mut copied := 0
	if r.offset < r.buf.len {
		copied = copy(mut buf, r.buf[r.offset..])
		r.offset += copied
		if copied == buf.len {
			return copied
		}
	}
	read := r.reader.read(mut buf[copied..]) or {
		if copied > 0 {
			return copied
		}
		return err
	}
	return copied + read
}

// peek returns the next n bytes without consuming them.
pub fn (mut r BufferedPeekReader) peek(n int) ![]u8 {
	if n < 0 {
		return error('image: negative peek length')
	}
	for r.buf.len - r.offset < n {
		missing := n - (r.buf.len - r.offset)
		mut tmp := []u8{len: missing}
		read := r.reader.read(mut tmp)!
		if read <= 0 {
			return io.Eof{}
		}
		r.buf << tmp[..read]
	}
	return r.buf[r.offset..r.offset + n]
}

fn match_magic(magic string, b []u8) bool {
	if magic.len != b.len {
		return false
	}
	for i, c in b {
		if magic[i] != c && magic[i] != `?` {
			return false
		}
	}
	return true
}

// sniff determines the registered format of r's data.
fn sniff(mut r PeekReader) ?Format {
	for f in registered_formats {
		b := r.peek(f.magic.len) or { continue }
		if match_magic(f.magic, b) {
			return f
		}
	}
	return none
}

// decode decodes an image from a registered format.
pub fn decode(reader io.Reader) !(Image, string) {
	mut r := as_reader(reader)
	f := sniff(mut r) or { return error(err_format) }
	m := f.decode(mut r)!
	return m, f.name
}

// decode_config decodes the color model and dimensions of a registered format.
pub fn decode_config(reader io.Reader) !(Config, string) {
	mut r := as_reader(reader)
	f := sniff(mut r) or { return error(err_format) }
	c := f.decode_config(mut r)!
	return c, f.name
}
