// JSONHandler writes one JSON object per record (newline delimited JSON).
module log

import context
import io
import math
import time

// JSONHandler renders records as newline delimited JSON objects.
pub struct JSONHandler {
	cfg HandlerConfig
}

// new_json_handler returns a JSON handler writing to 'writer'.
pub fn new_json_handler(writer io.Writer, opts HandlerOptions) &JSONHandler {
	return &JSONHandler{
		cfg: make_config(make_shared(writer, opts, opts.level), [], [])
	}
}

// new_json_handler_var returns a JSON handler with a runtime adjustable level.
pub fn new_json_handler_var(writer io.Writer, opts HandlerOptions, level_var &LevelVar) &JSONHandler {
	mut s := make_shared(writer, opts, level_var.get())
	s.level_var = level_var
	return &JSONHandler{ cfg: make_config(s, [], []) }
}

pub fn (h &JSONHandler) enabled(level Level) bool {
	return h.cfg.enabled(level)
}

pub fn (h &JSONHandler) handle(ctx context.Context, r Record) ! {
	// ReplaceAttr is user code: run it before taking the handler lock so a
	// callback that logs re-entrantly cannot deadlock.
	cfg_fields := h.cfg.apply_replace(h.cfg.groups, h.cfg.attrs)
	call_fields := h.cfg.apply_replace(r.groups, r.attrs)
	mut fixed_fields := []Attr{cap: r.fixed.len}
	for i in 0 .. r.fixed.len {
		a := h.cfg.apply_one(r.groups, r.fixed.items[i])
		if a.key != '' {
			fixed_fields << a
		}
	}
	line := h.render(r, cfg_fields, fixed_fields, call_fields)
	mut s := h.cfg.st
	s.writer.write(line) or {}
}

fn (h &JSONHandler) render(r Record, cfg_fields []Attr, fixed_fields []Attr, call_fields []Attr) []u8 {
	mut s := h.cfg.st
	s.mu.lock()
	defer { s.mu.unlock() }

	s.buf.clear()
	opts := s.opts
	now := if opts.local_time { time.now() } else { time.utc() }
	append_byte(mut s.buf, '{')
	append_str(mut s.buf, '"time":"')
	if timestamp_direct_supported(opts.time_format) {
		append_timestamp(mut s.buf, now, opts.time_format, opts.local_time)
	} else {
		append_escaped(mut s.buf, format_time(now, opts.time_format, opts.custom_time_format))
	}
	append_byte(mut s.buf, '"')
	append_str(mut s.buf, ',"level":')
	append_json_string(mut s.buf, r.level.str())
	append_str(mut s.buf, ',"msg":')
	append_json_string(mut s.buf, r.msg)

	if opts.add_source {
		if src := r.source {
			append_str(mut s.buf, ',"source":{"file":')
			append_json_string(mut s.buf, src.file)
			append_str(mut s.buf, ',"line":')
			append_str(mut s.buf, src.line.str())
			append_str(mut s.buf, ',"func":')
			append_json_string(mut s.buf, src.func)
			append_byte(mut s.buf, '}')
		}
	}

	for a in cfg_fields {
		if is_reserved_key(a.key) {
			continue
		}
		append_byte(mut s.buf, ',')
		append_json_attr(mut s.buf, a.key, a.value)
	}
	for a in fixed_fields {
		if is_reserved_key(a.key) {
			continue
		}
		append_byte(mut s.buf, ',')
		append_json_attr(mut s.buf, a.key, a.value)
	}
	for a in call_fields {
		if is_reserved_key(a.key) {
			continue
		}
		append_byte(mut s.buf, ',')
		append_json_attr(mut s.buf, a.key, a.value)
	}
	append_byte(mut s.buf, '}')
	append_byte(mut s.buf, '\n')
	return s.buf.clone()
}

pub fn (h &JSONHandler) set_level(level Level) {
	mut s := h.cfg.st
	s.mu.lock()
	defer { s.mu.unlock() }
	s.level = level
	if v := s.level_var {
		v.set(level)
	}
}

pub fn (h &JSONHandler) with_attrs(attrs []Attr) Handler {
	c := make_config(h.cfg.st, concat_attrs(h.cfg.all_attrs(), attrs), h.cfg.groups)
	return &JSONHandler{ cfg: c }
}

pub fn (h &JSONHandler) with_group(name string) Handler {
	mut groups := h.cfg.groups.clone()
	groups << name
	c := make_config(h.cfg.st, h.cfg.all_attrs(), groups)
	return &JSONHandler{ cfg: c }
}

// append_json_string writes a quoted, JSON escaped string directly into the
// scratch buffer. It avoids the intermediate allocation made by json_escape.
fn append_json_string(mut buf []u8, s string) {
	buf << u8(34)
	append_escaped(mut buf, s)
	buf << u8(34)
}

// utf8_seq_len returns the expected length of a UTF-8 sequence starting at i.
fn utf8_seq_len(b []u8, i int) int {
	c := b[i]
	if c >= 0xc2 && c <= 0xdf {
		return 2
	}
	if c >= 0xe0 && c <= 0xef {
		return 3
	}
	if c >= 0xf0 && c <= 0xf4 {
		return 4
	}
	return 0
}

// valid_utf8_seq checks continuation bytes and the overlong/surrogate limits.
fn valid_utf8_seq(b []u8, i int, n int) bool {
	if i + n > b.len {
		return false
	}
	for j in 1 .. n {
		if b[i + j] < 0x80 || b[i + j] > 0xbf {
			return false
		}
	}
	c := b[i]
	if n == 2 {
		return c >= 0xc2
	}
	if n == 3 {
		if c == 0xe0 && b[i + 1] < 0xa0 {
			return false
		}
		if c == 0xed && b[i + 1] >= 0xa0 {
			return false
		}
	}
	if n == 4 {
		if c == 0xf0 && b[i + 1] < 0x90 {
			return false
		}
		if c == 0xf4 && b[i + 1] >= 0x90 {
			return false
		}
	}
	return true
}

// append_escaped writes the escaped content of a JSON string without quotes.
// Invalid UTF-8 is replaced with the standard U+FFFD escape so strict JSON
// readers always accept the record.
fn append_escaped(mut buf []u8, s string) {
	b := s.bytes()
	mut i := 0
	for i < b.len {
		c := b[i]
		match c {
			u8(34) {
				buf << u8(92)
				buf << u8(34)
				i++
			}
			u8(92) {
				buf << u8(92)
				buf << u8(92)
				i++
			}
			8 {
				buf << u8(92)
				buf << u8(98)
				i++
			}
			9 {
				buf << u8(92)
				buf << u8(116)
				i++
			}
			10 {
				buf << u8(92)
				buf << u8(110)
				i++
			}
			12 {
				buf << u8(92)
				buf << u8(102)
				i++
			}
			13 {
				buf << u8(92)
				buf << u8(114)
				i++
			}
			else {
				if c < 0x20 {
					buf << u8(92)
					buf << u8(117)
					buf << u8(48)
					buf << u8(48)
					buf << hex_digit(c >> 4)
					buf << hex_digit(c & 0x0f)
					i++
				} else if c < 0x80 {
					buf << c
					i++
				} else {
					n := utf8_seq_len(b, i)
					if n > 0 && valid_utf8_seq(b, i, n) {
						for j in 0 .. n {
							buf << b[i + j]
						}
						i += n
					} else {
						buf << u8(92)
						buf << u8(117)
						buf << u8(102)
						buf << u8(102)
						buf << u8(102)
						buf << u8(100)
						i++
					}
				}
			}
		}
	}
}

fn append_json_attr(mut buf []u8, key string, v Value) {
	append_json_string(mut buf, key)
	append_byte(mut buf, ':')
	match v {
		string {
			append_json_string(mut buf, v)
		}
		i64 {
			append_str(mut buf, v.str())
		}
		f64 {
			// JSON has no representation for NaN or infinity.
			if !math.is_finite(v) {
				append_str(mut buf, 'null')
			} else {
				append_str(mut buf, v.str())
			}
		}
		bool {
			append_str(mut buf, if v { 'true' } else { 'false' })
		}
		time.Time {
			append_json_string(mut buf, v.format_rfc3339_micro())
		}
		[]Attr {
			append_byte(mut buf, '{')
			mut first := true
			for a in v {
				if !first {
					append_byte(mut buf, ',')
				}
				first = false
				append_json_attr(mut buf, a.key, a.value)
			}
			append_byte(mut buf, '}')
		}
	}
}
