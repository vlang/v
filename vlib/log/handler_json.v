// JSONHandler writes one JSON object per record (newline delimited JSON).
module log

import context
import io
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

	for a in h.cfg.attrs {
		append_byte(mut s.buf, ',')
		append_json_attr(mut s.buf, a.key, a.value)
	}
	for i in 0 .. r.fixed.len {
		append_byte(mut s.buf, ',')
		a := r.fixed.items[i]
		append_json_attr(mut s.buf, a.key, a.value)
	}
	fields := h.cfg.apply_replace(r.groups, r.attrs)
	for a in fields {
		append_byte(mut s.buf, ',')
		append_json_attr(mut s.buf, a.key, a.value)
	}
	append_byte(mut s.buf, '}')
	append_byte(mut s.buf, '\n')
	s.writer.write(s.buf) or {}
}

pub fn (h &JSONHandler) set_level(level Level) {
	mut s := h.cfg.st
	s.level = level
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

// append_escaped writes the escaped content of a JSON string without quotes.
fn append_escaped(mut buf []u8, s string) {
	for c in s.bytes() {
		match c {
			u8(34) {
				buf << u8(92)
				buf << u8(34)
			}
			u8(92) {
				buf << u8(92)
				buf << u8(92)
			}
			8 {
				buf << u8(92)
				buf << u8(98)
			}
			9 {
				buf << u8(92)
				buf << u8(116)
			}
			10 {
				buf << u8(92)
				buf << u8(110)
			}
			12 {
				buf << u8(92)
				buf << u8(102)
			}
			13 {
				buf << u8(92)
				buf << u8(114)
			}
			else {
				if c < 0x20 {
					buf << u8(92)
					buf << u8(117)
					buf << u8(48)
					buf << u8(48)
					buf << hex_digit(c >> 4)
					buf << hex_digit(c & 0x0f)
				} else {
					buf << c
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
			append_str(mut buf, v.str())
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
