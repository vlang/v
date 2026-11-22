// TextHandler writes records in the traditional human readable log format.
// With no attributes it emits exactly the same bytes as the legacy Log methods.
module log

import context
import io
import time

// TextHandler renders records as plain text.
pub struct TextHandler {
	cfg HandlerConfig
}

// new_text_handler returns a text handler writing to 'writer'.
pub fn new_text_handler(writer io.Writer, opts HandlerOptions) &TextHandler {
	return &TextHandler{
		cfg: make_config(make_shared(writer, opts, opts.level), [], [])
	}
}

// new_text_handler_var returns a text handler whose level can be changed at
// runtime through 'level_var'.
pub fn new_text_handler_var(writer io.Writer, opts HandlerOptions, level_var &LevelVar) &TextHandler {
	mut s := make_shared(writer, opts, level_var.get())
	s.level_var = level_var
	return &TextHandler{ cfg: make_config(s, [], []) }
}

pub fn (h &TextHandler) enabled(level Level) bool {
	return h.cfg.enabled(level)
}

pub fn (h &TextHandler) handle(ctx context.Context, r Record) ! {
	mut s := h.cfg.st
	s.mu.lock()
	defer { s.mu.unlock() }

	s.buf.clear()
	opts := s.opts
	now := if opts.local_time { time.now() } else { time.utc() }
	if timestamp_direct_supported(opts.time_format) {
		append_timestamp(mut s.buf, now, opts.time_format, opts.local_time)
	} else {
		append_str(mut s.buf, format_time(now, opts.time_format, opts.custom_time_format))
	}
	append_byte(mut s.buf, ' ')
	append_byte(mut s.buf, '[')
	append_str(mut s.buf, s.tags[int(r.level)])
	append_byte(mut s.buf, ']')
	append_byte(mut s.buf, ' ')
	append_str(mut s.buf, r.msg)

	all := h.cfg.apply_replace(r.groups, r.attrs)
	for a in h.cfg.attrs {
		append_attr(mut s.buf, h.cfg.groups, a)
	}
	for i in 0 .. r.fixed.len {
		append_attr(mut s.buf, r.groups, r.fixed.items[i])
	}
	for a in all {
		append_attr(mut s.buf, r.groups, a)
	}
	append_byte(mut s.buf, '\n')

	s.writer.write(s.buf) or {}
}

pub fn (h &TextHandler) set_level(level Level) {
	mut s := h.cfg.st
	s.level = level
}

pub fn (h &TextHandler) with_attrs(attrs []Attr) Handler {
	c := make_config(h.cfg.st, concat_attrs(h.cfg.all_attrs(), attrs), h.cfg.groups)
	return &TextHandler{ cfg: c }
}

pub fn (h &TextHandler) with_group(name string) Handler {
	mut groups := h.cfg.groups.clone()
	groups << name
	c := make_config(h.cfg.st, h.cfg.all_attrs(), groups)
	return &TextHandler{ cfg: c }
}

fn append_attr(mut buf []u8, groups []string, a Attr) {
	append_byte(mut buf, ' ')
	for g in groups {
		append_str(mut buf, g)
		append_byte(mut buf, '.')
	}
	append_str(mut buf, a.key)
	append_byte(mut buf, '=')
	append_str(mut buf, value_str(a.value))
}

fn value_str(v Value) string {
	return match v {
		string {
			v
		}
		i64 {
			v.str()
		}
		f64 {
			v.str()
		}
		bool {
			v.str()
		}
		time.Time {
			v.format_rfc3339_micro()
		}
		[]Attr {
			mut out := ''
			for a in v {
				out += '${a.key}=${value_str(a.value)} '
			}
			out.trim_space()
		}
	}
}
