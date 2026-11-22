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

pub fn (h &JSONHandler) handle(ctx context.Context, mut r Record) ! {
	mut s := h.cfg.st
	s.mu.lock()
	defer { s.mu.unlock() }

	s.buf.clear()
	opts := s.opts
	now := if opts.local_time { time.now() } else { time.utc() }
	ts := format_time(now, opts.time_format, opts.custom_time_format)
	append_byte(mut s.buf, '{')
	append_str(mut s.buf, '"time":"')
	append_str(mut s.buf, json_escape(ts))
	append_str(mut s.buf, '","level":"')
	append_str(mut s.buf, json_escape(r.level.str()))
	append_str(mut s.buf, '","msg":"')
	append_str(mut s.buf, json_escape(r.msg))
	append_byte(mut s.buf, '"')

	if opts.add_source {
		if src := r.source {
			append_str(mut s.buf, ',"source":{"file":"')
			append_str(mut s.buf, json_escape(src.file))
			append_str(mut s.buf, '","line":')
			append_str(mut s.buf, src.line.str())
			append_str(mut s.buf, ',"func":"')
			append_str(mut s.buf, json_escape(src.func))
			append_str(mut s.buf, '"}')
		}
	}

	for a in h.cfg.attrs {
		append_byte(mut s.buf, ',')
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

fn append_json_attr(mut buf []u8, key string, v Value) {
	append_byte(mut buf, '"')
	append_str(mut buf, json_escape(key))
	append_str(mut buf, '":')
	match v {
		string {
			append_byte(mut buf, '"')
			append_str(mut buf, json_escape(v))
			append_byte(mut buf, '"')
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
			append_byte(mut buf, '"')
			append_str(mut buf, json_escape(v.format_rfc3339_micro()))
			append_byte(mut buf, '"')
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
