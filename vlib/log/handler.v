// Handler is the pluggable backend of a RecordLogger.
//
// Handlers decide how a Record is formatted and where it is written. Derived
// handlers (created with with_attrs / with_group) share the underlying writer
// and scratch buffer, so logging remains safe when several loggers are used
// concurrently.
module log

import context
import io
import sync

// ReplaceAttr can rewrite, filter (by returning Attr{}), or reorder an
// attribute before it is rendered. `groups` is the active group path.
pub type ReplaceAttr = fn (groups []string, attr Attr) Attr

// HandlerOptions configures the built-in handlers.
pub struct HandlerOptions {
pub:
	level              Level = .info
	add_source         bool
	replace_attr       ?ReplaceAttr
	time_format        TimeFormat = .tf_rfc3339_micro
	custom_time_format string
	local_time         bool
	color              bool = true
	short_tags         bool
	output_label       string
}

// LevelVar is a dynamically adjustable level threshold. Unlike a plain Level,
// it can be changed while the application is running.
pub struct LevelVar {
mut:
	level Level = .info
}

// new_level_var returns a LevelVar initialised at `level`.
pub fn new_level_var(level Level) &LevelVar {
	return &LevelVar{ level: level }
}

// set changes the minimum level.
pub fn (mut l LevelVar) set(level Level) {
	l.level = level
}

// get returns the current minimum level.
pub fn (l &LevelVar) get() Level {
	return l.level
}

// Handler receives records from a RecordLogger.
pub interface Handler {
	enabled(level Level) bool
	handle(ctx context.Context, mut r Record) !
	with_attrs(attrs []Attr) Handler
	with_group(name string) Handler
	set_level(level Level)
}

// shared holds the mutable state shared by all handlers derived from one root.
@[heap]
struct HandlerShared {
mut:
	mu        &sync.Mutex = sync.new_mutex()
	buf       []u8
	writer    io.Writer
	opts      HandlerOptions
	level     Level = .info
	level_var ?&LevelVar
	tags      [6]string
}

// config is per-handler immutable state (attributes and group path).
struct HandlerConfig {
	st     &HandlerShared
	attrs  []Attr
	groups []string
}

fn make_shared(writer io.Writer, opts HandlerOptions, level Level) &HandlerShared {
	mut s := &HandlerShared{
		writer: writer
		opts:   opts
		level:  level
	}
	// Rendering tags once avoids building colour escape strings per record.
	levels := [
		Level.disabled,
		Level.fatal,
		Level.error,
		Level.warn,
		Level.info,
		Level.debug,
	]
	for i, lvl in levels {
		s.tags[i] = if opts.color {
			tag_to_console(lvl, opts.short_tags)
		} else {
			tag_to_file(lvl, opts.short_tags)
		}
	}
	return s
}

fn (c HandlerConfig) all_attrs() []Attr {
	// Handlers only read these attributes, so sharing the slice is safe and
	// avoids allocating a clone for every record.
	return c.attrs
}

fn make_config(st &HandlerShared, attrs []Attr, groups []string) HandlerConfig {
	return HandlerConfig{ st: st, attrs: attrs, groups: groups }
}

fn concat_attrs(a []Attr, b []Attr) []Attr {
	mut out := []Attr{cap: a.len + b.len}
	out << a
	out << b
	return out
}

fn (c HandlerConfig) enabled(level Level) bool {
	// The Level enum is ordered from least to most verbose, so a message is
	// enabled when the configured threshold is at least as verbose as it.
	if v := c.st.level_var {
		return int(v.get()) >= int(level)
	}
	return int(c.st.level) >= int(level)
}

// apply_replace runs the configured ReplaceAttr over a list of attributes.
fn (c HandlerConfig) apply_replace(groups []string, attrs []Attr) []Attr {
	replace := c.st.opts.replace_attr or { return attrs }
	mut out := []Attr{cap: attrs.len}
	for a in attrs {
		na := replace(groups, a)
		if na.key != '' {
			out << na
		}
	}
	return out
}

// set_handler_level changes the minimum level of a handler.
pub fn set_handler_level(h Handler, level Level) {
	h.set_level(level)
}

// DiscardHandler drops every record. Useful for tests and benchmarks.
pub struct DiscardHandler {
	cfg HandlerConfig
}

// new_discard_handler returns a handler that discards all records.
pub fn new_discard_handler() &DiscardHandler {
	return &DiscardHandler{
		cfg: make_config(make_shared(io.Writer(io.discard), HandlerOptions{}, Level.disabled), [], [])
	}
}

pub fn (h &DiscardHandler) enabled(level Level) bool {
	return false
}

pub fn (h &DiscardHandler) handle(ctx context.Context, mut r Record) ! {}

pub fn (h &DiscardHandler) set_level(level Level) {}

pub fn (h &DiscardHandler) with_attrs(attrs []Attr) Handler {
	return &DiscardHandler{ cfg: make_config(h.cfg.st, h.cfg.all_attrs(), h.cfg.groups) }
}

pub fn (h &DiscardHandler) with_group(name string) Handler {
	mut groups := h.cfg.groups.clone()
	groups << name
	return &DiscardHandler{ cfg: make_config(h.cfg.st, h.cfg.all_attrs(), groups) }
}

// MultiHandler fans records out to several handlers.
pub struct MultiHandler {
	handlers []Handler
}

// new_multi_handler returns a handler that forwards to every given handler.
pub fn new_multi_handler(handlers ...Handler) &MultiHandler {
	return &MultiHandler{ handlers: handlers }
}

pub fn (h &MultiHandler) enabled(level Level) bool {
	for sub in h.handlers {
		if sub.enabled(level) {
			return true
		}
	}
	return false
}

pub fn (h &MultiHandler) handle(ctx context.Context, mut r Record) ! {
	for sub in h.handlers {
		if sub.enabled(r.level) {
			sub.handle(ctx, mut r) or {}
		}
	}
}

pub fn (h &MultiHandler) set_level(level Level) {
	for sub in h.handlers {
		sub.set_level(level)
	}
}

pub fn (h &MultiHandler) with_attrs(attrs []Attr) Handler {
	mut out := []Handler{cap: h.handlers.len}
	for sub in h.handlers {
		out << sub.with_attrs(attrs)
	}
	return &MultiHandler{ handlers: out }
}

pub fn (h &MultiHandler) with_group(name string) Handler {
	mut out := []Handler{cap: h.handlers.len}
	for sub in h.handlers {
		out << sub.with_group(name)
	}
	return &MultiHandler{ handlers: out }
}
