// RecordLogger is the slog-style logging API.
//
// A RecordLogger is immutable from the caller's point of view, so the same
// logger can be shared by many threads. Deriving a logger with `with` or
// `with_group` returns a new logger that shares its handler (and therefore its
// writer and scratch buffer) with the original.
module log

import context

// RecordLogger logs structured records through a Handler.
pub struct RecordLogger {
pub:
	name string
mut:
	handler Handler
	attrs   []Attr
	empty   []Attr
	groups  []string
	ctx     context.Context
}

// new_logger returns a logger that sends records to `handler`.
pub fn new_logger(handler Handler) &RecordLogger {
	return &RecordLogger{
		handler: handler
		empty:   []Attr{}
		ctx:     context.background()
	}
}

// new_named_logger returns a logger whose name is included in each record.
pub fn new_named_logger(name string, handler Handler) &RecordLogger {
	return &RecordLogger{
		name:    name
		handler: handler
		empty:   []Attr{}
		ctx:     context.background()
	}
}

// with returns a derived logger that adds `attrs` to every record.
pub fn (l &RecordLogger) with(attrs ...Attr) &RecordLogger {
	return &RecordLogger{
		name:    l.name
		handler: l.handler.with_attrs(attrs)
		attrs:   concat_attrs(l.attrs, attrs)
		empty:   l.empty
		groups:  l.groups
		ctx:     l.ctx
	}
}

// with_attrs is like with, but takes a prebuilt slice (no variadic allocation).
pub fn (l &RecordLogger) with_attrs(attrs []Attr) &RecordLogger {
	return &RecordLogger{
		name:    l.name
		handler: l.handler.with_attrs(attrs)
		attrs:   concat_attrs(l.attrs, attrs)
		empty:   l.empty
		groups:  l.groups
		ctx:     l.ctx
	}
}

// with_group returns a derived logger whose attributes are nested under
// `name`.
pub fn (l &RecordLogger) with_group(name string) &RecordLogger {
	mut groups := l.groups.clone()
	groups << name
	return &RecordLogger{
		name:    l.name
		handler: l.handler.with_group(name)
		attrs:   l.attrs
		empty:   l.empty
		groups:  groups
		ctx:     l.ctx
	}
}

// with_context returns a derived logger carrying `ctx` on each record.
pub fn (l &RecordLogger) with_context(ctx context.Context) &RecordLogger {
	return &RecordLogger{
		name:    l.name
		handler: l.handler
		attrs:   l.attrs
		empty:   l.empty
		groups:  l.groups
		ctx:     ctx
	}
}

// enabled reports whether the handler would log at `level`.
pub fn (l &RecordLogger) enabled(level Level) bool {
	return l.handler.enabled(level)
}

// log emits a record at `level` with the given attributes.
pub fn (l &RecordLogger) log(level Level, msg string, attrs ...Attr) {
	l.send_if(level, msg, attrs)
}

fn (l &RecordLogger) send_if(level Level, msg string, attrs []Attr) {
	if !l.handler.enabled(level) {
		return
	}
	l.send(level, msg, attrs)
}

// log_attrs emits a record using the zero allocation attribute builder.
pub fn (l &RecordLogger) log_attrs(level Level, msg string, attrs &Attrs) {
	if !l.handler.enabled(level) {
		return
	}
	l.send_built(level, msg, attrs.values())
}

fn (l &RecordLogger) send(level Level, msg string, attrs []Attr) {
	mut r := Record{
		level:  level
		msg:    msg
		attrs:  attrs
		groups: l.groups
		logger: l.name
	}
	l.handler.handle(l.ctx, r) or {}
}

fn (l &RecordLogger) send_built(level Level, msg string, attrs []Attr) {
	l.send(level, msg, attrs)
}

// log_fixed emits a record using a stack only fixed attribute list. Unlike the
// variadic methods this builds no slice at the call site.
pub fn (l &RecordLogger) log_fixed(level Level, msg string, attrs &FixedAttrs) {
	if !l.handler.enabled(level) {
		return
	}
	// len is private, but a derived handler or internal caller could still
	// produce an inconsistent list; clamp before handing it to the renderer.
	mut f := FixedAttrs{}
	n := if attrs.len < 4 { attrs.len } else { 4 }
	for i in 0 .. n {
		f.items[i] = attrs.items[i]
	}
	f.len = n
	mut r := Record{
		level:  level
		msg:    msg
		attrs:  l.empty
		fixed:  f
		groups: l.groups
		logger: l.name
	}
	l.handler.handle(l.ctx, r) or {}
}

// debug logs a debug record.
pub fn (l &RecordLogger) debug(msg string, attrs ...Attr) {
	l.send_if(.debug, msg, attrs)
}

// info logs an info record.
pub fn (l &RecordLogger) info(msg string, attrs ...Attr) {
	l.send_if(.info, msg, attrs)
}

// warn logs a warning record.
pub fn (l &RecordLogger) warn(msg string, attrs ...Attr) {
	l.send_if(.warn, msg, attrs)
}

// error logs an error record.
pub fn (l &RecordLogger) error(msg string, attrs ...Attr) {
	l.send_if(.error, msg, attrs)
}

// fatal logs a fatal record and panics, regardless of the configured level.
@[noreturn]
pub fn (l &RecordLogger) fatal(msg string, attrs ...Attr) {
	if l.handler.enabled(.fatal) {
		l.send(.fatal, msg, attrs)
	}
	panic('${l.name}: ${msg}')
}

// debug_context logs a debug record using `ctx`.
pub fn (l &RecordLogger) debug_context(ctx context.Context, msg string, attrs ...Attr) {
	if !l.handler.enabled(.debug) {
		return
	}
	l.send_context(ctx, .debug, msg, attrs)
}

// info_context logs an info record using `ctx`.
pub fn (l &RecordLogger) info_context(ctx context.Context, msg string, attrs ...Attr) {
	if !l.handler.enabled(.info) {
		return
	}
	l.send_context(ctx, .info, msg, attrs)
}

// warn_context logs a warning record using `ctx`.
pub fn (l &RecordLogger) warn_context(ctx context.Context, msg string, attrs ...Attr) {
	if !l.handler.enabled(.warn) {
		return
	}
	l.send_context(ctx, .warn, msg, attrs)
}

// error_context logs an error record using `ctx`.
pub fn (l &RecordLogger) error_context(ctx context.Context, msg string, attrs ...Attr) {
	if !l.handler.enabled(.error) {
		return
	}
	l.send_context(ctx, .error, msg, attrs)
}

fn (l &RecordLogger) send_context(ctx context.Context, level Level, msg string, attrs []Attr) {
	mut r := Record{
		level:  level
		msg:    msg
		attrs:  attrs
		groups: l.groups
		logger: l.name
	}
	l.handler.handle(ctx, r) or {}
}
