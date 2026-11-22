// Log records passed to handlers.
module log

import time

// SourceLocation identifies the source position that emitted a log record.
// It is populated when HandlerOptions.add_source is enabled (compiler support
// for call site capture is required; see the `caller` helpers below).
pub struct SourceLocation {
pub:
	file string
	line int
	func string
}

// str renders a source location as file:line.
pub fn (s SourceLocation) str() string {
	return '${s.file}:${s.line}'
}

// Record is the structured representation of a single log event. Handlers
// receive a Record and are responsible for formatting and writing it.
pub struct Record {
pub mut:
	time   time.Time
	level  Level
	msg    string
	attrs  []Attr
	fixed  FixedAttrs
	groups []string
	source ?SourceLocation
	logger string
}

// clone returns a copy of the record with a fresh attribute slice, so a
// handler may safely add derived attributes without mutating the caller's.
pub fn (r &Record) clone() Record {
	return Record{
		time:   r.time
		level:  r.level
		msg:    r.msg
		attrs:  r.attrs.clone()
		fixed:  r.fixed
		groups: r.groups.clone()
		source: r.source
		logger: r.logger
	}
}
