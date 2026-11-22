// Structured logging attributes.
//
// An Attr is a typed key/value pair. It is a plain value type: building one does
// not allocate. Use the constructors below (`str`, `int`, `bool`, ...) to create
// attributes, and pass them to a RecordLogger's logging methods.
module log

import time

// Value is the set of value kinds a log attribute can hold.
pub type Value = string | i64 | f64 | bool | time.Time | []Attr

// Attr is a typed key/value pair used by structured log records.
pub struct Attr {
pub:
	key   string
	value Value
}

// str returns an Attr holding a string value.
pub fn str(key string, value string) Attr {
	return Attr{ key: key, value: value }
}

// int returns an Attr holding an integer value.
pub fn int(key string, value i64) Attr {
	return Attr{ key: key, value: value }
}

// float returns an Attr holding a floating point value.
pub fn float(key string, value f64) Attr {
	return Attr{ key: key, value: value }
}

// bool returns an Attr holding a boolean value.
pub fn bool(key string, value bool) Attr {
	return Attr{ key: key, value: value }
}

// time_attr returns an Attr holding a time.Time value.
pub fn time_attr(key string, value time.Time) Attr {
	return Attr{ key: key, value: value }
}

// group returns an Attr holding a nested group of attributes.
pub fn group(key string, attrs ...Attr) Attr {
	return Attr{ key: key, value: attrs }
}

// err returns an Attr holding the message of an error. Keeping errors as
// strings avoids allocating an error interface in the logging hot path;
// callers that need the original error can pass err.msg() themselves.
pub fn err(key string, msg string) Attr {
	return Attr{ key: key, value: msg }
}

// Attrs is a reusable builder for the low allocation logging path. Its buffer
// is allocated once; reusing the same builder avoids the per-call attribute
// slice that the variadic methods allocate.
@[heap]
pub struct Attrs {
mut:
	buf []Attr = []Attr{cap: 8}
	len int
}

// add appends an attribute to the builder. The buffer starts with room for
// eight attributes and grows if more are added; no attribute is dropped.
pub fn (mut a Attrs) add(attr Attr) &Attrs {
	a.buf << attr
	a.len++
	return a
}

// clear resets the builder so it can be reused for the next record.
pub fn (mut a Attrs) clear() {
	a.buf.clear()
	a.len = 0
}

// values returns a slice over the builder's buffer. It does not allocate, but
// the slice is only valid until the builder is cleared or modified.
pub fn (a &Attrs) values() []Attr {
	return a.buf[0..a.len]
}

// FixedAttrs is a stack only attribute list for the lowest allocation logging
// path. Pass it to RecordLogger.log_fixed; no slice is built at the call site.
pub struct FixedAttrs {
pub mut:
	items [4]Attr
	len   int
}

// fixed1 returns a fixed attribute list holding one attribute.
pub fn fixed1(a Attr) FixedAttrs {
	mut f := FixedAttrs{ len: 1 }
	f.items[0] = a
	return f
}

// fixed2 returns a fixed attribute list holding two attributes.
pub fn fixed2(a Attr, b Attr) FixedAttrs {
	mut f := fixed1(a)
	f.items[1] = b
	f.len = 2
	return f
}

// fixed3 returns a fixed attribute list holding three attributes.
pub fn fixed3(a Attr, b Attr, c Attr) FixedAttrs {
	mut f := fixed2(a, b)
	f.items[2] = c
	f.len = 3
	return f
}

// fixed4 returns a fixed attribute list holding four attributes.
pub fn fixed4(a Attr, b Attr, c Attr, d Attr) FixedAttrs {
	mut f := fixed3(a, b, c)
	f.items[3] = d
	f.len = 4
	return f
}

// attrs returns a builder preloaded with one attribute.
pub fn attrs(first Attr) Attrs {
	mut a := Attrs{}
	a.add(first)
	return a
}
