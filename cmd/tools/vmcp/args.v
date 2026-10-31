// Argument decoding and result rendering for the tools.
//
// Every tool answers in JSON: an agent reads structured output far more
// reliably than it reads prose, and the same payload can be diffed or stored.
module main

import json2 as json
import v.astjson

// Args is a decoded tool argument object.
//
// An MCP client is free to send only the properties it knows about, so a missing
// or null property reads as the zero value rather than as an error. A tool that
// genuinely requires a value checks it and reports the problem itself.
pub struct Args {
mut:
	raw   json.Any
	inner map[string]json.Any = {}
}

// decode_args reads the JSON object a tool call carries.
//
// A payload that is not an object, or one that fails to parse, yields args with
// an empty `inner`, so a tool reports a missing property instead of the client
// seeing an opaque decode failure.
pub fn decode_args(payload string) Args {
	mut args := Args{}
	if payload.trim_space() == '' {
		return args
	}
	parsed := json.decode[json.Any](payload) or { return args }
	args.raw = parsed
	args.inner = parsed.as_map()
	return args
}

// str reads a string property, or `fallback` when it is absent.
pub fn (a &Args) text(key string, fallback string) string {
	value := a.inner[key] or { return fallback }
	return value.str()
}

// required_str reads a string property that must be present and non-empty.
pub fn (a &Args) required_str(key string) !string {
	value := a.text(key, '')
	if value.trim_space() == '' {
		return error('`${key}` is required')
	}
	return value
}

// has reports whether the property was sent at all, which is how a tool tells
// "absent, use the default" from "sent empty".
pub fn (a &Args) has(key string) bool {
	return key in a.inner
}

// int reads an integer property, or `fallback` when it is absent.
pub fn (a &Args) int(key string, fallback int) int {
	value := a.inner[key] or { return fallback }
	return value.int()
}

// boolean reads a boolean property, or `fallback` when it is absent.
pub fn (a &Args) boolean(key string, fallback bool) bool {
	value := a.inner[key] or { return fallback }
	return value.bool()
}

// list reads an array-of-strings property, skipping anything that is not a
// string.
pub fn (a &Args) list(key string) []string {
	value := a.inner[key] or { return [] }
	mut out := []string{}
	for item in value.as_array() {
		out << item.str()
	}
	return out
}

// Value is one member of a JSON object, as text or as already-rendered JSON.
//
// The distinction is explicit rather than guessed from the text: a diagnostic
// message that happens to contain a brace must stay a quoted string, and only a
// caller that says `raw` gets its text inserted verbatim.
pub struct Value {
pub:
	text string
	// raw reports that `text` is rendered JSON, not plain text.
	raw bool
}

// text_value wraps plain text for `object`.
pub fn text_value(text string) Value {
	return Value{
		text: text
	}
}

// raw_value wraps rendered JSON for `object`, so a nested object or array is
// inserted without being re-parsed or quoted.
pub fn raw_value(rendered string) Value {
	return Value{
		text: rendered
		raw:  true
	}
}

// Member is one key of a JSON object and the value under it.
pub struct Member {
pub:
	key   string
	value Value
}

// text_pair builds one plain-text member for `object`.
pub fn text_pair(key string, text string) Member {
	return Member{
		key:   key
		value: text_value(text)
	}
}

// raw_pair builds one already-rendered member for `object`.
pub fn raw_pair(key string, rendered string) Member {
	return Member{
		key:   key
		value: raw_value(rendered)
	}
}

// object builds a JSON object from its members.
//
// An empty text value leaves its key out, so an absent answer does not show up as
// an empty string. A `raw` value is inserted as it stands.
pub fn object(members ...Member) string {
	mut w := astjson.Writer{}
	w.begin_object()
	for member in members {
		if member.value.raw {
			w.key_raw(member.key, member.value.text)
			continue
		}
		if member.value.text == '' {
			continue
		}
		w.key(member.key)
		w.string(member.value.text)
	}
	w.end_object()
	return w.str()
}

// string_array renders a list of strings as a JSON array.
pub fn string_array(values []string) string {
	mut w := astjson.Writer{}
	w.begin_array()
	for value in values {
		w.array_item()
		w.string(value)
	}
	w.end_array()
	return w.str()
}
