// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.token

pub enum AttrKind {
	plain           // [name]
	string          // ['name']
	number          // [123]
	bool            // [true] || [false]
	comptime_define // [if name]
}

// e.g. `@[unsafe]`
@[minify]
pub struct Attr {
pub:
	name    string // [name]
	has_arg bool
	arg     string // [name: arg]
	kind    AttrKind
	quote   u8 = `'` // quote for .string attrs: `"` or `'`
	ct_opt  bool // true for [if user_defined_name?]
	pos     token.Pos
	has_at  bool // new syntax `@[attr]`
	// original call-style metadata for `@[foo(...)]`, used by vfmt
	call_name     string
	call_arg_name string
	call_arg_idx  int = -1
pub mut:
	ct_expr   Expr // .kind == comptime_define, for [if !name]
	ct_evaled bool // whether ct_skip has been evaluated already
	ct_skip   bool // is the comptime expr *false*, filled by checker
}

pub fn (a &Attr) debug() string {
	return 'Attr{ name: "${a.name}", has_arg: ${a.has_arg}, arg: "${a.arg}", kind: ${a.kind}, ct_expr: ${a.ct_expr}, ct_opt: ${a.ct_opt}, ct_skip: ${a.ct_skip}, call_name: "${a.call_name}", call_arg_name: "${a.call_arg_name}", call_arg_idx: ${a.call_arg_idx} }'
}

// str returns the string representation without square brackets
pub fn (a &Attr) str() string {
	mut s := ''
	quote := if a.quote == `"` { '"' } else { "'" }
	mut arg := if a.has_arg {
		s += '${a.name}: '
		a.arg
	} else {
		a.name
	}
	s += match a.kind {
		.plain, .number, .bool { arg }
		.string { '${quote}${arg}${quote}' }
		.comptime_define { 'if ${arg}' }
	}

	return s
}

pub fn (attrs []Attr) contains(str string) bool {
	return attrs.any(it.name == str)
}

pub fn (attrs []Attr) contains_arg(str string, arg string) bool {
	return attrs.any(it.has_arg && it.name == str && it.arg == arg)
}

@[direct_array_access]
pub fn (attrs []Attr) find_first(aname string) ?Attr {
	for a in attrs {
		if a.name == aname {
			return a
		}
	}
	return none
}

@[direct_array_access]
pub fn (attrs []Attr) find_last(aname string) ?Attr {
	for idx := attrs.len - 1; idx > -1; idx-- {
		a := attrs[idx]
		if a.name == aname {
			return a
		}
	}
	return none
}

@[direct_array_access]
pub fn (attrs []Attr) find_comptime_define() ?int {
	for idx in 0 .. attrs.len {
		if attrs[idx].kind == .comptime_define {
			return idx
		}
	}
	return none
}
