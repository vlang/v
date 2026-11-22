// Package capi derives a C API surface from the public declarations of a V
// module. It is used by `v c-api` to release a V package as a C library.
module capi

import os
import strings
import v.flat
import v.parser
import v.pref

pub struct ApiParam {
pub:
	name        string
	type_name   string
	is_receiver bool
	is_mut      bool
}

pub struct ApiFunc {
pub:
	name        string
	params      []ApiParam
	return_type string
	doc         string
	is_method   bool
	receiver    string
pub mut:
	is_unsafe   bool
	c_name      string
	custom_name string
	// qualifier is the V import alias used when wrappers live outside the
	// module that declares the function (combined multi-module builds).
	qualifier  string
	error_code int
}

pub struct ApiField {
pub:
	name      string
	type_name string
}

pub struct ApiEnumMember {
pub:
	name  string
	value string
}

pub struct ApiType {
pub:
	name         string
	kind         string // struct or enum
	members      []string
	enum_members []ApiEnumMember
	fields       []ApiField
	doc          string
pub mut:
	c_name    string
	qualifier string
}

pub struct ApiConst {
pub:
	name  string
	value string
	doc   string
pub mut:
	c_name string
}

pub struct ApiModel {
pub:
	module_name string
	prefix      string
pub mut:
	warnings []string
	funcs    []ApiFunc
	types    []ApiType
	consts   []ApiConst
}

pub fn (m ApiModel) is_enum(name string) bool {
	for t in m.types {
		if t.kind == 'enum' && t.name == name {
			return true
		}
	}
	return false
}

pub fn (m ApiModel) is_struct(name string) bool {
	for t in m.types {
		if t.kind == 'struct' && t.name == name {
			return true
		}
	}
	return false
}

pub fn (m ApiModel) type_by_name(name string) ?ApiType {
	for t in m.types {
		if t.name == name {
			return t
		}
	}
	return none
}

// collect_model parses the non-test V files in dir and records their public
// declarations. prefix overrides the C symbol prefix; when empty, the module
// name's last segment is used.
pub fn collect_model(dir string, prefix string) !ApiModel {
	return collect_model_qualified(dir, prefix, '')
}

// collect_model_qualified records public declarations and, when qualifier is
// non-empty, marks functions for calls through an imported module alias.
pub fn collect_model_qualified(dir string, prefix string, qualifier string) !ApiModel {
	entries := os.ls(dir) or { return error('cannot list ${dir}: ${err}') }
	mut effective_prefix := if prefix == '' { '' } else { sanitize(prefix) }
	mut module_name := ''
	mut funcs := []ApiFunc{}
	mut types := []ApiType{}
	mut consts := []ApiConst{}
	mut docs := map[string]string{}
	mut parse_warnings := []string{}
	for entry in entries {
		if !entry.ends_with('.v') || entry.ends_with('_test.v') || entry.ends_with('.c.v') {
			continue
		}
		// Backend-specific files (`x.js.v`, `x.wasm.v`, `x.native.v`) are not
		// compiled by the C backend; their symbols must not enter the surface.
		if entry.split('.').len > 2 || (entry.contains('_d_') && !entry.contains('_notd_')) {
			// `_d_flag.v` files are compiled only with `-d flag`.
			continue
		}
		path := os.join_path(dir, entry)
		text := os.read_file(path) or { continue }
		collect_docs(text, mut docs)
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_text(path, text)
		for d in p.diagnostics {
			if d.severity == 'error' {
				parse_warnings << '${d.file}:${d.line}: ${d.message}'
			}
		}
		attrs_by_node := attribute_map(a)
		for ni, node in a.nodes {
			if node.kind == .module_decl {
				if module_name == '' {
					module_name = node.value
				}
				continue
			}
			if node.kind == .fn_decl {
				if node.generic_params().len > 0 {
					// Generic functions have no single C ABI signature.
					continue
				}
				attrs := attrs_by_node[ni] or { []string{} }
				is_pub := node.op == .arrow
				explicit := has_attr(attrs, 'c_api_export', 'CApiExport')
				if !is_pub && !explicit {
					continue
				}
				if has_attr(attrs, 'c_api_skip', 'CApiSkip') || is_deprecated(attrs) {
					continue
				}
				mut func := func_from_node(a, node, docs, effective_prefix, custom_name_of(attrs), qualifier)
				if is_operator_name(func.name.all_after_last('.')) {
					// Operator methods (`+`, `-`, ...) have no C-ABI spelling.
					continue
				}
				func.error_code = error_code_of(attrs)
				func.is_unsafe = 'unsafe' in attrs
				funcs << func
				continue
			}
			if node.kind == .struct_decl {
				if node.op != .arrow {
					continue
				}
				mut fields := []ApiField{}
				for ci in 0 .. node.children_count {
					field := a.child_node(node, ci)
					if field.kind == .field_decl {
						fields << ApiField{
							name:      field.value
							type_name: flat.source_type_name(field.typ)
						}
					}
				}
				types << ApiType{
					name:      node.value
					kind:      'struct'
					fields:    fields
					qualifier: qualifier
					doc:       docs['type:${node.value}']
				}
				continue
			}
			if node.kind == .enum_decl {
				if node.op != .arrow {
					continue
				}
				mut members := []string{}
				mut enum_members := []ApiEnumMember{}
				mut next_value := 0
				for i in 0 .. node.children_count {
					child := a.child_node(node, i)
					if child.kind != .enum_field {
						continue
					}
					members << child.value
					mut value := next_value.str()
					for ci in 0 .. child.children_count {
						val_child := a.child_node(child, ci)
						if val_child.kind == .int_literal {
							value = val_child.value
							break
						}
					}
					enum_members << ApiEnumMember{ name: child.value, value: value }
					if parsed := parse_int_strict(value) {
						next_value = parsed + 1
					}
				}
				types << ApiType{
					name:         node.value
					kind:         'enum'
					members:      members
					enum_members: enum_members
					qualifier:    qualifier
					doc:          docs['type:${node.value}']
				}
				continue
			}
			if node.kind == .const_decl && node.op == .arrow {
				for i in 0 .. node.children_count {
					child := a.child_node(node, i)
					if child.kind == .const_field {
						consts << ApiConst{
							name:  child.value
							value: const_value_of(text, child.value)
							doc:   docs['const:${child.value}']
						}
					}
				}
			}
		}
	}
	if module_name == '' {
		return error('no V module declaration found in ${dir}')
	}
	if effective_prefix == '' {
		effective_prefix = sanitize(module_name.all_after_last('.'))
	}
	mut model := ApiModel{
		module_name: module_name
		prefix:      effective_prefix
		funcs:       funcs
		types:       types
		consts:      consts
		warnings:    parse_warnings
	}
	for i, f in model.funcs {
		model.funcs[i].c_name = if f.custom_name != '' {
			sanitize(f.custom_name)
		} else {
			'${effective_prefix}_${sanitize(f.name)}'
		}
	}
	for i, c in model.consts {
		model.consts[i].c_name = '${effective_prefix}_get_${sanitize(c.name)}'
	}
	for i, t in model.types {
		model.types[i].c_name = '${effective_prefix}_${sanitize(t.name)}'
	}
	return model
}

// attribute_map collects `@[...]` groups, which the flat AST stores as
// directive nodes pointing back at the declaration node index.
fn attribute_map(a &flat.FlatAst) map[int][]string {
	mut out := map[int][]string{}
	for node in a.nodes {
		if node.kind != .directive || !node.value.starts_with('@attributes:') {
			continue
		}
		idx := parse_int_strict(node.value[12..]) or { continue }
		if idx < 0 {
			continue
		}
		out[idx] = node.generic_params()
	}
	return out
}

fn is_deprecated(attrs []string) bool {
	for attr in attrs {
		if attr.starts_with('deprecated') {
			return true
		}
	}
	return false
}

fn is_operator_name(name string) bool {
	if name.len == 0 {
		return true
	}
	c := name[0]
	return !((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_`)
}

fn has_attr(attrs []string, bare string, qualified string) bool {
	for attr in attrs {
		if attr == bare || attr.ends_with('.${qualified}') {
			return true
		}
	}
	return false
}

fn custom_name_of(attrs []string) string {
	for attr in attrs {
		mut marker := 'c_api_name:'
		if attr.contains('.CApiName:') {
			marker = '.CApiName:'
		}
		if idx := attr.index(marker) {
			name := attr[idx + marker.len..].trim_space()
			if name.len >= 2 && ((name.starts_with("'") && name.ends_with("'"))
				|| (name.starts_with('"') && name.ends_with('"'))) {
				return name[1..name.len - 1]
			}
			return sanitize(name)
		}
	}
	return ''
}

// parse_int_strict accepts only decimal/hex integer text, unlike V's lenient
// `.int()` which returns the leading digits of arbitrary text.
fn parse_int_strict(value string) ?int {
	s := value.trim_space()
	if s.len == 0 {
		return none
	}
	mut neg := false
	mut i := 0
	if s[0] == `-` {
		neg = true
		i = 1
	} else if s[0] == `+` {
		i = 1
	}
	if i >= s.len {
		return none
	}
	mut base := 10
	if s.len > i + 2 && s[i] == `0` && (s[i + 1] == `x` || s[i + 1] == `X`) {
		base = 16
		i += 2
	}
	if i >= s.len {
		return none
	}
	mut n := 0
	for c in s[i..] {
		d := if c >= `0` && c <= `9` {
			int(c - `0`)
		} else if base == 16 && c >= `a` && c <= `f` {
			int(c - `a`) + 10
		} else if base == 16 && c >= `A` && c <= `F` {
			int(c - `A`) + 10
		} else {
			return none
		}
		n = n * base + d
	}
	return if neg { -n } else { n }
}

fn error_code_of(attrs []string) int {
	for attr in attrs {
		mut marker := 'c_api_errno:'
		if attr.contains('.CApiErrno:') {
			marker = '.CApiErrno:'
		}
		if idx := attr.index(marker) {
			return parse_int_strict(attr[idx + marker.len..]) or { 0 }
		}
	}
	return 0
}

fn func_from_node(a &flat.FlatAst, node &flat.Node, docs map[string]string, prefix string, custom_name string, qualifier string) ApiFunc {
	mut params := []ApiParam{}
	mut receiver := ''
	for i in 0 .. node.children_count {
		child := a.child_node(node, i)
		if child.kind != .param {
			continue
		}
		typ := flat.source_type_name(child.typ)
		is_receiver := child.op == .dot
		if is_receiver {
			receiver = typ
		}
		params << ApiParam{
			name:        if child.value == '' { 'arg${i}' } else { child.value }
			type_name:   typ
			is_receiver: is_receiver
			is_mut:      child.is_mut
		}
	}
	mut ret := ''
	if node.typ != 0 && node.typ != flat.source_type_void {
		ret = flat.source_type_name(node.typ)
	}
	cname := if prefix == '' { sanitize(node.value) } else { '${prefix}_${sanitize(node.value)}' }
	// Method declarations are keyed by short name in collect_docs, while the
	// AST node value is qualified (`Type.method`).
	doc_name := if node.value.contains('.') { node.value.all_after_last('.') } else { node.value }
	return ApiFunc{
		name:        node.value
		c_name:      cname
		custom_name: custom_name
		qualifier:   qualifier
		params:      params
		return_type: ret
		doc:         docs['fn:${doc_name}']
		is_method:   receiver != ''
		receiver:    receiver
	}
}

pub fn sanitize(name string) string {
	mut out := strings.Builder{}
	for c in name {
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			out.write_byte(u8(c))
		} else {
			out.write_byte(`_`)
		}
	}
	return out.str()
}

fn collect_docs(text string, mut docs map[string]string) {
	lines := text.split('\n')
	mut pending := []string{}
	for line in lines {
		trimmed := line.trim_space()
		if trimmed.starts_with('//') {
			pending << trimmed[2..].trim_space()
			continue
		}
		if key := decl_key(trimmed) {
			docs[key] = pending.join('\n')
		}
		pending = []
	}
}

fn decl_key(line string) ?string {
	kinds := ['pub fn ', 'pub struct ', 'pub enum ', 'pub const ']
	for kind in kinds {
		if !line.starts_with(kind) {
			continue
		}
		mut rest := line[kind.len..]
		// Methods spell the receiver first: `pub fn (c &T) name(...)`.
		if kind == 'pub fn ' && rest.starts_with('(') {
			if close := rest.index(')') {
				rest = rest[close + 1..].trim_space()
			}
		}
		idx := rest.index(' ') or { rest.len }
		mut name := rest[..idx]
		if name.contains('(') {
			name = name.all_before('(')
		}
		if name.contains('[') {
			name = name.all_before('[')
		}
		mut prefix := 'const'
		if kind == 'pub fn ' {
			prefix = 'fn'
		} else if kind == 'pub struct ' || kind == 'pub enum ' {
			prefix = 'type'
		}
		return '${prefix}:${name}'
	}
	return none
}

fn const_value_of(text string, name string) string {
	lines := text.split('\n')
	for li, line in lines {
		t := line.trim_space()
		prefix := 'pub const ${name}'
		if !t.starts_with(prefix) {
			continue
		}
		rest := t[prefix.len..].trim_space()
		if !rest.starts_with('=') {
			continue
		}
		mut out := rest[1..].trim_space()
		mut depth := bracket_depth(out)
		if depth <= 0 {
			return out
		}
		for next in lines[li + 1..] {
			out += '\n' + next.trim_space()
			depth += bracket_depth(next)
			if depth <= 0 {
				break
			}
		}
		return out
	}
	return ''
}

fn bracket_depth(line string) int {
	mut depth := 0
	for c in line {
		if c == `(` || c == `[` || c == `{` {
			depth++
		} else if c == `)` || c == `]` || c == `}` {
			depth--
		}
	}
	return depth
}
