// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import v.ast

pub fn (mut f Fmt) attrs(attrs []ast.Attr) {
	if attrs_have_call_syntax(attrs) {
		f.call_syntax_attrs(attrs)
		return
	}
	f.legacy_attrs(attrs)
}

fn (mut f Fmt) legacy_attrs(attrs []ast.Attr) {
	mut sorted_attrs := attrs.clone()
	// Sort the attributes. The ones with arguments come first
	sorted_attrs.sort_with_compare(fn (a &ast.Attr, b &ast.Attr) int {
		d := b.arg.len - a.arg.len
		return if d != 0 { d } else { compare_strings(b.arg, a.arg) }
	})
	for i, attr in sorted_attrs {
		if attr.arg.len == 0 {
			f.single_line_attrs(sorted_attrs[i..])
			break
		}
		f.writeln('@[${attr}]')
	}
}

fn (mut f Fmt) call_syntax_attrs(attrs []ast.Attr) {
	mut i := 0
	for i < attrs.len {
		if attrs[i].call_name.len > 0 {
			group, next_idx := attr_call_group(attrs, i)
			f.writeln('@[${attr_call_group_str(group)}]')
			i = next_idx
			continue
		}
		mut j := i
		for j < attrs.len && attrs[j].call_name.len == 0 {
			j++
		}
		f.legacy_attrs(attrs[i..j])
		i = j
	}
}

@[params]
pub struct AttrsOptions {
pub:
	same_line bool
}

pub fn (mut f Fmt) single_line_attrs(attrs []ast.Attr, options AttrsOptions) {
	if attrs.len == 0 {
		return
	}
	if attrs_have_call_syntax(attrs) {
		if options.same_line {
			f.write(' ')
		}
		f.write('@[')
		f.write(single_line_attrs_text(attrs))
		f.write(']')
		if !options.same_line {
			f.writeln('')
		}
		return
	}
	f.legacy_single_line_attrs(attrs, options)
}

fn (mut f Fmt) legacy_single_line_attrs(attrs []ast.Attr, options AttrsOptions) {
	mut sorted_attrs := attrs.clone()
	sorted_attrs.sort(a.name < b.name)
	if options.same_line {
		f.write(' ')
	}
	f.write('@[')
	for i, attr in sorted_attrs {
		if i > 0 {
			f.write('; ')
		}
		f.write('${attr}')
	}
	f.write(']')
	if !options.same_line {
		f.writeln('')
	}
}

fn inline_attrs_len(attrs []ast.Attr) int {
	if attrs.len == 0 {
		return 0
	}
	return 3 + single_line_attrs_text(attrs).len // ' [' + ']'.len
}

fn attrs_have_call_syntax(attrs []ast.Attr) bool {
	return attrs.any(it.call_name.len > 0)
}

fn single_line_attrs_text(attrs []ast.Attr) string {
	if !attrs_have_call_syntax(attrs) {
		mut sorted_attrs := attrs.clone()
		sorted_attrs.sort(a.name < b.name)
		mut parts := []string{cap: sorted_attrs.len}
		for attr in sorted_attrs {
			parts << '${attr}'
		}
		return parts.join('; ')
	}
	mut parts := []string{}
	mut i := 0
	for i < attrs.len {
		if attrs[i].call_name.len > 0 {
			group, next_idx := attr_call_group(attrs, i)
			parts << attr_call_group_str(group)
			i = next_idx
			continue
		}
		parts << '${attrs[i]}'
		i++
	}
	return parts.join('; ')
}

fn attr_call_group(attrs []ast.Attr, start int) ([]ast.Attr, int) {
	first := attrs[start]
	mut end := start + 1
	for end < attrs.len && attrs[end].call_name == first.call_name
		&& attrs[end].pos.pos == first.pos.pos {
		end++
	}
	return attrs[start..end], end
}

fn attr_call_group_str(attrs []ast.Attr) string {
	if attrs.len == 0 {
		return ''
	}
	mut ordered_attrs := attrs.clone()
	ordered_attrs.sort(a.call_arg_idx < b.call_arg_idx)
	mut args := []string{}
	for attr in ordered_attrs {
		if !attr.has_arg {
			continue
		}
		mut arg := ''
		if attr.call_arg_name.len > 0 {
			arg += '${attr.call_arg_name}: '
		}
		arg += attr_value_str(attr)
		args << arg
	}
	if args.len == 0 {
		return '${attrs[0].call_name}()'
	}
	return '${attrs[0].call_name}(${args.join(', ')})'
}

fn attr_value_str(attr ast.Attr) string {
	quote := if attr.quote == `"` { '"' } else { "'" }
	return match attr.kind {
		.plain, .number, .bool { attr.arg }
		.string { '${quote}${attr.arg}${quote}' }
		.comptime_define { 'if ${attr.arg}' }
	}
}
