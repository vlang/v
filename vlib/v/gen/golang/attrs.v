// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module golang

import v.ast

pub fn (mut f Gen) attrs(attrs []ast.Attr) {
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
		f.writeln('[${attr}]')
	}
}

[params]
pub struct AttrsOptions {
	inline bool
}

pub fn (mut f Gen) single_line_attrs(attrs []ast.Attr, options AttrsOptions) {
	if attrs.len == 0 {
		return
	}
	mut sorted_attrs := attrs.clone()
	sorted_attrs.sort(a.name < b.name)
	if options.inline {
		f.write(' ')
	}
	f.write('[')
	for i, attr in sorted_attrs {
		if i > 0 {
			f.write('; ')
		}
		f.write('${attr}')
	}
	f.write(']')
	if !options.inline {
		f.writeln('')
	}
}

fn inline_attrs_len(attrs []ast.Attr) int {
	if attrs.len == 0 {
		return 0
	}
	mut n := 2 // ' ['.len
	for i, attr in attrs {
		if i > 0 {
			n += 2 // '; '.len
		}
		n += '${attr}'.len
	}
	n++ // ']'.len
	return n
}
