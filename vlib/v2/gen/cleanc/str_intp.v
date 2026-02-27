// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import strings

fn (mut g Gen) gen_string_inter_literal(node ast.StringInterLiteral) {
	// Use sprintf approach with asprintf (allocates automatically)
	// Wrapped in GCC compound expression ({ ... })
	// Build format string, stripping V string delimiters from values
	mut fmt_str := strings.new_builder(64)
	for i, raw_val in node.values {
		mut val := raw_val
		// Strip only the outer string delimiters. Using trim_* here can over-strip
		// escaped quotes in edge chunks like: 'A "${x}"', leaving a trailing `\`.
		if i == 0 && val.len > 0 && val[0] in [`'`, `"`] {
			val = val[1..]
		}
		if i == node.values.len - 1 && val.len > 0 && val[val.len - 1] in [`'`, `"`] {
			val = val[..val.len - 1]
		}
		// `c_string_literal_content_to_c` escapes quotes and preserves multiline
		// content; keep only tab escaping here to preserve `\t` marker semantics.
		// Escape `%` to `%%` so literal percent signs in the V string are not
		// consumed as printf format specifiers by asprintf.
		escaped := val.replace('%', '%%').replace('\t', '\\t')
		fmt_str.write_string(escaped)
		if i < node.inters.len {
			inter := node.inters[i]
			if inter.resolved_fmt != '' {
				// Transformer already resolved the format specifier
				fmt_str.write_string(inter.resolved_fmt)
			} else {
				fmt_str.write_string(g.get_sprintf_format(inter))
			}
		}
	}
	fmt_lit := c_string_literal_content_to_c(fmt_str.str())
	g.sb.write_string('({ char* _sip; int _sil = asprintf(&_sip, ${fmt_lit}')
	// Write arguments
	for inter in node.inters {
		g.sb.write_string(', ')
		if inter.resolved_fmt != '' {
			// Transformer already transformed the arg, emit directly
			g.expr(inter.expr)
		} else {
			g.write_sprintf_arg(inter)
		}
	}
	g.sb.write_string('); ${c_v_string_expr_from_ptr_len('_sip', '_sil', false)}; })')
}

fn (mut g Gen) write_sprintf_arg(inter ast.StringInter) {
	expr_type := g.get_expr_type(inter.expr)
	expr_src := g.expr_to_string(inter.expr)
	str_fn := g.get_str_fn_for_type(expr_type) or { '' }
	// Float types: use V's str() for default formatting ('0.0' not '0.000000')
	if expr_type in ['f64', 'f32', 'float_literal'] && inter.format == .unformatted {
		str_name := if expr_type == 'f32' { 'f32__str' } else { 'f64__str' }
		g.sb.write_string('${str_name}(')
		if expr_src == '' {
			g.sb.write_string('0')
		} else {
			g.sb.write_string(expr_src)
		}
		g.sb.write_string(').str')
		return
	}
	if expr_type == 'string' {
		if expr_src == '' {
			g.sb.write_string('""')
			return
		}
		g.sb.write_string(expr_src)
		g.sb.write_string('.str')
	} else if expr_type == 'bool' {
		g.sb.write_string('(')
		if expr_src == '' {
			g.sb.write_string('false')
		} else {
			g.sb.write_string(expr_src)
		}
		g.sb.write_string(' ? "true" : "false")')
	} else if str_fn != '' {
		g.sb.write_string('${str_fn}(')
		if expr_src == '' {
			g.sb.write_string('0')
		} else {
			g.sb.write_string(expr_src)
		}
		g.sb.write_string(').str')
	} else {
		if expr_src == '' {
			g.sb.write_string('0')
		} else {
			g.sb.write_string(expr_src)
		}
	}
}

fn (mut g Gen) get_sprintf_format(inter ast.StringInter) string {
	mut fmt := '%'
	mut width := inter.width
	mut precision := inter.precision
	// Extract width/precision from format_expr if not set explicitly
	if width == 0 && precision == 0 && inter.format_expr !is ast.EmptyExpr {
		if inter.format_expr is ast.BasicLiteral {
			val := inter.format_expr.value
			if val.contains('.') {
				// ".3" or "0.3" → precision
				parts := val.split('.')
				if parts.len == 2 {
					if parts[0].len > 0 && parts[0] != '0' {
						width = parts[0].int()
					}
					precision = parts[1].int()
				}
			} else {
				// Plain number → width (e.g., "03" for zero-padded)
				if val.starts_with('0') && val.len > 1 {
					fmt += '0'
				}
				width = val.int()
			}
		} else if inter.format_expr is ast.PrefixExpr {
			// Negative alignment: -10 → left-align with width 10
			if inter.format_expr.op == .minus && inter.format_expr.expr is ast.BasicLiteral {
				fmt += '-'
				width = inter.format_expr.expr.value.int()
			}
		}
	}
	// Width
	if width > 0 {
		fmt += '${width}'
	}
	// Precision
	if precision > 0 {
		fmt += '.${precision}'
	}
	// Format specifier
	if inter.format != .unformatted {
		match inter.format {
			.decimal { fmt += 'd' }
			.float { fmt += 'f' }
			.hex { fmt += 'x' }
			.octal { fmt += 'o' }
			.character { fmt += 'c' }
			.exponent { fmt += 'e' }
			.exponent_short { fmt += 'g' }
			.binary { fmt += 'd' } // binary not supported in printf, fallback to decimal
			.pointer_address { fmt += 'p' }
			.string { fmt += 's' }
			.unformatted { fmt += 'd' }
		}
		return fmt
	}
	// Infer from expression type
	expr_type := g.get_expr_type(inter.expr)
	match expr_type {
		'string' {
			return '%s'
		}
		'int', 'i8', 'i16', 'i32' {
			return '%d'
		}
		'i64' {
			return '%lld'
		}
		'u8', 'u16', 'u32' {
			return '%u'
		}
		'u64' {
			return '%llu'
		}
		'f32', 'f64', 'float_literal' {
			// Use %s with V's str() function for default formatting,
			// since C's %f produces '0.000000' instead of V's '0.0'.
			return '%s'
		}
		'bool' {
			return '%s'
		}
		'rune' {
			return '%c'
		}
		'char' {
			return '%c'
		}
		else {
			if _ := g.get_str_fn_for_type(expr_type) {
				return '%s'
			}
			return '%d'
		}
	}
}

// get_str_fn_for_type returns the name of the str() function for a type, if one exists.
