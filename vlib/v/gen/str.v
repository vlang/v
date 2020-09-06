// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gen

import v.ast
import v.table

fn (mut g Gen) write_str_fn_definitions() {
	// _STR function can't be defined in vlib
	g.writeln("
void _STR_PRINT_ARG(const char *fmt, char** refbufp, int *nbytes, int *memsize, int guess, ...) {
	va_list args;
	va_start(args, guess);
	// NB: (*memsize - *nbytes) === how much free space is left at the end of the current buffer refbufp
	// *memsize === total length of the buffer refbufp
	// *nbytes === already occupied bytes of buffer refbufp
	// guess === how many bytes were taken during the current vsnprintf run
	for(;;) {
		if (guess < *memsize - *nbytes) {
			guess = vsnprintf(*refbufp + *nbytes, *memsize - *nbytes, fmt, args);
			if (guess < *memsize - *nbytes) { // result did fit into buffer
				*nbytes += guess;
				break;
			}
		}
		// increase buffer (somewhat exponentially)
		*memsize += (*memsize + *memsize) / 3 + guess;
		*refbufp = (char*)realloc((void*)*refbufp, *memsize);
	}
	va_end(args);
}

string _STR(const char *fmt, int nfmts, ...) {
	va_list argptr;
	int memsize = 128;
	int nbytes = 0;
	char* buf = (char*)malloc(memsize);
	va_start(argptr, nfmts);
	for (int i=0; i<nfmts; ++i) {
		int k = strlen(fmt);
		bool is_fspec = false;
		for (int j=0; j<k; ++j) {
			if (fmt[j] == '%') {
				j++;
				if (fmt[j] != '%') {
					is_fspec = true;
					break;
				}
			}
		}
		if (is_fspec) {
			char f = fmt[k-1];
			char fup = f & 0xdf; // toupper
			bool l = fmt[k-2] == 'l';
			bool ll = l && fmt[k-3] == 'l';
			if (f == 'u' || fup == 'X' || f == 'o' || f == 'd' || f == 'c') { // int...
				if (ll) _STR_PRINT_ARG(fmt, &buf, &nbytes, &memsize, k+16, va_arg(argptr, long long));
				else if (l) _STR_PRINT_ARG(fmt, &buf, &nbytes, &memsize, k+10, va_arg(argptr, long));
				else _STR_PRINT_ARG(fmt, &buf, &nbytes, &memsize, k+8, va_arg(argptr, int));
			} else if (fup >= 'E' && fup <= 'G') { // floating point
				_STR_PRINT_ARG(fmt, &buf, &nbytes, &memsize, k+10, va_arg(argptr, double));
			} else if (f == 'p') {
				_STR_PRINT_ARG(fmt, &buf, &nbytes, &memsize, k+14, va_arg(argptr, void*));
			} else if (f == 's') { // v string
				string s = va_arg(argptr, string);
				if (fmt[k-4] == '*') { // %*.*s
					int fwidth = va_arg(argptr, int);
					if (fwidth < 0)
						fwidth -= (s.len - utf8_str_visible_length(s));
					else
						fwidth += (s.len - utf8_str_visible_length(s));
					_STR_PRINT_ARG(fmt, &buf, &nbytes, &memsize, k+s.len-4, fwidth, s.len, s.str);
				} else { // %.*s
					_STR_PRINT_ARG(fmt, &buf, &nbytes, &memsize, k+s.len-4, s.len, s.str);
				}
			} else {
				//v_panic(tos3('Invaid format specifier'));
			}
		} else {
			_STR_PRINT_ARG(fmt, &buf, &nbytes, &memsize, k);
		}
		fmt += k+1;
	}
	va_end(argptr);
	buf[nbytes] = 0;
	buf = (char*)realloc((void*)buf, nbytes+1);
#ifdef DEBUG_ALLOC
	//puts('_STR:');
	puts(buf);
#endif
#if _VAUTOFREE
	//g_cur_str = (byteptr)buf;
#endif
	return tos2((byteptr)buf);
}

string _STR_TMP(const char *fmt, ...) {
	va_list argptr;
	va_start(argptr, fmt);
	size_t len = vsnprintf(0, 0, fmt, argptr) + 1;
	va_end(argptr);
	va_start(argptr, fmt);
	vsprintf((char *)g_str_buf, fmt, argptr);
	va_end(argptr);

#ifdef DEBUG_ALLOC
	//puts('_STR_TMP:');
	//puts(g_str_buf);
#endif
	string res = tos(g_str_buf,  len);
	res.is_lit = 1;
	return res;

} // endof _STR_TMP

")
}

fn (mut g Gen) string_literal(node ast.StringLiteral) {
	if node.is_raw {
		escaped_val := node.val.replace_each(['"', '\\"', '\\', '\\\\'])
		g.write('tos_lit("$escaped_val")')
		return
	}
	escaped_val := node.val.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n'])
	if g.is_c_call || node.language == .c {
		// In C calls we have to generate C strings
		// `C.printf("hi")` => `printf("hi");`
		g.write('"$escaped_val"')
	} else {
		// TODO calculate the literal's length in V, it's a bit tricky with all the
		// escape characters.
		// Clang and GCC optimize `strlen("lorem ipsum")` to `11`
		// g.write('tos4("$escaped_val", strlen("$escaped_val"))')
		// g.write('tos4("$escaped_val", $it.val.len)')
		// g.write('_SLIT("$escaped_val")')
		g.write('tos_lit("$escaped_val")')
	}
}

// optimize string interpolation in string builders:
// `sb.writeln('a=$a')` =>
// `sb.writeln('a='); sb.writeln(a.str())`
fn (mut g Gen) string_inter_literal_sb_optimized(call_expr ast.CallExpr) {
	node := call_expr.args[0].expr as ast.StringInterLiteral
	// sb_name := g.cur_call_expr.left
	// g.go_before_stmt(0)
	g.writeln('// sb inter opt')
	is_nl := call_expr.name == 'writeln'
	// println('optimize sb $call_expr.name')
	for i, val in node.vals {
		escaped_val := val.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n', '%', '%%'])
		// if val == '' {
		// break
		// continue
		// }
		g.write('strings__Builder_write(&')
		g.expr(call_expr.left)
		g.write(', tos_lit("')
		g.write(escaped_val)
		g.writeln('"));')
		//
		if i >= node.exprs.len {
			break
		}
		// if node.expr_types.len <= i || node.exprs.len <= i {
		// continue
		// }
		if is_nl && i == node.exprs.len - 1 {
			g.write('strings__Builder_writeln(&')
		} else {
			g.write('strings__Builder_write(&')
		}
		g.expr(call_expr.left)
		g.write(', ')
		typ := node.expr_types[i]
		sym := g.table.get_type_symbol(typ)
		// if typ.is_number() {
		if sym.kind == .alias && (sym.info as table.Alias).parent_type.is_number() {
			// Handle number aliases TODO this must be more generic, handled by g.typ()?
			g.write('int_str(')
		} else {
			g.write(g.typ(typ))
			g.write('_str(')
		}
		g.expr(node.exprs[i])
		g.writeln('));')
	}
	g.writeln('')
	// println(node.vals)
	return
}

fn (mut g Gen) string_inter_literal(node ast.StringInterLiteral) {
	mut cur_line := ''
	mut tmp := ''
	free := false && !g.pref.experimental && g.pref.autofree && g.inside_call && !g.inside_return &&
		g.inside_ternary == 0 && !g.inside_const
	// && g.cur_fn != 0 &&
	// g.cur_fn.name != ''
	/*
	if false && free {
		// Save the string expr in a temporary variable, so that it can be removed after the call.
		tmp = g.new_tmp_var()
		/*
		scope := g.file.scope.innermost(node.pos.pos)
		scope.register(tmp, ast.Var{
			name: tmp
			typ: table.string_type
		})
		*/
		// g.insert_before_stmt('// str tmp var\nstring $tmp = ')
		cur_line = g.go_before_stmt(0)
		g.writeln('// free _str2 $g.inside_call')
		g.write('string $tmp = ')
		g.strs_to_free += 'string_free(&$tmp); /*tmp str*/'
	}
	*/
	g.write('_STR("')
	// Build the string with %
	mut end_string := false
	for i, val in node.vals {
		escaped_val := val.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n', '%', '%%'])
		if i >= node.exprs.len {
			if escaped_val.len > 0 {
				end_string = true
				g.write('\\000')
				g.write(escaped_val)
			}
			break
		}
		g.write(escaped_val)
		// write correct format specifier to intermediate string
		g.write('%')
		fspec := node.fmts[i]
		mut fmt := if node.pluss[i] { '+' } else { '' }
		if node.fills[i] && node.fwidths[i] >= 0 {
			fmt = '${fmt}0'
		}
		if node.fwidths[i] != 0 {
			fmt = '$fmt${node.fwidths[i]}'
		}
		if node.precisions[i] != 0 {
			fmt = '${fmt}.${node.precisions[i]}'
		}
		if fspec == `s` {
			if node.fwidths[i] == 0 {
				g.write('.*s')
			} else {
				g.write('*.*s')
			}
		} else if node.expr_types[i].is_float() {
			g.write('$fmt${fspec:c}')
		} else if node.expr_types[i].is_pointer() {
			if fspec == `p` {
				g.write('${fmt}p')
			} else {
				g.write('$fmt"PRI${fspec:c}PTR"')
			}
		} else if node.expr_types[i].is_int() {
			if fspec == `c` {
				g.write('${fmt}c')
			} else {
				g.write('$fmt"PRI${fspec:c}')
				if node.expr_types[i] in [table.i8_type, table.byte_type] {
					g.write('8')
				} else if node.expr_types[i] in [table.i16_type, table.u16_type] {
					g.write('16')
				} else if node.expr_types[i] in [table.i64_type, table.u64_type] {
					g.write('64')
				} else {
					g.write('32')
				}
				g.write('"')
			}
		} else {
			// TODO: better check this case
			g.write('$fmt"PRId32"')
		}
		if i < node.exprs.len - 1 {
			g.write('\\000')
		}
	}
	num_string_parts := if end_string { node.exprs.len + 1 } else { node.exprs.len }
	g.write('", $num_string_parts, ')
	// Build args
	for i, expr in node.exprs {
		if node.expr_types[i] == table.string_type {
			if g.inside_vweb_tmpl {
				g.write('vweb__filter(')
				g.expr(expr)
				g.write(')')
			} else {
				g.expr(expr)
			}
		} else if node.expr_types[i] == table.bool_type {
			g.expr(expr)
			g.write(' ? _SLIT("true") : _SLIT("false")')
		} else if node.expr_types[i].is_number() || node.expr_types[i].is_pointer() ||
			node.fmts[i] == `d` {
			if node.expr_types[i].is_signed() && node.fmts[i] in [`x`, `X`, `o`] {
				// convert to unsigned first befors C's integer propagation strikes
				if node.expr_types[i] == table.i8_type {
					g.write('(byte)(')
				} else if node.expr_types[i] == table.i16_type {
					g.write('(u16)(')
				} else if node.expr_types[i] == table.int_type {
					g.write('(u32)(')
				} else {
					g.write('(u64)(')
				}
				g.expr(expr)
				g.write(')')
			} else {
				g.expr(expr)
			}
		} else if node.fmts[i] == `s` {
			g.gen_expr_to_string(expr, node.expr_types[i])
		} else {
			g.expr(expr)
		}
		if node.fmts[i] == `s` && node.fwidths[i] != 0 {
			g.write(', ${node.fwidths[i]}')
		}
		if i < node.exprs.len - 1 {
			g.write(', ')
		}
	}
	g.write(')')
	if free {
		g.writeln(';')
		g.write(cur_line)
		g.write(tmp)
	}
}
