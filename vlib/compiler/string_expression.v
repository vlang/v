// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

fn (p mut Parser) string_expr() {
	is_raw := p.tok == .name && p.lit == 'r'
	is_cstr := p.tok == .name && p.lit == 'c'
	if is_raw || is_cstr {
		p.next()
	}
	str := p.lit
	// No ${}, just return a simple string
	if p.peek() != .str_dollar || is_raw {
		f := if is_raw { cescaped_path(str).replace('"', '\\"') } else { format_str(str) }
		// `C.puts('hi')` => `puts("hi");`
		/*
		Calling a C function sometimes requires a call to a string method
		C.fun('ssss'.to_wide()) =>  fun(string_to_wide(tos3("ssss")))
		*/

		if (p.calling_c && p.peek() != .dot) || is_cstr || (p.pref.translated && p.mod == 'main') {
			if p.os == .windows && p.mod == 'ui' {
				p.gen('L"$f"')
			}
			else {
				p.gen('"$f"')
			}
		}
		else if p.is_sql {
			p.gen("'$str'")
		}
		else if p.is_js {
			p.gen('tos("$f")')
		}
		else {
			p.gen('tos3("$f")')
		}
		p.next()
		if p.scanner.is_fmt && p.tok == .not {
			// handle '$age'!
			// TODO remove this hack, do this automatically
			p.fgen(' ')
			p.check(.not)
		}
		return
	}
	$if js {
		p.error('js backend does not support string formatting yet')
	}
	p.is_alloc = true // $ interpolation means there's allocation
	mut args := '"'
	mut format := '"'
	mut complex_inter := false // for vfmt
	for p.tok == .str {
		// Add the string between %d's
		p.lit = p.lit.replace('%', '%%')
		format += format_str(p.lit)
		p.next() // skip $
		if p.tok != .str_dollar {
			continue
		}
		// Handle .dollar
		p.check(.str_dollar)
		// If there's no string after current token, it means we are in
		// a complex expression (`${...}`)
		if p.peek() != .str {
			p.fgen('{')
			complex_inter = true
		}
		// Get bool expr inside a temp var
		typ,val_ := p.tmp_expr()
		val := val_.trim_space()
		args += ', $val'
		if typ == 'string' {
			// args += '.str'
			// printf("%.*s", a.len, a.str) syntax
			args += '.len, ${val}.str'
		}
		if typ == 'ustring' {
			args += '.len, ${val}.s.str'
		}
		if typ == 'bool' {
			// args += '.len, ${val}.str'
		}
		// Custom format? ${t.hour:02d}
		custom := p.tok == .colon
		if custom {
			mut cformat := ''
			p.next()
			if p.tok == .dot {
				cformat += '.'
				p.next()
			}
			if p.tok == .minus {
				// support for left aligned formatting
				cformat += '-'
				p.next()
			}
			cformat += p.lit // 02
			p.next()
			fspec := p.lit // f
			cformat += fspec
			if fspec == 's' {
				// println('custom str F=$cformat | format_specifier: "$fspec" | typ: $typ ')
				if typ != 'string' {
					p.error('only V strings can be formatted with a :${cformat} format, but you have given "${val}", which has type ${typ}')
				}
				args = args.all_before_last('${val}.len, ${val}.str') + '${val}.str'
			}
			format += '%$cformat'
			p.next()
		}
		else {
			f := p.typ_to_fmt(typ, 0)
			if f == '' {
				is_array := typ.starts_with('array_')
				typ2 := p.table.find_type(typ)
				has_str_method := p.table.type_has_method(typ2, 'str')
				if is_array || has_str_method {
					if is_array && !has_str_method {
						p.gen_array_str(typ2)
					}
					tmp_var := p.get_tmp()
					p.cgen.insert_before('string $tmp_var = ${typ}_str(${val});')
					args = args.all_before_last(val) + '${tmp_var}.len, ${tmp_var}.str'
					format += '%.*s '
				}
				else {
					p.error('unhandled sprintf format "$typ" ')
				}
			}
			format += f
		}
		// println('interpolation format is: |${format}| args are: |${args}| ')
	}
	if complex_inter {
		p.fgen('}')
	}
  
	// p.fgen('\'')
	// println("hello %d", num) optimization.
	if p.cgen.nogen {
		return
	}
	// println: don't allocate a new string, just print	it.
	$if !windows {
		cur_line := p.cgen.cur_line.trim_space()
		if cur_line == 'println (' && p.tok != .plus {
			p.cgen.resetln(cur_line.replace('println (', 'printf('))
			p.gen('$format\\n$args')
			return
		}
	}
	// '$age'! means the user wants this to be a tmp string (uses global buffer, no allocation,
	// won't be used	again)
	// TODO remove this hack, do this automatically
	if p.tok == .not {
		p.fgen(' ')
		p.check(.not)
		p.gen('_STR_TMP($format$args)')
	}
	else {
		// Otherwise do len counting + allocation + sprintf
		p.gen('_STR($format$args)')
	}
}

