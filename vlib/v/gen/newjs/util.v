module newjs

import net.urllib
import v.ast

fn (mut fc FuncContext) write(b []byte) int {
	fc.output << b

	return b.len
}

fn (mut fc FuncContext) print(str string) {
	fc.write('\t'.repeat(fc.mod_ctx.indentation).bytes())
	fc.write(str.bytes())

	fc.write('\n'.bytes())
	fc.write(fc.delayed_output)
	fc.delayed_output = []
}

fn (mut fc FuncContext) indent(f fn()) {
	fc.mod_ctx.indentation++
	f()
	fc.mod_ctx.indentation--
}

fn (mut fc FuncContext) catch_output(indent int, f fn()) []byte {
	origoutput := fc.output
	fc.output = []
	fc.mod_ctx.indentation += indent
	f()
	caught := fc.output
	fc.output = origoutput
	fc.mod_ctx.indentation -= indent
	return caught
}

fn (mut fc FuncContext) print_cond(cond bool, on_true string, on_false string) {
	if !cond {
		fc.print('/* ${on_true.replace('*/', '<star>/')} */ ${on_false}')
		return 
	}

	fc.print('${on_true}')
}

fn (mut fc FuncContext) delayed(f fn()) {
	fc.delayed_output = fc.catch_output(0, f)
}

fn (mut fc FuncContext) new_variable_with_level(name_ string, mod_level bool) string {
	if name_ == '' {
		panic('new_variable: empty name')
	}

	mut name := encode_ident(name_)

	if fc.mod_ctx.minify {
		mut i := 0

		for {
			mut offset := int('a'[0])
			if mod_level {
				offset = int('A'[0])
			}

			mut j := 0

			name = ''

			for {
				name = '${rune(offset+(j%26))}${name}'

				j = j / 26 - 1
				if j == -1 {
					break
				}

				if fc.all_vars[name] == 0 {
					break
				}

				i++
			} 
		}
	}

	n := fc.all_vars[name]
	fc.all_vars[name] = n + 1

	mut var_name := name
	if n > 0 {
		var_name = '${name}\$${n}'
	}

	if mod_level {
		mut c2 := fc.parent

		for unsafe { c2 != nil } {
			c2.all_vars[var_name] = n + 1
			c2 = c2.parent
		}
	}

	fc.local_vars << var_name

	return var_name
}

/*
fn (mut fc FuncContext) new_ident(name string, t ast.Type) &ast.Ident {
	id := &ast.Ident {
		language: .js
		tok_kind: .name
		comptime: false
		mod: fc.mod_ctx.mod.name
		name: name 
		kind: .variable 
		info: IdentVar {
			typ: t
			is_mut: true 
			is_static: false 
			is_volatile: false 
			is_optional: false 
			share: .mut_t
		}
	}
}*/

fn is_mod_level(fc &FuncContext, sym &ast.Ident) bool {
	return unsafe { sym.scope == nil || (sym.scope == fc.mod_ctx.gen.table.global_scope || sym.scope.parent == fc.mod_ctx.gen.table.global_scope) }
}

fn (mut fc FuncContext) ident_name(o &ast.Ident) string {
	if is_mod_level(fc, o) {
		fc.mod_ctx.depdendencies[o] = true 

		if o.mod != fc.mod_ctx.mod.name {
			return '${fc.mod_var_str(o.mod)}.${o.name}'
		}
	}

	name := fc.mod_ctx.object_names[o] or {
		name := fc.new_variable_with_level(o.name, is_mod_level(fc, o))
		fc.mod_ctx.object_names[o] = name 
		name 
	}

	if o.obj is ast.Var {
		if fc.mod_ctx.escaping_vars[o.obj] {
			return '${name}[0]'
		}
	}

	return name
}

fn (mut fc FuncContext) mod_var(mod &ast.Module) string {
	if mod.name == fc.mod_ctx.mod.name {
		return '\$mod'
	}

	mod_var := fc.mod_ctx.mod_vars[mod.name] or {
		'\$modules["${mod.name}"]'
	}

	return mod_var
}

fn (mut fc FuncContext) mod_var_str(mod string) string {
	if mod == fc.mod_ctx.mod.name {
		return '\$mod'
	}

	mod_var := fc.mod_ctx.mod_vars[mod] or {
		'\$modules["${mod}"]'
	}

	return mod_var
}

fn encode_ident(name string) string {
	return urllib.query_escape(name).replace('%', '$')
}

fn (fc &FuncContext) write_pos() {
	// tbd
}