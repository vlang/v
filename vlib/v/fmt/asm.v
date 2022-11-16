// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import v.ast

fn (mut f Fmt) asm_stmt(stmt ast.AsmStmt) {
	f.write('asm ')
	if stmt.is_volatile {
		f.write('volatile ')
	} else if stmt.is_goto {
		f.write('goto ')
	}
	f.writeln('${stmt.arch} {')
	f.indent++

	f.asm_templates(stmt.templates)

	if stmt.output.len != 0 || stmt.input.len != 0 || stmt.clobbered.len != 0 {
		f.write('; ')
	}
	f.asm_ios(stmt.output)

	if stmt.input.len != 0 || stmt.clobbered.len != 0 {
		f.write('; ')
	}
	f.asm_ios(stmt.input)

	if stmt.clobbered.len != 0 {
		f.write('; ')
	}
	f.asm_clobbered(stmt.clobbered)

	f.indent--
	f.writeln('}')
	if f.indent == 0 {
		f.writeln('')
	}
}

fn (mut f Fmt) asm_arg(arg ast.AsmArg) {
	match arg {
		ast.AsmRegister {
			f.asm_reg(arg)
		}
		ast.AsmAlias {
			f.write('${arg.name}')
		}
		ast.IntegerLiteral, ast.FloatLiteral, ast.CharLiteral {
			f.write(arg.val)
		}
		ast.BoolLiteral {
			f.write(arg.val.str())
		}
		string {
			f.write(arg)
		}
		ast.AsmAddressing {
			if arg.segment != '' {
				f.write(arg.segment)
				f.write(':')
			}
			f.write('[')
			base := arg.base
			index := arg.index
			displacement := arg.displacement
			scale := arg.scale
			match arg.mode {
				.base {
					f.asm_arg(base)
				}
				.displacement {
					f.asm_arg(displacement)
				}
				.base_plus_displacement {
					f.asm_arg(base)
					f.write(' + ')
					f.asm_arg(displacement)
				}
				.index_times_scale_plus_displacement {
					f.asm_arg(index)
					f.write(' * ${scale} + ')
					f.asm_arg(displacement)
				}
				.base_plus_index_plus_displacement {
					f.asm_arg(base)
					f.write(' + ')
					f.asm_arg(index)
					f.write(' + ')
					f.asm_arg(displacement)
				}
				.base_plus_index_times_scale_plus_displacement {
					f.asm_arg(base)
					f.write(' + ')
					f.asm_arg(index)
					f.write(' * ${scale} + ')
					f.asm_arg(displacement)
				}
				.rip_plus_displacement {
					f.asm_arg(base)
					f.write(' + ')
					f.asm_arg(displacement)
				}
				.invalid {
					panic('fmt: invalid addressing mode')
				}
			}
			f.write(']')
		}
		ast.AsmDisp {
			if arg.val.len >= 2 && arg.val[arg.val.len - 1] in [`b`, `f`]
				&& arg.val[..arg.val.len - 1].bytes().all(it.is_digit()) {
				f.write(arg.val[arg.val.len - 1].ascii_str())
				f.write(arg.val[..arg.val.len - 1])
			} else {
				f.write(arg.val)
			}
		}
	}
}

fn (mut f Fmt) asm_reg(reg ast.AsmRegister) {
	f.write(reg.name)
}

fn (mut f Fmt) asm_templates(templates []ast.AsmTemplate) {
	for template in templates {
		if template.is_directive {
			f.write('.')
		}
		f.write('${template.name}')
		if template.is_label {
			f.write(':')
		} else if template.args.len > 0 {
			f.write(' ')
		}
		for i, arg in template.args {
			f.asm_arg(arg)
			if i + 1 < template.args.len {
				f.write(', ')
			}
		}
		if template.comments.len == 0 {
			f.writeln('')
		} else {
			f.comments(template.comments, inline: false)
		}
	}
}

fn (mut f Fmt) asm_clobbered(clobbered []ast.AsmClobbered) {
	for i, clob in clobbered {
		if i != 0 {
			f.write('  ')
		}
		f.write(clob.reg.name)

		if clob.comments.len == 0 {
			f.writeln('')
		} else {
			f.comments(clob.comments, inline: false)
		}
	}
}

fn (mut f Fmt) asm_ios(ios []ast.AsmIO) {
	for i, io in ios {
		if i != 0 {
			f.write('  ')
		}

		f.write('${io.constraint} (${io.expr})')
		mut as_block := true
		if io.expr is ast.Ident {
			if io.expr.name == io.alias {
				as_block = false
			}
		}
		if as_block && io.alias != '' {
			f.write(' as ${io.alias}')
		}
		if io.comments.len == 0 {
			f.writeln('')
		} else {
			f.comments(io.comments, inline: false)
		}
	}
}
