module parser

import ast
import v.pref

// https://www.felixcloutier.com/x86/lock
const allowed_lock_prefix_ins = ['add', 'adc', 'and', 'btc', 'btr', 'bts', 'cmpxchg', 'cmpxchg8b',
	'cmpxchg16b', 'dec', 'inc', 'neg', 'not', 'or', 'sbb', 'sub', 'xor', 'xadd', 'xchg']

fn (mut p Parser) asm_stmt(is_top_level bool) ast.AsmStmt {
	p.inside_asm = true
	p.inside_asm_template = true
	defer {
		p.inside_asm = false
		p.inside_asm_template = false
	}
	p.n_asm = 0
	if is_top_level {
		p.top_level_statement_start()
	}
	mut backup_scope := p.scope

	pos := p.tok.pos()

	p.check(.key_asm)
	mut arch := pref.arch_from_string(p.tok.lit) or { pref.Arch._auto }

	if is_top_level && arch == .wasm32 {
		p.error("wasm doesn't support toplevel assembly")
	}

	mut is_volatile := false
	mut is_goto := false
	if p.tok.kind == .key_volatile {
		arch = pref.arch_from_string(p.peek_tok.lit) or { pref.Arch._auto }
		is_volatile = true
		p.next()
	} else if p.tok.kind == .key_goto {
		arch = pref.arch_from_string(p.peek_tok.lit) or { pref.Arch._auto }
		is_goto = true
		p.next()
	}
	if arch == ._auto && !p.pref.is_fmt {
		if p.tok.lit == '' {
			p.error('missing assembly architecture. Try i386, amd64, arm64, or wasm.')
		}
		p.error('unknown assembly architecture')
	}
	if p.tok.kind != .name {
		p.error('must specify assembly architecture')
	} else {
		p.next()
	}

	p.check_for_impure_v(ast.pref_arch_to_table_language(arch), p.prev_tok.pos())

	p.check(.lcbr)
	p.scope = &ast.Scope{
		parent:               unsafe { nil } // you shouldn't be able to reference other variables in assembly blocks
		detached_from_parent: true
		start_pos:            p.tok.pos
		objects:              ast.all_registers(mut p.table, arch) //
	}

	mut local_labels := []string{}
	// riscv: https://github.com/jameslzhu/riscv-card/releases/download/latest/riscv-card.pdf
	// x86: https://www.felixcloutier.com/x86/
	// arm: https://developer.arm.com/documentation/dui0068/b/arm-instruction-reference
	mut templates := []ast.AsmTemplate{}
	for p.tok.kind !in [.semicolon, .rcbr, .eof] {
		template_pos := p.tok.pos()
		mut name := ''
		mut comments := []ast.Comment{}
		if p.tok.kind == .name && arch == .amd64 && p.tok.lit in ['rex', 'vex', 'xop'] {
			name += p.tok.lit
			p.next()
			for p.tok.kind == .dot {
				p.next()
				name += '.' + p.tok.lit
				p.check(.name)
			}
			name += ' '
		}
		is_directive := p.tok.kind == .dot
		if is_directive {
			p.next()
		}
		if p.tok.kind in [.key_in, .key_lock, .key_orelse, .key_select, .key_return] { // `in`, `lock`, `or`, `select`, `return` are v keywords that are also x86/arm/riscv/wasm instructions.
			name += p.tok.kind.str()
			if p.tok.kind == .key_lock && arch in [.i386, .amd64] {
				p.next()

				has_suffix := p.tok.lit[p.tok.lit.len - 1] in [`b`, `w`, `l`, `q`]
				if !(p.tok.lit in allowed_lock_prefix_ins
					|| (has_suffix && p.tok.lit[0..p.tok.lit.len - 1] in allowed_lock_prefix_ins)) {
					p.error('The lock prefix cannot be used on this instruction')
				}
				name += ' '
				name += p.tok.lit
			}
			p.next()
		} else if p.tok.kind == .number {
			name += p.tok.lit
			p.next()
		} else if p.tok.kind == .comment {
			for p.tok.kind == .comment {
				comments << p.comment()
			}
		} else {
			name += p.tok.lit
			p.check(.name)
		}
		// dots are part of instructions for some riscv extensions and webassembly, arm64
		if arch in [.rv32, .rv64, .wasm32, .arm64, .loongarch64] {
			for p.tok.kind == .dot {
				name += '.'
				p.next()
				// wasm: i32.const
				if arch == .wasm32 && p.tok.kind == .key_const {
					name += 'const'
					p.next()
				} else {
					name += p.tok.lit
					p.check(.name)
				}
			}
		}
		mut is_label := false

		mut args := []ast.AsmArg{}
		if p.tok.line_nr == p.prev_tok.line_nr {
			args_loop: for {
				if p.prev_tok.pos().line_nr < p.tok.pos().line_nr {
					break
				}
				mut segment := ''
				if p.tok.kind == .name && p.peek_tok.kind == .colon {
					segment = p.tok.lit
					p.next()
					p.next()
				}
				match p.tok.kind {
					.name {
						args << p.reg_or_alias()
					}
					.string {
						// wasm: call 'wasi_unstable' 'proc_exit'
						args << p.tok.lit
						p.next()
					}
					.number {
						number_lit := p.parse_number_literal()
						match number_lit {
							ast.FloatLiteral {
								args << ast.FloatLiteral{
									...number_lit
								}
							}
							ast.IntegerLiteral {
								if is_directive {
									args << ast.AsmDisp{
										val: number_lit.val
										pos: number_lit.pos
									}
								} else {
									args << ast.IntegerLiteral{
										...number_lit
									}
								}
							}
							else {
								p.error('p.parse_number_literal() invalid output: `${number_lit}`')
							}
						}
					}
					.chartoken {
						args << ast.CharLiteral{
							val: p.tok.lit
							pos: p.tok.pos()
						}
						p.next()
					}
					.colon {
						is_label = true
						p.next()
						local_labels << name
						break
					}
					.lsbr {
						if arch == .wasm32 {
							p.error("wasm doesn't have addressing operands")
						}
						mut addressing := p.asm_addressing()
						addressing.segment = segment
						args << addressing
					}
					.rcbr {
						break
					}
					.semicolon {
						break
					}
					else {
						p.error('invalid token in assembly block')
					}
				}
				if p.tok.kind == .comma {
					p.next()
				} else {
					break
				}
			}
			// if p.prev_tok.pos().line_nr < p.tok.pos().line_nr {
			// 	break
			// }
		}
		for p.tok.kind == .comment {
			comments << p.comment()
		}
		if is_directive && name in ['globl', 'global'] {
			for arg in args {
				p.global_labels << (arg as ast.AsmAlias).name
			}
		}
		templates << ast.AsmTemplate{
			name:         name
			args:         args
			comments:     comments
			is_label:     is_label
			is_directive: is_directive
			pos:          template_pos.extend(p.tok.pos())
		}
	}
	mut scope := p.scope
	p.scope = backup_scope
	p.inside_asm_template = false
	mut output, mut input, mut clobbered, mut global_labels := []ast.AsmIO{}, []ast.AsmIO{}, []ast.AsmClobbered{}, []string{}
	if !is_top_level {
		if p.tok.kind == .semicolon {
			output = p.asm_ios(true)
			if p.tok.kind == .semicolon {
				input = p.asm_ios(false)
			}
			if p.tok.kind == .semicolon {
				// because p.reg_or_alias() requires the scope with registers to recognize registers.
				backup_scope = p.scope
				p.scope = scope
				p.next()
				for p.tok.kind == .name {
					reg := ast.AsmRegister{
						name: p.tok.lit
						typ:  0
						size: -1
					}
					p.next()

					mut comments := []ast.Comment{}
					for p.tok.kind == .comment {
						comments << p.comment()
					}
					clobbered << ast.AsmClobbered{
						reg:      reg
						comments: comments
					}

					if p.tok.kind in [.rcbr, .semicolon] {
						break
					}
				}

				if is_goto && p.tok.kind == .semicolon {
					p.next()
					for p.tok.kind == .name {
						global_labels << p.tok.lit
						p.next()
					}
				}
			}
		}
	} else if p.tok.kind == .semicolon {
		p.error('extended assembly is not allowed as a top level statement')
	}
	p.scope = backup_scope
	p.check(.rcbr)
	if is_top_level {
		p.top_level_statement_end()
	}
	scope.end_pos = p.prev_tok.pos

	return ast.AsmStmt{
		arch:          arch
		is_goto:       is_goto
		is_volatile:   is_volatile
		templates:     templates
		output:        output
		input:         input
		clobbered:     clobbered
		pos:           pos.extend(p.prev_tok.pos())
		is_basic:      is_top_level || output.len + input.len + clobbered.len == 0
		scope:         scope
		global_labels: global_labels
		local_labels:  local_labels
	}
}

fn (mut p Parser) reg_or_alias() ast.AsmArg {
	p.check(.name)
	if p.prev_tok.lit in p.scope.objects {
		x := unsafe { p.scope.objects[p.prev_tok.lit] }
		if x is ast.AsmRegister {
			return ast.AsmArg(x as ast.AsmRegister)
		} else {
			p.error('non-register ast.ScopeObject found in scope')
			return ast.AsmDisp{} // should not be reached
		}
	} else if p.prev_tok.len >= 2 && p.prev_tok.lit[0] in [`b`, `f`]
		&& p.prev_tok.lit[1..].bytes().all(it.is_digit()) {
		return ast.AsmDisp{
			val: p.prev_tok.lit[1..] + p.prev_tok.lit[0].ascii_str()
		}
	} else {
		return ast.AsmAlias{
			name: p.prev_tok.lit
			pos:  p.prev_tok.pos()
		}
	}
}

// fn (mut p Parser) asm_addressing() ast.AsmAddressing {
// 	pos := p.tok.pos()
// 	p.check(.lsbr)
// 	unknown_addressing_mode := 'unknown addressing mode. supported ones are [displacement],	[base], [base + displacement] [index ∗ scale + displacement], [base + index ∗ scale + displacement], [base + index + displacement] [rip + displacement]'
// 	mut mode := ast.AddressingMode.invalid
// 	if p.peek_tok.kind == .rsbr {
// 		if p.tok.kind == .name {
// 			mode = .base
// 		} else if p.tok.kind == .number {
// 			mode = .displacement
// 		} else {
// 			p.error(unknown_addressing_mode)
// 		}
// 	} else if p.peek_tok.kind == .mul {
// 		mode = .index_times_scale_plus_displacement
// 	} else if p.tok.lit == 'rip' {
// 		mode = .rip_plus_displacement
// 	} else if p.peek_tok3.kind == .mul {
// 		mode = .base_plus_index_times_scale_plus_displacement
// 	} else if p.peek_tok.kind == .plus && p.peek_tok3.kind == .rsbr {
// 		mode = .base_plus_displacement
// 	} else if p.peek_tok.kind == .plus && p.peek_tok3.kind == .plus {
// 		mode = .base_plus_index_plus_displacement
// 	} else {
// 		p.error(unknown_addressing_mode)
// 	}
// 	mut displacement, mut base, mut index, mut scale := u32(0), ast.AsmArg{}, ast.AsmArg{}, -1

// 	match mode {
// 		.base {
// 			base = p.reg_or_alias()
// 		}
// 		.displacement {
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.base_plus_displacement {
// 			base = p.reg_or_alias()
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.index_times_scale_plus_displacement {
// 			index = p.reg_or_alias()
// 			p.check(.mul)
// 			scale = p.tok.lit.int()
// 			p.check(.number)
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.base_plus_index_times_scale_plus_displacement {
// 			base = p.reg_or_alias()
// 			p.check(.plus)
// 			index = p.reg_or_alias()
// 			p.check(.mul)
// 			scale = p.tok.lit.int()
// 			p.check(.number)
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.rip_plus_displacement {
// 			base = p.reg_or_alias()
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.base_plus_index_plus_displacement {
// 			base = p.reg_or_alias()
// 			p.check(.plus)
// 			index = p.reg_or_alias()
// 			p.check(.plus)
// 			displacement = p.tok.lit.u32()
// 			p.check(.number)
// 		}
// 		.invalid {} // there was already an error above
// 	}

// 	p.check(.rsbr)
// 	return ast.AsmAddressing{
// 		base: base
// 		displacement: displacement
// 		index: index
// 		scale: scale
// 		mode: mode
// 		pos: pos.extend(p.prev_tok.pos())
// 	}
// }

fn (mut p Parser) asm_addressing() ast.AsmAddressing {
	pos := p.tok.pos()
	p.check(.lsbr)
	unknown_addressing_mode := 'unknown addressing mode. supported ones are [displacement],	[base], [base + displacement], [index ∗ scale + displacement], [base + index ∗ scale + displacement], [base + index + displacement], [rip + displacement]'
	// this mess used to look much cleaner before the removal of peek_tok2/3, see above code for cleaner version
	if p.peek_tok.kind == .rsbr { // [displacement] or [base]
		if p.tok.kind == .name {
			base := p.reg_or_alias()
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode: .base
				base: base
				pos:  pos.extend(p.prev_tok.pos())
			}
		} else if p.tok.kind == .number {
			displacement := if p.tok.kind == .name {
				p.reg_or_alias()
			} else {
				x := ast.AsmArg(ast.AsmDisp{
					val: p.tok.lit
					pos: p.tok.pos()
				})
				p.check(.number)
				x
			}
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode:         .displacement
				displacement: displacement
				pos:          pos.extend(p.prev_tok.pos())
			}
		} else {
			p.error(unknown_addressing_mode)
		}
	}
	if p.peek_tok.kind == .plus && p.tok.kind == .name { // [base + displacement], [base + index ∗ scale + displacement], [base + index + displacement] or [rip + displacement]
		if p.tok.lit == 'rip' {
			rip := p.reg_or_alias()
			p.next()

			displacement := if p.tok.kind == .name {
				p.reg_or_alias()
			} else {
				x := ast.AsmArg(ast.AsmDisp{
					val: p.tok.lit
					pos: p.tok.pos()
				})
				p.check(.number)
				x
			}
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode:         .rip_plus_displacement
				base:         rip
				displacement: displacement
				pos:          pos.extend(p.prev_tok.pos())
			}
		}
		base := p.reg_or_alias()
		p.next()
		if p.peek_tok.kind == .rsbr {
			if p.tok.kind == .number {
				displacement := if p.tok.kind == .name {
					p.reg_or_alias()
				} else {
					x := ast.AsmArg(ast.AsmDisp{
						val: p.tok.lit
						pos: p.tok.pos()
					})
					p.check(.number)
					x
				}
				p.check(.rsbr)
				return ast.AsmAddressing{
					mode:         .base_plus_displacement
					base:         base
					displacement: displacement
					pos:          pos.extend(p.prev_tok.pos())
				}
			} else {
				p.error(unknown_addressing_mode)
			}
		}
		index := p.reg_or_alias()
		if p.tok.kind == .mul {
			p.next()
			scale := p.tok.lit.int()
			p.check(.number)
			p.check(.plus)
			displacement := if p.tok.kind == .name {
				p.reg_or_alias()
			} else {
				x := ast.AsmArg(ast.AsmDisp{
					val: p.tok.lit
					pos: p.tok.pos()
				})
				p.check(.number)
				x
			}
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode:         .base_plus_index_times_scale_plus_displacement
				base:         base
				index:        index
				scale:        scale
				displacement: displacement
				pos:          pos.extend(p.prev_tok.pos())
			}
		} else if p.tok.kind == .plus {
			p.next()
			displacement := if p.tok.kind == .name {
				p.reg_or_alias()
			} else {
				x := ast.AsmArg(ast.AsmDisp{
					val: p.tok.lit
					pos: p.tok.pos()
				})
				p.check(.number)
				x
			}
			p.check(.rsbr)
			return ast.AsmAddressing{
				mode:         .base_plus_index_plus_displacement
				base:         base
				index:        index
				displacement: displacement
				pos:          pos.extend(p.prev_tok.pos())
			}
		}
	}
	if p.peek_tok.kind == .mul { // [index ∗ scale + displacement]
		index := p.reg_or_alias()
		p.next()
		scale := p.tok.lit.int()
		p.check(.number)
		p.check(.plus)
		displacement := if p.tok.kind == .name {
			p.reg_or_alias()
		} else {
			x := ast.AsmArg(ast.AsmDisp{
				val: p.tok.lit
				pos: p.tok.pos()
			})
			p.check(.number)
			x
		}
		p.check(.rsbr)
		return ast.AsmAddressing{
			mode:         .index_times_scale_plus_displacement
			index:        index
			scale:        scale
			displacement: displacement
			pos:          pos.extend(p.prev_tok.pos())
		}
	}
	p.error(unknown_addressing_mode)
	return ast.AsmAddressing{}
}

fn (mut p Parser) asm_ios(output bool) []ast.AsmIO {
	mut res := []ast.AsmIO{}
	p.check(.semicolon)
	if p.tok.kind in [.rcbr, .semicolon] {
		return []
	}
	for {
		if p.tok.kind == .eof {
			p.error('reached eof in asm_ios')
			return []
		}
		pos := p.tok.pos()

		mut constraint := ''
		if p.tok.kind == .lpar {
			constraint = if output { '+r' } else { 'r' } // default constraint, though vfmt fmts to `+r` and `r`
		} else {
			constraint += match p.tok.kind {
				.assign {
					'='
				}
				.plus {
					'+'
				}
				.mod {
					'%'
				}
				.amp {
					'&'
				}
				else {
					''
				}
			}
			if constraint != '' {
				p.next()
			}
			constraint += p.tok.lit
			if p.tok.kind == .at {
				p.next()
			} else {
				if p.tok.kind == .number {
					// Numbered constraints - https://gcc.gnu.org/onlinedocs/gcc/Simple-Constraints.html
					if p.tok.lit.int() >= 10 {
						p.error_with_pos('The digit must be between 0 and 9 only', pos)
						return []
					}
					p.check(.number)
				} else {
					p.check(.name)
				}
			}
		}
		mut expr := p.expr(0)
		if mut expr is ast.ParExpr {
			expr = expr.expr
		} else {
			p.error('asm in/output must be enclosed in brackets')
			return []
		}
		mut alias := ''
		if p.tok.kind == .key_as {
			p.next()
			alias = p.tok.lit
			p.check(.name)
		} else if mut expr is ast.Ident {
			alias = expr.name
		}
		// for constraints like `a`, no alias is needed, it is referred to as rcx
		mut comments := []ast.Comment{}
		for p.tok.kind == .comment {
			comments << p.comment()
		}

		res << ast.AsmIO{
			alias:      alias
			constraint: constraint
			expr:       expr
			comments:   comments
			pos:        pos.extend(p.prev_tok.pos())
		}
		p.n_asm++
		if p.tok.kind in [.semicolon, .rcbr] {
			break
		}
	}
	return res
}
