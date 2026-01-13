// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module serialise

import v.ast
// import v.eval
import math.bits
import strconv

@[noinit]
pub struct Pool {
mut:
	table &ast.Table
	// eval    eval.Eval
	structs map[ast.Type]StructInfo
	strings []StringInfo // string intern
pub:
	null_terminated bool
	intern_strings  bool
	store_relocs    bool
pub mut:
	buf               []u8
	relocs            []Reloc
	highest_alignment int
}

struct StringInfo {
	pos int
	len int
}

pub struct StructInfo {
pub mut:
	offsets []int
}

pub struct Reloc {
pub:
	pos    int
	offset int
}

pub fn (mut p Pool) type_struct_info(typ ast.Type) !StructInfo {
	ts := p.table.sym(typ)

	if ts.info !is ast.Struct {
		return error('this should never happen')
	}

	return p.structs[typ.idx()] or {
		return error('most likely no implementation for type `${ts}`. maybe it is not implemented in builtin for this backend')
	}
}

pub fn (mut p Pool) calculate_all_size_align() {
	for mut sym in p.table.type_symbols {
		if sym.idx == 0 {
			continue
		}
		_, _ := p.type_size(ast.new_type(sym.idx)) or { -1, -1 } // ignore errors right here to error later on the offending code
	}
}

pub fn (mut p Pool) type_size(typ ast.Type) !(int, int) {
	if typ.has_option_or_result() {
		return p.type_size(ast.error_type_idx)
	}
	if typ.nr_muls() > 0 {
		return p.table.pointer_size, p.table.pointer_size
	}
	mut sym := p.table.sym(typ)
	if sym.size != -1 {
		return sym.size, sym.align
	}
	mut size := 0
	mut align := 0
	match sym.kind {
		.placeholder, .void, .none, .generic_inst {}
		.voidptr, .byteptr, .charptr, .function, .usize, .isize, .any, .thread, .chan {
			size = p.table.pointer_size
			align = p.table.pointer_size
		}
		.i8, .u8, .char, .bool {
			size = 1
			align = 1
		}
		.i16, .u16 {
			size = 2
			align = 2
		}
		.i32, .u32, .rune, .f32 {
			size = 4
			align = 4
		}
		.int {
			size = 4
			align = 4
		}
		.i64, .u64, .int_literal, .f64, .float_literal {
			size = 8
			align = 8
		}
		.alias {
			size, align = p.type_size((sym.info as ast.Alias).parent_type) or {
				return error('type aliased by `${sym}`: ${err}')
			}
		}
		.struct, .string, .multi_return {
			mut max_alignment := 0
			mut total_size := 0
			mut stri := StructInfo{}
			if sym.info is ast.UnknownTypeInfo {
				// TODO: this is a big fat hack to not error right here for types that are unimplemented in builtin but still get generated in the compiler
				//         thus actually make this workable with builtin or implement builtin fully there; since the offsets are not defined for this type it will error later on... errors should only pop up when an such a type is actually used
				return error('no implementation for type `${sym}`. maybe it is not implemented in builtin for this backend')
			}
			types := if mut sym.info is ast.Struct {
				sym.info.fields.map(it.typ)
			} else {
				(sym.info as ast.MultiReturn).types
			}
			for ftyp in types {
				field_size, alignment := p.type_size(ftyp) or {
					return error('field of `${sym}`: ${err}')
				}
				if field_size == 0 {
					stri.offsets << 0
					continue
				}
				if alignment > max_alignment {
					max_alignment = alignment
				}
				padding := (alignment - total_size % alignment) % alignment
				stri.offsets << total_size + padding
				total_size += field_size + padding
			}
			p.structs[typ.idx()] = stri
			size = (total_size + max_alignment - 1) / max_alignment * max_alignment
			align = max_alignment
		}
		.sum_type, .interface, .aggregate {
			match mut sym.info {
				ast.SumType, ast.Aggregate {
					size = (sym.info.fields.len + 2) * p.table.pointer_size
					align = p.table.pointer_size
				}
				ast.Interface {
					size = (sym.info.fields.len + 2) * p.table.pointer_size
					align = p.table.pointer_size
					for etyp in sym.info.embeds {
						esize, _ := p.type_size(etyp) or {
							return error('type embedded by `${sym}`: ${err}')
						}
						size += esize - 2 * p.table.pointer_size
					}
				}
				else {
					// unreachable
				}
			}
		}
		.array_fixed {
			info := sym.info as ast.ArrayFixed
			elem_size, elem_align := p.type_size(info.elem_type) or {
				return error('element type of fixed array: ${err}')
			}
			size = info.size * elem_size
			align = elem_align
		}
		.enum {
			size, align = p.type_size((sym.info as ast.Enum).typ) or {
				return error('enum type: ${err}')
			}
		}
		// TODO: hardcoded:
		.map {
			return error('maps are not implemented')
		}
		.array {
			size = if p.table.pointer_size == 8 {
				$if new_int ? && x64 { 48 } $else { 32 }
			} else {
				24
			}
			align = p.table.pointer_size
		}
	}
	sym.size = size
	sym.align = align
	return size, align
}

@[params]
pub struct PoolOpts {
pub:
	null_terminated bool = true
	intern_strings  bool = true
	store_relocs    bool = true
}

pub fn new_pool(table &ast.Table, opts PoolOpts) Pool {
	return Pool{
		table:           table
		null_terminated: opts.null_terminated
		intern_strings:  opts.intern_strings
		store_relocs:    opts.store_relocs
	}
}

fn (mut p Pool) zero_fill(size int) {
	// TODO: eventually support a way to utilise a BSS section

	for i := 0; i < size; i++ {
		p.buf << 0
	}
}

fn (mut p Pool) alignment(align int) int {
	if align > p.highest_alignment {
		p.highest_alignment = align
	}
	padding := (align - p.buf.len % align) % align
	p.zero_fill(padding)
	pos := p.buf.len
	return pos
}

/*
fn (mut p Pool) append_struct(init ast.StructInit) ?int {
	old_len := p.buf.len

	size, align := p.type_size(v.typ)
	ts := g.table.sym(v.typ)
	ts_info := ts.info as ast.Struct

	pos := p.alignment(align)
	
	if init.fields.len == 0 && !(ts_info.fields.any(it.has_default_expr)) {
		for i := 0 ; i < size ; i++ {
			p.buf << 0
		}
		return pos
	}

	/* for i, f in ts_info.fields {
		field_to_be_set := init.fields.map(it.name).filter(f.name)
	} */

	/* for i, f in ts_info.fields {
		field_to_be_set := init.fields.map(it.name).contains(f.name)

		if !field_to_be_set {
			offset := g.structs[v.typ.idx()].offsets[i]
			offset_var := g.offset(v, f.typ, offset)

			fsize, _ := g.get_type_size_align(f.typ)
			
			if f.has_default_expr {
				g.expr(f.default_expr, f.typ)
				g.set(offset_var)
			} else {
				g.zero_fill(offset_var, fsize)
			}						
		}
	}

	for f in init.fields {
		field := ts.find_field(f.name) or {
			g.w_error('could not find field `${f.name}` on init')
		}
		
		offset := g.structs[v.typ.idx()].offsets[field.i]
		offset_var := g.offset(v, f.expected_type, offset)

		g.expr(f.expr, f.expected_type)
		g.set(offset_var)
	} */

	return pos
}*/

pub fn eval_escape_codes_raw(str string) !string {
	mut buffer := []u8{}

	mut i := 0
	for i < str.len {
		if str[i] != `\\` {
			buffer << str[i]
			i++
			continue
		}

		// skip \
		i++
		match str[i] {
			`\\`, `'`, `"` {
				buffer << str[i]
				i++
			}
			`a`, `b`, `f` {
				buffer << str[i] - u8(90)
				i++
			}
			`n` {
				buffer << `\n`
				i++
			}
			`r` {
				buffer << `\r`
				i++
			}
			`t` {
				buffer << `\t`
				i++
			}
			`u` {
				i++
				utf8 := strconv.parse_int(str[i..i + 4], 16, 16) or {
					return error('invalid \\u escape code (${str[i..i + 4]})')
				}
				i += 4
				buffer << u8(utf8)
				buffer << u8(utf8 >> 8)
			}
			`v` {
				buffer << `\v`
				i++
			}
			`x` {
				i++
				c := strconv.parse_int(str[i..i + 2], 16, 8) or {
					return error('invalid \\x escape code (${str[i..i + 2]})')
				}
				i += 2
				buffer << u8(c)
			}
			`0`...`7` {
				c := strconv.parse_int(str[i..i + 3], 8, 8) or {
					return error('invalid escape code \\${str[i..i + 3]}')
				}
				i += 3
				buffer << u8(c)
			}
			else {
				return error('invalid escape code \\${str[i]}')
			}
		}
	}

	return buffer.bytestr()
}

pub fn eval_escape_codes(str_lit ast.StringLiteral) !string {
	if str_lit.is_raw {
		return str_lit.val
	}

	return eval_escape_codes_raw(str_lit.val)
}

pub fn (mut p Pool) append_string(val string) int {
	data := val.bytes()

	if p.intern_strings {
		for str in p.strings {
			if data.len > str.len || (p.null_terminated && data.len != str.len) {
				continue
			}

			// TODO: aggressive string interning if `p.null_terminated`
			if p.buf[str.pos..str.pos + data.len] == data {
				return str.pos
			}
		}
	}

	pos := p.buf.len
	p.buf << data
	if p.null_terminated {
		p.buf << 0
	}

	p.strings << StringInfo{
		pos: pos
		len: data.len
	}

	return pos
}

pub fn (mut p Pool) append(init ast.Expr, typ ast.Type) !(int, bool) {
	match init {
		ast.BoolLiteral {
			pos := p.buf.len
			p.buf << u8(init.val)
			return pos, true
		}
		ast.FloatLiteral {
			assert typ.is_pure_float()

			mut pos := 0
			if typ == ast.f32_type {
				pos = p.alignment(4)
				p.u32(bits.f32_bits(init.val.f32()))
			} else {
				pos = p.alignment(8)
				p.u64(bits.f64_bits(init.val.f64()))
			}

			return pos, true
		}
		ast.IntegerLiteral {
			assert typ.is_pure_int()

			size, align := p.table.type_size(typ)
			pos := p.alignment(align)

			match size {
				1 {
					p.u8(u8(init.val.i8()))
				}
				2 {
					p.u16(u16(init.val.i16()))
				}
				4 {
					p.u32(u32(init.val.int()))
				}
				8 {
					p.u64(u64(init.val.i64()))
				}
				else {}
			}

			return pos, true
		}
		ast.CharLiteral {
			// 3 extra bytes for improved program correctness, thank me later
			rne := u32(eval_escape_codes_raw(init.val) or { panic('Pool.append: ${err}') }.runes()[0])
			pos := p.alignment(4)
			p.u32(rne)

			return pos, true
		}
		ast.StringLiteral {
			val := eval_escape_codes(init) or { panic('Pool.append: ${err}') }
			str_pos := p.append_string(val)

			if typ != ast.string_type {
				// c'str'
				return str_pos, true
			}

			_, align := p.type_size(ast.string_type)!
			tss := p.table.sym(ast.string_type).info as ast.Struct
			pos := p.alignment(align)

			for field in tss.fields {
				match field.name {
					'str' {
						p.ptr(str_pos)
					}
					'len' {
						p.u32(u32(val.len))
					}
					'is_lit' {
						p.u32(1)
					}
					else {
						panic('ast.string: field `${field.name}` is unknown')
					}
				}
			}

			return pos, true
		}
		else {
			size, align := p.type_size(typ)!
			pos := p.alignment(align)
			p.zero_fill(size)
			return pos, false
		}
	}
}

fn (mut p Pool) u64(v u64) {
	p.buf << u8(v)
	p.buf << u8(v >> u64(8))
	p.buf << u8(v >> u64(16))
	p.buf << u8(v >> u64(24))
	p.buf << u8(v >> u64(32))
	p.buf << u8(v >> u64(40))
	p.buf << u8(v >> u64(48))
	p.buf << u8(v >> u64(56))
}

fn (mut p Pool) u32(v u32) {
	p.buf << u8(v)
	p.buf << u8(v >> u32(8))
	p.buf << u8(v >> u32(16))
	p.buf << u8(v >> u32(24))
}

fn (mut p Pool) u16(v u16) {
	p.buf << u8(v)
	p.buf << u8(v >> u32(8))
}

fn (mut p Pool) u8(v u8) {
	p.buf << v
}

fn (mut p Pool) ptr(offset int) int {
	assert p.table.pointer_size in [1, 2, 4, 8]
	pos := p.buf.len // p.alignment(p.table.pointer_size)

	if p.store_relocs {
		p.relocs << Reloc{
			pos:    pos
			offset: offset
		}
	}

	match p.table.pointer_size {
		1 {
			p.u8(u8(offset))
		}
		2 {
			p.u16(u16(offset))
		}
		4 {
			p.u32(u32(offset))
		}
		8 {
			p.u64(u64(offset))
		}
		else {}
	}
	return pos
}
