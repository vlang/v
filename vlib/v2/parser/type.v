// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v2.ast

@[inline]
fn (mut p Parser) expect_type() ast.Expr {
	// return p.try_type() or {
	// 	p.error(err.msg())
	// }
	typ := p.try_type()
	if typ is ast.EmptyExpr {
		p.error('expecting type, got `${p.tok}`')
	}
	return typ
}

// TODO: use result or stick with empty expr?
// pub fn (mut p Parser) try_type() !ast.Expr {
fn (mut p Parser) try_type() ast.Expr {
	match p.tok {
		// pointer: `&type`
		.amp {
			p.next()
			return ast.PrefixExpr{
				op:   .amp
				expr: p.expect_type()
			}
		}
		// comptime type: `$enum` | `$struct` |  etc
		.dollar {
			p.next()
			return ast.ComptimeExpr{
				// TODO: best way to handle this?
				expr: match p.tok {
					.name {
						p.ident()
					}
					else {
						// TODO: match only allowed tokens otherwise error
						ast.Ident{
							pos:  p.pos
							name: p.tok().str()
						}
					}
				}
			}
		}
		// variadic: `...type`
		.ellipsis {
			p.next()
			// TODO: what will we use here?
			return ast.PrefixExpr{
				op:   .ellipsis
				expr: p.expect_type()
			}
		}
		// atomic | shared
		// eg. typespec in struct field with modifier. other cases handled in expr()
		.key_atomic, .key_shared {
			kind := p.tok
			p.next()
			return ast.ModifierExpr{
				kind: kind
				expr: p.expect_type()
			}
		}
		// function: `fn(type) type`
		.key_fn {
			p.next()
			return ast.Type(p.fn_type())
		}
		// nil
		.key_nil {
			p.next()
			return ast.Type(ast.NilType{})
		}
		// none
		.key_none {
			p.next()
			return ast.Type(ast.NoneType{})
		}
		// inline / anonymous struct
		.key_struct {
			p.next()
			generic_params := if p.tok == .lsbr { p.generic_list() } else { []ast.Expr{} }
			embedded, fields := p.struct_decl_fields(.v)
			// TODO: should we use this or just StructDecl
			// even though it technically is not one? hrmm
			return ast.Type(ast.AnonStructType{
				generic_params: generic_params
				embedded:       embedded
				fields:         fields
			})
		}
		// tuple (multi return): `(type, type)`
		.lpar {
			p.next()
			// expect at least two (so we otherwise error)
			mut types := [p.expect_type()]
			p.expect(.comma)
			types << p.expect_type()
			// more than two
			for p.tok == .comma {
				p.next()
				types << p.expect_type()
			}
			p.expect(.rpar)
			return ast.Type(ast.TupleType{
				types: types
			})
		}
		// array: `[]type` | `[len]type`
		.lsbr {
			p.next()
			// dynamic array
			if p.tok == .rsbr {
				p.next()
				return ast.Type(ast.ArrayType{
					elem_type: p.expect_type()
				})
			}
			// fixed array
			len_expr := p.expr(.lowest)
			p.expect(.rsbr)
			return ast.Type(ast.ArrayFixedType{
				len:       len_expr
				elem_type: p.expect_type()
			})
		}
		// name | chan | map
		.name {
			pos := p.pos
			// TODO: cleam this up
			name := p.ident_or_named_type()
			if p.tok == .lsbr && name !is ast.Type && p.tok != .semicolon {
				// TODO: is there a better solution than this. maybe it should be the
				// concern of p.fn_parameters() & p.struct_decl() rather than this?
				// `fn(param_a []type)` | `struct { field_a []type }`
				if name is ast.Ident && name.name.len + pos < p.pos {
					return name
				}
				// TODO: using ast.GenericArgs here may not be correct,
				// perhaps we should rename it to ast.GenericTypes
				// if name is ast.Ident && p.pos == pos + name.name.len { return name }
				return ast.Type(ast.GenericType{
					name:   name
					params: p.generic_list()
				})
			}
			return name
		}
		// result: `!` | `!type`
		.not {
			p.next()
			return ast.Type(ast.ResultType{
				base_type: if p.tok != .semicolon { p.try_type() } else { ast.empty_expr }
				// base_type: p.tok != .semicolon { p.try_type() or { ast.empty_expr } } else { ast.empty_expr }
			})
		}
		// option: `?` | `?type`
		.question {
			p.next()
			return ast.Type(ast.OptionType{
				base_type: if p.tok != .semicolon { p.try_type() } else { ast.empty_expr }
				// base_type: if p.tok != .semicolon { p.try_type() or { ast.empty_expr } } else { ast.empty_expr }
			})
		}
		else {
			// return error('expecting type, got `$p.tok`')
			return ast.empty_expr
		}
	}
}

// function type / signature
fn (mut p Parser) fn_type() ast.FnType {
	generic_params := if p.tok == .lsbr { p.generic_list() } else { []ast.Expr{} }
	params := p.fn_parameters()
	return ast.FnType{
		generic_params: generic_params
		params:         params
		return_type:    if p.tok != .semicolon { p.try_type() } else { ast.empty_expr }
	}
}

// `ident` | `map[type]type | `(`chan`|`chan type`) | (`thread`|`thread type`)
@[direct_array_access]
fn (mut p Parser) ident_or_named_type() ast.Expr {
	pos := p.pos
	// `map[type]type`
	if p.lit.len == 3 && p.lit[0] == `m` && p.lit[1] == `a` && p.lit[2] == `p` {
		p.next()
		if p.tok == .lsbr {
			p.next()
			key_type := p.expect_type()
			p.expect(.rsbr)
			return ast.Type(ast.MapType{
				key_type:   key_type
				value_type: p.expect_type()
			})
		}
		// struct called `map` in builtin
		return ast.Ident{
			name: 'map'
			pos:  pos
		}
	}
	// `chan` | `chan type`
	if p.lit.len == 4 && p.lit[0] == `c` && p.lit[1] == `h` && p.lit[2] == `a` && p.lit[3] == `n` {
		p.next()
		elem_type := if p.tok != .semicolon { p.try_type() } else { ast.empty_expr }
		if elem_type !is ast.EmptyExpr {
			return ast.Type(ast.ChannelType{
				elem_type: elem_type
			})
		}
		// struct called `chan` in builtin
		return ast.Ident{
			name: 'chan'
			pos:  pos
		}
	}
	// `thread` | `thread type`
	else if p.lit.len == 6 && p.lit[0] == `t` && p.lit[1] == `h` && p.lit[2] == `r`
		&& p.lit[3] == `e` && p.lit[4] == `a` && p.lit[5] == `d` {
		p.next()
		return ast.Type(ast.ThreadType{
			elem_type: if p.tok != .semicolon { p.try_type() } else { ast.empty_expr }
		})
	}
	// `ident` | `selector` - ident or type name
	return p.ident_or_selector_expr()
}

// `ident` | (`Type`|`Type[T]`) | (`mod.Type`|`mod.Type[T]`)
fn (mut p Parser) ident_or_type() ast.Expr {
	ident_or_selector := p.ident_or_selector_expr()
	if p.tok == .lsbr {
		return ast.Type(ast.GenericType{
			name:   ident_or_selector
			params: p.generic_list()
		})
	}
	return ident_or_selector
}
