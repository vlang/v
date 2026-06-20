// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ssa

import v2.ast
import v2.types

fn int_contract_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn int_contract_cast(type_name string, value string) ast.Expr {
	return ast.Expr(ast.CallOrCastExpr{
		lhs:  ast.Expr(ast.Ident{
			name: type_name
		})
		expr: int_contract_num(value)
	})
}

fn test_const_untyped_integer_array_serializes_as_dense_i32() {
	mut mod := Module.new('array_int_contract')
	env := types.Environment.new()
	mut b := Builder.new_with_env(mod, env)
	data := b.try_serialize_const_array(ast.ArrayInitExpr{
		exprs: [int_contract_num('1'), int_contract_num('2'),
			int_contract_num('3')]
	})
	assert data == [u8(1), 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0]
}

fn test_const_explicit_i64_array_serializes_as_dense_i64() {
	mut mod := Module.new('array_int_contract_i64')
	env := types.Environment.new()
	mut b := Builder.new_with_env(mod, env)
	data := b.try_serialize_const_array(ast.ArrayInitExpr{
		exprs: [int_contract_cast('i64', '1'), int_contract_cast('i64', '2')]
	})
	assert data == [u8(1), 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
}

fn test_const_explicit_u8_array_serializes_as_dense_u8() {
	mut mod := Module.new('array_int_contract_u8')
	env := types.Environment.new()
	mut b := Builder.new_with_env(mod, env)
	data := b.try_serialize_const_array(ast.ArrayInitExpr{
		exprs: [int_contract_cast('u8', '1'), int_contract_cast('u8', '2')]
	})
	assert data == [u8(1), 2]
}

fn test_const_explicit_rune_array_serializes_as_dense_i32() {
	mut mod := Module.new('array_int_contract_rune')
	env := types.Environment.new()
	mut b := Builder.new_with_env(mod, env)
	data := b.try_serialize_const_array(ast.ArrayInitExpr{
		exprs: [int_contract_cast('rune', '65'), int_contract_cast('rune', '8364')]
	})
	assert data == [u8(65), 0, 0, 0, 172, 32, 0, 0]
}
