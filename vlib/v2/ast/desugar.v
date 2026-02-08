// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// NOTE: this is just a very naive example of how it could possibly work.
// actual implementation may work during AST -> IR (or not). it may also
// need type information which we don't have here. as I said, just an example.
pub fn (match_expr &MatchExpr) desugar() Expr {
	// Keep this helper conservative for now; match lowering happens in transformer.
	return match_expr.expr
}

pub fn (or_expr &OrExpr) desugar() Expr {
	// Keep this helper conservative for now; or-lowering happens in transformer.
	return or_expr.expr
}
