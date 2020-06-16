// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast

fn (mut p Parser) sql_expr() ast.SqlExpr {
	return ast.SqlExpr{}
}
