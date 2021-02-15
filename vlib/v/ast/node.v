// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.token
import v.table
import v.errors

// enum field in enum declaration
pub struct EnumField {
pub:
	name          string
	pos           token.Position
	comments      []Comment // comment after Enumfield in the same line
	next_comments []Comment // comments between current EnumField and next EnumField
	expr          Expr      // the value of current EnumField; 123 in `ename = 123`
	has_expr      bool      // true, when .expr has a value
}

pub struct Field {
pub:
	name string
	pos  token.Position
pub mut:
	typ table.Type
}

// Each V source file is represented by one ast.File structure.
// When the V compiler runs, the parser will fill an []ast.File.
// That array is then passed to V's checker.
pub struct File {
pub:
	path         string // absolute path of the source file - '/projects/v/file.v'
	path_base    string // file name - 'file.v' (useful for tracing)
	mod          Module // the module of the source file (from `module xyz` at the top)
	global_scope &Scope
pub mut:
	scope            &Scope
	stmts            []Stmt            // all the statements in the source file
	imports          []Import          // all the imports
	auto_imports     []string          // imports that were implicitely added
	embedded_files   []EmbeddedFile    // list of files to embed in the binary
	imported_symbols map[string]string // used for `import {symbol}`, it maps symbol => module.symbol
	errors           []errors.Error    // all the checker errors in the file
	warnings         []errors.Warning  // all the checker warings in the file
	generic_fns      []&FnDecl
}

pub struct IfBranch {
pub:
	cond     Expr
	pos      token.Position
	body_pos token.Position
	comments []Comment
pub mut:
	stmts     []Stmt
	smartcast bool // true when cond is `x is SumType`, set in checker.if_expr // no longer needed with union sum types TODO: remove
	scope     &Scope
}

pub struct MatchBranch {
pub:
	exprs         []Expr      // left side
	ecmnts        [][]Comment // inline comments for each left side expr
	stmts         []Stmt      // right side
	pos           token.Position
	is_else       bool
	post_comments []Comment // comments below ´... }´
pub mut:
	scope &Scope
}

pub struct SelectBranch {
pub:
	stmt          Stmt   // `a := <-ch` or `ch <- a`
	stmts         []Stmt // right side
	pos           token.Position
	comment       Comment // comment above `select {`
	is_else       bool
	is_timeout    bool
	post_comments []Comment
}

pub struct StructField {
pub:
	pos              token.Position
	type_pos         token.Position
	comments         []Comment
	default_expr     Expr
	has_default_expr bool
	attrs            []table.Attr
	is_public        bool
pub mut:
	name string
	typ  table.Type
}

pub struct StructInitField {
pub:
	expr          Expr
	pos           token.Position
	comments      []Comment
	next_comments []Comment
pub mut:
	name          string
	typ           table.Type
	expected_type table.Type
}

pub fn (node Node) position() token.Position {
	match node {
		Stmt {
			mut pos := node.position()
			if node is Import {
				for sym in node.syms {
					pos = pos.extend(sym.pos)
				}
			}
			return pos
		}
		Expr {
			return node.position()
		}
		StructField {
			return node.pos.extend(node.type_pos)
		}
		MatchBranch, SelectBranch, Field, EnumField, ConstField, StructInitField, GlobalField,
		table.Param {
			return node.pos
		}
		IfBranch {
			return node.pos.extend(node.body_pos)
		}
		ScopeObject {
			match node {
				ConstField, GlobalField, Var { return node.pos }
			}
		}
		File {
			mut pos := token.Position{}
			if node.stmts.len > 0 {
				first_pos := node.stmts.first().position()
				last_pos := node.stmts.last().position()
				pos = first_pos.extend_with_last_line(last_pos, last_pos.line_nr)
			}
			return pos
		}
	}
}

pub fn (node Node) children() []Node {
	mut children := []Node{}
	if node is Expr {
		match node {
			StringInterLiteral, Assoc, ArrayInit {
				return node.exprs.map(Node(it))
			}
			SelectorExpr, PostfixExpr, UnsafeExpr, AsCast, ParExpr, IfGuardExpr, SizeOf, Likely,
			TypeOf, ArrayDecompose {
				children << node.expr
			}
			LockExpr, OrExpr {
				return node.stmts.map(Node(it))
			}
			StructInit {
				return node.fields.map(Node(it))
			}
			AnonFn {
				children << Stmt(node.decl)
			}
			CallExpr {
				children << node.left
				children << Expr(node.or_block)
			}
			InfixExpr {
				children << node.left
				children << node.right
			}
			PrefixExpr {
				children << node.right
			}
			IndexExpr {
				children << node.left
				children << node.index
			}
			IfExpr {
				children << node.left
				children << node.branches.map(Node(it))
			}
			MatchExpr {
				children << node.cond
				children << node.branches.map(Node(it))
			}
			SelectExpr {
				return node.branches.map(Node(it))
			}
			ChanInit {
				children << node.cap_expr
			}
			MapInit {
				children << node.keys.map(Node(it))
				children << node.vals.map(Node(it))
			}
			RangeExpr {
				children << node.low
				children << node.high
			}
			CastExpr {
				children << node.expr
				children << node.arg
			}
			ConcatExpr {
				return node.vals.map(Node(it))
			}
			ComptimeCall, ComptimeSelector {
				children << node.left
			}
			else {}
		}
	} else if node is Stmt {
		match node {
			Block, DeferStmt, ForCStmt, ForInStmt, ForStmt, CompFor {
				return node.stmts.map(Node(it))
			}
			ExprStmt, AssertStmt {
				children << node.expr
			}
			InterfaceDecl {
				return node.methods.map(Node(Stmt(it)))
			}
			AssignStmt {
				children << node.left.map(Node(it))
				children << node.right.map(Node(it))
			}
			Return {
				return node.exprs.map(Node(it))
			}
			// NB: these four decl nodes cannot be merged as one branch
			StructDecl {
				return node.fields.map(Node(it))
			}
			GlobalDecl {
				return node.fields.map(Node(it))
			}
			ConstDecl {
				return node.fields.map(Node(it))
			}
			EnumDecl {
				return node.fields.map(Node(it))
			}
			FnDecl {
				if node.is_method {
					children << Node(node.receiver)
				}
				children << node.params.map(Node(it))
				children << node.stmts.map(Node(it))
			}
			else {}
		}
	} else if node is ScopeObject {
		match node {
			GlobalField, ConstField, Var { children << node.expr }
		}
	} else {
		match node {
			GlobalField, ConstField, EnumField, StructInitField {
				children << node.expr
			}
			SelectBranch {
				children << node.stmt
				children << node.stmts.map(Node(it))
			}
			IfBranch, File {
				return node.stmts.map(Node(it))
			}
			MatchBranch {
				children << node.stmts.map(Node(it))
				children << node.exprs.map(Node(it))
			}
			else {}
		}
	}
	return children
}
