// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

import v.ast

pub fn (mut g Gen) comptime_cond(cond ast.Expr, pkg_exists bool) bool {
	match cond {
		ast.BoolLiteral {
			return cond.val
		}
		ast.ParExpr {
			g.comptime_cond(cond.expr, pkg_exists)
		}
		ast.PrefixExpr {
			if cond.op == .not {
				return !g.comptime_cond(cond.right, pkg_exists)
			}
		}
		ast.InfixExpr {
			match cond.op {
				.and {
					return g.comptime_cond(cond.left, pkg_exists)
						&& g.comptime_cond(cond.right, pkg_exists)
				}
				.logical_or {
					return g.comptime_cond(cond.left, pkg_exists)
						|| g.comptime_cond(cond.right, pkg_exists)
				}
				.eq {
					return g.comptime_cond(cond.left, pkg_exists) == g.comptime_cond(cond.right,
						pkg_exists)
				}
				.ne {
					return g.comptime_cond(cond.left, pkg_exists) != g.comptime_cond(cond.right,
						pkg_exists)
				}
				// wasm doesn't support generics
				// .key_is, .not_is
				else {}
			}
		}
		ast.Ident {
			return g.comptime_if_to_ifdef(cond.name, false)
		}
		ast.ComptimeCall {
			return pkg_exists // more documentation needed here...
		}
		ast.PostfixExpr {
			return g.comptime_if_to_ifdef((cond.expr as ast.Ident).name, true)
		}
		else {}
	}
	g.w_error('wasm.comptime_cond(): unhandled node: ' + cond.type_name())
}

pub fn (mut g Gen) comptime_if_expr(node ast.IfExpr, expected ast.Type, existing_rvars []Var) {
	if !node.is_expr && !node.has_else && node.branches.len == 1 {
		if node.branches[0].stmts.len == 0 {
			// empty ifdef; result of target OS != conditional => skip
			return
		}
	}

	for i, branch in node.branches {
		has_expr := !(node.has_else && i + 1 >= node.branches.len)

		if has_expr && !g.comptime_cond(branch.cond, branch.pkg_exist) {
			continue
		}
		// !node.is_expr || cond
		// handles else case, and if cond is true
		g.rvar_expr_stmts(branch.stmts, expected, existing_rvars)
		break
	}
}

pub fn (mut g Gen) comptime_if_to_ifdef(name string, is_comptime_option bool) bool {
	match name {
		// platforms/os-es/compilers:
		'windows', 'ios', 'macos', 'mach', 'darwin', 'linux', 'freebsd', 'openbsd', 'bsd',
		'android', 'solaris', 'js_node', 'js_freestanding', 'js_browser', 'es5', 'js', 'native',
		'glibc', 'gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus', 'gcboehm', 'prealloc',
		'freestanding', 'amd64', 'aarch64', 'arm64' {
			return false
		}
		'wasm' {
			return true
		}
		//
		'debug' {
			return g.pref.is_debug
		}
		'prod' {
			return g.pref.is_prod
		}
		// wasm doesn't support testing
		'test' {
			return false
		}
		// wasm doesn't support threads
		'threads' {
			return false
		}
		'no_bounds_checking' {
			return g.pref.no_bounds_checking
		}
		// bitness:
		'x64' {
			return false
		}
		'x32' {
			return true
		}
		// endianness:
		'little_endian' {
			return true
		}
		'big_endian' {
			return false
		}
		else {
			// note: this works but there might be some things missing from what I saw in the other platforms
			//		 but it is better than nothing
			if name in g.pref.compile_defines {
				return true
			} else {
				return false
			}

			// taken from JS: what does it do??
			/*
			if is_comptime_option
				|| (g.pref.compile_defines_all.len > 0 && name in g.pref.compile_defines_all) {
				return 'checkDefine("CUSTOM_DEFINE_${name}")'
			}
			return error('bad os ifdef name "${name}"')
			*/
		}
	}
	g.w_error('wasm.comptime_if_to_ifdef(): unhandled `${name}`, is_comptime_option: ${is_comptime_option}')
}
