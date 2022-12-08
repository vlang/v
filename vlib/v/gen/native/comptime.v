// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import v.ast

fn (mut g Gen) comptime_at(node ast.AtExpr) string {
	return node.val
}

fn (mut g Gen) comptime_conditional(node ast.IfExpr) ?[]ast.Stmt {
	if node.branches.len == 0 {
		return none
	}

	for i, branch in node.branches {
		// handle $else branch, which does not have a condition
		if (node.has_else && i + 1 == node.branches.len) || g.comptime_is_truthy(branch.cond) {
			return branch.stmts
		}
	}

	return none
}

fn (mut g Gen) comptime_is_truthy(cond ast.Expr) bool {
	match cond {
		ast.BoolLiteral {
			return cond.val
		}
		ast.ParExpr {
			return g.comptime_is_truthy(cond.expr)
		}
		ast.PrefixExpr {
			match cond.op {
				.not {
					return !g.comptime_is_truthy(cond.right)
				}
				else {
					g.n_error('Compile time infix expr `${cond}` is not handled by the native backed.')
				}
			}
		}
		ast.PostfixExpr {
			return g.comptime_ident((cond.expr as ast.Ident).name, true)
		}
		ast.InfixExpr {
			match cond.op {
				.logical_or {
					return g.comptime_is_truthy(cond.left) || g.comptime_is_truthy(cond.right)
				}
				.and {
					return g.comptime_is_truthy(cond.left) && g.comptime_is_truthy(cond.right)
				}
				.eq {
					return g.comptime_is_truthy(cond.left) == g.comptime_is_truthy(cond.right)
				}
				.ne {
					return g.comptime_is_truthy(cond.left) != g.comptime_is_truthy(cond.right)
				}
				else {
					g.n_error('Compile time infix expr `${cond}` is not handled by the native backend.')
				}
			}
		}
		ast.Ident {
			return g.comptime_ident(cond.name, false)
		}
		ast.ComptimeCall {
			g.n_error('Comptime calls are not implemented')
		}
		else {
			// should be unreachable
			g.n_error('Compile time conditional `${cond}` is not handled by the native backend.')
		}
	}
	return false
}

fn (mut g Gen) comptime_ident(name string, is_comptime_optional bool) bool {
	return match name {
		//
		// Operating systems
		//
		'windows' {
			g.pref.os == .windows
		}
		'ios' {
			g.pref.os == .ios
		}
		'macos', 'mac', 'darwin' {
			g.pref.os == .macos
		}
		'linux' {
			g.pref.os == .linux
		}
		'serenity' {
			g.pref.os == .serenity
		}
		'vinix' {
			g.pref.os == .vinix
		}
		'freebsd' {
			g.pref.os == .freebsd
		}
		'openbsd' {
			g.pref.os == .openbsd
		}
		'netbsd' {
			g.pref.os == .netbsd
		}
		'bsd' {
			g.pref.os in [.freebsd, .openbsd, .netbsd]
		}
		'dragonfly' {
			g.pref.os == .dragonfly
		}
		'android' {
			g.pref.os == .android
		}
		'termux' {
			g.pref.os == .termux
		}
		'solaris' {
			g.pref.os == .solaris
		}
		'haiku' {
			g.pref.os == .haiku
		}
		//
		// C compilers, these will probably always be false
		//
		'gcc' {
			g.pref.ccompiler_type == .gcc
		}
		'tinyc' {
			g.pref.ccompiler_type == .tinyc
		}
		'clang' {
			g.pref.ccompiler_type == .clang
		}
		'mingw' {
			g.pref.ccompiler_type == .mingw
		}
		'msvc' {
			g.pref.ccompiler_type == .msvc
		}
		'cplusplus' {
			g.pref.ccompiler_type == .cplusplus
		}
		//
		// Platforms
		//
		'amd64', 'x64' {
			g.pref.arch == .amd64
		}
		'arm64' {
			g.pref.arch == .arm64
		}
		'x86' {
			false // native only supports 64-bit systems
		}
		'little_endian' {
			true // all systems targeted by native should be little-endian
		}
		'big_endian' {
			false // all systems targeted by native should be little-endian
		}
		//
		// Other
		//
		'debug' {
			g.pref.is_debug
		}
		'prod' {
			g.pref.is_prod
		}
		'test' {
			g.pref.is_test
		}
		'js' {
			g.pref.arch == .js_node
		}
		'glibc' {
			g.pref.is_glibc
		}
		'prealloc' {
			g.pref.prealloc
		}
		'no_bounds_checking' {
			false // TODO
		}
		'freestanding' {
			g.pref.arch == .js_freestanding
		}
		'no_segfault_handler' {
			false // TODO
		}
		'no_backtrace' {
			false // TODO
		}
		'no_main' {
			g.pref.is_script
		}
		else {
			if is_comptime_optional
				|| (g.pref.compile_defines_all.len > 0 && name in g.pref.compile_defines_all) {
				true
			} else {
				g.n_error('Unhandled os ifdef name "${name}".')
				false
			}
		}
	}
}
