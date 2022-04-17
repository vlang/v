module js

import v.ast
import v.pref

fn (mut g JsGen) comptime_if(node ast.IfExpr) {
	if !node.is_expr && !node.has_else && node.branches.len == 1 {
		if node.branches[0].stmts.len == 0 {
			// empty ifdef; result of target OS != conditional => skip
			return
		}
	}

	for i, branch in node.branches {
		if i == node.branches.len - 1 && node.has_else {
			g.writeln('else')
		} else {
			if i == 0 {
				g.write('if (')
			} else {
				g.write('else if (')
			}
			g.comptime_if_cond(branch.cond, branch.pkg_exist)
			g.writeln(')')
		}

		if node.is_expr {
			print('$branch.stmts')
			len := branch.stmts.len
			if len > 0 {
				last := branch.stmts.last() as ast.ExprStmt
				if len > 1 {
					tmp := g.new_tmp_var()
					g.inc_indent()
					g.writeln('let $tmp;')
					g.writeln('{')
					g.stmts(branch.stmts[..len - 1])
					g.write('\t$tmp = ')
					g.stmt(last)
					g.writeln('}')
					g.dec_indent()
					g.writeln('$tmp;')
				} else {
					g.stmt(last)
				}
			}
		} else {
			g.writeln('{')
			g.stmts(branch.stmts)
			g.writeln('}')
		}
	}
}

/*
// returning `false` means the statements inside the $if can be skipped
*/
// returns the value of the bool comptime expression
fn (mut g JsGen) comptime_if_cond(cond ast.Expr, pkg_exist bool) bool {
	match cond {
		ast.BoolLiteral {
			g.expr(cond)
			return true
		}
		ast.ParExpr {
			g.write('(')
			is_cond_true := g.comptime_if_cond(cond.expr, pkg_exist)
			g.write(')')
			return is_cond_true
		}
		ast.PrefixExpr {
			g.write(cond.op.str())
			return g.comptime_if_cond(cond.right, pkg_exist)
		}
		ast.PostfixExpr {
			ifdef := g.comptime_if_to_ifdef((cond.expr as ast.Ident).name, true) or {
				verror(err.msg())
				return false
			}
			g.write('$ifdef')
			return true
		}
		ast.InfixExpr {
			match cond.op {
				.and, .logical_or {
					l := g.comptime_if_cond(cond.left, pkg_exist)
					g.write(' $cond.op ')
					r := g.comptime_if_cond(cond.right, pkg_exist)
					return if cond.op == .and { l && r } else { l || r }
				}
				.key_is, .not_is {
					left := cond.left
					mut name := ''
					mut exp_type := ast.Type(0)
					got_type := (cond.right as ast.TypeNode).typ
					// Handle `$if x is Interface {`
					// mut matches_interface := 'false'
					if left is ast.TypeNode && cond.right is ast.TypeNode
						&& g.table.sym(got_type).kind == .interface_ {
						// `$if Foo is Interface {`
						interface_sym := g.table.sym(got_type)
						if interface_sym.info is ast.Interface {
							// q := g.table.sym(interface_sym.info.types[0])
							checked_type := g.unwrap_generic(left.typ)
							// TODO PERF this check is run twice (also in the checker)
							// store the result in a field
							is_true := g.table.does_type_implement_interface(checked_type,
								got_type)
							// true // exp_type in interface_sym.info.types
							if cond.op == .key_is {
								if is_true {
									g.write('1')
								} else {
									g.write('0')
								}
								return is_true
							} else if cond.op == .not_is {
								if is_true {
									g.write('0')
								} else {
									g.write('1')
								}
								return !is_true
							}
							// matches_interface = '/*iface:$got_type $exp_type*/ true'
							//}
						}
					} else if left is ast.SelectorExpr {
						name = '${left.expr}.$left.field_name'
						exp_type = g.comptime_var_type_map[name]
					} else if left is ast.TypeNode {
						// this is only allowed for generics currently, otherwise blocked by checker
						exp_type = g.unwrap_generic(left.typ)
					}

					if cond.op == .key_is {
						g.write('$exp_type == $got_type')
						return exp_type == got_type
					} else {
						g.write('$exp_type != $got_type')
						return exp_type != got_type
					}
				}
				.eq, .ne {
					// TODO Implement `$if method.args.len == 1`
					g.write('1')
					return true
				}
				else {
					return true
				}
			}
		}
		ast.Ident {
			ifdef := g.comptime_if_to_ifdef(cond.name, false) or { 'true' } // handled in checker
			g.write('$ifdef')
			return true
		}
		ast.ComptimeCall {
			g.write('$pkg_exist')
			return true
		}
		else {
			// should be unreachable, but just in case
			g.write('1')
			return true
		}
	}
}

fn (mut g JsGen) comptime_if_to_ifdef(name string, is_comptime_optional bool) ?string {
	match name {
		// platforms/os-es:
		'windows' {
			return '(\$process.platform == "windows")'
		}
		'ios' {
			return '(\$process.platform == "darwin")'
		}
		'macos' {
			return '(\$process.platform == "darwin")'
		}
		'mach' {
			return '(\$process.platform == "darwin")'
		}
		'darwin' {
			return '(\$process.platform == "darwin")'
		}
		'linux' {
			return '(\$process.platform == "linux")'
		}
		'freebsd' {
			return '(\$process.platform == "freebsd")'
		}
		'openbsd' {
			return '(\$process.platform == "openbsd")'
		}
		'bsd' {
			return '(\$process.platform == "freebsd" || (\$process.platform == "openbsd"))'
		}
		'android' {
			return '(\$process.platform == "android")'
		}
		'solaris' {
			return '(\$process.platform == "sunos")'
		}
		'js_node' {
			if g.pref.backend == .js_node {
				return 'true'
			} else {
				return 'false'
			}
		}
		'js_freestanding' {
			if g.pref.backend == .js_freestanding {
				return 'true'
			} else {
				return 'false'
			}
		}
		'js_browser' {
			if g.pref.backend == .js_browser {
				return 'true'
			} else {
				return 'false'
			}
		}
		'es5' {
			if g.pref.output_es5 {
				return 'true'
			} else {
				return 'false'
			}
		}
		//
		'js' {
			return 'true'
		}
		// compilers:
		'gcc' {
			return 'false'
		}
		'tinyc' {
			return 'false'
		}
		'clang' {
			return 'false'
		}
		'mingw' {
			return 'false'
		}
		'msvc' {
			return 'false'
		}
		'cplusplus' {
			return 'false'
		}
		// other:
		'threads' {
			return 'false'
		}
		'gcboehm' {
			return 'false'
		}
		// todo(playX): these should return true or false depending on CLI options
		'debug' {
			return 'false'
		}
		'prod' {
			return 'false'
		}
		'test' {
			return 'false'
		}
		'glibc' {
			return 'false'
		}
		'prealloc' {
			return 'false'
		}
		'no_bounds_checking' {
			return 'checkDefine("CUSTOM_DEFINE_no_bounds_checking")'
		}
		'freestanding' {
			return '_VFREESTANDING'
		}
		// architectures:
		'amd64' {
			return '(\$process.arch == "x64")'
		}
		'aarch64', 'arm64' {
			return '(\$process.arch == "arm64)'
		}
		// bitness:
		'x64' {
			return '(\$process.arch == "x64")'
		}
		'x32' {
			return '(\$process.arch == "x32")'
		}
		// endianness:
		'little_endian' {
			return '(\$os.endianess == "LE")'
		}
		'big_endian' {
			return '(\$os.endianess == "BE")'
		}
		else {
			if is_comptime_optional
				|| (g.pref.compile_defines_all.len > 0 && name in g.pref.compile_defines_all) {
				return 'checkDefine("CUSTOM_DEFINE_$name")'
			}
			return error('bad os ifdef name "$name"') // should never happen, caught in the checker
		}
	}
	return none
}
