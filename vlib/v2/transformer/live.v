// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import os
import v2.ast

// inject_live_reload generates hot code reloading infrastructure for @[live] functions.
//
// Approach: function pointer indirection with direct memory patching.
// @[live] functions are recompiled via `v2 -backend arm64 -hot-fn` which extracts
// raw machine code. The code is read into an mmap'd executable page and the
// global function pointer is updated to point at it.
//
// Supports multiple @[live] functions (including methods) across different callers.
//
// 1. Injects GlobalDecl for function pointers, code page, and mtime tracking
// 2. Adds initialization code to main() (mmap code page, init pointers, read initial mtime)
// 3. Replaces calls to @[live] functions with indirect calls through global pointers
// 4. Injects reload checks at the top of for-loop bodies in ALL functions
fn (mut t Transformer) inject_live_reload(mut files []ast.File) {
	if t.live_fns.len == 0 {
		return
	}

	// Resolve source file to absolute path so the reload command works
	// regardless of the binary's working directory.
	source_file := os.real_path(t.live_source_file)

	// Build C extern function declarations
	c_decls := build_live_c_decls()

	// Build GlobalDecl for live reload state
	global_decls := t.build_live_globals()

	// Build preamble statements for main()
	preamble := t.build_live_preamble(source_file)

	// Build reload-check statements for for-loop bodies
	check_stmts := t.build_live_check(source_file)

	// Find the user file that contains main()
	for i, file in files {
		mut has_main := false
		for stmt in file.stmts {
			if stmt is ast.FnDecl && !stmt.is_method && stmt.name == 'main' {
				has_main = true
				break
			}
		}
		if !has_main {
			continue
		}

		mut new_stmts := []ast.Stmt{cap: c_decls.len + global_decls.len + file.stmts.len}

		// Prepend C function declarations
		for cd in c_decls {
			new_stmts << cd
		}

		// Prepend GlobalDecl statements
		for gd in global_decls {
			new_stmts << gd
		}

		mut changed := false
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if !stmt.is_method && stmt.name == 'main' {
					// Inject preamble + rewrite calls + inject reload checks
					mut fn_stmts := []ast.Stmt{cap: preamble.len + stmt.stmts.len}
					for ps in preamble {
						fn_stmts << ps
					}
					live_stmts, _ := t.inject_live_into_stmts(stmt.stmts, check_stmts)
					for fs in live_stmts {
						fn_stmts << fs
					}
					new_stmts << ast.FnDecl{
						attributes: stmt.attributes
						is_public:  stmt.is_public
						is_method:  stmt.is_method
						is_static:  stmt.is_static
						receiver:   stmt.receiver
						language:   stmt.language
						name:       stmt.name
						typ:        stmt.typ
						stmts:      fn_stmts
						pos:        stmt.pos
					}
					changed = true
					continue
				}
				// For ALL other functions: rewrite calls + inject reload checks into for loops
				live_stmts, fn_changed := t.inject_live_into_stmts(stmt.stmts, check_stmts)
				if fn_changed {
					new_stmts << ast.FnDecl{
						attributes: stmt.attributes
						is_public:  stmt.is_public
						is_method:  stmt.is_method
						is_static:  stmt.is_static
						receiver:   stmt.receiver
						language:   stmt.language
						name:       stmt.name
						typ:        stmt.typ
						stmts:      live_stmts
						pos:        stmt.pos
					}
					changed = true
					continue
				}
			}
			new_stmts << stmt
		}

		if changed {
			files[i] = ast.File{
				attributes: file.attributes
				mod:        file.mod
				name:       file.name
				stmts:      new_stmts
				imports:    file.imports
			}
			break
		}
	}
}

// inject_live_into_stmts walks a statement list, prepends reload-check
// statements to the body of every ForStmt, and replaces calls to @[live]
// functions with indirect calls through global pointers.
// Returns the modified stmts and whether any changes were made.
fn (t &Transformer) inject_live_into_stmts(stmts []ast.Stmt, check_stmts []ast.Stmt) ([]ast.Stmt, bool) {
	mut result := []ast.Stmt{cap: stmts.len}
	mut any_changed := false
	for stmt in stmts {
		if stmt is ast.ForStmt {
			any_changed = true
			// Prepend check stmts to the for-loop body, then rewrite calls in body
			mut new_body := []ast.Stmt{cap: check_stmts.len + stmt.stmts.len}
			for cs in check_stmts {
				new_body << cs
			}
			for fs in stmt.stmts {
				new_body << t.rewrite_live_call_in_stmt(fs)
			}
			result << ast.Stmt(ast.ForStmt{
				init:  stmt.init
				cond:  stmt.cond
				post:  stmt.post
				stmts: new_body
			})
			continue
		}
		new_stmt, changed := t.rewrite_live_call_in_stmt_b(stmt)
		if changed {
			any_changed = true
		}
		result << new_stmt
	}
	return result, any_changed
}

// rewrite_live_call_in_stmt rewrites calls to @[live] functions in a statement
// with indirect calls through global pointers. Always returns the rewritten stmt.
fn (t &Transformer) rewrite_live_call_in_stmt(stmt ast.Stmt) ast.Stmt {
	new_stmt, _ := t.rewrite_live_call_in_stmt_b(stmt)
	return new_stmt
}

// rewrite_live_call_in_stmt_b rewrites calls to @[live] functions in a statement
// with indirect calls through global pointers. Returns the stmt and whether it changed.
fn (t &Transformer) rewrite_live_call_in_stmt_b(stmt ast.Stmt) (ast.Stmt, bool) {
	if stmt is ast.ExprStmt {
		new_expr, changed := t.rewrite_live_call_in_expr(stmt.expr)
		if changed {
			return ast.Stmt(ast.ExprStmt{
				expr: new_expr
			}), true
		}
	} else if stmt is ast.AssignStmt {
		mut new_rhs := []ast.Expr{cap: stmt.rhs.len}
		mut any_changed := false
		for rhs in stmt.rhs {
			new_rhs_expr, changed := t.rewrite_live_call_in_expr(rhs)
			new_rhs << new_rhs_expr
			if changed {
				any_changed = true
			}
		}
		if any_changed {
			return ast.Stmt(ast.AssignStmt{
				op:  stmt.op
				lhs: stmt.lhs
				rhs: new_rhs
			}), true
		}
	}
	return stmt, false
}

// rewrite_live_call_in_expr checks if an expression is a call to a @[live] function
// and if so, replaces it with an indirect call through the global pointer.
fn (t &Transformer) rewrite_live_call_in_expr(expr ast.Expr) (ast.Expr, bool) {
	if expr is ast.CallExpr {
		// Check regular function calls: fn_name(args)
		if expr.lhs is ast.Ident {
			for lf in t.live_fns {
				if !lf.is_method && expr.lhs.name == lf.decl_name {
					return ast.Expr(ast.CallExpr{
						lhs:  mk_ident('__live_${lf.mangled_name}')
						args: transform_mut_args(expr.args)
					}), true
				}
			}
		}
		// Check method calls: obj.method_name(args)
		if expr.lhs is ast.SelectorExpr {
			for lf in t.live_fns {
				if lf.is_method && expr.lhs.rhs.name == lf.decl_name {
					mut new_args := []ast.Expr{cap: 1 + expr.args.len}
					new_args << ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: expr.lhs.lhs
					})
					for arg in transform_mut_args(expr.args) {
						new_args << arg
					}
					return ast.Expr(ast.CallExpr{
						lhs:  mk_ident('__live_${lf.mangled_name}')
						args: new_args
					}), true
				}
			}
		}
	}
	// Check function references in struct init: frame_fn: frame → frame_fn: __live_frame
	if expr is ast.Ident {
		for lf in t.live_fns {
			if !lf.is_method && expr.name == lf.decl_name {
				return mk_ident('__live_${lf.mangled_name}'), true
			}
		}
	}
	return expr, false
}

// transform_mut_args strips `mut` modifiers from arguments so that call_indirect
// (which has no function signature) passes the value directly.
fn transform_mut_args(args []ast.Expr) []ast.Expr {
	mut result := []ast.Expr{cap: args.len}
	for arg in args {
		if arg is ast.ModifierExpr && arg.kind == .key_mut {
			result << arg.expr
		} else {
			result << arg
		}
	}
	return result
}

// build_live_globals generates GlobalDecl statements for live reload state.
fn (t &Transformer) build_live_globals() []ast.Stmt {
	mut stmts := []ast.Stmt{}

	// __global __live_mtime : i64
	stmts << mk_global_decl('__live_mtime', 'i64')

	// __global __live_code_page : voidptr  (mmap'd RWX page for hot code)
	stmts << mk_global_decl('__live_code_page', 'voidptr')

	// __global __live_code_offset : i64  (current write offset into code page)
	stmts << mk_global_decl('__live_code_offset', 'i64')

	// Per live function: global function pointer
	for lf in t.live_fns {
		stmts << mk_global_decl('__live_${lf.mangled_name}', 'voidptr')
	}

	return stmts
}

// build_live_preamble generates the statements prepended to main() body.
// mmap a code page, init function pointers, read initial source file mtime.
fn (t &Transformer) build_live_preamble(source_file string) []ast.Stmt {
	mut stmts := []ast.Stmt{}

	// Per live function: init function pointer to original function address
	for lf in t.live_fns {
		// __live_<mangled> = voidptr(&fn_name)
		stmts << mk_assign('__live_${lf.mangled_name}', mk_cast('voidptr', ast.Expr(ast.PrefixExpr{
			op:   .amp
			expr: mk_ident(lf.mangled_name)
		})))
	}

	// __live_code_page = C.mmap(nil, 0x4000, 3, 0x1802, -1, 0)
	// PROT_READ|PROT_WRITE=3, MAP_ANON|MAP_PRIVATE|MAP_JIT=0x1802
	stmts << mk_assign('__live_code_page', mk_call('C.mmap', [
		mk_unsafe_nil(),
		mk_hex('0x4000'),
		mk_int('3'),
		mk_hex('0x1802'),
		mk_neg_one(),
		mk_int('0'),
	]))

	// mut __live_sb := [144]u8{}
	stmts << mk_decl_assign('__live_sb', mk_fixed_array_init('144', 'u8'))

	// if C.stat(c'source.v', &__live_sb[0]) == 0 {
	//     __live_mtime = unsafe { *(&i64(&__live_sb[48])) }
	// }
	stmts << mk_stat_check('__live_sb', source_file, '__live_mtime')

	return stmts
}

// build_live_check generates the reload-check statements injected at the top
// of for-loop bodies. Checks if source file changed, recompiles each @[live]
// function to raw machine code, reads it into the code page, and updates the pointer.
fn (t &Transformer) build_live_check(source_file string) []ast.Stmt {
	mut stmts := []ast.Stmt{}

	// mut __live_ns := [144]u8{}
	stmts << mk_decl_assign('__live_ns', mk_fixed_array_init('144', 'u8'))

	// if C.stat(c'source.v', &__live_ns[0]) == 0 { ... }
	stat_call := mk_call('C.stat', [
		mk_cstring(source_file),
		ast.Expr(ast.PrefixExpr{
			op:   .amp
			expr: ast.Expr(ast.IndexExpr{
				lhs:  mk_ident('__live_ns')
				expr: mk_int('0')
			})
		}),
	])

	// Inner: __live_nm := unsafe { *(&i64(&__live_ns[48])) }
	mtime_read := mk_decl_assign('__live_nm', mk_unsafe_deref_i64('__live_ns', '48'))

	// if __live_nm > __live_mtime { ... reload all live functions ... }
	mut reload_body := []ast.Stmt{}

	// __live_mtime = __live_nm
	reload_body << mk_assign('__live_mtime', mk_ident('__live_nm'))

	// __live_t0 := C.clock_gettime_nsec_np(4)  -- CLOCK_MONOTONIC_RAW
	reload_body << mk_decl_assign('__live_t0', mk_call('C.clock_gettime_nsec_np', [
		mk_int('4'),
	]))

	// C.printf(c'[live] reloading...\n')
	reload_body << mk_call_stmt('C.printf', [mk_cstring('[live] reloading...\\n')])
	reload_body << mk_call_stmt('C.fflush', [mk_unsafe_nil()])

	// Resolve v2 binary path
	v2_path := if t.pref != unsafe { nil } && t.pref.vroot.len > 0 {
		t.pref.vroot + '/cmd/v2/v2'
	} else {
		'./v2'
	}

	// For each @[live] function: recompile, fread, patch
	for lf in t.live_fns {
		bin_path := '/tmp/_hot_${lf.mangled_name}.bin'
		compile_cmd := '${v2_path} -backend arm64 -nocache -hot-fn ${lf.mangled_name} -o ${bin_path} ${source_file} >/dev/null 2>&1'

		// C.system(c'v2 -backend arm64 -nocache -hot-fn <name> -o /tmp/_hot_<name>.bin source.v ...')
		reload_body << mk_call_stmt('C.system', [mk_cstring(compile_cmd)])

		// __live_f := C.fopen(c'/tmp/_hot_<name>.bin', c'rb')
		f_var := '__live_f_${lf.mangled_name}'
		reload_body << mk_decl_assign(f_var, mk_call('C.fopen', [
			mk_cstring(bin_path),
			mk_cstring('rb'),
		]))

		// if __live_f != nil { ... read code, patch pointer ... }
		mut patch_body := []ast.Stmt{}

		// C.fseek(f, 0, 2)  -- SEEK_END
		patch_body << mk_call_stmt('C.fseek', [mk_ident(f_var),
			mk_int('0'), mk_int('2')])

		// __live_sz := int(C.ftell(f))
		sz_var := '__live_sz_${lf.mangled_name}'
		patch_body << mk_decl_assign(sz_var, mk_cast('int', mk_call('C.ftell', [
			mk_ident(f_var),
		])))

		// C.fseek(f, 0, 0)  -- SEEK_SET
		patch_body << mk_call_stmt('C.fseek', [mk_ident(f_var),
			mk_int('0'), mk_int('0')])

		// __live_dest := voidptr(u64(__live_code_page) + u64(__live_code_offset))
		dest_var := '__live_dest_${lf.mangled_name}'
		patch_body << mk_decl_assign(dest_var, mk_cast('voidptr', ast.Expr(ast.InfixExpr{
			op:  .plus
			lhs: mk_cast('u64', mk_ident('__live_code_page'))
			rhs: mk_cast('u64', mk_ident('__live_code_offset'))
		})))

		// C.mprotect(__live_code_page, 0x4000, 3)  -- PROT_READ|PROT_WRITE
		patch_body << mk_call_stmt('C.mprotect', [
			mk_ident('__live_code_page'),
			mk_hex('0x4000'),
			mk_int('3'),
		])

		// C.fread(__live_dest, 1, usize(__live_sz), f)
		patch_body << mk_call_stmt('C.fread', [
			mk_ident(dest_var),
			mk_int('1'),
			mk_cast('usize', mk_ident(sz_var)),
			mk_ident(f_var),
		])

		// C.fclose(f)
		patch_body << mk_call_stmt('C.fclose', [mk_ident(f_var)])

		// C.mprotect(__live_code_page, 0x4000, 5)  -- PROT_READ|PROT_EXEC
		patch_body << mk_call_stmt('C.mprotect', [
			mk_ident('__live_code_page'),
			mk_hex('0x4000'),
			mk_int('5'),
		])

		// C.sys_icache_invalidate(__live_code_page, 0x4000)
		patch_body << mk_call_stmt('C.sys_icache_invalidate', [
			mk_ident('__live_code_page'),
			mk_hex('0x4000'),
		])

		// __live_<name> = __live_dest
		patch_body << mk_assign('__live_${lf.mangled_name}', mk_ident(dest_var))

		// __live_code_offset = __live_code_offset + i64(__live_sz)
		// Align to 16 bytes: offset = (offset + sz + 15) & ~15
		patch_body << mk_assign('__live_code_offset', ast.Expr(ast.InfixExpr{
			op:  .amp
			lhs: ast.Expr(ast.InfixExpr{
				op:  .plus
				lhs: ast.Expr(ast.InfixExpr{
					op:  .plus
					lhs: mk_ident('__live_code_offset')
					rhs: mk_cast('i64', mk_ident(sz_var))
				})
				rhs: mk_int('15')
			})
			rhs: mk_neg('16')
		}))

		// C.printf(c'[live] patched %s (%d bytes)\n', c'<name>', __live_sz)
		patch_body << mk_call_stmt('C.printf', [
			mk_cstring('[live] patched %s (%d bytes)\\n'),
			mk_cstring(lf.mangled_name),
			mk_ident(sz_var),
		])

		// if __live_f != nil { ...patch_body... }
		reload_body << ast.Stmt(ast.ExprStmt{
			expr: ast.IfExpr{
				cond:  ast.Expr(ast.InfixExpr{
					op:  .ne
					lhs: mk_ident(f_var)
					rhs: mk_unsafe_nil()
				})
				stmts: patch_body
			}
		})
	}

	// __live_t1 := C.clock_gettime_nsec_np(4)
	reload_body << mk_decl_assign('__live_t1', mk_call('C.clock_gettime_nsec_np', [
		mk_int('4'),
	]))

	// __live_ms := (__live_t1 - __live_t0) / 1000000
	reload_body << mk_decl_assign('__live_ms', ast.Expr(ast.InfixExpr{
		op:  .div
		lhs: ast.Expr(ast.InfixExpr{
			op:  .minus
			lhs: mk_ident('__live_t1')
			rhs: mk_ident('__live_t0')
		})
		rhs: mk_int('1000000')
	}))

	// C.printf(c'[live] done (%lldms)\n', __live_ms)
	reload_body << mk_call_stmt('C.printf', [
		mk_cstring('[live] done (%lldms)\\n'),
		mk_ident('__live_ms'),
	])
	reload_body << mk_call_stmt('C.fflush', [mk_unsafe_nil()])

	// if __live_nm > __live_mtime { ...reload_body... }
	inner_if := ast.Stmt(ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  ast.Expr(ast.InfixExpr{
				op:  .gt
				lhs: mk_ident('__live_nm')
				rhs: mk_ident('__live_mtime')
			})
			stmts: reload_body
		}
	})

	// Full if C.stat(...) == 0 { mtime_read; inner_if }
	stmts << ast.Stmt(ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  ast.Expr(ast.InfixExpr{
				op:  .eq
				lhs: stat_call
				rhs: mk_int('0')
			})
			stmts: [mtime_read, inner_if]
		}
	})

	return stmts
}

// mk_global_decl generates: __global <name> : <type>
fn mk_global_decl(name string, typ_name string) ast.Stmt {
	return ast.Stmt(ast.GlobalDecl{
		fields: [
			ast.FieldDecl{
				name: name
				typ:  mk_ident(typ_name)
			},
		]
	})
}

// mk_stat_check generates:
//   if C.stat(c'<source>', &<buf_name>[0]) == 0 {
//       <mtime_var> = unsafe { *(&i64(&<buf_name>[48])) }
//   }
fn mk_stat_check(buf_name string, source_file string, mtime_var string) ast.Stmt {
	stat_call := mk_call('C.stat', [
		mk_cstring(source_file),
		ast.Expr(ast.PrefixExpr{
			op:   .amp
			expr: ast.Expr(ast.IndexExpr{
				lhs:  mk_ident(buf_name)
				expr: mk_int('0')
			})
		}),
	])

	mtime_assign := mk_assign(mtime_var, mk_unsafe_deref_i64(buf_name, '48'))

	return ast.Stmt(ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  ast.Expr(ast.InfixExpr{
				op:  .eq
				lhs: stat_call
				rhs: mk_int('0')
			})
			stmts: [mtime_assign]
		}
	})
}

// mk_unsafe_deref_i64 generates: unsafe { *(&i64(&<buf>[<offset>])) }
fn mk_unsafe_deref_i64(buf_name string, offset string) ast.Expr {
	return ast.Expr(ast.UnsafeExpr{
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.PrefixExpr{
					op:   .mul
					expr: ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: mk_cast('i64', ast.Expr(ast.PrefixExpr{
							op:   .amp
							expr: ast.Expr(ast.IndexExpr{
								lhs:  mk_ident(buf_name)
								expr: mk_int(offset)
							})
						}))
					})
				})
			}),
		]
	})
}

// Helper functions for constructing AST nodes

fn mk_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn mk_int(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn mk_hex(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn mk_neg_one() ast.Expr {
	return ast.Expr(ast.PrefixExpr{
		op:   .minus
		expr: mk_int('1')
	})
}

fn mk_neg(value string) ast.Expr {
	return ast.Expr(ast.PrefixExpr{
		op:   .minus
		expr: mk_int(value)
	})
}

fn mk_cstring(value string) ast.Expr {
	return ast.Expr(ast.StringLiteral{
		kind:  .c
		value: "'${value}'"
	})
}

fn mk_cast(type_name string, expr ast.Expr) ast.Expr {
	return ast.Expr(ast.CallOrCastExpr{
		lhs:  mk_ident(type_name)
		expr: expr
	})
}

fn mk_call(fn_name string, args []ast.Expr) ast.Expr {
	// For C functions (C.stat, C.mmap, etc.), create a SelectorExpr so the SSA
	// builder's resolve_call_name correctly strips the C module prefix.
	lhs := if fn_name.starts_with('C.') {
		ast.Expr(ast.SelectorExpr{
			lhs: mk_ident('C')
			rhs: ast.Ident{
				name: fn_name[2..]
			}
		})
	} else {
		mk_ident(fn_name)
	}
	return ast.Expr(ast.CallExpr{
		lhs:  lhs
		args: args
	})
}

fn mk_call_stmt(fn_name string, args []ast.Expr) ast.Stmt {
	return ast.Stmt(ast.ExprStmt{
		expr: mk_call(fn_name, args)
	})
}

fn mk_decl_assign(name string, value ast.Expr) ast.Stmt {
	return ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: name
		})]
		rhs: [value]
	})
}

fn mk_assign(name string, value ast.Expr) ast.Stmt {
	return ast.Stmt(ast.AssignStmt{
		op:  .assign
		lhs: [ast.Expr(ast.Ident{
			name: name
		})]
		rhs: [value]
	})
}

fn mk_fixed_array_init(size string, elem_type string) ast.Expr {
	return ast.Expr(ast.ArrayInitExpr{
		typ: ast.Expr(ast.Type(ast.ArrayFixedType{
			len:       ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: size
			})
			elem_type: ast.Expr(ast.Ident{
				name: elem_type
			})
		}))
	})
}

fn mk_unsafe_nil() ast.Expr {
	return ast.Expr(ast.UnsafeExpr{
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.Keyword{
					tok: .key_nil
				})
			}),
		]
	})
}

// build_live_c_decls generates C extern function declarations needed by live reload.
fn build_live_c_decls() []ast.Stmt {
	mut decls := []ast.Stmt{}

	// fn C.stat(path &u8, buf &u8) int
	decls << mk_c_fn_decl('stat', [
		mk_param('path', '&u8'),
		mk_param('buf', '&u8'),
	], 'int')

	// fn C.mmap(addr voidptr, len usize, prot int, flags int, fd int, offset i64) voidptr
	decls << mk_c_fn_decl('mmap', [
		mk_param('addr', 'voidptr'),
		mk_param('len', 'usize'),
		mk_param('prot', 'int'),
		mk_param('flags', 'int'),
		mk_param('fd', 'int'),
		mk_param('offset', 'i64'),
	], 'voidptr')

	// fn C.mprotect(addr voidptr, len usize, prot int) int
	decls << mk_c_fn_decl('mprotect', [
		mk_param('addr', 'voidptr'),
		mk_param('len', 'usize'),
		mk_param('prot', 'int'),
	], 'int')

	// fn C.sys_icache_invalidate(start voidptr, size usize)
	decls << mk_c_fn_decl('sys_icache_invalidate', [
		mk_param('start', 'voidptr'),
		mk_param('size', 'usize'),
	], '')

	// fn C.fopen(path &u8, mode &u8) voidptr
	decls << mk_c_fn_decl('fopen', [
		mk_param('path', '&u8'),
		mk_param('mode', '&u8'),
	], 'voidptr')

	// fn C.fread(buf voidptr, size usize, count usize, f voidptr) usize
	decls << mk_c_fn_decl('fread', [
		mk_param('buf', 'voidptr'),
		mk_param('size', 'usize'),
		mk_param('count', 'usize'),
		mk_param('f', 'voidptr'),
	], 'usize')

	// fn C.fseek(f voidptr, offset i64, whence int) int
	decls << mk_c_fn_decl('fseek', [
		mk_param('f', 'voidptr'),
		mk_param('offset', 'i64'),
		mk_param('whence', 'int'),
	], 'int')

	// fn C.ftell(f voidptr) i64
	decls << mk_c_fn_decl('ftell', [
		mk_param('f', 'voidptr'),
	], 'i64')

	// fn C.fclose(f voidptr) int
	decls << mk_c_fn_decl('fclose', [
		mk_param('f', 'voidptr'),
	], 'int')

	// fn C.system(cmd &u8) int
	decls << mk_c_fn_decl('system', [
		mk_param('cmd', '&u8'),
	], 'int')

	// fn C.printf(fmt &u8) int  (variadic)
	decls << mk_c_fn_decl('printf', [
		mk_param('fmt', '&u8'),
	], 'int')

	// fn C.fflush(f voidptr) int
	decls << mk_c_fn_decl('fflush', [
		mk_param('f', 'voidptr'),
	], 'int')

	// fn C.clock_gettime_nsec_np(clock_id int) u64
	decls << mk_c_fn_decl('clock_gettime_nsec_np', [
		mk_param('clock_id', 'int'),
	], 'u64')

	return decls
}

fn mk_c_fn_decl(name string, params []ast.Parameter, ret_type string) ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		language: .c
		name:     name
		typ:      ast.FnType{
			params:      params
			return_type: if ret_type.len > 0 {
				mk_ident(ret_type)
			} else {
				ast.empty_expr
			}
		}
	})
}

fn mk_param(name string, typ_name string) ast.Parameter {
	typ_expr := if typ_name.starts_with('&') {
		ast.Expr(ast.PrefixExpr{
			op:   .amp
			expr: mk_ident(typ_name[1..])
		})
	} else {
		mk_ident(typ_name)
	}
	return ast.Parameter{
		name: name
		typ:  typ_expr
	}
}
