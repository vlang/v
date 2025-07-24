// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module eval

import v.ast
import v.pref
import v.util
import v.builder

pub fn new_eval(table &ast.Table, pref_ &pref.Preferences) Eval {
	return Eval{
		table: table
		pref:  pref_
	}
}

// Host API

pub fn create() Eval {
	t := ast.new_table()
	mut p, _ := pref.parse_args([], ['interpret', ''])
	p.is_script = true
	return new_eval(t, p)
}

pub fn (mut e Eval) push_val(val Object) {
	e.stack_vals << val
}

pub fn (mut e Eval) add_file(filepath string) {
	e.user_files << filepath
}

pub fn (mut e Eval) run(expression string, args ...Object) ![]Object {
	mut prepend := 'fn host_pop() voidptr { return 0 }\n'
	mut b := builder.new_builder(e.pref)
	e.table = b.table

	mut files := b.get_builtin_files()
	files << e.user_files
	b.set_module_lookup_paths()

	b.interpret_text(prepend + expression, files)!
	e.register_symbols(mut b.parsed_files)
	e.run_func(e.mods['main']['main'] or { ast.FnDecl{} } as ast.FnDecl, ...args)
	return e.return_values
}

// const/global is `Object`
type Symbol = Object | ast.EmptyStmt | ast.FnDecl

pub struct Eval {
pub:
	pref &pref.Preferences = unsafe { nil }
pub mut:
	table                  &ast.Table = unsafe { nil }
	mods                   map[string]map[string]Symbol
	future_register_consts map[string]map[string]map[string]ast.ConstField // mod:file:name:field
	local_vars             map[string]Var
	local_vars_stack       []map[string]Var
	stack_vals             []Object // host stack popped by host_pop() on interpreted code
	user_files             []string // user additional files
	scope_idx              int      // this is increased when e.open_scope() is called, decreased when e.close_scope() (and all variables with that scope level deleted)
	returning              bool
	return_values          []Object
	executed_return_stmt   bool // already executed a return stmt in func
	cur_mod                string
	cur_file               string

	trace_file_paths     []string
	trace_function_names []string
	back_trace           []EvalTrace
}

pub struct EvalTrace {
	fn_idx   int
	file_idx int
	line     int
}

pub fn (mut e Eval) eval(mut files []&ast.File) {
	e.register_symbols(mut files)
	// println(files.map(it.path_base))
	e.run_func(e.mods['main']['main'] or { ast.FnDecl{} } as ast.FnDecl)
}

// first arg is receiver (if method)
pub fn (mut e Eval) run_func(func ast.FnDecl, _args ...Object) {
	e.back_trace << EvalTrace{func.idx, if unsafe { func.source_file != 0 } {
		func.source_file.idx
	} else {
		0
	}, func.pos.line_nr}
	old_mod := e.cur_mod
	old_file := e.cur_file
	e.cur_mod = func.mod
	e.cur_file = func.file
	defer {
		e.cur_mod = old_mod
		e.cur_file = old_file
		e.back_trace.pop()
	}
	is_main := func.name == 'main.main'

	mut args := _args.clone()
	if !is_main && func.params.len != args.len && !func.is_variadic {
		e.error('mismatched parameter length for ${func.name}: got `${args.len}`, expected `${func.params.len}`')
	}

	if func.name in ['print', 'println', 'eprint', 'eprintln', 'panic'] {
		s := args[0].string() // stringify because println accepts anything as argument
		match func.name {
			'print' {
				print(s)
			}
			'println' {
				println(s)
			}
			'eprint' {
				eprint(s)
			}
			'eprintln' {
				eprintln(s)
			}
			'panic' {
				e.panic(s)
			}
			else {}
		}
	} else {
		e.local_vars_stack << e.local_vars
		e.local_vars = {}
		old_scope := e.scope_idx
		e.scope_idx = 0
		e.open_scope()
		// have to do this because of cgen error
		args__ := if func.is_method { args[1..] } else { args }
		if !is_main {
			for i, arg in args__ {
				var_name := (func.params[i]).name
				e.local_vars[var_name] = Var{
					val:       arg
					scope_idx: e.scope_idx
				}
			}
		}
		if func.is_method {
			print(e.back_trace)
			println(func.receiver.typ.set_nr_muls(0))
			e.local_vars[func.receiver.name] = Var{
				val:       args[0]
				scope_idx: e.scope_idx
			}
		}
		e.executed_return_stmt = false
		e.stmts(func.stmts)
		e.returning = false
		e.close_scope()
		e.scope_idx = old_scope
		e.local_vars = e.local_vars_stack.pop()
	}
}

pub fn (mut e Eval) register_symbols(mut files []&ast.File) {
	for mut file in files {
		file.idx = e.trace_file_paths.len
		e.trace_file_paths << file.path
		mod := file.mod.name
		for mut stmt in file.stmts {
			if mut stmt is ast.FnDecl {
				stmt.idx = e.trace_function_names.len
				e.trace_function_names << stmt.name
			}
		}
		e.register_symbol_stmts(file.stmts, mod, file.path)
	}
	for mod, const_files in e.future_register_consts {
		e.cur_mod = mod

		for file, fields in const_files {
			e.cur_file = file
			for _, field in fields {
				e.returning = true
				e.return_values = []
				e.mods[mod][field.name.all_after_last('.')] = e.expr(field.expr, field.typ)
				e.returning = false
				e.return_values = []
				if mod == 'os' && field.name.all_after_last('.') == 'args' {
					mut res := Array{}
					res.val << e.pref.out_name.all_after_last('/')
					for arg in e.pref.run_args {
						res.val << arg
					}
					e.mods[mod][field.name.all_after_last('.')] = Object(res)
				}
			}
		}
	}
}

pub fn (mut e Eval) register_symbol_stmts(stmts []ast.Stmt, mod string, file string) {
	for stmt in stmts {
		e.register_symbol(stmt, mod, file)
	}
}

pub fn (mut e Eval) comptime_cond(cond ast.Expr) bool {
	match cond {
		ast.Ident {
			cname := cond.name
			if cname in ast.valid_comptime_if_os {
				mut ident_result := false
				if !e.pref.output_cross_c {
					if cname_enum_val := pref.os_from_string(cname) {
						if cname_enum_val == e.pref.os {
							ident_result = true
						}
					}
				}
				$if trace_comptime_os_checks ? {
					eprintln('>>> ident_name: ${ident_name} | e.pref.os: ${e.pref.os} | ident_result: ${ident_result}')
				}
				return ident_result
			} else if cname in ast.valid_comptime_if_compilers {
				return pref.cc_from_string(cname) == e.pref.ccompiler_type
			} else if cname in ast.valid_comptime_if_platforms {
				match cname {
					'amd64' { return e.pref.arch == .amd64 }
					'i386' { return e.pref.arch == .i386 }
					'aarch64' { return e.pref.arch == .arm64 }
					'arm64' { return e.pref.arch == .arm64 }
					'arm32' { return e.pref.arch == .arm32 }
					'rv64' { return e.pref.arch == .rv64 }
					'rv32' { return e.pref.arch == .rv32 }
					's390x' { return e.pref.arch == .s390x }
					'ppc64le' { return e.pref.arch == .ppc64le }
					'loongarch64' { return e.pref.arch == .loongarch64 }
					else { e.error('unknown comptime platforms \$if ${cname}') }
				}
			} else if cname in ast.valid_comptime_if_cpu_features {
				match cname {
					'x64' { e.pref.m64 }
					'x32' { !e.pref.m64 }
					else { e.error('unknown comptime cpu features \$if ${cname}') }
				}
			} else if cname in ast.valid_comptime_if_other {
				match cname {
					'apk' {
						return e.pref.is_apk
					}
					'js' {
						return e.pref.backend.is_js()
					}
					'debug' {
						return e.pref.is_debug
					}
					'prod' {
						return e.pref.is_prod
					}
					'profile' {
						return e.pref.is_prof
					}
					'test' {
						return e.pref.is_test
					}
					'musl' {
						e.error('unknown comptime other \$if ${cname}')
					}
					'glibc' {
						e.error('unknown comptime other \$if ${cname}')
					}
					'threads' {
						return e.table.gostmts > 0
					}
					'prealloc' {
						return e.pref.prealloc
					}
					'no_bounds_checking' {
						return cname in e.pref.compile_defines_all
					}
					'autofree' {
						return e.pref.autofree
					}
					'freestanding' {
						return e.pref.is_bare && !e.pref.output_cross_c
					}
					'interpreter' {
						return e.pref.backend == .interpret
					}
					'es5' {
						return e.pref.output_es5
					}
					'wasm32' {
						return e.pref.os == .wasm32
					}
					'wasm32_wasi' {
						return e.pref.os == .wasm32_wasi
					}
					'fast_math' {
						return e.pref.fast_math
					}
					'native' {
						return e.pref.backend == .native
					}
					else {
						e.error('unknown comptime other \$if ${cname}')
					}
				}
			}
		}
		ast.PrefixExpr {
			match cond.op {
				.not {
					return !e.comptime_cond(cond.right)
				}
				else {
					e.error('unsupported prefix expression')
				}
			}
		}
		ast.InfixExpr {
			left := e.comptime_cond(cond.left)
			right := e.comptime_cond(cond.right)
			return e.infix_expr(left, right, cond.op, ast.bool_type) as bool
		}
		else {
			e.error('unsupported expression')
		}
	}
	return false
}

pub fn (mut e Eval) register_symbol(stmt ast.Stmt, mod string, file string) {
	match stmt {
		ast.Module {
			// ignore module declarations for now
		}
		ast.FnDecl {
			// this mess because c error
			x := ast.Stmt(stmt)
			y := Symbol(x as ast.FnDecl)
			e.mods[mod][stmt.name.all_after_last('.')] = y
		}
		ast.Import {} // already handled by builder, TODO: get `as` name
		ast.StructDecl {} // these are already parsed by the checker into e.table
		ast.InterfaceDecl {}
		ast.EnumDecl {}
		ast.TypeDecl {}
		ast.GlobalDecl {}
		ast.HashStmt {}
		ast.ConstDecl {
			// evaluate them later since they may use functions defined after this point
			for field in stmt.fields {
				e.future_register_consts[mod][file][field.name] = field
			}
		}
		ast.ExprStmt {
			x := stmt.expr
			match x {
				ast.IfExpr {
					if !x.is_comptime {
						e.error('only comptime ifs are allowed in top level')
					}
					for i, branch in x.branches {
						if e.comptime_cond(branch.cond) || x.branches.len == i + 1 {
							e.register_symbol_stmts(branch.stmts, mod, file)
							break
						}
					}
				}
				else {
					e.error('unknown declaration expression statement ${x.type_name()}')
				}
			}
		}
		else {
			e.error('unhandled declaration statement ${stmt.type_name()}')
		}
	}
}

@[noreturn]
fn (e &Eval) error(msg string) {
	eprintln('> V interpreter backtrace:')
	e.print_backtrace()
	util.verror('interpreter', msg)
}

fn (e &Eval) panic(s string) {
	eprintln('V panic: ${s}')
	eprintln('V hash: ${vcurrent_hash()}')
	e.print_backtrace()
	exit(1)
}

fn (e &Eval) print_backtrace() {
	for i := e.back_trace.len - 1; i >= 0; i-- {
		t := e.back_trace[i]
		file_path := if path := e.trace_file_paths[t.file_idx] {
			util.path_styled_for_error_messages(path)
		} else {
			t.file_idx.str()
		}
		fn_name := e.trace_function_names[t.fn_idx] or { t.fn_idx.str() }
		word := if i == e.back_trace.len - 1 { 'at' } else { 'by' }
		eprintln('${file_path}:${t.line + 1}: ${word} ${fn_name}')
	}
}
