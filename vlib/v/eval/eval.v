// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module eval

import v.ast
import v.pref
import v.util
import v.builder

$if interpreter {
	$compile_error('cannot run the V interpreter with `-interpret` backend')
}

pub fn new_eval(table &ast.Table, pref_ &pref.Preferences) Eval {
	return Eval{
		table: table
		pref: pref_
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
	pref &pref.Preferences = unsafe { nil }
pub mut:
	table                  &ast.Table = unsafe { nil }
	mods                   map[string]map[string]Symbol
	future_register_consts map[string]map[string]map[string]ast.ConstField // mod:file:name:field
	local_vars             map[string]Var
	local_vars_stack       []map[string]Var
	inside_main            bool
	stack_vals             []Object // host stack popped by host_pop() on interpreted code
	user_files             []string // user additional files
	scope_idx              int      // this is increased when e.open_scope() is called, decreased when e.close_scope() (and all variables with that scope level deleted)
	returning              bool
	return_values          []Object
	cur_mod                string
	cur_file               string
	//
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
	e.run_func(e.mods['main']['main'] or { ast.FnDecl{} } as ast.FnDecl)
}

// first arg is reciever (if method)
pub fn (mut e Eval) run_func(func ast.FnDecl, _args ...Object) {
	e.back_trace << EvalTrace{func.idx, func.source_file.idx, func.pos.line_nr}
	old_inside_main := e.inside_main
	old_mod := e.cur_mod
	old_file := e.cur_file
	e.cur_mod = func.mod
	e.cur_file = func.source_file.path
	e.inside_main = func.is_main
	defer {
		e.inside_main = old_inside_main
		e.cur_mod = old_mod
		e.cur_file = old_file
		e.back_trace.pop()
	}
	is_main := func.name == 'main.main'
	//
	mut args := _args.clone()
	if !is_main && func.params.len != args.len && !func.is_variadic {
		e.error('mismatched parameter length for ${func.name}: got `${args.len}`, expected `${func.params.len}`')
	}
	if func.name == 'wait' && func.is_method && _args[0] is Thread {
		e.returning = true
		e.return_values = [(_args[0] as Thread).val.wait()]
		return
	}
	match func.name {
		'print_backtrace' {
			e.print_backtrace()
		}
		'print', 'println', 'eprint', 'eprintln', 'panic' {
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
		}
		'exit' {
			exit(int(args[0].int_val()))
		}
		else {
			e.local_vars_stack << e.local_vars
			e.local_vars = {}
			old_scope := e.scope_idx
			e.scope_idx = 0
			e.open_scope()
			// have to do this because of cgen error
			args__ := if func.is_method { args[1..] } else { args }
			if !is_main {
				for i, arg in args__ {
					e.local_vars[func.params[i].name] = Var{
						val: arg
						scope_idx: e.scope_idx
					}
				}
			}
			if func.is_method {
				e.local_vars[func.receiver.name] = Var{
					val: args[0]
					scope_idx: e.scope_idx
				}
			}
			e.stmts(func.stmts)
			e.returning = false
			e.close_scope()
			e.scope_idx = old_scope
			e.local_vars = e.local_vars_stack.pop()
		}
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
		// TODO: fix consts deps
		e.cur_mod = mod
		for file, fields in const_files {
			e.cur_file = file
			for _, field in fields {
				field_name_after_last_dot := field.name.all_after_last('.')
				if mod == 'os' && field_name_after_last_dot == 'args' {
					mut res := Array{}
					res.val << e.pref.out_name.all_after_last('/')
					for arg in e.pref.run_args {
						res.val << arg
					}
					e.mods[mod][field_name_after_last_dot] = Object(res)
				} else {
					e.mods[mod][field_name_after_last_dot] = e.expr(field.expr, field.typ)
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

pub fn (mut e Eval) register_symbol(stmt ast.Stmt, mod string, file string) {
	match stmt {
		ast.EmptyStmt {
			// ignore
		}
		ast.Module {
			// ignore module declarations for now
		}
		ast.FnDecl {
			e.mods[mod][stmt.name.all_after_last('.')] = Symbol(ast.Stmt(stmt) as ast.FnDecl)
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
					if x.is_comptime {
						if x.comptime_branch_idx > -1 {
							e.register_symbol_stmts(x.branches[x.comptime_branch_idx].stmts,
								mod, file)
						}
					} else {
						e.error('only comptime `if`s are allowed in top level')
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

fn (e Eval) error(msg string) {
	if e.back_trace.len > 0 {
		eprintln('> V interpeter backtrace:')
		e.print_backtrace()
	}
	util.verror('interpreter', msg)
}

fn (e Eval) panic(s string) {
	commithash := unsafe { tos5(&char(C.V_CURRENT_COMMIT_HASH)) }
	eprintln('V panic: ${s}')
	eprintln('V hash: ${commithash}')
	e.print_backtrace()
	exit(1)
}

fn (e Eval) print_backtrace() {
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
