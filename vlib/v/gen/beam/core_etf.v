module beam

// Core Erlang ETF term emission helpers.
//
// When etf_mode is true, these helpers emit Erlang term text representing
// the Core Erlang AST directly ({c_module,...}, {c_literal,...}, etc).
// When etf_mode is false, they emit Core Erlang text syntax.
//
// This abstraction allows the expression/statement generators to be
// format-independent while switching between text and ETF modes.

// --- Literal helpers ---

// Emit an integer literal.
fn (mut g CoreGen) emit_int(val string) {
	if g.etf_mode {
		g.write_core('{c_literal,[],${val}}')
	} else {
		g.write_core(val)
	}
}

// Emit an atom literal. name should be unquoted (e.g., "true", "ok", "v.main").
fn (mut g CoreGen) emit_atom(name string) {
	if g.etf_mode {
		g.write_core("{c_literal,[],'${name}'}")
	} else {
		g.write_core("'${name}'")
	}
}

// Emit a raw atom (already properly quoted/formatted for the current mode).
fn (mut g CoreGen) emit_atom_raw(name string) {
	if g.etf_mode {
		g.write_core('{c_literal,[],${name}}')
	} else {
		g.write_core(name)
	}
}

// Emit a binary/string literal.
fn (mut g CoreGen) emit_binary(val string) {
	if g.etf_mode {
		g.write_core('{c_literal,[],<<"${val}">>}')
	} else {
		g.write_core('<<"${val}">>')
	}
}

// Emit a float literal.
fn (mut g CoreGen) emit_float(val string) {
	if g.etf_mode {
		g.write_core('{c_literal,[],${val}}')
	} else {
		g.write_core(val)
	}
}

// Emit a charlist (Erlang string) literal.
// In ETF mode: {c_literal,[],"string"}, in text mode: [int|[int|...[]]]
fn (mut g CoreGen) emit_charlist(val string) {
	if g.etf_mode {
		g.write_core('{c_literal,[],"${core_escape_charlist(val)}"}')
	} else {
		g.write_core(core_charlist(val))
	}
}

// Emit a nil/empty list literal.
fn (mut g CoreGen) emit_nil() {
	if g.etf_mode {
		g.write_core('{c_literal,[],[]}')
	} else {
		g.write_core('[]')
	}
}

// --- Variable helpers ---

// Emit a variable reference.
fn (mut g CoreGen) emit_var(name string) {
	if g.etf_mode {
		g.write_core("{c_var,[],'${name}'}")
	} else {
		g.write_core(name)
	}
}

// Emit a function name/arity variable (for exports and definitions).
fn (mut g CoreGen) emit_fname(name string, arity int) {
	if g.etf_mode {
		g.write_core("{c_var,[],{'${name}',${arity}}}")
	} else {
		g.write_core("'${name}'/${arity}")
	}
}

// Emit float formatting options: [{decimals, 10}, compact]
// Used by float_to_binary/2 for human-readable output (e.g. "3.0" not "3.00e+00")
fn (mut g CoreGen) core_float_format_opts() {
	if g.etf_mode {
		g.write_core("{c_literal,[],[{'decimals', 10}, 'compact']}")
	} else {
		g.write_core("[{'decimals', 10} | ['compact' | []]]")
	}
}

// --- Call helpers ---

// Begin a cross-module call: call 'mod':'fn'(args...)
fn (mut g CoreGen) begin_call(mod string, fn_name string) {
	if g.etf_mode {
		g.write_core("{c_call,[],{c_literal,[],'${mod}'},{c_literal,[],'${fn_name}'},[")
	} else {
		g.write_core("call '${mod}':'${fn_name}'(")
	}
}

// End a cross-module call.
fn (mut g CoreGen) end_call() {
	if g.etf_mode {
		g.write_core(']}')
	} else {
		g.write_core(')')
	}
}

// Begin an intra-module apply: apply 'fn'/arity(args...)
fn (mut g CoreGen) begin_apply(fn_name string, arity int) {
	if g.etf_mode {
		g.write_core("{c_apply,[],{c_var,[],{'${fn_name}',${arity}}},[")
	} else {
		g.write_core("apply '${fn_name}'/${arity}(")
	}
}

// Begin an apply with dynamic function value.
fn (mut g CoreGen) begin_apply_var() {
	if g.etf_mode {
		g.write_core('{c_apply,[],')
	} else {
		g.write_core('apply ')
	}
}

// Transition from function to arguments in dynamic apply.
fn (mut g CoreGen) mid_apply_args() {
	if g.etf_mode {
		g.write_core(',[')
	} else {
		g.write_core('(')
	}
}

// End an apply.
fn (mut g CoreGen) end_apply() {
	if g.etf_mode {
		g.write_core(']}')
	} else {
		g.write_core(')')
	}
}

// --- Let binding helpers ---

// Begin a let binding: let <Var> = Expr in Body
fn (mut g CoreGen) begin_let(var_name string) {
	if g.etf_mode {
		g.write_core("{c_let,[],[{c_var,[],'${var_name}'}],")
	} else {
		g.write_core('let <${var_name}> = ')
	}
}

// Transition from expression to body in let.
fn (mut g CoreGen) mid_let() {
	if g.etf_mode {
		g.write_core(',')
	} else {
		g.write_core(' in ')
	}
}

// End a let binding (ETF only needs closing paren).
fn (mut g CoreGen) end_let() {
	if g.etf_mode {
		g.write_core('}')
	}
	// Text mode: no explicit end marker for let
}

// Begin a multi-variable let: let <V1,V2> = Expr in Body
fn (mut g CoreGen) begin_let_multi(var_names []string) {
	if g.etf_mode {
		mut vars := []string{}
		for name in var_names {
			vars << "{c_var,[],'${name}'}"
		}
		g.write_core('{c_let,[],[${vars.join(",")}],')
	} else {
		g.write_core('let <${var_names.join(", ")}> = ')
	}
}

// --- Case helpers ---

// Begin a case expression.
fn (mut g CoreGen) begin_case() {
	if g.etf_mode {
		g.write_core('{c_case,[],')
	} else {
		g.write_core('case ')
	}
}

// Transition from scrutinee to clauses.
fn (mut g CoreGen) mid_case_clauses() {
	if g.etf_mode {
		g.write_core(',[')
	} else {
		g.write_core(' of ')
	}
}

// Separator between clauses.
fn (mut g CoreGen) clause_sep() {
	if g.etf_mode {
		g.write_core(',')
	} else {
		// Text mode: newline between clauses (handled by caller)
	}
}

// End a case expression.
fn (mut g CoreGen) end_case() {
	if g.etf_mode {
		g.write_core(']}')
	} else {
		g.write_core(' end')
	}
}

// --- Clause helpers ---

// Begin a clause: <Pat> when Guard -> Body
fn (mut g CoreGen) begin_clause() {
	if g.etf_mode {
		g.write_core('{c_clause,[],[')
	} else {
		g.write_core('<')
	}
}

// Transition from patterns to guard.
fn (mut g CoreGen) mid_clause_guard() {
	if g.etf_mode {
		g.write_core('],')
	} else {
		g.write_core('> when ')
	}
}

// Transition from guard to body.
fn (mut g CoreGen) mid_clause_body() {
	if g.etf_mode {
		g.write_core(',')
	} else {
		g.write_core(' -> ')
	}
}

// End a clause.
fn (mut g CoreGen) end_clause() {
	if g.etf_mode {
		g.write_core('}')
	}
	// Text mode: no explicit end
}

// --- Fun helpers ---

// Begin a fun/lambda definition.
fn (mut g CoreGen) begin_fun(param_names []string) {
	if g.etf_mode {
		mut params := []string{}
		for name in param_names {
			params << "{c_var,[],'${name}'}"
		}
		g.write_core('{c_fun,[],[${params.join(",")}],')
	} else {
		g.write_core('fun (${param_names.join(", ")}) ->\n')
	}
}

// End a fun definition.
fn (mut g CoreGen) end_fun() {
	if g.etf_mode {
		g.write_core('}')
	}
	// Text mode: no explicit end for fun (caller handles structure)
}

// --- Sequence (do) helpers ---

// Emit a sequence: do Expr1 Expr2
fn (mut g CoreGen) begin_seq() {
	if g.etf_mode {
		g.write_core('{c_seq,[],')
	} else {
		g.write_core('do ')
	}
}

// Separator between sequence expressions.
fn (mut g CoreGen) mid_seq() {
	if g.etf_mode {
		g.write_core(',')
	} else {
		g.write_core('\n')
	}
}

// End a sequence.
fn (mut g CoreGen) end_seq() {
	if g.etf_mode {
		g.write_core('}')
	}
	// Text mode: no explicit end
}

// --- Try/catch helpers ---

// Begin a try expression: try Expr of <Var> -> Body catch <E1,E2,E3> -> Handler end
fn (mut g CoreGen) begin_try() {
	if g.etf_mode {
		g.write_core('{c_try,[],')
	} else {
		g.write_core('try ')
	}
}

// Transition from expression to success variable/body.
fn (mut g CoreGen) mid_try_of(var_name string) {
	if g.etf_mode {
		g.write_core(",[{c_var,[],'${var_name}'}],")
	} else {
		g.write_core(' of <${var_name}> -> ')
	}
}

// Transition from body to catch.
fn (mut g CoreGen) mid_try_catch(e1 string, e2 string, e3 string) {
	if g.etf_mode {
		g.write_core(",[{c_var,[],'${e1}'},{c_var,[],'${e2}'},{c_var,[],'${e3}'}],")
	} else {
		g.write_core(' catch <${e1},${e2},${e3}> -> ')
	}
}

// End a try expression.
fn (mut g CoreGen) end_try() {
	if g.etf_mode {
		g.write_core('}')
	} else {
		g.write_core(' end')
	}
}

// --- Receive helpers ---

// Begin a receive expression.
fn (mut g CoreGen) begin_receive() {
	if g.etf_mode {
		g.write_core('{c_receive,[],[')
	} else {
		g.write_core('receive ')
	}
}

// Transition from clauses to timeout.
fn (mut g CoreGen) mid_receive_after() {
	if g.etf_mode {
		g.write_core('],')
	} else {
		g.write_core(' after ')
	}
}

// Transition from timeout to action.
fn (mut g CoreGen) mid_receive_action() {
	if g.etf_mode {
		g.write_core(',')
	} else {
		g.write_core(' -> ')
	}
}

// End a receive expression.
fn (mut g CoreGen) end_receive() {
	if g.etf_mode {
		g.write_core('}')
	} else {
		g.write_core(' end')
	}
}

// --- Letrec helpers ---

// Begin a letrec: letrec 'name'/arity = fun(...) -> BODY in AFTER
fn (mut g CoreGen) begin_letrec(fn_name string, arity int) {
	if g.etf_mode {
		g.write_core("{c_letrec,[],[{{c_var,[],{'${fn_name}',${arity}}},{c_fun,[],[")
	} else {
		g.write_core("letrec '${fn_name}'/${arity} = fun (")
	}
}

// Transition from letrec params to body.
fn (mut g CoreGen) mid_letrec_body() {
	if g.etf_mode {
		g.write_core('],')
	} else {
		g.write_core(') ->\n')
	}
}

// Transition from letrec body to after expression.
fn (mut g CoreGen) mid_letrec_after() {
	if g.etf_mode {
		g.write_core('}}],')
	} else {
		g.write_core('\nin ')
	}
}

// End a letrec.
fn (mut g CoreGen) end_letrec() {
	if g.etf_mode {
		g.write_core('}')
	}
	// Text mode: no explicit end
}

// Emit letrec parameter (variable in param list).
fn (mut g CoreGen) emit_letrec_param(name string) {
	if g.etf_mode {
		g.write_core("{c_var,[],'${name}'}")
	} else {
		g.write_core(name)
	}
}

// --- Tuple helpers ---

// Begin a tuple.
fn (mut g CoreGen) begin_tuple() {
	if g.etf_mode {
		g.write_core('{c_tuple,[],[')
	} else {
		g.write_core('{')
	}
}

// End a tuple.
fn (mut g CoreGen) end_tuple() {
	if g.etf_mode {
		g.write_core(']}')
	} else {
		g.write_core('}')
	}
}

// --- Cons/list helpers ---

// Begin a cons cell: [H|T]
fn (mut g CoreGen) begin_cons() {
	if g.etf_mode {
		g.write_core('{c_cons,[],')
	} else {
		g.write_core('[')
	}
}

// Separator between head and tail of cons.
fn (mut g CoreGen) mid_cons() {
	if g.etf_mode {
		g.write_core(',')
	} else {
		g.write_core('|')
	}
}

// End a cons cell.
fn (mut g CoreGen) end_cons() {
	if g.etf_mode {
		g.write_core('}')
	} else {
		g.write_core(']')
	}
}

// --- Values (multi-value) helpers ---

// Begin a values expression: <V1, V2>
fn (mut g CoreGen) begin_values() {
	if g.etf_mode {
		g.write_core('{c_values,[],[')
	} else {
		g.write_core('<')
	}
}

// End a values expression.
fn (mut g CoreGen) end_values() {
	if g.etf_mode {
		g.write_core(']}')
	} else {
		g.write_core('>')
	}
}

// --- Module structure helpers ---

// Emit the module header.
fn (mut g CoreGen) emit_module_header(erl_mod string, exports []string, behaviours []string) {
	if g.etf_mode {
		// {c_module, [], {c_literal,[],ModName}, [Exports], [Attrs], [
		g.write_core("{c_module,[],{c_literal,[],'${erl_mod}'},[")
		g.write_core(exports.join(','))
		g.write_core('],')
		// Attributes
		if behaviours.len > 0 {
			mut attrs := []string{}
			for b in behaviours {
				attrs << "{{c_literal,[],behaviour},{c_literal,[],['${b}']}}"
			}
			g.write_core('[${attrs.join(",")}]')
		} else {
			g.write_core('[]')
		}
		g.write_core(',[')
	} else {
		g.write_core("module '${erl_mod}' [${exports.join(',\n                ')}]")
		g.out.writeln('')
		if behaviours.len > 0 {
			mut attrs := []string{}
			for b in behaviours {
				attrs << "{'behaviour', ['${b}']}"
			}
			g.writeln_core('    attributes [${attrs.join(', ')}]')
		} else {
			g.writeln_core('    attributes []')
		}
	}
}

// Begin a function definition.
fn (mut g CoreGen) begin_fndef(name string, arity int) {
	if g.etf_mode {
		g.write_core("{{c_var,[],{'${name}',${arity}}},")
	} else {
		g.writeln_core("'${name}'/${arity} =")
	}
}

// End a function definition.
fn (mut g CoreGen) end_fndef() {
	if g.etf_mode {
		g.write_core('}')
	}
	// Text mode: no explicit end
}

// Separator between function definitions.
fn (mut g CoreGen) fndef_sep() {
	if g.etf_mode {
		g.write_core(',')
	}
	// Text mode: newlines handled by caller
}

// Close the module.
fn (mut g CoreGen) emit_module_footer() {
	if g.etf_mode {
		g.write_core(']}')
	} else {
		g.writeln_core('end')
	}
}

// Emit a comma separator (for argument lists etc).
fn (mut g CoreGen) emit_sep() {
	g.write_core(', ')
}

// Emit a 'true' guard.
fn (mut g CoreGen) emit_true_guard() {
	if g.etf_mode {
		g.write_core("{c_literal,[],true}")
	} else {
		g.write_core("'true'")
	}
}

// Emit a primop call (for raising exceptions etc).
fn (mut g CoreGen) begin_primop(name string) {
	if g.etf_mode {
		g.write_core("{c_primop,[],{c_literal,[],'${name}'},[")
	} else {
		g.write_core("primop '${name}'(")
	}
}

// End a primop call.
fn (mut g CoreGen) end_primop() {
	if g.etf_mode {
		g.write_core(']}')
	} else {
		g.write_core(')')
	}
}
