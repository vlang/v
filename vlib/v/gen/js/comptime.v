module js

import v.ast

fn (mut g JsGen) gen_branch_context_string() string {
	mut arr := []string{}

	// gen `T=int,X=string`
	if g.fn_decl != unsafe { nil } && g.fn_decl.generic_names.len > 0
		&& g.fn_decl.generic_names.len == g.cur_concrete_types.len {
		for i in 0 .. g.fn_decl.generic_names.len {
			arr << g.fn_decl.generic_names[i] + '=' +
				g.table.type_to_str(g.cur_concrete_types[i]).replace('main.', '')
		}
	}

	// TODO: support comptime `$for`
	return arr.join(',')
}

fn (mut g JsGen) comptime_if_result(branch ast.IfBranch) bool {
	idx_str := g.gen_branch_context_string() + '|id=${branch.id}|'
	$if debug_comptime_branch_context ? {
		g.write('/* ${idx_str} */')
	}
	if comptime_is_true := g.table.comptime_is_true[idx_str] {
		return comptime_is_true.val
	} else {
		panic('checker error: cond result idx string not found => [${idx_str}]')
		return false
	}
}
