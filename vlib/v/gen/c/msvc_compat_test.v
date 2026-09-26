module c

// msvc_norm collapses whitespace, so the tests do not depend on the exact layout.
fn msvc_norm(s string) string {
	return s.fields().join(' ')
}

fn msvc_lower_fn(body string) string {
	return msvc_norm(msvc_compat_c_source('int f(int a, int b) {\n${body}\n}\n'))
}

fn test_statement_expression_in_declaration_is_hoisted() {
	out := msvc_lower_fn('int x = ({ int t = g(a); t * 2; });')
	assert !out.contains('({')
	assert out.contains('int t__vmsvc1 = g(a); int x = (t__vmsvc1 * 2);')
}

fn test_statement_expression_locals_are_renamed_apart() {
	out := msvc_lower_fn('int x = ({ int t = 1; t + ({ int t = 2; t; }) + t; });')
	assert out.contains('int t__vmsvc1 = 1; int t__vmsvc2 = 2; int x = (t__vmsvc1 + (t__vmsvc2) + t__vmsvc1);')
}

fn test_member_names_are_not_renamed() {
	out := msvc_lower_fn('int x = ({ S t = s; t.t + p->t; });')
	assert out.contains('S t__vmsvc1 = s; int x = (t__vmsvc1.t + p->t);')
}

fn test_logical_and_guards_the_right_operand() {
	out := msvc_lower_fn('if (a && ({ int t = g(b); t > 0; })) { h(); }')
	assert out.contains('int cond__vmsvc2; cond__vmsvc2 = !!(a); if (cond__vmsvc2) { int t__vmsvc1 = g(b); cond__vmsvc2 = !!((t__vmsvc1 > 0)); } if (cond__vmsvc2) { h(); }')
}

fn test_logical_or_guards_the_right_operand() {
	out := msvc_lower_fn('int x = a || ({ int t = g(b); t; }) || b;')
	assert out.contains('if (!cond__vmsvc2) { int t__vmsvc1 = g(b); cond__vmsvc2 = !!((t__vmsvc1)); } int x = cond__vmsvc2 || b;')
}

fn test_conditional_branches_run_only_their_own_statements() {
	out := msvc_lower_fn('int x = a ? ({ int t = g(1); t; }) : ({ int u[2] = {1, 2}; u[b]; });')
	assert out.contains('int t__vmsvc1; int u__vmsvc2[2]; if (cond__vmsvc3) { t__vmsvc1 = g(1); } else { memcpy(u__vmsvc2, (int[2]){1, 2}, sizeof(u__vmsvc2)); }')
	assert out.contains('int x = (cond__vmsvc3 ? ((t__vmsvc1)) : ((u__vmsvc2[b])));')
}

fn test_while_condition_is_evaluated_every_iteration() {
	out := msvc_lower_fn('while (({ int t = g(a); t; })) { a--; }')
	assert out.contains('for (;;) { int t__vmsvc1 = g(a); if (!((t__vmsvc1))) break; { a--; } }')
}

fn test_do_while_continue_reaches_the_condition() {
	out := msvc_lower_fn('do { if (a) continue; for (;;) { continue; } } while (({ int t = g(a); t; }));')
	assert out.contains('for (;;) { { if (a) goto cont__vmsvc2; for (;;) { continue; } } cont__vmsvc2: ; int t__vmsvc1 = g(a); if (!((t__vmsvc1))) break; }')
}

fn test_for_post_expression_runs_after_continue() {
	out := msvc_lower_fn('for (int i = 0; i < a; i = ({ int t = i; t + 1; })) { if (i == b) continue; h(i); }')
	assert out.contains('for (int i = 0; i < a;) { { if (i == b) goto cont__vmsvc2; h(i); } cont__vmsvc2: ; int t__vmsvc1 = i; i = (t__vmsvc1 + 1); }')
}

fn test_else_if_chain_with_statement_conditions_stays_flat() {
	out := msvc_lower_fn('if (a) { h(1); } else if (({ int t = g(b); t; })) { h(2); } else { h(3); }')
	assert out.contains('{ int taken__vmsvc1 = 0; if (a) { taken__vmsvc1 = 1; { h(1); } } if (!taken__vmsvc1) { int t__vmsvc2 = g(b); if ((t__vmsvc2)) { taken__vmsvc1 = 1; { h(2); } } } if (!taken__vmsvc1) { h(3); } }')
}

fn test_long_else_if_chain_does_not_nest() {
	mut chain := 'if (a == 0) { h(0); }'
	for i in 1 .. 200 {
		chain += ' else if (({ int t = g(${i}); t == a; })) { h(${i}); }'
	}
	out := msvc_lower_fn(chain)
	assert !out.contains('({')
	mut depth := 0
	mut max_depth := 0
	for ch in out {
		if ch == `{` {
			depth++
			if depth > max_depth {
				max_depth = depth
			}
		} else if ch == `}` {
			depth--
		}
	}
	assert max_depth < 8
}

fn test_hoisted_statements_after_a_case_label() {
	out := msvc_lower_fn('switch (a) { case 1: b = ({ int t = g(a); t; }); break; default: break; }')
	assert out.contains('case 1: ; int t__vmsvc1 = g(a); b = (t__vmsvc1); break;')
}

fn test_unbraced_if_body_gets_a_block() {
	out := msvc_lower_fn('if (a) b = ({ int t = g(a); t; });')
	assert out.contains('if (a) { int t__vmsvc1 = g(a); b = (t__vmsvc1); }')
}

fn test_struct_self_casts_and_empty_initializers() {
	src := 'typedef struct string string;\ntypedef string Alias;\ntypedef struct P P;\nstring f(string s) {\n\tP p = (P){};\n\tAlias a = (Alias)(s);\n\tint n = sizeof(string);\n\treturn (string)s;\n}\n'
	out := msvc_norm(msvc_compat_c_source(src))
	assert out.contains('P p = (P){0};')
	assert out.contains('Alias a = (s);')
	assert out.contains('int n = sizeof(string);')
	assert out.contains('return s;')
}

fn test_static_initializers_drop_compound_literals() {
	src := 'typedef struct P P;\ntypedef int A3[3];\nconst P p = (P){1, 2};\nconst P q = (P){};\nconst A3 a = (A3){1, 2, 3};\nP* r = &(P){1, 2};\n'
	out := msvc_norm(msvc_compat_c_source(src))
	assert out.contains('const P p = {1, 2};')
	assert out.contains('const P q = {0};')
	assert out.contains('const A3 a = {1, 2, 3};')
	assert out.contains('P* r = &(P){1, 2};')
}

fn test_strings_comments_and_directives_are_kept() {
	src := 'static const char* s = "({ x; })";\nint f(void) {\n#line 3 "a.v"\n\t// ({ comment })\n\treturn ({ int t = 1; t; });\n}\n'
	out := msvc_compat_c_source(src)
	assert out.contains('"({ x; })"')
	assert out.contains('// ({ comment })')
	assert out.contains('#line 3 "a.v"')
	assert msvc_norm(out).contains('int t__vmsvc1 = 1; return (t__vmsvc1);')
}

fn test_functions_without_extensions_are_unchanged() {
	src := 'int f(int a) {\n\tif (a) {\n\t\treturn a + 1;\n\t}\n\treturn 0;\n}\n'
	assert msvc_compat_c_source(src) == src
}

fn test_extern_declarations_and_prototypes_keep_their_names() {
	out := msvc_lower_fn('x = ({ extern int g_count; void helper(int); helper(g_count); g_count; });')
	assert out.contains('extern int g_count; void helper(int); helper(g_count); x = (g_count);')
}

fn test_statement_expressions_in_macro_loop_bodies_stay_in_the_loop() {
	out := msvc_lower_fn('int n = 0; each(item, list) { n += ({ int t = g(item); t; }); } h(n);')
	assert out.contains('int n = 0; each(item, list) { int t__vmsvc1 = g(item); n += (t__vmsvc1); } h(n);')
}
