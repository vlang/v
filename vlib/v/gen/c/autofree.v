// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import strings
import v.ast

fn (g &Gen) needs_scope_cleanup() bool {
	// `g.perceus_dropping`: when Perceus emission is decoupled from `-autofree`
	// (E mode: `-d perceus` without autofree, so autofree's scope-exit freeing /
	// clone / temp machinery — incompatible with a tracing GC — never runs), the
	// only frees in the program are the Perceus drops. This flag is true ONLY
	// transiently inside perceus_drop(), so it authorizes that single free dispatch
	// without enabling any other scope cleanup.
	return g.is_autofree || g.pref.gc_mode == .boehm_leak || g.perceus_dropping
}

fn (mut g Gen) autofree_scope_vars(pos int, line_nr int, free_parent_scopes bool) {
	if !g.needs_scope_cleanup() {
		return
	}
	// g.writeln('// afsv pos=${pos} line_nr=${line_nr} freeparent_scopes=${free_parent_scopes}')
	g.autofree_scope_vars_stop(pos, line_nr, free_parent_scopes, -1)
}

fn (mut g Gen) autofree_scope_vars_stop(pos int, line_nr int, free_parent_scopes bool, stop_pos int) {
	if !g.needs_scope_cleanup() {
		return
	}
	if g.is_builtin_mod {
		// In `builtin` everything is freed manually.
		return
	}
	if pos == -1 {
		// TODO: why can pos be -1?
		return
	}
	// eprintln('> free_scope_vars(${pos})')
	scope := g.file.scope.innermost(pos)
	// g.writeln('// scope start pos=${scope.start_pos} ')
	if scope.start_pos == 0 {
		// TODO: why can scope.pos be 0? (only outside fns?)
		return
	}
	g.trace_autofree('// autofree_scope_vars(pos=${pos} line_nr=${line_nr} scope.pos=${scope.start_pos} scope.end_pos=${scope.end_pos})')
	g.autofree_scope_vars2(scope, scope.start_pos, scope.end_pos, line_nr, free_parent_scopes,
		stop_pos)
}

@[if trace_autofree ?]
fn (mut g Gen) trace_autofree(line string) {
	g.writeln(line)
}

//@[if print_autofree_vars ?]
// fn (mut g Gen) print_autofree_var(var string, position string, comment string) {
fn (mut g Gen) print_autofree_var(var ast.Var, comment string) {
	if !g.pref.print_autofree_vars && g.pref.print_autofree_vars_in_fn == '' {
		return
	}
	println('autofree: ${g.file.path}:${var.pos.line_nr}: skipping `${var.name}` in fn `${g.last_fn_c_name}`. ${comment}')
}

fn (mut g Gen) autofree_scope_vars2(scope &ast.Scope, start_pos int, end_pos int, line_nr int, free_parent_scopes bool,
	stop_pos int) {
	if scope == unsafe { nil } {
		return
	}
	g.trace_autofree('// scopeobjects.len == ${scope.objects.len}')
	for _, obj in scope.objects {
		match obj {
			ast.Var {
				g.trace_autofree('// var "${obj.name}" var.pos=${obj.pos.pos} var.line_nr=${obj.pos.line_nr}')
				// Perceus (`-d perceus`): this var is dropped at its last use, so
				// skip the scope-exit free here to avoid a double free.
				if g.is_perceus && obj.pos.pos in g.perceus_suppress {
					g.trace_autofree('// perceus: skipping scope-exit free of "${obj.name}" (dropped at last use)')
					continue
				}
				if obj.name in g.returned_var_names {
					g.print_autofree_var(obj, 'returned from function')
					g.trace_autofree('// skipping returned var')
					continue
				}
				if obj.is_or {
					// Skip vars inited with the `or {}`, since they are generated
					// after the or block in C.
					g.trace_autofree('// skipping `or {}` var "${obj.name}"')
					continue
				}
				if obj.is_tmp {
					// Skip for loop vars
					g.print_autofree_var(obj, 'tmp var (loop?)')
					g.trace_autofree('// skipping tmp var "${obj.name}"')
					continue
				}
				if obj.is_inherited {
					g.print_autofree_var(obj, 'inherited')
					g.trace_autofree('// skipping inherited var "${obj.name}"')
					continue
				}
				// if var.typ == 0 {
				// // TODO: why 0?
				// continue
				// }
				// if v.pos.pos > end_pos {
				if obj.pos.pos > end_pos
					|| (obj.pos.pos < start_pos && obj.pos.line_nr == line_nr)
					|| (end_pos < scope.end_pos && obj.expr is ast.IfExpr) {
					// Do not free vars that were declared after this scope
					continue
				}
				if obj.expr is ast.IfGuardExpr {
					continue
				}
				if obj.expr is ast.UnsafeExpr && obj.expr.expr is ast.CallExpr
					&& (obj.expr.expr as ast.CallExpr).is_method {
					if left_var := scope.objects[obj.expr.expr.left.str()] {
						if func := g.table.find_method(g.table.final_sym(left_var.typ),
							obj.expr.expr.name)
						{
							if func.attrs.contains('reused') && left_var is ast.Var
								&& left_var.expr is ast.CastExpr {
								if left_var.expr.expr.is_literal() {
									continue
								}
							}
						}
					}
				}
				g.autofree_variable(obj)
			}
			else {}
		}
	}
	for g.autofree_scope_stmts.len > 0 {
		g.write(g.autofree_scope_stmts.pop())
	}
	// Free all vars in parent scopes as well:
	// ```
	// s := ...
	// if ... {
	// s.free()
	// return
	// }
	// ```
	// if scope.parent != unsafe { nil } && line_nr > 0 {
	if free_parent_scopes && scope.parent != unsafe { nil } && !scope.detached_from_parent
		&& (stop_pos == -1 || scope.parent.start_pos >= stop_pos) {
		g.trace_autofree('// af parent scope:')
		g.autofree_scope_vars2(scope.parent, start_pos, end_pos, line_nr, true, stop_pos)
	}
}

// perceus_drop emits a deterministic free for `v` at the current (last-use)
// position, reusing the same per-type free dispatch as scope-exit autofree
// (`autofree_variable` buffers into `autofree_scope_stmts`; we flush it here so
// the free lands inline after the drop site instead of at scope exit). The
// scope-exit free for `v` is suppressed via `perceus_suppress`, so this is the
// var's sole free — same body, earlier position.
fn (mut g Gen) perceus_drop(v ast.Var) {
	before := g.autofree_scope_stmts.len
	// Authorize freeing a proven-unique user-reference (`&Foo`) here even without
	// `-experimental`; Perceus uniqueness is the soundness proof that gate lacks.
	old_dropping := g.perceus_dropping
	g.perceus_dropping = true
	g.autofree_variable(v)
	g.perceus_dropping = old_dropping
	if g.autofree_scope_stmts.len > before {
		pending := g.autofree_scope_stmts[before..].clone()
		g.autofree_scope_stmts.trim(before)
		for s in pending {
			g.write(s)
		}
	}
}

fn (mut g Gen) autofree_variable(v ast.Var) {
	// filter out invalid variables
	if v.typ == 0 {
		return
	}
	sym := g.table.sym(v.typ)
	// if v.name.contains('output2') {
	if g.is_autofree {
		// eprintln('   > var name: ${v.name:-20s} | is_arg: ${v.is_arg.str():6} | var type: ${int(v.typ):8} | type_name: ${sym.name:-33s}')
	}
	// }
	base_typ := v.typ.set_nr_muls(0).clear_option_and_result()
	if g.type_has_unresolved_generic_parts(base_typ) {
		g.print_autofree_var(v, 'unresolved generic type')
		return
	}
	mut free_fn := g.styp(base_typ) + '_free'
	if sym.kind == .array {
		free_fn = g.get_free_method(base_typ)
		g.autofree_var_call(free_fn, v)
		return
	}
	if sym.kind == .map {
		free_fn = g.get_free_method(base_typ)
		g.autofree_var_call(free_fn, v)
		return
	}
	if sym.kind == .string {
		// Don't free simple string literals.
		match v.expr {
			ast.StringLiteral {
				g.print_autofree_var(v, 'string literal')
				g.trace_autofree('// str literal')
			}
			else {
				// NOTE/TODO: assign_stmt multi returns variables have no expr
				// since the type comes from the called fns return type
				/*
				f := v.name[0]
				if
					//!(f >= `a` && f <= `d`) {
					//f != `c` {
					v.name!='cvar_name' {
					t := typeof(v.expr)
				return '// other ' + t + '\n'
				}
				*/
			}
		}

		g.autofree_var_call('builtin__string_free', v)
		return
	}
	if sym.is_builtin() {
		free_fn = 'builtin__${free_fn}'
	}
	// Free user reference types
	is_user_ref := v.typ.is_ptr() && sym.name.after('.')[0].is_capital()
	// if g.pref.experimental && v.typ.is_ptr() && sym.name.after('.')[0].is_capital() {
	if is_user_ref {
		// Perceus (`g.perceus_dropping`) authorizes freeing this user-reference
		// because its uniqueness analysis PROVED this `&Foo` uniquely owned and dead
		// here — which is exactly what the blanket `-experimental` flag cannot
		// establish. So we free it whether or not `-experimental` is set; without a
		// proof (no Perceus, no -experimental) we still leave it to leak.
		if g.pref.experimental || g.perceus_dropping {
			if g.pref.gc_mode == .vgc && g.perceus_dropping && v.name in g.perceus_deep_drop {
				// DEEP free under E: the deep-drop analysis proved this `&Foo`
				// uniquely owns its heap fields (born fresh, no field aliased/
				// reassigned), so cascade-free the nested heap fields via the
				// generated `<Foo>_free` BEFORE reclaiming the struct allocation —
				// otherwise the field buffers (e.g. a `[]u8` member) leak and drive
				// GC pressure (R2-E-FINDINGS.md cause #2).
				struct_typ := v.typ.set_nr_muls(0)
				mut field_free := g.get_free_method(struct_typ)
				if g.table.sym(struct_typ).is_builtin() {
					field_free = 'builtin__${field_free}'
				}
				g.autofree_var_call(field_free, v)
				g.autofree_var_call('builtin___v_free', v)
			} else {
				// Under a tracing GC (E mode: a Perceus drop of a unique `&Foo`), the
				// object was allocated by the GC allocator, so it must be returned via
				// the GC's free (`builtin___v_free` routes to `vgc_free`), NOT libc
				// `free` — freeing a GC-arena pointer with libc aborts ("pointer being
				// freed was not allocated"). Manual / autofree-without-GC keeps libc
				// `free` (allocations are libc there), so the validated `-autofree`
				// path is byte-identical.
				uref_free := if g.pref.gc_mode == .vgc { 'builtin___v_free' } else { 'free' }
				g.autofree_var_call(uref_free, v)
			}
		} else {
			g.print_autofree_var(v, 'user reference type, use -experimental to autofree those')
		}
	}
	if sym.has_method('free') {
		g.autofree_var_call(free_fn, v)
	}
}

fn (mut g Gen) autofree_var_call(free_fn_name string, v ast.Var) {
	if v.is_arg {
		// fn args should not be autofreed
		return
	}
	if v.is_used && v.is_autofree_tmp {
		// tmp expr vars do not need to be freed again here
		return
	}
	if g.is_builtin_mod {
		return
	}
	if !g.needs_scope_cleanup() {
		return
	}
	// if v.is_autofree_tmp && !g.doing_autofree_tmp {
	// return
	// }
	if v.name.contains('expr_write_string_1_') {
		// TODO: remove this temporary hack
		return
	}
	mut af := strings.new_builder(128)
	if v.typ.is_ptr() && v.typ.idx() != ast.u8_type_idx {
		// `g.perceus_dropping`: Perceus proved this pointer both UNIQUE and the OWNER
		// of fresh memory (`p := &Foo{...}`), so freeing it is sound even though it is
		// not `is_auto_heap` — that gate exists precisely because plain autofree
		// cannot establish ownership; Perceus can.
		if !v.is_auto_heap && !g.table.sym(v.typ).has_method('free') && !g.perceus_dropping {
			return
		}
		af.write_string('\t')
		if v.typ.share() == .shared_t {
			af.write_string(free_fn_name.replace_each(['__shared__', '']))
		} else {
			af.write_string(free_fn_name)
		}
		af.write_string('(')
		if v.typ.has_flag(.option) {
			base_type := g.base_type(v.typ)
			af.write_string('(${base_type}*)')
		}
		if v.typ.share() == .shared_t {
			af.write_string('&')
		}
		af.write_string(strings.repeat(`*`, v.typ.nr_muls() - 1)) // dereference if it is a pointer to a pointer
		af.write_string(c_name(v.name))
		if v.typ.share() == .shared_t {
			af.write_string('->val')
		}
		if v.typ.has_flag(.option) {
			// `.data` only — the call's opening `(` (written above) is closed by the
			// single `)` below. Previously this wrote `.data)`, double-closing the call
			// for an option-pointer var (`b := &?Foo{}`) and emitting an extraneous `)`
			// (a `-gc e` / Perceus-drop C compile error; the non-option path closes only
			// once below, so this aligns the two).
			af.write_string('.data')
		}

		af.writeln('); // autofreed ptr var')
	} else {
		if v.typ == ast.error_type && !v.is_autofree_tmp {
			return
		}
		if v.is_auto_heap {
			af.writeln('\t${free_fn_name}(${c_name(v.name)}); // autofreed heap var ${g.cur_mod.name} ${g.is_builtin_mod}')
		} else if v.typ.has_flag(.option) {
			base_type := g.base_type(v.typ)
			af.writeln('\tif (${c_name(v.name)}.state != 2) {')
			af.writeln('\t\t${free_fn_name}((${base_type}*)${c_name(v.name)}.data); // autofreed option var ${g.cur_mod.name} ${g.is_builtin_mod}')
			af.writeln('\t}')
		} else if v.typ.idx() != ast.u8_type_idx {
			af.writeln('\t${free_fn_name}(&${c_name(v.name)}); // autofreed var ${g.cur_mod.name} ${g.is_builtin_mod}')
		}
	}
	g.autofree_scope_stmts << af.str()
}

fn (mut g Gen) detect_used_var_on_return(expr ast.Expr) {
	match expr {
		ast.Ident {
			g.returned_var_names[expr.name] = true
		}
		ast.StructInit {
			for field_expr in expr.init_fields {
				g.detect_used_var_on_return(field_expr.expr)
			}
		}
		else {}
	}
}
