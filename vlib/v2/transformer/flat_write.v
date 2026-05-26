// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast

// flat_write.v scaffolds the "transformer writes flat directly" multi-session
// port. The wedge `transform_files_to_flat` (transformer.v ~line 1855) today
// composes the legacy per-file transform with a boundary `ast.flatten_files()`
// so callers can adopt the flat-output API now. The peak-memory win documented
// in project_v2_flat_migration.md only materialises when the per-file rewrite
// sites stop materialising legacy `ast.File` / `ast.Stmt` / `ast.Expr` values
// and emit `FlatNode`s into the supplied `ast.FlatBuilder` directly.
//
// The entry point is `transform_file_index_to_flat` below. Its body is the
// staging ground for the port: every session in the plan replaces one rewrite
// site inside `transform_file` with a flat-emitting equivalent, leaving the
// outer signature stable. The differential harness in
// transformer_flat_diff_test.v pins both the wedge invariant (4th row,
// `to_flat_*`) and the per-file invariant (5th row, `per_file_*`) so each
// session's change must keep both signatures bit-equal.
//
// ----- Migration phases -----
//
// The 55-site audit from session 1 is the inventory of work, but the port
// itself decomposes into roughly six phases. Each phase ships one
// architectural seam; the per-rewrite-site ports of phase 4 are then
// mechanical follow-ups that plug into the seams.
//
// Phase 1 (DONE, session 1): scaffolding.
//   `transform_file_index_to_flat` exists. Harness 5th row pins per-file
//   parity against a manual rehydrate+transform+append loop. Body is the
//   identity decomposition: rehydrate + transform_file + append_file.
//
// Phase 2 (DONE, session 2): file-level emission decomposition.
//   `ast.FlatBuilder.append_file_with_stmt_ids` accepts pre-emitted stmt
//   FlatNodeIds for the file's stmt list. `transform_file_index_to_flat`
//   now mirrors `transform_file`'s prologue itself, runs `transform_stmts`
//   for the body, and emits each transformed stmt via the new per-stmt
//   seam (`transform_stmt_to_flat` → `out.emit_stmt(...)`). The post-stmt
//   file root is assembled via `append_file_with_stmt_ids`. The per-stmt
//   seam is where phases 3..5 plug per-variant direct-emit logic.
//
// Phase 3 (DONE, session 3): per-stmt direct-emit dispatch.
//   `transform_file_index_to_flat` now bypasses `transform_stmts` at the
//   file level (top-level stmts never trigger its multi-stmt expansions —
//   all those paths live in function bodies). `transform_stmt_to_flat`
//   carries an 11-variant leaf-arm match for stmt kinds that are identity
//   in `transform_stmt`'s `else { stmt }` case (AsmStmt, Directive,
//   EmptyStmt, EnumDecl, FlowControlStmt, ImportStmt, InterfaceDecl,
//   ModuleStmt, StructDecl, TypeDecl, []Attribute); these direct-emit and
//   skip `transform_stmt` entirely. Non-leaf arms take the legacy
//   round-trip `out.emit_stmt(t.transform_stmt(stmt))`. The dispatch
//   surface is now ready for phase 4 ports.
//
// Phase 4 (NEXT — per-rewrite-site ports).
//   For each non-leaf Stmt variant (AssignStmt, BlockStmt, ConstDecl,
//   DeferStmt, ExprStmt, FnDecl, ForStmt, ForInStmt, GlobalDecl,
//   ComptimeStmt, LabelStmt, ReturnStmt, AssertStmt) rewrite the rewrite
//   logic inside `transform_X` to emit `FlatNode`s via `out.emit(...)`
//   directly instead of constructing legacy `ast.Stmt` / `ast.Expr`
//   values. Add `transform_expr_to_flat` with the same dispatch shape for
//   expression-level rewrites. The inventory below is the checklist.
//
// Phase 5: post-pass port.
//   `post_pass` (runtime const init injection, helper functions, str/clone
//   generation, ...) still mutates `[]ast.File`. Port it to either emit
//   directly into the builder or run as a separate flat-write pass after
//   `transform_file_index_to_flat` is complete for all files.
//
// Phase 6: drop legacy materialisation.
//   `transform_files_to_flat`'s second return value (`[]ast.File`) is the
//   last consumer keeping legacy AST alive. Once the SSA builder consumes
//   flat directly, the return shape changes to `ast.FlatAst` only and the
//   boundary `flatten_files()` inside the wedge is removed. This is where
//   the peak-memory win from project_v2_flat_migration.md materialises.
//
// ----- Rewrite Site Inventory (55 sites, audited 2026-05-26) -----
//
// Phase 4's checklist. Categories are coarse — within a category the
// rewrites share most of their helper state (e.g. all or-block expansions
// share `expand_*_or_expr`).
//
// Control-flow lowering (~15 sites):
//   - if.v: if-guard expansion (assign + stmt forms, 2 sites)
//   - for.v: for-in map / array / range / fixed-array / untyped (6 sites)
//   - expr.v: IfGuardExpr in IfExpr, match-expression lowering (~7 sites)
//
// Or-block / Result / Option (~8 sites):
//   - transformer.v: expand_direct_or_expr_assign, expand_single_or_expr,
//     string-range or-block, return-or-block (4 sites)
//   - expr.v: OrExpr in expression context, deref-or, index-or (~4 sites)
//
// Operator / method lowering (~12 sites):
//   - expr.v: operator overload, in-operator, infix coercions, prefix lower,
//     index/slice rewrites, comptime field access (~10 sites)
//   - fn.v: CallExpr method-to-function rewrites, CallOrCastExpr (2 sites)
//
// Structure init (~8 sites):
//   - struct.v: array init, indexed array init, spread, map init, struct
//     init, default-field fill (6 sites)
//   - fn.v: generic specialization synthetic args, FnDecl rewrites (2 sites)
//
// String operations (~3 sites):
//   - transformer.v: string interpolation lowering, embed_file
//   - expr.v: string range / string method dispatch
//
// Defer / lock / go (~4 sites):
//   - transformer.v: defer lowering, lock/rlock expansion, go-wrapper
//   - fn.v: native interface vtable elision
//
// Generic specialization (~2 sites):
//   - fn.v: monomorphize call rewrite
//   - transformer.v: lower_assoc_expr
//
// Misc (~3 sites):
//   - transformer.v: assert message lowering, assign_stmt multi-return,
//     transform_stmt dispatcher special cases
//   - orm.v: SQL expression lowering (1 site)
//
// ----- Per-file flat-write entry point -----

// transform_file_index_to_flat is the per-file entry point for the multi-
// session port. It rehydrates a single file from `input_flat`, mirrors
// `transform_file`'s prologue, and emits each top-level stmt through the
// `transform_stmt_to_flat` seam. Returns the FlatNodeId of the appended
// file root, or `ast.invalid_flat_node_id` for an empty / missing source
// file.
//
// As of phase 3 the loop bypasses `transform_stmts` at the file level —
// top-level stmt variants don't trigger any of `transform_stmts`'
// multi-stmt expansions (AssignStmt / ExprStmt / ForStmt / AssertStmt
// expansions all live in function bodies, not at file scope) so per-stmt
// transform via the seam is equivalent. A defensive `t.pending_stmts`
// drain catches any leak from constructs that have not been audited yet.
//
// Callers must invoke `pre_pass_from_flat(input_flat)` before the per-file
// loop and `post_pass(mut collected_files)` after, mirroring the wedge. The
// per-file API does not run those passes itself so future phases can
// interleave file emissions with pre/post bookkeeping without re-running it.
pub fn (mut t Transformer) transform_file_index_to_flat(input_flat &ast.FlatAst, fi int, mut out ast.FlatBuilder) ast.FlatNodeId {
	src_arr := input_flat.to_files_range(fi, fi + 1)
	if src_arr.len == 0 {
		return ast.invalid_flat_node_id
	}
	file := src_arr[0]
	// Mirror transform_file's per-file prologue. transform_stmt and the
	// rewrite sites read these fields to resolve cross-stmt references.
	t.cur_file_name = file.name
	t.cur_module = file.mod
	if scope := t.get_module_scope(file.mod) {
		t.scope = scope
	} else {
		t.scope = unsafe { nil }
	}
	mut stmt_ids := []ast.FlatNodeId{cap: file.stmts.len}
	for stmt in file.stmts {
		id := t.transform_stmt_to_flat(stmt, mut out)
		// Defensive pending_stmts drain. Inner transform_stmts (called by
		// transform_fn_decl etc.) drains its own pending_stmts before
		// returning; this catches any leak from a top-level construct that
		// hasn't been audited. Hoist them ahead of the just-emitted stmt so
		// the ordering matches transform_stmts' append_transformed_stmt path.
		if t.pending_stmts.len > 0 {
			stmt_ids << id
			pending := t.pending_stmts.clone()
			t.pending_stmts.clear()
			last := stmt_ids.pop()
			for ps in pending {
				stmt_ids << out.emit_stmt(ps)
			}
			stmt_ids << last
		} else {
			stmt_ids << id
		}
	}
	return out.append_file_with_stmt_ids(file, stmt_ids)
}

// transform_stmt_to_flat is the per-stmt dispatch for phase 3 of the port.
// Leaf-arm variants (those that fall into `transform_stmt`'s `else { stmt }`
// case, i.e. identity in the legacy transform) are direct-emitted into `out`,
// skipping the `transform_stmt` dispatch and return-value construction.
// Non-leaf variants still take the legacy round-trip:
// `out.emit_stmt(t.transform_stmt(stmt))`. Phase 4 sessions replace one
// non-leaf arm at a time with direct flat-emit logic that bypasses the
// legacy ast construction at the rewrite site itself.
//
// The harness 5th row pins bit-equality of this seam against a reference
// rehydrate+transform+append loop; any per-arm divergence trips it.
pub fn (mut t Transformer) transform_stmt_to_flat(stmt ast.Stmt, mut out ast.FlatBuilder) ast.FlatNodeId {
	// `[]Attribute` is a Stmt variant but cannot appear in a match arm with
	// regular types; check it first via `is`.
	if stmt is []ast.Attribute {
		return out.emit_stmt(stmt)
	}
	match stmt {
		ast.AsmStmt, ast.Directive, ast.EmptyStmt, ast.EnumDecl, ast.FlowControlStmt,
		ast.ImportStmt, ast.InterfaceDecl, ast.ModuleStmt, ast.StructDecl, ast.TypeDecl {
			return out.emit_stmt(stmt)
		}
		else {
			return out.emit_stmt(t.transform_stmt(stmt))
		}
	}
}
