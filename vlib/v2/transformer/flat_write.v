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
// Phase 3 (NEXT): per-stmt direct-emit dispatch.
//   `transform_stmt_to_flat` grows a `match stmt { ... }` arm per variant.
//   Initially every arm delegates to `out.emit_stmt(t.transform_stmt(stmt))`
//   (identical to today). Each session in phase 3 replaces one arm with
//   direct emission that skips the legacy `ast.Stmt` round-trip. Start with
//   the leaves that have no rewrites in `transform_stmt` (Directive,
//   EmptyStmt, AsmStmt) so the first arm-port is bit-identity by
//   construction.
//
// Phase 4 (per-rewrite-site ports).
//   Once `transform_stmt_to_flat` covers every Stmt variant, add
//   `transform_expr_to_flat` with the same dispatch shape. Then port each
//   of the 55 rewrite sites: the trigger logic moves into the new dispatch
//   arms and the output is written directly via `out.emit(...)`. The
//   inventory below is the checklist for this phase.
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
// `transform_file`'s prologue, runs `transform_stmts` for the body, and emits
// the transformed stmts plus the file header into `out`. Returns the
// FlatNodeId of the appended file root, or `ast.invalid_flat_node_id` for an
// empty / missing source file.
//
// As of phase 2 the body emits stmts one at a time via
// `transform_stmt_to_flat`, decoupling the file-level shape from the stmt
// list. Each future session in phase 3 replaces one arm of
// `transform_stmt_to_flat`'s dispatch with direct flat emission.
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
	transformed_stmts := t.transform_stmts(file.stmts)
	mut stmt_ids := []ast.FlatNodeId{cap: transformed_stmts.len}
	for stmt in transformed_stmts {
		stmt_ids << t.transform_stmt_to_flat(stmt, mut out)
	}
	return out.append_file_with_stmt_ids(file, stmt_ids)
}

// transform_stmt_to_flat is the per-stmt seam for phase 3 of the port. Today
// every variant falls through to the legacy round-trip
// (`out.emit_stmt(t.transform_stmt(stmt))`); the input `stmt` has already
// been through `transform_stmts` (and therefore `transform_stmt`) at the
// call site in `transform_file_index_to_flat`, so the inner call here is a
// no-op identity transform that just routes the stmt into the builder.
//
// Phase 3 sessions replace one arm at a time with direct `out.emit(...)`
// logic that skips the legacy ast.Stmt construction at the rewrite site.
// Until then this function preserves bit-equal output: the harness 5th row
// guarantees the per-stmt path matches a reference rehydrate+transform+append
// loop.
pub fn (mut t Transformer) transform_stmt_to_flat(stmt ast.Stmt, mut out ast.FlatBuilder) ast.FlatNodeId {
	return out.emit_stmt(stmt)
}
