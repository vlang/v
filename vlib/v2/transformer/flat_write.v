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
// Phase 4 (IN PROGRESS — per-rewrite-site ports).
//   For each non-leaf Stmt variant (AssignStmt, BlockStmt, ConstDecl,
//   DeferStmt, ExprStmt, FnDecl, ForStmt, ForInStmt, GlobalDecl,
//   ComptimeStmt, LabelStmt, ReturnStmt, AssertStmt) rewrite the rewrite
//   logic inside `transform_X` to emit `FlatNode`s via `out.emit(...)`
//   directly instead of constructing legacy `ast.Stmt` / `ast.Expr`
//   values. The inventory below is the checklist.
//
//   Session 1 (2026-05-26): expression dispatch surface + GlobalDecl port.
//     `transform_expr_to_flat` mirrors `transform_stmt_to_flat`'s shape with
//     9 leaf-arm direct-emit variants (BasicLiteral, EmptyExpr, Keyword,
//     LifetimeExpr, RangeExpr, SelectExpr, StringLiteral, Tuple, Type — all
//     identity in `transform_expr`'s `else { expr }` case); non-leaf arms
//     take the legacy round-trip `out.emit_expr(t.transform_expr(expr))`.
//     The GlobalDecl arm in `transform_stmt_to_flat` direct-emits via
//     `transform_expr_to_flat` for each field's typ/value plus the new
//     `emit_field_decl_by_ids` / `emit_global_decl_by_ids` builder helpers,
//     skipping the `transform_global_decl` round-trip entirely. The
//     `__global init` fixture in the 5th harness row pins it.
//
//   Session 2 (2026-05-26): ConstDecl port + harness fixture.
//     The ConstDecl arm in `transform_stmt_to_flat` direct-emits via
//     `transform_expr_to_flat` for each field's value plus the new
//     `emit_field_init_by_id` / `emit_const_decl_by_ids` builder helpers,
//     skipping the `transform_const_decl` round-trip entirely. Mirrors the
//     GlobalDecl shape but uses FieldInit (name + value only, no typ/attrs)
//     and carries the `is_public` flag. A new `fixture_const_decl` covers
//     leaf (BasicLiteral, StringLiteral) and non-leaf (InfixExpr) const
//     values across all 5 harness rows.
//
//   Session 3 (2026-05-26): FnDecl dispatch arm (shape port).
//     The FnDecl arm in `transform_stmt_to_flat` calls `transform_fn_decl`
//     for the body work (prologue / scope setup / `transform_stmts` /
//     defer lowering — the next port target) but decomposes the resulting
//     `ast.FnDecl` and emits via the new `emit_parameter` / `emit_type` /
//     `emit_fn_decl_by_ids` builder helpers. Body stmts go through
//     `out.emit_stmt(body_stmt)` per stmt and the list is assembled via
//     `emit_aux_list_from_ids`. Bit-equal to the legacy
//     `out.emit_stmt(t.transform_fn_decl(stmt))` round-trip; the value is
//     that the dispatch arm now exists, so the next session can refactor
//     `transform_fn_decl` to emit body stmts straight into the builder
//     without touching this dispatch site. Every existing fixture
//     exercises this arm — all 46 harness tests pin it.
//
//   Session 4 (2026-05-26): FnDecl wrapper-struct elision.
//     Extracts `transform_fn_decl_parts(decl) (attrs, stmts)` in fn.v as
//     the body-work driver — returns the two variable parts of the lowered
//     FnDecl (final attribute list and final transformed + defer-lowered
//     stmts) and leaves the immutable name / typ / receiver / language /
//     is_public / is_method / is_static / pos fields to be re-attached by
//     the caller. Existing `transform_fn_decl` becomes a thin wrapper that
//     calls the helper and assembles the `ast.FnDecl` (no behavioural
//     change for legacy callers). The FnDecl flat-write arm now calls
//     `transform_fn_decl_parts` directly, so the `ast.FnDecl` wrapper
//     struct that session 3 built and then decomposed is never allocated.
//     First real (if modest) memory saving in the flat-write port: one
//     FnDecl struct per fn under V2_MARKUSED_FLAT-style flat-output paths.
//
//   Session 5 (2026-05-26): ParenExpr expr direct-emit + harness fixture.
//     The ParenExpr arm in `transform_expr_to_flat` direct-emits via a
//     recursive `transform_expr_to_flat` for the inner expression plus the
//     new `emit_paren_expr_by_id` builder helper, skipping the
//     `ast.ParenExpr` struct allocation per occurrence (the `transform_expr`
//     ParenExpr arm just recurses + rebuilds the wrapper — identity in
//     shape). First non-leaf *expression* port — paves the way for porting
//     larger expression rewrites (PrefixExpr, CastExpr, InfixExpr ...) into
//     the dispatch surface. New `fixture_paren_expr` (file-scope consts
//     wrapping leaf and nested-paren values) covers the arm across all 5
//     harness rows — 46 → 51 transformer-diff tests.
//
//   Session 6 (2026-05-26): PrefixExpr expr direct-emit + harness fixture.
//     The PrefixExpr arm in `transform_expr_to_flat` direct-emits via a
//     recursive `transform_expr_to_flat` for the inner expression plus the
//     new `emit_prefix_expr_by_id` builder helper, skipping the
//     `ast.PrefixExpr` struct allocation for the common case (unary `-`,
//     `~`, `!`). Two legacy fallback guards keep semantics intact:
//     `expr.op == .amp` (transform_prefix_expr removes `&` redundancy with
//     attribute-aware lowering — not bit-equal to a structural rebuild) and
//     `expr.op == .arrow && expr.expr is ast.OrExpr` (channel-recv with
//     or-block is lowered by `expand_chan_recv_or_expr` — full
//     statement-list synthesis). Both stay on the round-trip
//     `out.emit_expr(t.transform_expr(expr))` path. New
//     `fixture_prefix_expr` (file-scope consts with `-`, `~`, `!`) covers
//     the arm across all 5 harness rows — 51 → 56 transformer-diff tests.
//
//   Session 7 (2026-05-26): ModifierExpr expr direct-emit + harness fixture.
//     The ModifierExpr arm in `transform_expr_to_flat` direct-emits via a
//     recursive `transform_expr_to_flat` for the inner expression plus the
//     new `emit_modifier_expr_by_id` builder helper, skipping the
//     `ast.ModifierExpr` struct allocation per occurrence (`mut x`,
//     `shared x`, `atomic x`, `static x`, `volatile x` — all pure wrappers
//     around the inner expression in `transform_expr`'s arm; kind + pos
//     copied verbatim). Structurally identical to the ParenExpr session
//     except the kind token packs into the meta u16 (mirror of
//     `add_expr(ModifierExpr)`'s encoding). New `fixture_modifier_expr`
//     (a `mut`-parameter fn + a caller that passes `mut a`) covers the arm
//     across all 5 harness rows — 56 → 61 transformer-diff tests.
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
		ast.ConstDecl {
			// Mirror transform_const_decl: transform each field's value via
			// transform_expr_to_flat, keep name + is_public unchanged, and emit
			// the flat encoding directly. The intermediate `ast.ConstDecl` /
			// `[]ast.FieldInit` allocations from the legacy round-trip drop
			// out. Emit order matches add_stmt(ConstDecl): per-field (value,
			// field_init), then the fields aux_list, then the stmt.
			mut field_ids := []ast.FlatNodeId{cap: stmt.fields.len}
			for field in stmt.fields {
				value_id := t.transform_expr_to_flat(field.value, mut out)
				field_ids << out.emit_field_init_by_id(field.name, value_id)
			}
			fields_list_id := out.emit_aux_list_from_ids(field_ids)
			return out.emit_const_decl_by_ids(stmt.is_public, fields_list_id)
		}
		ast.GlobalDecl {
			// Mirror transform_global_decl: transform each field's typ and value
			// via transform_expr_to_flat, copy attributes/name/is_public/is_mut
			// unchanged, and emit the flat encoding directly. The intermediate
			// `ast.GlobalDecl` / `[]ast.FieldDecl` allocations from the legacy
			// round-trip drop out. Emit order matches add_stmt(GlobalDecl):
			// decl attributes list first, then per-field (typ, value, field
			// attrs, field_decl), then the fields aux_list, then the stmt.
			decl_attrs_id := out.emit_attribute_list(stmt.attributes)
			mut field_ids := []ast.FlatNodeId{cap: stmt.fields.len}
			for field in stmt.fields {
				typ_id := t.transform_expr_to_flat(field.typ, mut out)
				value_id := t.transform_expr_to_flat(field.value, mut out)
				field_attrs_id := out.emit_attribute_list(field.attributes)
				field_ids << out.emit_field_decl_by_ids(field.name, typ_id, value_id,
					field_attrs_id)
			}
			fields_list_id := out.emit_aux_list_from_ids(field_ids)
			return out.emit_global_decl_by_ids(decl_attrs_id, fields_list_id)
		}
		ast.FnDecl {
			// Direct emit: call `transform_fn_decl_parts` (the body-work driver
			// extracted from `transform_fn_decl`) for the two variable parts —
			// final attributes (possibly augmented for `@[live]`) and the final
			// transformed + defer-lowered stmt list — and assemble the FnDecl
			// flat encoding via the typed builder helpers. Skips the
			// `ast.FnDecl` wrapper struct that `transform_fn_decl` builds in the
			// legacy path. The immutable is_public/is_method/is_static/receiver/
			// language/name/typ/pos fields come straight from `stmt`. Bit-equal
			// to the legacy `out.emit_stmt(t.transform_fn_decl(stmt))` round-trip.
			attrs, stmts := t.transform_fn_decl_parts(stmt)
			receiver_id := out.emit_parameter(stmt.receiver)
			typ_id := out.emit_type(ast.Type(stmt.typ))
			attrs_id := out.emit_attribute_list(attrs)
			mut stmt_ids := []ast.FlatNodeId{cap: stmts.len}
			for body_stmt in stmts {
				stmt_ids << out.emit_stmt(body_stmt)
			}
			stmts_list_id := out.emit_aux_list_from_ids(stmt_ids)
			return out.emit_fn_decl_by_ids(stmt.name, stmt.is_public, stmt.is_method,
				stmt.is_static, stmt.language, stmt.pos, receiver_id, typ_id, attrs_id,
				stmts_list_id)
		}
		else {
			return out.emit_stmt(t.transform_stmt(stmt))
		}
	}
}

// transform_expr_to_flat is the per-expr dispatch for phase 4 of the port.
// Leaf-arm expr variants (BasicLiteral, EmptyExpr, Keyword, LifetimeExpr,
// RangeExpr, SelectExpr, StringLiteral, Tuple, Type — all identity in
// `transform_expr`'s `else { expr }` case) are direct-emitted into `out`,
// skipping the legacy transform_expr dispatch. Non-leaf variants take the
// legacy round-trip `out.emit_expr(t.transform_expr(expr))`. Phase 4
// sessions replace one non-leaf arm at a time with direct-emit logic that
// bypasses the legacy ast construction at the rewrite site itself.
//
// The 5th harness row pins bit-equality against the reference rehydrate+
// transform+append loop; any per-arm divergence trips it.
pub fn (mut t Transformer) transform_expr_to_flat(expr ast.Expr, mut out ast.FlatBuilder) ast.FlatNodeId {
	match expr {
		ast.BasicLiteral, ast.EmptyExpr, ast.Keyword, ast.LifetimeExpr, ast.RangeExpr,
		ast.SelectExpr, ast.StringLiteral, ast.Tuple, ast.Type {
			return out.emit_expr(expr)
		}
		ast.ParenExpr {
			// ParenExpr is a pure wrapper: its `transform_expr` arm just
			// recurses into `expr.expr` and rebuilds the wrapper. Direct-emit
			// skips the `ast.ParenExpr` struct allocation per occurrence by
			// transforming the inner expression via `transform_expr_to_flat`
			// (so nested leaves also bypass the legacy round-trip) and
			// assembling the flat node via the new `emit_paren_expr_by_id`
			// builder helper. Mirrors `add_expr(ParenExpr)` encoding exactly.
			inner_id := t.transform_expr_to_flat(expr.expr, mut out)
			return out.emit_paren_expr_by_id(inner_id, expr.pos)
		}
		ast.PrefixExpr {
			// PrefixExpr's `transform_expr` arm has three lowering side cases:
			// `&` operand that is a type cast (amp_type_cast_expr), `&` operand
			// that is an `AssocExpr` (`&{base | field: val}`), and `^` operand
			// that is an `OrExpr` (rewrites to `OrExpr` wrapping a deref).
			// These produce *different* expression shapes — fall back to the
			// legacy round-trip so the rewrite logic runs in one place. For all
			// other operators (`-`, `!`, `~`, `++`, `--`, ...) the legacy arm
			// is a pure wrapper around `transform_expr(expr.expr)`; direct-emit
			// recurses into `transform_expr_to_flat` and assembles the flat
			// node via the new `emit_prefix_expr_by_id` helper, skipping the
			// `ast.PrefixExpr` struct allocation per occurrence.
			needs_legacy := expr.op == .amp || (expr.op == .arrow && expr.expr is ast.OrExpr)
			if needs_legacy {
				return out.emit_expr(t.transform_expr(expr))
			}
			inner_id := t.transform_expr_to_flat(expr.expr, mut out)
			return out.emit_prefix_expr_by_id(expr.op, inner_id, expr.pos)
		}
		ast.ModifierExpr {
			// ModifierExpr (`mut x`, `shared x`, `atomic x`, `static x`,
			// `volatile x`) is a pure wrapper around its inner expression — the
			// `transform_expr` arm just recurses into `expr.expr` and rebuilds
			// the wrapper with `kind` and `pos` copied verbatim. Direct-emit
			// skips the `ast.ModifierExpr` struct allocation by transforming the
			// inner expression via `transform_expr_to_flat` and assembling the
			// flat node via the new `emit_modifier_expr_by_id` helper. Mirrors
			// `add_expr(ModifierExpr)` encoding exactly (kind in meta u16).
			inner_id := t.transform_expr_to_flat(expr.expr, mut out)
			return out.emit_modifier_expr_by_id(expr.kind, inner_id, expr.pos)
		}
		else {
			return out.emit_expr(t.transform_expr(expr))
		}
	}
}
