// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.token
import v2.types

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
//   Session 8 (2026-05-26): LambdaExpr expr direct-emit + harness fixture.
//     The LambdaExpr arm in `transform_expr_to_flat` direct-emits via a
//     recursive `transform_expr_to_flat` for the body expression plus a
//     leaf `out.emit_expr(ast.Expr(arg))` per arg Ident and the new
//     `emit_lambda_expr_by_ids` builder helper. The `transform_expr` arm
//     for LambdaExpr is a pure wrapper — args are an `[]Ident` copied
//     verbatim, only the body `expr.expr` is transformed — so direct-emit
//     mirrors that exactly: leaf args go through the legacy `emit_expr`
//     leaf path (Idents are identity), body goes through the recursive
//     direct-emit. Mirrors `add_expr(LambdaExpr)` encoding exactly:
//     edge[0] = body expr, edge[1..] = args. New `fixture_lambda_expr`
//     (an `apply(f fn (int) int, x int)` higher-order fn + a caller that
//     passes `|y| y + 1`) covers the arm across all 5 harness rows —
//     61 → 66 transformer-diff tests.
//
//   Session 9 (2026-05-26): FnLiteral expr direct-emit + harness fixture.
//     The FnLiteral arm in `transform_expr_to_flat` direct-emits via:
//     `out.emit_type(ast.Type(expr.typ))` for the FnType (verbatim, leaf),
//     `out.emit_expr(cv)` per captured_var (verbatim, leaf), and
//     `transform_stmts(expr.stmts)` + `out.emit_stmt(...)` per result for
//     the body stmts (still allocates a `[]Stmt` — the stmt-list expansion
//     seam is the next big port target), then the new
//     `emit_fn_literal_by_ids` builder helper. Skips the
//     `ast.FnLiteral` wrapper struct allocation per occurrence. Mirrors
//     `add_expr(FnLiteral)` encoding exactly: edge[0] = type,
//     edge[1..1+captured.len] = captured_vars, edge[1+captured.len..] =
//     stmts; `captured_vars.len` is packed into `extra`. New
//     `fixture_fn_literal` (a `make_doubler()` returning `fn (x int) int`
//     + a `use_fn_literal()` that binds a `fn (n int) int` to a local and
//     calls it) covers the arm across all 5 harness rows — 66 → 71
//     transformer-diff tests.
//
//   Session 10 (2026-05-26): PostfixExpr expr direct-emit + harness fixture.
//     The PostfixExpr arm in `transform_expr_to_flat` direct-emits via a
//     recursive `transform_expr_to_flat` for the inner expression plus the
//     new `emit_postfix_expr_by_id` builder helper. Skips the
//     `ast.PostfixExpr` struct allocation for the plain postfix ops (`++`
//     / `--`, i.e. `.inc` / `.dec`). One legacy fallback guard:
//     `expr.op == .not || expr.op == .question` — these ops trigger
//     non-trivial lowering in `transform_expr` (native backends keep
//     postfix; non-native rewrite `expr!` / `expr?` into a CastExpr over
//     the inner Result/Option base type plus string-range rename
//     `substr` → `substr_checked` plus unwrap fallback). These produce
//     different expression shapes, so they stay on the legacy round-trip.
//     New `fixture_postfix_expr` (a fn that mut-binds a local then runs
//     `a++` / `a--`) covers the arm across all 5 harness rows — 71 → 76
//     transformer-diff tests.
//
//   Session 11 (2026-05-26): CastExpr expr direct-emit + harness fixture.
//     The CastExpr arm in `transform_expr_to_flat` direct-emits via the
//     leaf `out.emit_expr(expr.typ)` (type is NOT transformed by
//     `transform_expr` — copied verbatim) plus a recursive
//     `transform_expr_to_flat` for the inner expression, then assembles
//     via the new `emit_cast_expr_by_ids` builder helper. Skips the
//     `ast.CastExpr` struct allocation per occurrence in the common case
//     (primitive casts: `int(x)`, `f64(y)`, `u8(z)`, etc.).
//     One legacy fallback guard: `t.type_expr_name_full(expr.typ)`
//     resolves to a name that `t.is_sum_type(name)` accepts. In that case
//     `transform_expr` may lower the cast into an explicit sum-type
//     initialisation (CallExpr to `<SumType>__from_<Variant>` / InitExpr),
//     which is a *different* expression shape. Both helpers are read-only
//     so the guard is cheap to evaluate. The existing
//     `fixture_sumtype_is_as` already exercises the fallback (it contains
//     `c := Shape(Circle{r: 2.0})`); new `fixture_cast_expr` (primitive
//     casts only) pins the fast path across all 5 harness rows — 76 → 81
//     transformer-diff tests.
//
//   Session 12 (2026-05-26): FieldInit expr direct-emit + harness fixture.
//     The FieldInit arm in `transform_expr_to_flat` direct-emits via a
//     recursive `transform_expr_to_flat` for the inner `value` plus the
//     existing `emit_field_init_by_id` builder helper (the same helper
//     ConstDecl uses for its aux_field_init child node — `add_expr(FieldInit)`
//     and `add_field_init(field)` produce bit-equal encodings so a single
//     emit helper covers both call sites). Skips the `ast.FieldInit` struct
//     allocation per occurrence — `transform_expr`'s FieldInit arm just
//     transforms `value` and copies `name` verbatim. FieldInit reaches the
//     expr dispatch when it appears as a standalone call arg in struct-
//     shorthand / named-arg syntax (`foo(name: value)`). New
//     `fixture_field_init` (struct + a `make(name: 'a', n: 1)` call) covers
//     the arm across all 5 harness rows — 81 → 86 transformer-diff tests.
//
//   Session 13 (2026-05-26): AsCastExpr expr direct-emit + harness fixture.
//     The AsCastExpr arm in `transform_expr_to_flat` direct-emits via the
//     leaf `out.emit_expr(expr.typ)` (type is NOT transformed by
//     `transform_expr` — copied verbatim) plus a recursive
//     `transform_expr_to_flat` for the inner expression, then assembles via
//     the new `emit_as_cast_expr_by_ids` builder helper. First port that
//     mutates Transformer state: to keep the `as` cast intact (a same-
//     expression smartcast would rewrite the operand into a direct sum
//     payload access and hide the original storage type from the backend),
//     the arm clones `smartcast_stack` + `smartcast_expr_counts`, drains
//     matching smartcast entries via a bounded `remove_smartcast_for_expr`
//     loop, transforms the inner expression with the drained state, then
//     restores the snapshot. Direct-emit mirrors the legacy prologue/restore
//     exactly so the snapshot semantics stay identical — the only thing
//     that changes is the inner recursion target (flat vs legacy). Skips
//     the `ast.AsCastExpr` struct allocation per occurrence. New
//     `fixture_as_cast_expr` (sum type Shape + `(s as Circle).r` / `(s as
//     Square).side` arms in a `pick(s Shape, k int) f64` fn) covers the
//     arm across all 5 harness rows — 86 → 91 transformer-diff tests.
//
//   Session 14 (2026-05-26): UnsafeExpr expr direct-emit + harness fixture.
//     The UnsafeExpr arm in `transform_expr_to_flat` has two paths matching
//     `transform_expr`'s arm:
//       1. `unsafe { nil }` normalises to a plain `nil` Ident (detected via
//          `t.is_unsafe_nil_expr(expr)`) — emitted as a leaf Ident through
//          `out.emit_expr(...)`. Identity in `transform_expr`'s else case.
//       2. otherwise transform `expr.stmts` via `transform_stmts` and emit
//          each result via `out.emit_stmt(...)`, then assemble the flat node
//          via the new `emit_unsafe_expr_by_ids` builder helper. Mirrors
//          `add_expr(UnsafeExpr)` encoding exactly (edges are body stmts in
//          order). Skips the `ast.UnsafeExpr` struct allocation per non-nil
//          occurrence.
//     The body stmt-list is still materialised because `transform_stmts`
//     returns one (the stmt-list expansion seam is the next big port
//     target). New `fixture_unsafe_expr` (a `mut p := unsafe { &int(0) }`
//     with a non-trivial body + plain `unsafe { nil }` for the
//     normalisation arm) covers both paths across all 5 harness rows —
//     91 → 96 transformer-diff tests.
//
//   Session 15 (2026-05-26): LockExpr expr direct-emit + harness fixture.
//     The LockExpr arm in `transform_expr_to_flat` rewrites
//     `lock x { body }` / `rlock x { body }` into a sequence of mutex
//     lock/unlock calls wrapped in an UnsafeExpr (compound expression).
//     `expand_lock_expr` produces `[lock_calls..., body..., unlock_calls...]`;
//     the legacy arm duplicates the last body stmt after the unlock calls so
//     that GCC compound-expression value semantics return the body's tail
//     value instead of `void`. Direct-emit mirrors the rewrite exactly —
//     same `expand_lock_expr` call, same duplicate-last fix-up, then emits
//     each stmt via `out.emit_stmt(...)` and assembles via the existing
//     `emit_unsafe_expr_by_ids` builder helper (the same helper UnsafeExpr
//     session 14 uses, since the legacy LockExpr arm produces an UnsafeExpr).
//     Crucial encoding detail: the legacy `ast.UnsafeExpr{stmts: stmts}` is
//     constructed with no `pos` field (defaults to zero), so direct-emit must
//     pass `token.Pos{}` (not `expr.pos`). First port to import `v2.token`
//     in `flat_write.v` — needed for the zero-pos literal. New
//     `fixture_lock_expr` (a `Counter` struct with `mut value int` + a
//     `bump(mut c Counter) int` that uses `lock c { c.value += 1; c.value }`)
//     covers the arm across all 5 harness rows — 96 → 101 transformer-diff
//     tests.
//
//   Session 16 (2026-05-26): OrExpr expr direct-emit + harness fixture.
//     The OrExpr arm in `transform_expr_to_flat` direct-emits the expression-
//     context or-block (the statement-level sites have dedicated handlers and
//     never reach here). `expand_single_or_expr` returns the data-access
//     expression that stands in for the or-block value and appends prefix
//     stmts (typed temp init, optional-state branch). When `prefix_stmts` is
//     non-empty, the legacy arm wraps everything in an `UnsafeExpr` (GCC
//     compound expression) — direct-emit mirrors that exactly via
//     `emit_unsafe_expr_by_ids` (same helper LockExpr session 15 uses),
//     passing `token.Pos{}` since the legacy wrapper has no explicit pos.
//     When empty, the result expression goes through `out.emit_expr(...)`
//     (leaf). Skips the `ast.UnsafeExpr` wrapper allocation per occurrence
//     in the compound-expression case. New `fixture_or_expr` (a fn
//     returning `pair(maybe(n) or { -1 }, 2)` — or-block nested in call
//     args) covers the arm across all 5 harness rows — 101 → 106
//     transformer-diff tests.
//
//   Session 17 (2026-05-26): IfGuardExpr expr direct-emit (degenerate arm).
//     IfGuardExpr only appears in normal usage as an IfExpr condition, where
//     `transform_if_expr` handles it (statement form via
//     `try_expand_if_guard_stmt`, expression form via the expr.v ~line 1371
//     site). The arm in `transform_expr_to_flat` is the degenerate standalone-
//     reach case: legacy `transform_expr` returns
//     `t.transform_expr(expr.stmt.rhs[0])` when `rhs` is non-empty, otherwise
//     returns `expr` unchanged. Direct-emit mirrors that — recursive
//     `transform_expr_to_flat` for the rhs[0] case, leaf `out.emit_expr(...)`
//     for the fallback. No new fixture: IfGuardExpr is unreachable in
//     standalone position in practice, so no diff-harness fixture exercises
//     it; the port is a completeness sweep that keeps the dispatch surface
//     coherent with the legacy `transform_expr` shape. All 106 existing
//     transformer-diff tests continue to pass.
//
//   Session 18 (2026-05-26): SelectorExpr deep helper port + fixture.
//     First "deep helper rewrite" port (per the planning conversation that
//     followed session 17). The full body of `transform_selector_expr`
//     (~130 lines in expr.v) is mirrored as `transform_selector_expr_to_flat`
//     in this file. Special-case branches that rewrite the selector into a
//     non-selector shape — typeof.name/idx (3 sub-branches: KeywordOperator,
//     CallExpr, CallOrCastExpr lhs), sumtype representation fields
//     (`_tag` / `_data` / `_*` chained under `_data`), smartcast match
//     (direct + field-access), `os.args` → `arguments()`, module-qualified
//     enum (`mod.Type.value` → `mod__Type__value` Ident or nested module
//     refs), and same-module enum (`Op.value` → `mod__Op__value` Ident) —
//     all build the legacy `ast.Expr` and emit via `out.emit_expr(...)`
//     (their results are Idents, BasicLiterals, or CallExprs that route
//     through `add_expr`'s normal encoding). Only the default `x.field`
//     path is direct-emit: recurse on lhs via `transform_expr_to_flat`,
//     emit rhs Ident via the leaf `out.emit_expr(...)`, and assemble via
//     the new `emit_selector_expr_by_ids` builder helper, skipping the
//     `ast.SelectorExpr` struct allocation per default-path occurrence.
//
//     Adds `import v2.types` to this file (needed for the `typ is types.Enum`
//     checks in the module-qualified / same-module enum branches). New
//     `fixture_selector_expr` exercises the default path with chained
//     selectors (`b.p.x`, `b.p.y`), non-Ident lhs (`arr[0].y` — IndexExpr),
//     and a selector-LHS assignment (`b.p.y = 42`) — 106 → 111 transformer-
//     diff tests.
//
//     Pattern note: this is the first port where the dispatch arm delegates
//     to a dedicated `transform_*_to_flat` helper instead of inlining the
//     logic in the match arm itself. Established because the legacy helper
//     has too many branches to inline (~130 lines of special-case
//     dispatch + lookup). Subsequent deep-helper ports (CallExpr, IfExpr,
//     InfixExpr, ...) will follow the same shape: one helper per legacy
//     `transform_*` with the same structure, special-case branches routed
//     through `emit_expr` for their non-default Expr results, default-path
//     direct-emit via new `emit_*_by_ids` builder helpers.
//
//   Session 19 (2026-05-26): IndexExpr deep helper port + fixture.
//     Second deep-helper rewrite, same shape as session 18. The body of
//     `transform_index_expr` (~140 lines in expr.v) is mirrored as
//     `transform_index_expr_to_flat`. Two branches produce non-IndexExpr
//     shapes and route through the legacy round-trip:
//     (a) Slice lowering (`expr.expr is ast.RangeExpr`) →
//         `transform_slice_index_expr` produces a CallExpr to
//         `array_slice` / `string_substr`.
//     (b) Map index on a non-eval backend → `map__get` lowering produces
//         an `ast.UnsafeExpr` wrapping prefix temps + cast + deref.
//     Map detection mirrors the legacy sequence exactly: `map_index_lhs_type`
//     → fallback to `lookup_var_type` for Ident lhs → `unwrap_map_type`.
//     All three lookups are immutable `&Transformer` calls so running them
//     up-front to decide dispatch is side-effect-free. Eval-backend map
//     case rebuilds an IndexExpr verbatim — falls through to direct-emit.
//
//     The gated path (`arr#[i]`), eval-backend map path, and default
//     `arr[i]` path all rebuild an IndexExpr with `lhs` and `expr`
//     recursively transformed and `is_gated` copied verbatim — direct-emit
//     via the new `emit_index_expr_by_ids` builder helper (`is_gated` flag
//     packed into the flags byte to match `add_expr(IndexExpr)` encoding
//     exactly). Skips the `ast.IndexExpr` struct allocation on the common
//     path.
//
//     New `fixture_index_expr` exercises all four paths in one fn: default
//     `arr[0] + arr[1]`, gated `arr#[0]`, map index `m['key']`, and slice
//     `arr[1..3].len` — 111 → 116 transformer-diff tests.
//
//   Session 20 (2026-05-26): ComptimeExpr deep helper port + fixture.
//     Third deep-helper rewrite, same shape as sessions 18/19. The body of
//     `transform_comptime_expr` (~50 lines in expr.v) is mirrored as
//     `transform_comptime_expr_to_flat`. Most branches lower the comptime
//     form into a non-ComptimeExpr shape — eval_comptime_if produces an
//     IfExpr result; `$res(...)` (both CallExpr and CallOrCastExpr lhs)
//     becomes a BasicLiteral `false`; `$embed_file(...)` (both forms)
//     becomes an InitExpr or chained CallExpr via
//     `transform_embed_file_comptime_expr`; `@VMODROOT`/`VMODROOT` Ident
//     becomes a StringLiteral via `vmodroot_string_literal`; the
//     `transform_embed_file_comptime_chain` rescue produces a non-comptime
//     shape — all six branches route through `out.emit_expr(...)`. Only
//     the default `ast.ComptimeExpr{expr: transform(inner), pos}` path is
//     direct-emit via the new `emit_comptime_expr_by_id` builder helper.
//     Skips the `ast.ComptimeExpr` struct allocation on the rare
//     default-path occurrence.
//
//     New `fixture_comptime_expr` exercises a `$if linux { return 1 }`
//     stmt body (the IfExpr branch — legacy fallback) — 116 → 121
//     transformer-diff tests.
//
//   Session 21 (2026-05-26): InitExpr deep helper port + fixture.
//     Fourth deep-helper rewrite, same shape as sessions 18-20. The body of
//     `transform_init_expr` (~220 lines in struct.v) is large and complex —
//     the per-field transformation involves sumtype wrapping,
//     ArrayInitExpr special-casing, expected-type resolution, and
//     missing-default fill-in via `add_missing_struct_field_defaults`. Only
//     one branch produces a non-InitExpr shape: the typed empty map
//     `map[K]V{}` lowers to a CallExpr to `new_map` (or `MapInitExpr` on the
//     eval backend) — that branch routes through `out.emit_expr(...)`. The
//     default path delegates the field work to legacy
//     `transform_init_expr` (its result is an `ast.InitExpr` with the same
//     `typ` and a fully-transformed fields list) and then direct-emits via
//     the new `emit_init_expr_by_ids` builder helper together with
//     `emit_field_init_by_id` per field. This skips the outer
//     `ast.InitExpr` wrapper allocation per occurrence; the per-field
//     `ast.FieldInit` wrappers still materialise on the default path (the
//     legacy function builds them). A later session can inline the
//     per-field loop directly to also skip those — that's a much bigger
//     port and not required to close this dispatch arm.
//
//     New `fixture_init_expr` exercises two struct types and a nested
//     struct literal with both fully-specified fields and a field with a
//     declared default (`tag string = "default"`) — the missing-default
//     fill-in path. 121 → 126 transformer-diff tests.
//
//   Session 22 (2026-05-26): ReturnStmt port + fixture.
//     First stmt-level port since session 3 (FnDecl). `transform_return_stmt`
//     (~110 lines in transformer.v) always returns an `ast.ReturnStmt`
//     (never lowers to a different shape). Its body does per-expression
//     sumtype wrapping (`should_wrap_return_sumtype`), enum-shorthand
//     resolution (`resolve_enum_shorthand`), smartcast handling (Ident
//     re-wrap, var-type sumtype shortcut), and native-backend pre-wrap
//     (`wrap_sumtype_value` for sumtype-returning fns). Too much logic to
//     inline for a single session. Wrap-only port: call legacy
//     `transform_return_stmt`, decompose the resulting ReturnStmt, emit
//     each transformed expr via `out.emit_expr(...)` (already transformed)
//     and assemble via the new `emit_return_stmt_by_ids` helper. Skips the
//     outer `ast.ReturnStmt` wrapper allocation per occurrence; the
//     `[]ast.Expr` list still materialises (legacy builds it).
//
//     New `fixture_return_stmt` covers three return shapes: multi-value
//     return (`return 1, 2`), sumtype-wrapping return
//     (`return Circle2{r: r}` in a `Shape2`-returning fn), and smartcast
//     return (`if s is Circle2 { return s }` — Ident re-wrap path).
//     126 → 131 transformer-diff tests.
//
//   Session 23 (2026-05-26): Ident expr direct-emit + fixture.
//     The Ident arm in `transform_expr_to_flat` direct-emits via the leaf
//     `out.emit_expr(ast.Expr(expr))` for the default path (Ident is
//     identity in `transform_expr`'s arm — same shape returned). Two
//     state-dependent rewrite branches fall back to legacy:
//       1. `@VMODROOT` → `vmodroot_string_literal(expr.pos)` produces a
//          StringLiteral (different shape).
//       2. `find_smartcast_for_expr(expr.name)` hit → smartcast lowering
//          via `apply_smartcast_direct_ctx` produces a direct sum payload
//          access (different shape).
//     Both gate checks use the immutable `&Transformer` receiver, safe to
//     probe upfront. Skips the `transform_expr` dispatch call per Ident
//     occurrence reached through a ported ancestor (ParenExpr, PrefixExpr,
//     ModifierExpr, CastExpr, AsCastExpr, FieldInit, LambdaExpr, IndexExpr,
//     SelectorExpr, ComptimeExpr, InitExpr default fields, etc.). No
//     struct allocation savings since the legacy default-path arm also
//     returns the input Ident verbatim — the win is dispatch-skip on the
//     hottest expression variant.
//
//     New `fixture_ident` covers file-scope Ident references nested inside
//     already-ported expr arms: `const neg_base = -base` (Ident inside
//     PrefixExpr arm), `const wrapped_base = (base)` (Ident inside
//     ParenExpr arm), `const inv_base = ~base` (Ident inside PrefixExpr
//     `~` arm), plus mut-param + multiple Ident references in a fn body
//     (Idents in InfixExpr operands — covered by the legacy InfixExpr
//     fallback path). 131 → 136 transformer-diff tests.
//
//     Pattern note: first port where the saving is dispatch-skip rather
//     than struct-allocation skip (since Ident's default `transform_expr`
//     arm is pure identity, no allocation). Future leaf-identity arms
//     (KeywordOperator default, etc.) follow the same pattern: gate
//     state-dependent special branches via immutable lookups, direct-emit
//     the identity path.
//
//   Session 24 (2026-05-26): KeywordOperator expr direct-emit + fixture.
//     KeywordOperator covers the comptime/macro-style call shapes
//     (`typeof(x)`, `sizeof(T)`, `isreftype(T)`, `dump(x)`, `likely(x)` /
//     `unlikely(x)`, `offsetof(T, f)`, `go ...` / `spawn ...`). The arm
//     in `transform_expr_to_flat` follows the session 23 (Ident) pattern:
//     a leaf-identity default path direct-emits via
//     `out.emit_expr(ast.Expr(expr))` and two gated state-dependent
//     special branches fall back to the legacy round-trip:
//       1. `key_typeof` with `exprs.len > 0` → `resolve_typeof_expr`
//          produces a StringLiteral when the name is non-empty.
//       2. `key_go` with `exprs.len > 0` → `lower_go_call(expr)` lowers
//          the spawn into a CallExpr to the generated goroutine wrapper.
//     All other ops (`sizeof`, `isreftype`, `dump`, `likely`, `unlikely`,
//     `offsetof`, `spawn`, also `typeof` when `resolve_typeof_expr`
//     returns empty) fall through to identity. Gate is the cheap
//     immutable `expr.op == ...` check — no side effects, no Transformer
//     state access. Same dispatch-skip win as session 23: skip the
//     `transform_expr` function call + match dispatch per occurrence
//     reached through a ported ancestor or a file-scope const value
//     (e.g. `const sz = sizeof(int)`).
//
//     New `fixture_keyword_operator` covers file-scope KeywordOperators
//     in ConstDecl values: `const sz_int = sizeof(int)`,
//     `const sz_bool = sizeof(bool)`, `const ref_int = isreftype(int)`
//     — all hit the default identity branch since the legacy arm only
//     specially handles `key_typeof` and `key_go`. 136 → 141
//     transformer-diff tests.
//
//   Session 25 (2026-05-26): GenericArgs expr direct-emit (completeness sweep).
//     GenericArgs covers the type-parameter shape `lhs[T]` (`typeof[int]`,
//     `MyGenericFn[string]`, ...). The `transform_expr` arm has three
//     branches: (1) `is_typeof_generic_args(expr)` → identity; (2)
//     `args.len == 1` + non-callable lhs type → `transform_index_expr`
//     lowers into an IndexExpr (different shape); (3) default →
//     `specialize_generic_callable_expr` lowers into an Ident or CallExpr
//     (different shape). The arm in `transform_expr_to_flat` follows the
//     session 23/24 pattern — gate the identity branch via the immutable
//     `is_typeof_generic_args` lookup (a `&Transformer` name check, no
//     state mutation) and direct-emit via leaf
//     `out.emit_expr(ast.Expr(expr))`; all other paths fall back to the
//     legacy round-trip.
//
//     Reachability is limited in practice: GenericArgs typically appears
//     as a CallExpr lhs (`typeof[int]()`, `myfn[T](x)`), and CallExpr is
//     unported — so the arm fires only through ported-ancestor recursion
//     (ParenExpr / SelectorExpr deep helper, etc.). Same shape as the
//     session 17 IfGuardExpr completeness sweep: no new fixture, keeps the
//     dispatch surface coherent with the legacy `transform_expr` shape.
//     All 141 existing transformer-diff tests continue to pass.
//
//   Session 26 (2026-05-26): StringInterLiteral deep helper port + fixture.
//     StringInterLiteral covers V's `'... ${x:d} ...'` interpolation syntax.
//     The legacy `transform_string_inter_literal` always returns a
//     StringInterLiteral (no branch produces a different shape) — it walks
//     each `ast.StringInter`, (a) optionally hoists a smartcasted payload via
//     `transform_expr` + `hoist_expr_to_temp` (mutates `t.pending_stmts`),
//     (b) wraps the inter expression via `transform_sprintf_arg` (type-aware
//     pointer-with-str / string-keeping logic), then (c) fills in the
//     resolved sprintf format via `resolve_sprintf_format`. The legacy body
//     allocates: one `ast.StringInterLiteral` wrapper, one `[]ast.StringInter`
//     list, and one `ast.StringInter` struct per inter.
//
//     `transform_string_inter_literal_to_flat` mirrors the per-inter loop
//     and emits each StringInter via `emit_string_inter_by_ids(format, width,
//     precision, expr_id, format_expr_id, resolved_fmt)` and the outer
//     literal via `emit_string_inter_literal_by_ids(kind, values, inter_ids,
//     token.Pos{})`. The smartcast/hoist mutations still happen identically
//     (we call the same legacy helpers); only the post-build allocations are
//     skipped. Note: legacy omits `pos` on the rebuilt literal (defaults to
//     `token.Pos{}`) — the port passes `token.Pos{}` explicitly for parity.
//     Same deep-port pattern as sessions 18 (SelectorExpr), 19 (IndexExpr),
//     20 (ComptimeExpr), 21 (InitExpr).
//
//     No new fixture in this session — existing fixtures already cover
//     StringInterLiteral nodes reached via ported ancestors (e.g.,
//     `'hi ${name}'` inside a ParenExpr / SelectorExpr / ConstDecl value).
//     All 141 transformer-diff tests continue to pass.
//
//   Session 27 (2026-05-26): ExprStmt port (stmt-level dispatch-skip).
//     First stmt-level identity-shape port. `transform_stmt`'s `ast.ExprStmt`
//     arm is identity-shape: it wraps the transformed inner expression in a
//     new `ast.ExprStmt`, with one state mutation — when `stmt.expr is
//     ast.IfExpr`, the arm flips `t.skip_if_value_lowering = true` around the
//     recursive `transform_expr` call so `transform_if_expr` knows the IfExpr
//     is in statement position (not value position) and skips temp-variable
//     lowering, then restores the prior flag.
//
//     The flat-write port mirrors the flag flip around a recursive
//     `transform_expr_to_flat` call (so any deep-helper-ported expr arm
//     reached inside also benefits from the flat path — Ident, SelectorExpr,
//     IndexExpr, InitExpr, ComptimeExpr, StringInterLiteral, ...), then
//     assembles the stmt_expr node via the new `emit_expr_stmt_by_id` helper.
//     Skips the `ast.ExprStmt` wrapper struct allocation and the legacy
//     `out.emit_stmt(t.transform_stmt(stmt))` round-trip's match dispatch
//     (both on `transform_stmt` and on `add_stmt`) on every expression
//     statement.
//
//     Reachability is excellent: ExprStmt is the most common statement form
//     after AssignStmt — every function body has them, every block has them,
//     every call as a top-level statement is an ExprStmt. No new fixture
//     this session — every existing fixture exercises ExprStmt via fn bodies
//     and asserts. All 141 transformer-diff tests continue to pass.
//
//     Pattern note: first port that recursively enters `transform_expr_to_flat`
//     from the stmt-level dispatch, propagating the flat-port savings into
//     all statement-position expressions (not just nested expressions inside
//     already-ported ConstDecl/GlobalDecl/ReturnStmt values).
//
//   Session 28 (2026-05-26): LabelStmt port (stmt-level recursive descent).
//     Second stmt-level identity-shape port after session 27 (ExprStmt).
//     `transform_stmt`'s LabelStmt arm always returns `LabelStmt{name:
//     stmt.name, stmt: transform_stmt(stmt.stmt)}` — `name` is verbatim,
//     single inner stmt recursively transformed. Direct-emit recurses via
//     `transform_stmt_to_flat` for the inner stmt (so any ported stmt arm
//     reached inside the label — ExprStmt, ReturnStmt, ForStmt, etc. — also
//     stays on the flat path) and assembles via the new
//     `emit_label_stmt_by_id(name, stmt_id)` helper. Mirrors
//     `add_stmt(LabelStmt)` encoding exactly (pos = `token.Pos{}`, interned
//     name, single edge to inner stmt). Skips the `ast.LabelStmt` wrapper
//     struct allocation and the `transform_stmt`/`add_stmt` match-dispatch
//     round-trip per occurrence.
//
//     Reachability is limited to source-level `label: stmt` syntax
//     (typically `outer:` before loops for labelled break/continue), but the
//     port is clean — single child stmt and a verbatim name. No new fixture
//     this session — existing fixtures don't exercise LabelStmt directly,
//     but the port keeps the stmt-dispatch surface coherent. All 141
//     transformer-diff tests continue to pass.
//
//     Pattern note: first stmt port that recursively enters
//     `transform_stmt_to_flat` itself (sibling of session 27's recursion into
//     `transform_expr_to_flat`), establishing the stmt-level recursive descent
//     pattern. Future stmt-level identity-shape ports (BlockStmt, DeferStmt,
//     ComptimeStmt $for branch) follow the same template — recurse via
//     `transform_stmt_to_flat` for child stmts, then emit via a new
//     `emit_*_by_ids` helper mirroring the matching `add_stmt` arm encoding.
//
//   Session 29 (2026-05-26): BlockStmt port (wrap-only stmt port).
//     Third stmt-level identity-shape port after sessions 27 (ExprStmt) and
//     28 (LabelStmt). `transform_stmt`'s BlockStmt arm always returns
//     `BlockStmt{stmts: transform_stmts(stmt.stmts)}` — identity-shape with
//     the body driver `transform_stmts` handling sibling-stmt smartcast
//     state snapshot/restore, pending-stmt hoisting, and one-to-many
//     expansion. Because the driver does extra inter-stmt work (and may
//     expand one input stmt into several outputs), we can't recurse via
//     `transform_stmt_to_flat` per-element without porting the driver
//     itself — that's a separate (bigger) refactor.
//
//     Wrap-only port: call legacy `transform_stmts(stmt.stmts)` to
//     materialise the transformed `[]ast.Stmt`, emit each via the legacy
//     `out.emit_stmt(...)` (the stmts are already transformed), and
//     assemble via the new `emit_block_stmt_by_ids` helper. Mirrors
//     `add_stmt(BlockStmt)` encoding exactly (pos = `token.Pos{}`, edges =
//     inner stmts in order). Skips the outer `ast.BlockStmt` wrapper struct
//     allocation per occurrence; the inner `[]ast.Stmt` from
//     `transform_stmts` still materialises (until the body driver is
//     ported).
//
//     Reachability is good — BlockStmt appears as a free-standing `{ ... }`
//     block in source. Most fn bodies don't directly use them (the FnDecl
//     arm calls `transform_stmts` itself), but `unsafe { ... }` and lambda
//     bodies in some shapes wrap their contents in BlockStmt. No new
//     fixture this session. All 141 transformer-diff tests continue to
//     pass.
//
//     Pattern note: first stmt port that takes the "wrap-only" shape
//     (sessions 9 FnLiteral, 14 UnsafeExpr, 22 ReturnStmt established this
//     pattern at the expr level). The body driver port (which would let
//     BlockStmt / DeferStmt / FnLiteral.stmts / UnsafeExpr.stmts all stream
//     directly into the builder instead of materialising `[]ast.Stmt`) is
//     the next significant target.
//
//   Session 55 (2026-05-27): InitExpr typed-empty-map direct-emit
//     refactor (seventh rewrite-site port — extends the post-routing-
//     pass mini-sweep to the InitExpr arm). `transform_init_expr_to_flat`
//     previously leaf-encoded the typed empty map branch
//     (`map[K]V{}`, `expr.fields.len == 0` + `MapType` typ) via
//     `out.emit_expr(t.transform_expr(ast.Expr(expr)))`. Session 55
//     direct-calls `transform_init_expr` (skipping the outer
//     `transform_expr` dispatch) and dispatches on result shape —
//     `ast.MapInitExpr` (eval backend) direct-emits via session 40's
//     `emit_map_init_expr_by_ids`; `ast.CallExpr` (non-eval, returns
//     a `new_map(sizeof(K), sizeof(V), &hash, &eq, &clone, &free)`
//     call) direct-emits via session 44's `emit_call_expr_by_ids`;
//     anything else falls through to leaf-emit.
//
//     Safety: `transform_init_expr` is the helper `transform_expr`'s
//     InitExpr arm calls. The MapInitExpr branch's children
//     (`typ`/`keys`/`vals`) are leaves built locally by the helper
//     (`MapInitExpr{typ: ast.Expr(ast.Type(expr.typ)), keys: [], vals:
//     [], pos}`); the CallExpr branch's args are also leaves built
//     locally (`KeywordOperator{op: .key_sizeof}` + Ident-wrapped
//     `&hash`/`&eq`/... fns) — no re-transform needed, leaf
//     `out.emit_expr` is correct.
//
//     Reachability: `map[K]V{}` is the canonical empty-map literal
//     and appears in every map-using V module. The dispatch-skip
//     win applies on every typed empty map occurrence; the
//     wrapper-allocation skip applies on every direct-emit path
//     (no longer build an `ast.InitExpr` wrapper before dispatch).
//     No new helper, no new fixture. All 168 transformer-diff tests
//     continue to pass.
//
//   Session 54 (2026-05-27): CastExpr sum-type lowering direct-emit
//     refactor (sixth rewrite-site port — extends the post-routing-
//     pass mini-sweep to the CastExpr arm). `transform_expr_to_flat`'s
//     CastExpr arm previously gated the sum-type lowering branch and
//     leaf-encoded via `out.emit_expr(t.transform_expr(expr))`,
//     re-entering the legacy dispatch only to reach
//     `wrap_sumtype_value`. Session 54 direct-calls
//     `wrap_sumtype_value(expr.expr, sumtype_name)` and leaf-emits
//     the wrapped result when it returns Some; when it returns none
//     (value already is the sum type, declared receiver matches,
//     generic-param guard, etc.), falls through to the existing
//     default direct-emit path (CastExpr rebuild via
//     `emit_cast_expr_by_ids`).
//
//     Safety: `wrap_sumtype_value` is the same helper
//     `transform_expr`'s CastExpr arm calls; calling it directly
//     skips one dispatch hop. The wrapped result (CastExpr/InitExpr/
//     CallExpr depending on variant shape — see `build_sumtype_init`)
//     is leaf-encoded via `out.emit_expr`: the result is already
//     fully-formed by the helper (it transforms the inner value
//     before wrapping at types.v:2145), so re-entering
//     `transform_expr_to_flat` would re-apply transforms — same
//     "do not route through dispatch" hazard as sessions 47-51.
//
//     Reachability: sum-type casts (`MySum(variant_value)`) appear
//     in V code wherever a sum-type variant is materialised from an
//     untyped expression — common in pattern-match arms, ast
//     construction, etc. The dispatch-skip win applies on every
//     sum-type cast occurrence. No new helper, no new fixture. All
//     168 transformer-diff tests continue to pass.
//
//   Session 53 (2026-05-27): GenericArgs branch-2/3 direct-emit
//     refactor (fifth rewrite-site port — extends the
//     post-routing-pass mini-sweep to the GenericArgs arm).
//     `transform_expr_to_flat`'s GenericArgs arm previously gated
//     the identity branch (typeof[T]) and leaf-encoded the other two
//     branches via `out.emit_expr(t.transform_expr(expr))`. Session
//     53 direct-handles each branch: branch 2 (single-arg,
//     non-callable lhs) cross-arm routes to
//     `transform_index_expr_to_flat` (same template as
//     GenericArgOrIndexExpr in session 33), branch 3 direct-calls
//     `specialize_generic_callable_expr` (returns Ident or
//     SelectorExpr — both identity in `transform_expr`) and
//     leaf-emits the result.
//
//     Safety: `is_typeof_generic_args` / `get_expr_type` /
//     `is_callable_type` are immutable lookups. The IndexExpr
//     re-entry mirrors the legacy disambiguation exactly. The
//     branch-3 result (Ident/SelectorExpr) is leaf-encoded — both
//     shapes are identity in `transform_expr`, so re-entering
//     `transform_expr_to_flat` would only add a dispatch hop with
//     no rewrite.
//
//     Reachability is limited (GenericArgs typically appears as a
//     CallExpr lhs which is unported — the arm fires through
//     ported-ancestor recursion: ParenExpr, SelectorExpr deep
//     helper, etc.), but the port removes the last legacy round-trip
//     from the arm. No new helper, no new fixture. All 168
//     transformer-diff tests continue to pass.
//
//   Session 52 (2026-05-27): Ident @VMODROOT / smartcast branch
//     direct-emit refactor (fourth rewrite-site port — extends the
//     post-routing-pass mini-sweep to the Ident arm).
//     `transform_expr_to_flat`'s Ident arm previously leaf-encoded
//     both rewrite branches via `out.emit_expr(t.transform_expr(expr))`,
//     re-entering the legacy dispatch only to reach the two distinct
//     rewrites. Session 52 direct-calls each helper instead:
//     `@VMODROOT` → `vmodroot_string_literal(expr.pos)` (immutable,
//     returns StringLiteral) → leaf-emit; smartcast hit →
//     `apply_smartcast_direct_ctx(expr, ctx)` (the same helper
//     `transform_expr` would call) → leaf-emit.
//
//     Safety: `vmodroot_string_literal` is a pure value-resolution
//     lookup (no expression rewriting). `apply_smartcast_direct_ctx`
//     returns an already-fully-formed expression (selector chain into
//     sum payload, or smartcast-rewritten expr) — leaf-encoding via
//     `out.emit_expr` is the safe escape hatch (routing through
//     `transform_expr_to_flat` would re-enter the helper-result's arm
//     and could re-apply smartcast rewrites).
//
//     Reachability: Ident is by far the most-frequent expr variant
//     in V code (every variable reference is an Ident). The default
//     identity path was already direct-emit since session 23; this
//     session removes the last legacy-round-trip path from the arm.
//     `@VMODROOT` is rare (build-config code only); smartcast hits
//     are common inside `if x is Type { ... }` / `match`-narrowed
//     blocks. No new helper, no new fixture. All 168 transformer-diff
//     tests continue to pass.
//
//   Session 51 (2026-05-27): KeywordOperator typeof/go branch split
//     direct-emit refactor (third rewrite-site port — extends the
//     post-routing-pass mini-sweep to the KeywordOperator arm).
//     `transform_expr_to_flat`'s KeywordOperator arm previously gated
//     both rewrite branches behind a single `(typeof || go) && len > 0`
//     check and leaf-encoded via `out.emit_expr(t.transform_expr(expr))`,
//     re-entering the legacy dispatch only to route to the two distinct
//     rewrites. Session 51 splits the branches: the typeof branch
//     direct-calls `resolve_typeof_expr` (an immutable `&Transformer`
//     lookup) and direct-emits the resulting StringLiteral via leaf
//     `out.emit_expr` (StringLiteral is identity in `transform_expr`);
//     the go branch direct-calls `lower_go_call(expr)` and dispatches
//     on the result shape — `ast.CallExpr` direct-emits via session
//     44's `emit_call_expr_by_ids`, the `KeywordOperator` identity
//     fallback (unresolved fn_name / non-Call inner) falls through to
//     leaf `out.emit_expr(result)`.
//
//     Safety: `resolve_typeof_expr` is a pure name-resolution lookup
//     (no mutation, no expression rewriting). `lower_go_call`
//     internally calls `t.transform_expr` on each arg and on the
//     selector lhs (for method calls) — args/receiver in the returned
//     CallExpr are fully-transformed, so direct-emit encodes them via
//     leaf `out.emit_expr` per arg. Routing through
//     `transform_expr_to_flat`'s CallExpr arm would re-run
//     `transform_call_expr` (fn.v:112 "Do NOT route" hazard) —
//     skipped, same as sessions 47/48/49/50.
//
//     Reachability: `typeof(expr).name` patterns are common in V
//     comptime code (introspection, error messages, generic dispatch
//     hints). `go ...` / `spawn ...` are less common but appear in
//     concurrent code. The dispatch-skip win applies on every
//     KeywordOperator occurrence with these ops; the StringLiteral
//     direct-emit (no wrapper allocation) is a small but pure win on
//     typeof hits. No new helper, no new fixture. All 168
//     transformer-diff tests continue to pass.
//
//   Session 50 (2026-05-27): IndexExpr map-index-lowering direct-emit
//     refactor (second rewrite-site port — completes the IndexExpr
//     non-identity-shape routing sweep started in session 49).
//     `transform_index_expr_to_flat`'s map-index branch (non-eval
//     backend, lhs unwraps to a Map type) was a leaf-encode shape:
//     `out.emit_expr(t.transform_expr(ast.Expr(expr)))` re-entered
//     `transform_expr`'s IndexExpr arm → `transform_index_expr` →
//     produced an UnsafeExpr containing temp-assign stmts plus a
//     deref-of-cast-of-`map__get` call. Session 50 refactor:
//     direct-call `transform_index_expr` (skipping the outer
//     `transform_expr` match dispatch) and direct-emit the resulting
//     UnsafeExpr via `emit_unsafe_expr_by_ids` (already present in
//     `vlib/v2/ast/flat.v`; no new helper needed).
//
//     Safety: `transform_index_expr` returns an `ast.UnsafeExpr` with
//     fully-transformed stmts (the helper calls `t.transform_expr` on
//     every child expr internally — lhs, key, default-zero, etc.).
//     Direct-emit encodes each stmt via leaf `out.emit_stmt` — no
//     re-transform. Routing through `transform_stmt_to_flat` per stmt
//     would re-run the legacy dispatch (some arms call into helpers
//     that re-transform); leaf-encoding is the safe escape hatch.
//     The detection chain (`map_index_lhs_type` / `unwrap_map_type`
//     / `is_eval_backend`) stays upfront so we only direct-call
//     `transform_index_expr` when we already know it will produce the
//     UnsafeExpr lowering — `if result is ast.UnsafeExpr` is a
//     defensive type-narrow that catches an upstream shape drift.
//
//     Reachability: map index reads `m[key]` on non-eval backends are
//     common in compiler-internal code (registries, lookup tables,
//     name resolution). The dispatch-skip win applies on every
//     map-index occurrence on the non-eval path. Same routing
//     template as sessions 47/48/49. No new helper, no new fixture.
//     All 168 transformer-diff tests continue to pass.
//
//   Session 49 (2026-05-27): IndexExpr slice-lowering direct-emit
//     refactor (first rewrite-site port — sibling of routing-pass
//     sessions 46-48). `transform_index_expr_to_flat`'s slice branch
//     (`expr.expr is ast.RangeExpr`) was a leaf-encode shape: it called
//     `out.emit_expr(t.transform_expr(ast.Expr(expr)))` which re-entered
//     `transform_expr`'s IndexExpr arm → `transform_index_expr` →
//     `transform_slice_index_expr`, materialising a CallExpr to
//     `string__substr` / `array__slice` / `array__slice_ni`. With session
//     44's `emit_call_expr_by_ids` in place, the slice branch can
//     direct-call `transform_slice_index_expr` (skipping the dispatch +
//     re-entry round-trip) and direct-emit the resulting CallExpr.
//
//     Safety: `transform_slice_index_expr` returns an `ast.CallExpr`
//     with fully-transformed lhs/args (it calls `t.transform_expr` on
//     range start/end internally and on the lhs upstream of the
//     direct-call). Direct-emit encodes via leaf `out.emit_expr` per
//     arg — no re-transform. Routing through `transform_expr_to_flat`'s
//     CallExpr arm would re-run `transform_call_expr` (the fn.v:112
//     "Do NOT route" hazard) — skipped. Same template as sessions 47
//     (CallOrCastExpr) and 48 (SqlExpr lowered).
//
//     Reachability is good — slice syntax `a[i..j]`, `s[..n]`, `arr[k..]`
//     is common in V code. The map-index lowering branch (lower in the
//     same function, returns UnsafeExpr) stays leaf-encoded for now;
//     porting it would need a new `emit_unsafe_expr_by_id` helper. This
//     is the first rewrite-site port after the routing-pass mini-sweep
//     finished — every dispatch arm in `transform_expr_to_flat` either
//     direct-emits or routes through a helper that does. No new helper,
//     no new fixture. All 168 transformer-diff tests continue to pass.
//
//   Session 48 (2026-05-27): SqlExpr lowered-CallExpr direct-emit
//     refactor (routing pass — completes the routing-pass mini-sweep).
//     Session 36 ported SqlExpr with two outcome shapes:
//     identity-SqlExpr direct-emits via `emit_sql_expr_by_id` (added in
//     same session); lowered branch (is_create + table lookup hit) goes
//     through `lower_sql_create_expr` → `t.transform_expr(call)` on a
//     synthesized CallExpr, leaf-encoded via `out.emit_expr(result)`.
//     With session 44's `emit_call_expr_by_ids` in place, the lowered
//     branch's CallExpr result can be direct-emitted via the helper —
//     same template as session 47.
//
//     Safety: `lower_sql_create_expr`'s `t.transform_expr(call)` enters
//     the CallExpr arm of `transform_expr` which calls
//     `transform_call_expr` once; its result has fully-transformed args.
//     Direct-emit here encodes lhs/args via leaf `out.emit_expr` — no
//     re-transform. Routing through `transform_expr_to_flat` would
//     re-run `transform_call_expr` (the fn.v:112 "Do NOT route" hazard
//     again) — skipped.
//
//     Reachability is limited (sql ... { create ... } blocks with a
//     looked-up table struct), but the routing keeps the SqlExpr arm
//     coherent with the CallOrCastExpr arm (session 47) and finishes
//     the routing-pass mini-sweep that sessions 45-48 ran: every
//     "always-lowers via helper" arm whose downstream shape has a
//     direct-emit helper now uses it (MatchExpr → IfExpr in session 46,
//     CallOrCastExpr → CallExpr in session 47, SqlExpr lowered →
//     CallExpr in session 48). No new helper, no new fixture. All 168
//     transformer-diff tests continue to pass.
//
//   Session 47 (2026-05-27): CallOrCastExpr direct-emit refactor
//     (routing pass — sibling of session 46's MatchExpr refactor).
//     Session 42 ported CallOrCastExpr to call `transform_call_or_cast_expr`
//     once and leaf-encode the result via `out.emit_expr(...)`. Session
//     44 landed `emit_call_expr_by_ids`, unlocking the downstream win:
//     the most common shape `transform_call_or_cast_expr` lowers to is
//     `ast.CallExpr` (every method-resolved call, generic-math inline,
//     filter/map expansion, flag-enum method, smartcast method, all
//     module-prefixed calls, and the unresolved fallback). The CallExpr
//     branch can now be direct-emitted via the helper instead of
//     leaf-encoded.
//
//     Safety: every CallExpr return path in `transform_call_or_cast_expr`
//     populates `lhs` and `args` from already-transformed values
//     (`transform_call_arg_with_sumtype_check`, `transform_expr` calls
//     upstream of each return). Direct-emit encodes lhs/args via leaf
//     `out.emit_expr` — no re-transform. Going through
//     `transform_expr_to_flat`'s CallExpr arm (which would re-run
//     `transform_call_expr` on the already-lowered result) IS unsafe
//     and explicitly flagged by an "// Do NOT route through
//     transform_call_expr because it would..." comment at fn.v:112; the
//     routing skips the dispatch arm and direct-emits instead.
//
//     The non-CallExpr shapes (CastExpr, BasicLiteral '0', and rarer
//     `lower_call_or_cast_expr` results) still route through leaf
//     `out.emit_expr(result)`. Reachability of the direct-emit win is
//     high — CallExpr is the dominant downstream shape for the
//     parser's ambiguous `T(x)`/`f(x)` form. No new helper, no new
//     fixture. All 168 transformer-diff tests continue to pass.
//
//   Session 46 (2026-05-27): MatchExpr direct-emit refactor (routing
//     pass — sibling of session 45's IfExpr port).
//     Session 35 ported MatchExpr to call `transform_match_expr` once
//     and leaf-encode the result via `out.emit_expr(...)`. Session 45
//     landed `emit_if_expr_by_ids` for the IfExpr arm, unlocking
//     MatchExpr's downstream win: `transform_match_expr` always lowers
//     to an `ast.IfExpr` (via `lower_match_expr_to_if`), so the IfExpr
//     can be direct-emitted via the new helper instead of routed
//     through leaf `out.emit_expr` (which still allocates one extra
//     wrapper layer inside `add_expr(IfExpr)`'s `push_expr` /
//     `push_stmt` walk).
//
//     Safety: `transform_match_expr` runs `transform_match_branch_stmts`
//     per branch and `transform_expr(expr.expr)` upstream of
//     `build_match_branch_cond`, so the IfExpr's `cond` / `stmts` /
//     `else_expr` are already transformed. Direct-emit encodes them via
//     leaf `out.emit_expr` / `out.emit_stmt` (no re-transform). Going
//     through `transform_expr_to_flat`'s IfExpr arm would re-run
//     `transform_if_expr` on the already-transformed result — would
//     double-transform body stmts, which is why this routing skips the
//     IfExpr dispatch arm entirely.
//
//     Edge case: `lower_match_expr_to_if` returns `ast.empty_expr` when
//     `branches.len == 0` (no branches → no if). The `is ast.IfExpr`
//     guard keeps that path on the leaf `out.emit_expr(result)` exit.
//
//     Pattern note: routing-pass template — when an "always-lowers via
//     helper" arm produces a result shape that now has a direct-emit
//     helper, switch from leaf-encoding to direct-emit (without
//     re-routing through the dispatch arm, which would double-
//     transform). Same trick applies to CallOrCastExpr (session 42 →
//     `emit_call_expr_by_ids`) and SqlExpr's lowered-CallExpr branch
//     (session 36) — both follow-up sessions. No new helper, no new
//     fixture. All 168 transformer-diff tests continue to pass.
//
//   Session 45 (2026-05-27): IfExpr port (always-lowers via helper, two
//     outcome shapes) — completes phase 4 expression-arm port sweep.
//     `transform_expr`'s IfExpr arm is a thin wrapper around
//     `transform_if_expr` (expr.v:1268), the largest control-flow helper
//     in the transformer. Many outcome shapes:
//       - &&-chain normalisation with embedded `is` checks → recursive
//         `transform_if_expr(outer_if / inner_if)` rebuild (IfExpr).
//       - smartcast hoisting (`if x is T && rest { ... }`) → IfExpr
//         with smartcast applied to the nested then-branch.
//       - sumtype tag/_type_id rewrite for `is`/`!is` cond → IfExpr.
//       - matched smartcast / type-narrowed branches → IfExpr with
//         hoisted decl-assigns + transformed stmts/else.
//       - direct option/result unwrap (`if x is none`) → IfExpr or
//         UnsafeExpr lowering with deref + tag check.
//       - default → `IfExpr{cond: transform(cond), stmts:
//         transform_stmts(stmts), else_expr: transform(else), pos}`
//         identity-shape rebuild.
//       - value-position lowering: `if_expr_is_value(transformed_if)`
//         AND not skipped → `lower_if_expr_value(transformed_if)` hoists
//         a temp decl-assign per branch, returning an Ident (NOT an
//         IfExpr).
//
//     Direct-emit calls `transform_if_expr` once and dispatches on result
//     type. Identity branch → encode `cond` + `else_expr` via leaf
//     `out.emit_expr`, each `stmt` via leaf `out.emit_stmt` (helper
//     already transformed body via `transform_stmts` —
//     `transform_stmt_to_flat` would double-transform and break parity
//     for hoisted smartcast / pending_stmts ordering) + assemble via the
//     new `emit_if_expr_by_ids(cond_id, else_id, stmt_ids, pos)` helper.
//     Lowered branch → route through leaf `out.emit_expr(result)`.
//
//     Pattern note: extends "always-lowers via helper, two outcome
//     shapes" (sessions 36 SqlExpr, 40 MapInitExpr, 41 ArrayInitExpr,
//     43 InfixExpr, 44 CallExpr). Reachability is high — `if` is one of
//     the two main control-flow constructs in V; every `if cond { ... }
//     else { ... }` passes through this arm. The wrapper-allocation skip
//     applies on the identity branch (statement-position ifs and ifs
//     without value lowering — most of them in compiler-style code).
//
//     Completes phase 4's expression-arm port sweep — with this landing,
//     every non-leaf arm in `transform_expr_to_flat` has a dedicated
//     port. The `else { return out.emit_expr(t.transform_expr(expr)) }`
//     fallback at the bottom of the dispatch is dead code (next
//     follow-up: remove it + the `KeywordOperator` / `GenericArgs`
//     intra-arm fallbacks that still route through the legacy round-trip
//     for state-dependent rewrites). MatchExpr's arm (session 35) can
//     now chain through `transform_expr_to_flat` to pick up IfExpr's
//     direct-emit win on the lowered result, instead of routing through
//     leaf `out.emit_expr` — another follow-up routing pass.
//
//     New pub helper: `emit_if_expr_by_ids(cond_id, else_id, stmt_ids,
//     pos)` in `vlib/v2/ast/flat.v` mirrors `add_expr(IfExpr)` encoding
//     (`emit_simple(.expr_if, pos, [cond, else, stmts...])`). No new
//     fixture this session. All 168 transformer-diff tests continue to
//     pass.
//
//   Session 44 (2026-05-27): CallExpr port (always-lowers via helper,
//     two outcome shapes).
//     `transform_expr`'s CallExpr arm is a thin wrapper around
//     `transform_call_expr` (fn.v:1881) — by far the largest single
//     helper in the transformer. It lowers many call shapes into
//     non-CallExpr results: generic-math inline, embed_file lowering,
//     $d resolution, .filter()/.map()/.sort() expansion, flag-enum
//     methods, array contains/index/last_index, smartcast hoisting,
//     BasicLiteral '0' elision (for elided fns), CastExpr for bare-type
//     calls, IfExpr / InitExpr for some intrinsics, etc. The default
//     tail (fn.v:2603-2610) is identity-shape:
//       `CallExpr{lhs: transform(lhs),
//                 args: [transform_call_arg_with_sumtype_check(...)],
//                 pos}` —
//     wrapped by `lower_missing_call_args` (fills omitted optional
//     args) and `lower_variadic_args` (collapses trailing args into a
//     `[]T` ArrayInitExpr).
//
//     Direct-emit calls `transform_call_expr` once and dispatches on
//     result type. Identity branch → encode `lhs` + each `arg` via leaf
//     `out.emit_expr` (helper already transformed — re-routing through
//     `transform_expr_to_flat` would double-transform and break parity
//     for hoisted smartcast temps + variadic lowering) + assemble via
//     the new `emit_call_expr_by_ids(lhs_id, arg_ids, pos)` helper.
//     Lowered branch → route through leaf `out.emit_expr(result)` so
//     the many lowering branches stay single-sourced.
//
//     Pattern note: extends "always-lowers via helper, two outcome
//     shapes" (sessions 36 SqlExpr, 40 MapInitExpr, 41 ArrayInitExpr,
//     43 InfixExpr). CallExpr is the highest-traffic arm of any port in
//     phase 4 — every function call, method call, intrinsic, and
//     lowered-array/map operation passes through it. The dispatch-skip
//     win applies on every occurrence; the wrapper-allocation skip
//     applies on the default-path identity branch (the most common
//     case for ordinary user-code calls).
//
//     With CallExpr ported, the CallOrCastExpr arm (session 42) and
//     the cascade of lowering helpers throughout the transformer that
//     produce CallExpr results could be retargeted to chain through
//     `transform_expr_to_flat` instead of leaf `out.emit_expr` — but
//     that's a follow-up routing pass, not part of this session. No
//     new fixture this session. All 168 transformer-diff tests continue
//     to pass.
//
//     New pub helper: `emit_call_expr_by_ids(lhs_id, arg_ids, pos)` in
//     `vlib/v2/ast/flat.v` mirrors `add_expr(CallExpr)` encoding
//     (emit_simple `.expr_call` with edges = [lhs, args...]).
//
//   Session 43 (2026-05-27): InfixExpr port (always-lowers via helper,
//     two outcome shapes).
//     `transform_expr`'s InfixExpr arm is a thin wrapper around
//     `transform_infix_expr` (expr.v:2124), which has many outcome shapes
//     that collapse into "identity InfixExpr" vs "lowered different
//     shape":
//       - `&&`-chain smartcast: `join_and_terms(terms)` rebuilds an
//         InfixExpr chain — identity shape.
//       - sum-type tag comparison (`is`/`!is`/`==`/`!=` against a
//         variant) → `make_infix_expr_at(...)` over `_tag` / `_type_id`
//         — identity shape (InfixExpr).
//       - string literal concat folding → StringLiteral.
//       - string + string → CallExpr to `string__plus`.
//       - operator overload → CallExpr to the user-defined `<op>`
//         method.
//       - alias arithmetic, smartcast-of-lhs/rhs, fixed-array concat,
//         etc. → various CallExpr / IndexExpr / etc.
//       - default → `InfixExpr{op, lhs: transform(lhs), rhs:
//         transform(rhs), pos}` — identity shape.
//
//     Direct-emit calls `transform_infix_expr` once and dispatches on
//     result type. Identity branches → encode lhs/rhs via leaf
//     `out.emit_expr` (helper already transformed — `transform_expr_to_flat`
//     would double-transform) + assemble via the new
//     `emit_infix_expr_by_ids(op, lhs_id, rhs_id, pos)` helper. Lowered
//     branches → route through leaf `out.emit_expr(result)`.
//
//     Pattern note: extends "always-lowers via helper, two outcome
//     shapes" (sessions 36 SqlExpr, 40 MapInitExpr, 41 ArrayInitExpr).
//     The identity branch is reached by the vast majority of arithmetic
//     / boolean / comparison infix operations in V code — every `a + b`,
//     `i < n`, `flag && x`, `xs == ys`, ... — so the direct-emit win is
//     broad.
//
//     Reachability is extremely high — infix is the workhorse of every
//     function body. The dispatch-skip win applies on every occurrence;
//     the wrapper-allocation skip applies on every identity-branch
//     occurrence. No new fixture this session. All 168 transformer-diff
//     tests continue to pass.
//
//     New pub helper: `emit_infix_expr_by_ids(op, lhs_id, rhs_id, pos)`
//     in `vlib/v2/ast/flat.v` mirrors `add_expr(InfixExpr)` encoding
//     (aux1=-1, aux2=-1, meta=u16(op), edges = [lhs, rhs]).
//
//   Session 42 (2026-05-27): CallOrCastExpr port (always-lowers via helper,
//     never-identity).
//     `transform_expr`'s CallOrCastExpr arm is a thin wrapper around
//     `transform_call_or_cast_expr` (fn.v:3955), which virtually always
//     lowers a CallOrCastExpr (the parser's ambiguity node for `Type(arg)`
//     vs `func(arg)`) into a different shape: CallExpr (most paths —
//     generic-math inline, filter/map expansion, sort lambda, flag-enum
//     methods, array contains/index, smartcast methods, all module-
//     prefixed and method-resolved calls, the "unresolved fallback"
//     CallExpr at the very end), CastExpr (when lhs is a known type
//     expression), BasicLiteral '0' (elided fn or `enum.zero()` for flag
//     enums), or whatever shape `lower_call_or_cast_expr` /
//     `t.transform_expr(t.transform_call_or_cast_expr(...))` recursion
//     produces.
//
//     There is no identity-shape rebuild — every code path produces a
//     non-CallOrCastExpr. The direct-emit win is purely the dispatch
//     skip on the outer `transform_expr` arm; the lowered result still
//     routes through leaf `out.emit_expr(...)` (no re-transform —
//     `transform_call_or_cast_expr` already transformed receivers and
//     args; double-transforming would break parity for smartcast
//     hoisting and similar mutation).
//
//     Pattern note: sibling of AssocExpr (session 34), MatchExpr (35) —
//     "always-lowers via single helper to a DIFFERENT shape".
//     Distinct from SqlExpr / MapInitExpr / ArrayInitExpr (sessions
//     36/40/41) which have an identity branch worth direct-emitting.
//     When CallExpr and InfixExpr get their own flat helpers (sessions
//     43-44), this arm could switch to
//     `t.transform_expr_to_flat(result, mut out)` to chain into those
//     helpers' direct-emit paths — but today they still hit the `else`
//     fallback, so routing through them adds no win.
//
//     Reachability: CallOrCastExpr is the parser's ambiguous
//     `T(x)`/`f(x)` form, common for single-arg calls and casts. By
//     transformer-exit time every CallOrCastExpr becomes one of the
//     shapes above, so the dispatch-skip win applies broadly. No new
//     helper, no new fixture. All 168 transformer-diff tests continue to
//     pass.
//
//   Session 41 (2026-05-27): ArrayInitExpr port (always-lowers via helper,
//     two outcome shapes).
//     `transform_expr`'s ArrayInitExpr arm is a thin wrapper around
//     `transform_array_init_expr` (struct.v:291), which has many outcome
//     shapes that collapse into "identity ArrayInitExpr" vs "lowered
//     CallExpr/UnsafeExpr":
//       (a) invalid data → identity (`return ast.Expr(expr)`).
//       (b) fixed-size array (`[1, 2, 3]!`, `[5]int{}`) → identity-shape
//           rebuild with transformed `exprs`/`init`/`cap`/`len`.
//       (c) eval backend → identity-shape rebuild with transformed
//           typ/init/cap/len/update_expr fields.
//       (d) spread `[...base, e1, e2]` → CallExpr to
//           `new_array_from_array_and_c_array(...)`.
//       (e) empty dynamic array `[]int{}` →
//           `__new_array_with_default_noscan(len, cap, sizeof(elem),
//           init)` CallExpr; or a ForStmt expansion when `init`
//           references `index`.
//       (f) keyed dynamic array `[1, 2, 3]` → `new_array_from_c_array(...)`
//           CallExpr.
//
//     Direct-emit calls `transform_array_init_expr` once and dispatches
//     on result type. Identity branches (a)/(b)/(c) all return an
//     `ast.ArrayInitExpr` — direct-emit via the new
//     `emit_array_init_expr_by_ids` helper, skipping the wrapper
//     allocation. Children are encoded via leaf `out.emit_expr` (the
//     helper already transformed them — `transform_expr_to_flat` would
//     double-transform). Lowering branches (d)/(e)/(f) return a
//     non-ArrayInitExpr shape — route through leaf
//     `out.emit_expr(result)` so the lowering logic stays
//     single-sourced.
//
//     Pattern note: extends "always-lowers via helper, two outcome
//     shapes" (sessions 36 SqlExpr, 40 MapInitExpr). The two-shape
//     template handles multiple identity branches uniformly via
//     post-helper type dispatch. Unlike MapInitExpr (only eval backend
//     keeps the identity shape), ArrayInitExpr keeps identity on every
//     backend for fixed-size literals — so the direct-emit win reaches
//     every backend for that branch, not just eval.
//
//     Reachability is very high — array literals (`[]`, `[1, 2, 3]`,
//     `[]int{}`, `[5]int{init: -1}`, `[1, 2, 3]!`) are ubiquitous in V
//     code. Most call/return-arg array literals on non-eval backends hit
//     branches (d)/(e)/(f), but fixed-size array literals (common in
//     lookup tables and configuration) hit (b) identity-rebuild on every
//     backend. No new fixture this session. All 168 transformer-diff
//     tests continue to pass.
//
//     New pub helper: `emit_array_init_expr_by_ids(typ_id, init_id,
//     cap_id, len_id, update_expr_id, expr_ids, pos)` in
//     `vlib/v2/ast/flat.v` mirrors `add_expr(ArrayInitExpr)` encoding
//     (`emit_simple(.expr_array_init, pos, [typ, init, cap, len,
//     update_expr, exprs...])`).
//
//   Session 40 (2026-05-27): MapInitExpr port (always-lowers via helper,
//     two outcome shapes).
//     `transform_expr`'s MapInitExpr arm is a thin wrapper around
//     `transform_map_init_expr` (struct.v:1010), which has two outcome
//     shapes:
//       (a) eval backend → identity-shape rebuild
//           `MapInitExpr{typ: transform(typ), keys: [transform(k)...],
//            vals: [transform(v)...], pos}`. Each child is already
//           transformed inside the helper.
//       (b) non-eval backend → lowers to a CallExpr:
//           `new_map(sizeof(K), sizeof(V), &hash, &eq, &clone, &free)` for
//           empty literals, `new_map_init(...)` for keyed literals.
//
//     Direct-emit calls `transform_map_init_expr` once and dispatches on
//     result type: `is ast.MapInitExpr` → encode typ/keys/vals via leaf
//     `out.emit_expr` (pure `add_expr` — `transform_expr_to_flat` would
//     double-transform) + assemble via the new
//     `emit_map_init_expr_by_ids(typ_id, key_ids, val_ids, pos)` helper.
//     Otherwise (lowered CallExpr) → route through leaf
//     `out.emit_expr(result)`.
//
//     Pattern note: extends the "always-lowers via helper, two outcome
//     shapes" template established by SqlExpr (session 36) and AssignStmt
//     (37). Identity branch picks up the direct-emit win (skips the
//     `ast.MapInitExpr` wrapper struct allocation + the `transform_expr`
//     match dispatch); lowered branch single-sources the `new_map`
//     construction logic in the legacy helper.
//
//     Reachability: map literals (`{}`, `{'a': 1}`, `map[string]int{}`)
//     are common across the compiler (registries, lookup tables, codegen
//     state). Most v2 backend builds use the non-eval path, so the
//     identity-branch direct-emit win applies only on the eval backend;
//     the dispatch-skip win applies on every backend. No new fixture this
//     session — existing fixtures exercise map literals via for-in maps,
//     tag tables, etc. All 168 transformer-diff tests continue to pass.
//
//     New pub helper: `emit_map_init_expr_by_ids(typ_id, key_ids, val_ids,
//     pos)` in `vlib/v2/ast/flat.v` mirrors `add_expr(MapInitExpr)`
//     encoding exactly (opcode `expr_map_init`, aux1=-1,
//     aux2=keys.len, edges = [typ, keys..., vals...]).
//
//   Session 39 (2026-05-27): ForInStmt port (always-lowers via helper,
//     cross-arm encoding reuse).
//     `transform_stmt`'s ForInStmt arm is a single helper call to
//     `transform_for_in_stmt` (for.v:1115), which wraps the ForInStmt as
//     the `init` of a synthetic ForStmt and delegates to
//     `transform_for_stmt`. The return type is `ast.ForStmt` — a
//     DIFFERENT shape from the input (ForInStmt → ForStmt). All the
//     for-in lowering (RangeExpr → range loop, runes_iterator → array-
//     with-value-type, fixed-array / dynamic-array / string / map /
//     untyped) happens inside `transform_for_stmt` once it sees the
//     ForInStmt in `init`.
//
//     Direct-emit calls `transform_for_in_stmt` once, then encodes the
//     already-transformed ForStmt children via leaf emitters (no
//     re-transform via `transform_*_to_flat` — would double-transform):
//     init/post stmts via `out.emit_stmt`, cond expr via `out.emit_expr`,
//     body stmts via `out.emit_stmt`. Reuses the existing
//     `emit_for_stmt_by_ids(init_id, cond_id, post_id, stmt_ids)` helper
//     from session 38 — no new flat helper needed since the lowered shape
//     is ForStmt.
//
//     Pattern note: stmt-level sibling of AssocExpr (session 34) and
//     MatchExpr (session 35) — the "always-lowers via single helper to a
//     DIFFERENT shape" template. Distinct from session 38 (ForStmt,
//     identity-shape result) because the input arm is ForInStmt but the
//     lowered shape is ForStmt. This is also "cross-arm encoding reuse"
//     — analogous to session 33 (GenericArgOrIndexExpr routes to
//     `transform_index_expr_to_flat` for the next stage) but here we
//     reuse the ENCODING helper rather than re-dispatching through
//     `transform_stmt_to_flat`. We must NOT route through
//     `transform_stmt_to_flat`'s ForStmt arm because that would re-call
//     `transform_for_stmt` on the already-fully-transformed ForStmt —
//     double work and possible smartcast double-application.
//
//     Skips the `ast.ForStmt` wrapper struct allocation for the result
//     and the outer `transform_stmt` match dispatch on every ForInStmt
//     occurrence. Reachability is high: `for x in arr`, `for k, v in
//     map`, `for r in s.runes()` are extremely common (every iterator in
//     the compiler itself uses them).
//
//     No new flat helper this session — reuses `emit_for_stmt_by_ids`
//     from session 38. All 168 transformer-diff tests continue to pass.
//
//   Session 38 (2026-05-27): ForStmt port (always-lowers via helper,
//     identity-shape result).
//     `transform_stmt`'s ForStmt arm is a single helper call to
//     `transform_for_stmt` (for.v:488), which always returns an
//     `ast.ForStmt` regardless of its internal lowering paths. The
//     helper handles for-in loops by dispatching on iterable type
//     (RangeExpr → range lowering; runes_iterator() → array-with-
//     value-type; smartcast array/string variant → array; fixed array
//     → fixed-array; dynamic array/string → array; native backend any
//     → untyped; map/channel/other → ForStmt-wrapped ForInStmt; no
//     type info → untyped) and for plain `for cond { ... }` loops,
//     sets up smartcast contexts from `is` checks in the condition
//     (flattened across `&&` chains), recursively transforms init/
//     cond/post and body via `transform_stmts`, then pops smartcasts
//     and returns the assembled ForStmt. Opens/closes a child scope
//     around the whole transform.
//
//     Direct-emit calls `transform_for_stmt` once, then encodes the
//     already-transformed children via leaf emitters (no re-transform
//     via `transform_*_to_flat` — would double-transform): init/post
//     stmts via `out.emit_stmt`, cond expr via `out.emit_expr`, body
//     stmts via `out.emit_stmt`. Assembles via the new
//     `emit_for_stmt_by_ids(init_id, cond_id, post_id, stmt_ids)`
//     helper, which mirrors `add_stmt(ForStmt)` encoding exactly
//     (pos = `token.Pos{}`, edges = [init, cond, post, body...]).
//
//     Pattern note: stmt-level sibling of SqlExpr branch (b)
//     (session 36) and AssignStmt's main-path (session 37) — the
//     "always-lowers via single helper call, identity-shape result"
//     template. Unlike AssignStmt, there's no pre-arm guard;
//     `transform_for_stmt` handles everything internally. The
//     remaining stmt-side helper-driven arm is ForInStmt (which
//     returns ForStmt — a different shape than its input, so it's a
//     sibling of AssocExpr session 34 / MatchExpr session 35:
//     always-lowers via single helper to a different shape).
//
//     Reachability is high — `for` loops appear in nearly every
//     non-trivial function body. Top-level ForStmts (file scope)
//     are rare, but the arm also fires from any recursive
//     `transform_stmt_to_flat` calls (currently ComptimeStmt's
//     non-$for branch). No new fixture this session. All 168
//     transformer-diff tests continue to pass.
//
//     New pub helper: `emit_for_stmt_by_ids(init_id, cond_id,
//     post_id, stmt_ids)` in `vlib/v2/ast/flat.v` (~line 577).
//
//   Session 37 (2026-05-27): AssignStmt port (pre-arm guard + helper).
//     `transform_stmt`'s AssignStmt arm has a pre-arm dispatch block
//     (transformer.v lines 2958-2966) plus the match-arm call:
//       1. `try_expand_or_expr_assign(stmt)` — stub returning `none`
//          (transformer.v:3804). No-op, skip from direct-emit too.
//       2. `try_transform_map_index_assign(stmt)` — may rewrite map
//          index assigns (`m[key] = v`, `m[k] += v`, `m[k].f = v`, etc.)
//          into an `ExprStmt(map__set(...))` or `BlockStmt` hoisting
//          temp-decl stmts before the call. Returned shape is NOT an
//          AssignStmt — route through leaf `out.emit_stmt(...)`.
//       3. else → `transform_assign_stmt(stmt)` returns an AssignStmt
//          with `lhs`/`rhs` exprs already transformed (string `+=` →
//          `string__plus` call, result/option payload writes, etc.).
//          Direct-emit encodes each lhs/rhs via leaf `out.emit_expr`
//          (pure `add_expr` — `transform_expr_to_flat` would
//          double-transform) and assembles via the new
//          `emit_assign_stmt_by_ids(op, lhs_ids, rhs_ids, pos)` helper.
//
//     Pattern note: stmt-level sibling of SqlExpr (session 36) —
//     "always-lowers via single helper call" with two outcome shapes
//     (rewrite vs identity-rebuild), with an additional pre-arm guard
//     that can short-circuit to a lowered shape. Most assigns reach the
//     identity-rebuild path (only map-index assigns hit the rewrite),
//     so the dispatch-skip win applies broadly. The lowered shapes from
//     `try_transform_map_index_assign` (BlockStmt/ExprStmt) currently
//     route through legacy `emit_stmt`; when a BlockStmt cross-arm
//     router lands they can switch to dispatched routing.
//
//     Mirrors `add_stmt(AssignStmt)` encoding exactly: aux1=-1,
//     aux2=lhs.len (lhs/rhs boundary), meta=u16(op), no flags, edges =
//     lhs in order followed by rhs in order. Skips the `ast.AssignStmt`
//     wrapper struct allocation per occurrence + the `transform_stmt`
//     match dispatch.
//
//     Reachability is very high — assignments appear in nearly every
//     function body. No new fixture this session; existing fixtures
//     exercise AssignStmt extensively (decl-assigns, mutations, compound
//     ops). All 168 transformer-diff tests continue to pass.
//
//     New pub helper: `emit_assign_stmt_by_ids(op, lhs_ids, rhs_ids,
//     pos)` in `vlib/v2/ast/flat.v` (~line 577).
//
//   Session 36 (2026-05-27): SqlExpr port (always-lowers via helper, two
//     outcome shapes).
//     Third sibling at the expr level of the "always-lowers via single
//     helper call" template (after AssocExpr session 34 and MatchExpr
//     session 35). `transform_expr`'s SqlExpr arm is a thin wrapper
//     around `transform_sql_expr` (orm.v), which has two outcome shapes:
//       (a) `is_create` AND `lower_sql_create_expr` finds the table
//           struct → lowers to a CallExpr `expr.create(table_name,
//           table_fields)`, then transforms that CallExpr and returns it
//           — the result is no longer a SqlExpr. Routed through leaf
//           `out.emit_expr(...)` (the lowered CallExpr is already
//           transformed; no re-transform needed).
//       (b) otherwise (read query, count, or create with missing table)
//           → identity-shape rebuild: `SqlExpr{expr:
//           transform_expr(expr.expr), table_name, is_count, is_create,
//           pos}`. Direct-emit encodes `result.expr` via leaf
//           `out.emit_expr` (pure `add_expr` encoder, no re-transform —
//           `transform_expr_to_flat` would double-transform since
//           `transform_sql_expr` already ran `transform_expr` on it) and
//           assembles via the new `emit_sql_expr_by_id(table_name,
//           is_count, is_create, expr_id, pos)` helper. Skips the
//           `ast.SqlExpr` wrapper struct allocation per occurrence.
//
//     Detection: call `transform_sql_expr` once, then dispatch on the
//     result's runtime type — `is ast.SqlExpr` → branch (b), else →
//     branch (a). This keeps the table-lookup logic single-sourced
//     (don't reimplement `lower_sql_create_expr` here just to detect the
//     branch upfront) and preserves the exact "rebuild on lookup miss"
//     semantics.
//
//     Pattern note: extends the "always-lowers via single helper call"
//     template with two outcome shapes. AssocExpr (34) lowers to a leaf
//     Ident; MatchExpr (35) lowers to an IfExpr (awaiting flat helper);
//     SqlExpr (36) either lowers to a CallExpr OR identity-rebuilds.
//     The branch-on-result-type pattern lets identity-shape rebuilds
//     pick up the direct-emit win even when the legacy helper has
//     multiple shapes. Future candidates: arms where a single legacy
//     helper has both a lowering path and an identity-shape rebuild
//     path (some CallExpr/InfixExpr lowering arms fit this).
//
//     Reachability is limited to `sql ... { ... }` blocks (rare in the
//     compiler itself). No new fixture this session — existing fixtures
//     don't exercise SqlExpr. All 168 transformer-diff tests continue
//     to pass.
//
//     New pub helper: `emit_sql_expr_by_id(table_name, is_count,
//     is_create, expr_id, pos)` in `vlib/v2/ast/flat.v` (~line 696).
//     Mirrors `add_expr(SqlExpr)`'s encoding exactly: opcode `expr_sql`,
//     pos, interned table_name (aux1), `flag_is_count | flag_is_create`
//     packed into flags, one edge to inner expr.
//
//   Session 35 (2026-05-27): MatchExpr port (always-lowers via helper).
//     Sibling of AssocExpr (session 34) at the expr level —
//     `transform_expr`'s MatchExpr arm is a single helper call to
//     `t.transform_match_expr(expr)`, which always lowers a `match`
//     construct to an IfExpr:
//       - sum-type match: each branch's smartcast is set up (variant
//         names → tags), then the chain `if x is Variant1 { ... } else
//         if x is Variant2 { ... } ...` is built; the smartcast stack is
//         restored after each branch.
//       - non-sum match: chains `x == c1`/`x in c_list` IfBranches in
//         declaration order, with `else` becoming the final IfBranch.
//     The lowered IfExpr is returned as `ast.Expr` (not pre-transformed
//     through `transform_expr`); the caller's match arm wraps it via
//     legacy `add_expr(transformed_if)` which re-enters `transform_expr`
//     on the IfExpr.
//
//     Direct-emit calls `transform_match_expr` and routes the IfExpr
//     result through the leaf `out.emit_expr(...)` — same as the `else`
//     fallback would, minus the `transform_expr` match dispatch on
//     MatchExpr. No dedicated IfExpr flat helper exists yet, so routing
//     through `transform_expr_to_flat` for the result would only add
//     one dispatch level without benefit. When an IfExpr flat helper
//     lands, change this arm to feed the result back through
//     `transform_expr_to_flat` (cross-arm routing, same pattern as
//     GenericArgOrIndexExpr → transform_index_expr_to_flat in session
//     33).
//
//     Pattern note: same "always-lowers via single helper call" shape
//     as AssocExpr (session 34) — both arms have a single
//     `t.lower_*(expr, ...)` body in legacy `transform_expr`. AssocExpr
//     lowers to a leaf Ident (smartcast context handled via pending
//     stmts); MatchExpr lowers to an IfExpr awaiting its own flat
//     helper. Reachability is high — `match` is one of the most common
//     constructs in the compiler. Win is purely the dispatch skip until
//     IfExpr gets a flat helper.
//
//     No new fixture this session — existing fixtures already exercise
//     MatchExpr extensively (sum-type matches, literal matches, range
//     matches). All 141 transformer-diff tests continue to pass.
//
//   Session 34 (2026-05-26): AssocExpr port (always-lowers via helper).
//     `transform_expr`'s AssocExpr arm is a single helper call —
//     `t.lower_assoc_expr(expr, false)` always lowers the struct-update
//     syntax `{base | field: val}` into a decl-assign of a typed tmp
//     followed by per-field assignments, all hoisted into
//     `t.pending_stmts` (which `transform_stmts` then drains as prefix
//     stmts before the current stmt). The arm returns the tmp Ident
//     (or `&tmp_ident` PrefixExpr in the `take_addr` form, which is
//     only invoked by the PrefixExpr `.amp` rewrite branch — not by
//     this arm).
//
//     Direct-emit calls `lower_assoc_expr` and routes the Ident result
//     through the leaf `out.emit_expr(...)` — same as the `else`
//     fallback minus the `transform_expr` match dispatch on AssocExpr.
//     The win is purely the dispatch skip. Routing through
//     `transform_expr_to_flat` for the result would add a dispatch
//     level without benefit since Idents are leaf direct-emit already.
//
//     Pattern note: establishes the "always-lowers via single helper
//     call" template — sibling of GenericArgOrIndexExpr (session 33)
//     where cross-arm routing applies because the lowered shape has a
//     dedicated flat helper. AssocExpr's result is a leaf Ident, so
//     direct `out.emit_expr` is the right exit. Future siblings:
//     MatchExpr (lowers to IfExpr via `transform_match_expr`) — same
//     shape, route through `out.emit_expr` until an IfExpr flat helper
//     exists; SqlExpr if it has a similar single-helper structure.
//
//     Reachability is moderate — struct-update syntax `{base | f: v}`
//     is used across the compiler for AST node copying. No new fixture
//     this session. All 141 transformer-diff tests continue to pass.
//
//   Session 33 (2026-05-26): GenericArgOrIndexExpr port (cross-arm routing).
//     `transform_expr`'s GenericArgOrIndexExpr arm disambiguates the parser
//     ambiguity for `x[y]` via the lhs type:
//       (a) callable lhs → `specialize_generic_callable_expr(lhs, [expr],
//           pos)` lowers to the generic arg specialization token (Ident or
//           CallExpr) — different shape, route through leaf
//           `out.emit_expr(...)`.
//       (b) otherwise → constructs an `ast.IndexExpr{lhs, expr,
//           is_gated: false, pos}` and runs it through
//           `transform_index_expr`. Direct-emit routes through the
//           existing `transform_index_expr_to_flat` helper so the IndexExpr
//           arm's default-path direct-emit (lhs + expr via
//           `transform_expr_to_flat`, assembled via
//           `emit_index_expr_by_ids`) also applies here.
//
//     Pattern note: first port where one ported `transform_*_to_flat`
//     helper calls another (`GenericArgOrIndexExpr` → `transform_index_
//     expr_to_flat`). Establishes the cross-arm routing template — when
//     an arm always lowers into a shape that already has a flat helper,
//     forward to that helper directly instead of going through legacy
//     `transform_expr` dispatch.
//
//     `is_callable_type` is an immutable lookup (`&Transformer` receiver);
//     `specialize_generic_callable_expr` is `mut`. Gate the mut-call
//     branch via an upfront immutable probe — same shape as the Ident /
//     KeywordOperator / GenericArgs ports (sessions 23-25). Reachability
//     is limited (the parser typically unifies into CallExpr/IndexExpr at
//     parse time), but the port removes one fallback path from the `else`
//     branch. No new fixture this session. All 141 transformer-diff tests
//     continue to pass.
//
//   Session 32 (2026-05-26): AssertStmt port (fallback stmt-level identity).
//     `transform_stmt`'s AssertStmt arm is the rare fallback path — most
//     assert stmts get expanded into `if !cond { panic(...) }` by
//     `transform_stmts` before they reach the per-stmt dispatch. The
//     fallback rebuilds `AssertStmt{expr: t.transform_expr(stmt.expr)}`
//     without setting `extra`, so the result always has `extra =
//     empty_expr` (struct default) and any original `stmt.extra` (the
//     `"assert cond, msg"` text) is DROPPED.
//
//     Direct-emit mirrors that exactly: transform `stmt.expr` via the
//     recursive `transform_expr_to_flat` (so any ported deep-helper expr
//     arm reached inside benefits too) and route through the new
//     `emit_assert_stmt_by_id` helper, which encodes the second edge as
//     `add_expr(empty_expr)` — hits the shared cached `empty_expr_id` so
//     repeat AssertStmts don't duplicate the node. Mirrors
//     `add_stmt(AssertStmt)` encoding exactly (pos = `token.Pos{}`, two
//     edges: expr + empty_expr). Skips the `ast.AssertStmt` wrapper
//     struct allocation per fallback occurrence.
//
//     Reachability is low but the port completes the stmt-level
//     identity-shape coverage planned in session 28's pattern note
//     (BlockStmt → session 29, DeferStmt → session 30, ComptimeStmt →
//     session 31, AssertStmt → this session). No new fixture this
//     session. All 141 transformer-diff tests continue to pass.
//
//   Session 31 (2026-05-26): ComptimeStmt port (two-branch stmt port).
//     `transform_stmt`'s ComptimeStmt arm has two branches:
//       (a) `stmt.stmt is ast.ForStmt` (the `$for field in Type.fields { ... }`
//           form): rebuild ComptimeStmt(ForStmt{init, cond, post, stmts:
//           transform_stmts(for_stmt.stmts)}) — `init`/`cond`/`post` are
//           copied verbatim (NOT transformed by the legacy arm), only the
//           body stmts go through the body driver.
//       (b) otherwise (`$if` as stmt, etc.): legacy returns
//           `transform_stmt(stmt.stmt)` — the ComptimeStmt wrapper is
//           DROPPED entirely; the result is whatever the inner stmt
//           transforms into.
//
//     Direct-emit: branch (a) synthesises a `ForStmt{init, cond, post,
//     transformed_stmts}` and routes it through legacy `out.emit_stmt(...)`
//     for the encoding (ForStmt itself isn't ported yet), then wraps via
//     the new `emit_comptime_stmt_by_id` helper. Branch (b) recurses via
//     `transform_stmt_to_flat(stmt.stmt, ...)` so any ported inner stmt
//     (ExprStmt, ReturnStmt, ...) also stays on the flat path.
//     Mirrors `add_stmt(ComptimeStmt)` encoding exactly (pos =
//     `token.Pos{}`, single edge to inner stmt). Skips the outer
//     `ast.ComptimeStmt` wrapper struct allocation per `$for` occurrence
//     AND the wrapper entirely on the non-`$for` recursive path.
//
//     Reachability is reasonable — compile-time reflection via `$for`
//     shows up in the compiler/runtime helpers. No new fixture this
//     session. All 141 transformer-diff tests continue to pass.
//
//     Pattern note: first stmt port with TWO branches that diverge at
//     emit time (wrap-and-build vs. drop-and-recurse). Establishes the
//     "branch-dispatch" template for future stmt ports where the legacy
//     arm has conditional shape changes (e.g. AssertStmt expand-in-driver
//     vs. fallback, PostfixExpr op-based lowering at the expr level
//     already used this shape on the expr side).
//
//   Session 30 (2026-05-26): DeferStmt port (wrap-only stmt port).
//     Sibling of session 29 (BlockStmt) — same wrap-only shape with a
//     `mode` field. `transform_stmt`'s DeferStmt arm always returns
//     `DeferStmt{mode: stmt.mode, stmts: transform_stmts(stmt.stmts)}` —
//     identity-shape with the body driver `transform_stmts` doing the
//     same inter-stmt work (smartcast snapshot/restore, pending-stmt
//     hoisting, one-to-many expansion). Same driver constraint as the
//     BlockStmt arm.
//
//     Wrap-only port: call legacy `transform_stmts(stmt.stmts)` to
//     materialise the transformed `[]ast.Stmt`, emit each via the legacy
//     `out.emit_stmt(...)` (already transformed), and assemble via the
//     new `emit_defer_stmt_by_ids(mode, stmt_ids)` helper. Mirrors
//     `add_stmt(DeferStmt)` encoding exactly: `pos = token.Pos{}`,
//     `flags |= flag_defer_func` when mode is `.function`, edges = inner
//     stmts in order. Skips the outer `ast.DeferStmt` wrapper struct
//     allocation per occurrence; the inner `[]ast.Stmt` still
//     materialises until the body driver is ported.
//
//     Reachability is limited but clean — `defer { ... }` appears in
//     resource-cleanup paths (file close, mutex unlock, ...) and
//     `defer (func) { ... }` (function-scope variant) in fewer places.
//     No new fixture this session. All 141 transformer-diff tests
//     continue to pass.
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
		// Defensive pending_stmts drain. Inner transform_stmts (called by
		// transform_fn_decl etc.) drains its own pending_stmts before
		// returning; the appender catches any leak from a top-level
		// construct that hasn't been audited and hoists them ahead of the
		// just-emitted stmt, matching the ordering invariant of
		// transform_stmts' `append_transformed_stmt` path.
		t.append_transformed_stmt_to_flat(mut stmt_ids, stmt, mut out)
	}
	return out.append_file_with_stmt_ids(file, stmt_ids)
}

// transform_stmts_to_flat is the consolidated seam for "transform a body
// stmt list, then encode each result into the flat builder" — the pattern
// repeated at every flat-write arm that wraps a body (BlockStmt, DeferStmt,
// ComptimeStmt $for body, UnsafeExpr body, FnLiteral body). Currently a
// literal pass-through over `transform_stmts` + leaf-encode loop, kept as a
// single function so future sessions can replace the body with direct-emit
// ports for `transform_stmts`'s expansion sites (comptime $if assign, or-
// block assign, tuple if-assign, lock/rlock, for-in-map, assert, ...) in
// one place. The FnDecl arm has its own seam (`transform_fn_decl_parts_to_flat`)
// because the FnDecl body driver also runs `lower_defer_stmts` between
// `transform_stmts` and the flat encoding — porting that needs a separate
// thrust.
pub fn (mut t Transformer) transform_stmts_to_flat(stmts []ast.Stmt, mut out ast.FlatBuilder) []ast.FlatNodeId {
	return t.transform_stmts_to_flat_direct(stmts, mut out)
}

// transform_stmts_to_flat_direct is the flat-direct mirror of
// `transform_stmts` (transformer.v:3072). Drives the per-stmt loop emitting
// `FlatNodeId`s straight into `out` instead of materialising a `[]ast.Stmt`
// intermediate. Mirrors every expansion site (comptime $if assign, native
// interface cast/sincos, or-block assign, tuple if-assign, tuple call-assign,
// if-guard assign, if-expr assign, $if expr-stmt, or-expr stmt, if-guard
// stmt, flag-enum set/clear, return-match, or-return, return-if, lock/rlock,
// map-index push/postfix, native selector postfix, for-in-map, assert) and
// the default fall-through via `append_transformed_stmt_to_flat`.
//
// Stmts produced by expansion helpers (legacy `try_expand_*` /
// `expand_*` helpers that still return `ast.Stmt` / `[]ast.Stmt`) are leaf-
// encoded via `out.emit_stmt(...)` — these helpers transform the contained
// exprs internally, so leaf-encoding is bit-equal to the legacy
// `result << expanded_X` push. The `transform_stmt(stmt)` dispatch on the
// default fall-through routes through `transform_stmt_to_flat`'s direct-emit
// arms via the appender, which skips the legacy stmt-wrapper allocation for
// the many already-ported variants.
//
// Saves the outer `[]ast.Stmt` allocation (one per call) and the stmt-
// wrapper allocations on the default path. Expansion sites still materialise
// their inner intermediates — those need per-helper `_to_flat` ports in
// future sessions to fully drop the legacy materialisation.
pub fn (mut t Transformer) transform_stmts_to_flat_direct(stmts []ast.Stmt, mut out ast.FlatBuilder) []ast.FlatNodeId {
	mut ids := []ast.FlatNodeId{cap: stmts.len}
	block_smartcast_depth := t.smartcast_stack.len
	has_smartcast_state := block_smartcast_depth > 0
	block_smartcast_stack := if has_smartcast_state {
		t.smartcast_stack.clone()
	} else {
		[]SmartcastContext{}
	}
	block_smartcast_counts := if has_smartcast_state {
		t.smartcast_expr_counts.clone()
	} else {
		map[string]int{}
	}
	for stmt in stmts {
		if t.smartcast_stack.len < block_smartcast_depth {
			t.smartcast_stack = block_smartcast_stack.clone()
			t.smartcast_expr_counts = block_smartcast_counts.clone()
		} else if t.smartcast_stack.len > block_smartcast_depth {
			t.truncate_smartcasts(block_smartcast_depth)
		}
		if stmt is ast.AssignStmt {
			assign_stmt := stmt as ast.AssignStmt
			if t.try_expand_comptime_if_assign_to_flat(assign_stmt, mut ids, mut out) {
				continue
			}
			t.track_interface_assign_to_flat(assign_stmt)
			if t.try_expand_interface_cast_assign_to_flat(assign_stmt, mut ids, mut out) {
				continue
			}
			if t.try_expand_sincos_assign_to_flat(assign_stmt, mut ids, mut out) {
				continue
			}
			if t.try_expand_or_expr_assign_stmts_to_flat(&assign_stmt, mut ids, mut out) {
				continue
			}
			if t.try_expand_tuple_if_assign_stmts_to_flat(assign_stmt, mut ids, mut out) {
				continue
			}
			if t.try_expand_tuple_call_assign_to_flat(assign_stmt, mut ids, mut out) {
				continue
			}
			if t.try_expand_if_guard_assign_stmts_to_flat(assign_stmt, mut ids, mut out) {
				continue
			}
			if t.try_expand_if_expr_assign_stmts_to_flat(assign_stmt, mut ids, mut out) {
				continue
			}
		}
		if stmt is ast.ExprStmt {
			if t.try_expand_comptime_if_stmt_to_flat(stmt, mut ids, mut out) {
				continue
			}
		}
		if stmt is ast.ExprStmt {
			if t.try_expand_or_expr_stmt_to_flat(stmt, mut ids, mut out) {
				continue
			}
			if t.try_expand_if_guard_stmt_to_flat(stmt, mut ids, mut out) {
				continue
			}
		}
		if stmt is ast.ExprStmt {
			if t.try_emit_flag_enum_set_clear_to_flat(stmt, mut ids, mut out) {
				continue
			}
		}
		if stmt is ast.ReturnStmt {
			if t.try_expand_return_match_expr_to_flat(stmt, mut ids, mut out) {
				continue
			}
			if t.try_expand_or_expr_return_to_flat(stmt, mut ids, mut out) {
				continue
			}
			if t.try_expand_return_if_expr_to_flat(stmt, mut ids, mut out) {
				continue
			}
		}
		if stmt is ast.ExprStmt {
			if stmt.expr is ast.LockExpr {
				t.expand_lock_expr_to_flat(stmt.expr, mut ids, mut out)
				continue
			}
			if t.try_emit_map_index_push_to_flat(stmt, mut ids, mut out) {
				continue
			}
			if t.try_emit_map_index_postfix_to_flat(stmt, mut ids, mut out) {
				continue
			}
			if t.try_emit_selector_postfix_to_flat(stmt, mut ids, mut out) {
				continue
			}
		}
		if stmt is ast.ForStmt {
			if t.try_expand_for_in_map_to_flat(stmt, mut ids, mut out) {
				continue
			}
		}
		if stmt is ast.AssertStmt {
			t.expand_assert_stmt_to_flat(stmt, mut ids, mut out)
			continue
		}
		t.append_transformed_stmt_to_flat(mut ids, stmt, mut out)
	}
	if t.smartcast_stack.len < block_smartcast_depth {
		t.smartcast_stack = block_smartcast_stack.clone()
		t.smartcast_expr_counts = block_smartcast_counts.clone()
	} else if t.smartcast_stack.len > block_smartcast_depth {
		t.truncate_smartcasts(block_smartcast_depth)
	}
	return ids
}

// append_transformed_stmt_to_flat is the flat-builder mirror of
// `append_transformed_stmt` (transformer.v:3061). Runs `transform_stmt_to_flat`
// on the input stmt (direct-emitting via the dispatch seam), then drains any
// `t.pending_stmts` produced by sub-transforms (e.g. or-block side effects
// pushed by `expand_single_or_expr`) by leaf-encoding them into `out` BEFORE
// the just-emitted main stmt — matching the ordering invariant of the legacy
// appender ("pending stmts hoist ahead of the transformed result").
//
// Scaffolding for the `transform_stmts` body driver port (arc #1). Future
// sessions fork the legacy `transform_stmts` body into a direct-emit driver
// that drives the per-stmt loop via this appender, replacing the inner
// `t.append_transformed_stmt(mut result, ast.Stmt(...))` calls with their
// `_to_flat` equivalents. Used today by `transform_file_index_to_flat`'s
// top-level loop (replacing the previous push/pop/restore pattern).
pub fn (mut t Transformer) append_transformed_stmt_to_flat(mut ids []ast.FlatNodeId, stmt ast.Stmt, mut out ast.FlatBuilder) {
	id := t.transform_stmt_to_flat(stmt, mut out)
	if t.pending_stmts.len > 0 {
		pending := t.pending_stmts.clone()
		t.pending_stmts.clear()
		for ps in pending {
			ids << out.emit_stmt(ps)
		}
	}
	ids << id
}

// expand_assert_stmt_to_flat is the flat-direct mirror of `expand_assert_stmt`
// (transformer.v:13931). It builds the same `ast.ExprStmt{expr: ast.IfExpr{...}}`
// lowering but pushes the resulting transformed stmt id directly onto `ids` via
// `append_transformed_stmt_to_flat` — skipping the outer `[]ast.Stmt{cap: 1}`
// allocation that the legacy helper returns. Body stmts and the IfExpr wrapper
// are still built as legacy ast structs and routed through the appender; per-
// helper direct-emit of the body eprintln/exit calls remains as future work.
//
// The shape mirror is exact: same header / expr_msg, same value-print branch
// gating (InfixExpr with comparison op and both operands simple), same exit(1)
// tail. Bit-equal with the legacy `expand_assert_stmt` + per-stmt loop pattern;
// pinned by `fixture_assert_stmt` in the harness across all 5 invariants.
pub fn (mut t Transformer) expand_assert_stmt_to_flat(stmt ast.AssertStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	expr_str := expr_to_v_string(stmt.expr)
	if !expr_has_valid_data(stmt.expr) {
		return
	}
	pos := stmt.expr.pos()
	line := t.get_line_for_pos(pos)
	fn_name := t.cur_fn_name_str
	header := '${t.cur_file_name}:${line}: fn ${fn_name}'
	expr_msg := '   > assert ${expr_str}'
	mut body_stmts := []ast.Stmt{}
	body_stmts << t.make_eprintln_str(header, pos)
	body_stmts << t.make_eprintln_str(expr_msg, pos)
	if stmt.expr is ast.InfixExpr && stmt.expr.op in [.eq, .ne, .lt, .gt, .le, .ge] {
		if assert_expr_is_simple(stmt.expr.lhs) && assert_expr_is_simple(stmt.expr.rhs) {
			body_stmts << t.make_eprintln_inter('    Left value: `', stmt.expr.lhs, '`', pos)
			body_stmts << t.make_eprintln_inter('   Right value: `', stmt.expr.rhs, '`', pos)
		}
	}
	body_stmts << ast.ExprStmt{
		expr: ast.CallExpr{
			lhs:  ast.Ident{
				name: 'exit'
			}
			args: [
				ast.Expr(ast.BasicLiteral{
					value: '1'
					kind:  .number
					pos:   pos
				}),
			]
			pos:  pos
		}
	}
	if_stmt := ast.Stmt(ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  ast.PrefixExpr{
				op:   .not
				expr: stmt.expr
				pos:  pos
			}
			stmts: body_stmts
			pos:   pos
		}
	})
	t.append_transformed_stmt_to_flat(mut ids, if_stmt, mut out)
}

// try_expand_tuple_if_assign_stmts_to_flat is the flat-direct mirror of
// `try_expand_tuple_if_assign_stmts` (if.v:888). Validates the same guard
// chain (decl_assign with tuple lhs + single IfExpr rhs + non-empty tuple +
// well-formed then/else branches). On match, mirrors the legacy result by
// pushing one decl-assign per tuple slot followed by the wrapping
// `ExprStmt(IfExpr(...))` directly via `append_transformed_stmt_to_flat` —
// skipping the outer `[]ast.Stmt{cap: n + 1}` allocation that the legacy
// helper returns. Returns true if the expansion was performed (caller
// should `continue`); false otherwise.
//
// Bit-equal with the legacy `try_expand_tuple_if_assign_stmts(...) +
// for-loop append_transformed_stmt_to_flat` pattern at the site. Inner
// helpers (`build_tuple_branch_assigns`) still build their own `[]ast.Stmt`
// for then/else branches; per-helper direct-emit of those branches remains
// future work. Pinned by `fixture_tuple_if_assign` across all 5 invariants.
pub fn (mut t Transformer) try_expand_tuple_if_assign_stmts_to_flat(stmt ast.AssignStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if stmt.op != .decl_assign {
		return false
	}
	is_tuple_lhs := stmt.lhs.len > 1 || (stmt.lhs.len == 1 && stmt.lhs[0] is ast.Tuple)
	if !is_tuple_lhs {
		return false
	}
	if stmt.rhs.len != 1 {
		return false
	}
	if stmt.rhs[0] !is ast.IfExpr {
		return false
	}
	tuple_lhs := if stmt.lhs.len > 1 {
		stmt.lhs
	} else if stmt.lhs[0] is ast.Tuple {
		(stmt.lhs[0] as ast.Tuple).exprs
	} else {
		stmt.lhs
	}
	n := tuple_lhs.len
	if n == 0 {
		return false
	}
	if_expr := stmt.rhs[0] as ast.IfExpr
	then_stmts := t.build_tuple_branch_assigns(if_expr.stmts, tuple_lhs, n, stmt.pos) or {
		return false
	}
	mut else_expr := ast.Expr(ast.empty_expr)
	if if_expr.else_expr is ast.IfExpr {
		else_if := if_expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			else_stmts := t.build_tuple_branch_assigns(else_if.stmts, tuple_lhs, n, stmt.pos) or {
				return false
			}
			else_expr = ast.IfExpr{
				stmts: else_stmts
				pos:   else_if.pos
			}
		} else {
			else_if_stmts := t.build_tuple_branch_assigns(else_if.stmts, tuple_lhs, n, stmt.pos) or {
				return false
			}
			else_expr = ast.IfExpr{
				cond:      else_if.cond
				stmts:     else_if_stmts
				else_expr: else_if.else_expr
				pos:       else_if.pos
			}
		}
	}
	for lhs_expr in tuple_lhs {
		t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [lhs_expr]
			rhs: [
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '0'
				}),
			]
			pos: stmt.pos
		}), mut out)
	}
	t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.ExprStmt{
		expr: ast.IfExpr{
			cond:      if_expr.cond
			stmts:     then_stmts
			else_expr: else_expr
			pos:       if_expr.pos
		}
	}), mut out)
	return true
}

// try_expand_tuple_call_assign_to_flat is the flat-direct mirror of
// `try_expand_tuple_call_assign` (transformer.v:3489). Mirrors the same guard
// chain (tuple lhs with n>=2 + single non-IfExpr rhs that is a CallExpr or a
// PostfixExpr-of-Call with `.not`/`.question` op). On match, bumps
// `temp_counter` exactly once (matching the legacy ordering) and emits, in
// order, the `_tuple_tN := <call>` decl plus n per-slot
// `lhs[i] = _tuple_tN.argI` SelectorExpr assignments directly via
// `append_transformed_stmt_to_flat` — skipping the outer
// `[]ast.Stmt{cap: n + 1}` allocation that the legacy helper returns.
// Returns true if the expansion was performed (caller should `continue`);
// false otherwise.
//
// Bit-equal with the legacy `try_expand_tuple_call_assign(...) + for-loop
// append_transformed_stmt_to_flat` pattern at the site (must preserve the
// single `temp_counter++` so `_tuple_tN` names stay aligned). Pinned by
// `fixture_tuple_call_assign` across all 5 invariants.
pub fn (mut t Transformer) try_expand_tuple_call_assign_to_flat(stmt ast.AssignStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	tuple_lhs := t.get_tuple_lhs(stmt) or { return false }
	n := tuple_lhs.len
	if n < 2 {
		return false
	}
	if stmt.rhs.len != 1 {
		return false
	}
	if stmt.rhs[0] is ast.IfExpr {
		return false
	}
	rhs_expr := stmt.rhs[0]
	mut is_call := rhs_expr is ast.CallExpr
	if rhs_expr is ast.PostfixExpr {
		if rhs_expr.op in [.not, .question] {
			is_call = rhs_expr.expr is ast.CallExpr
		}
	}
	if !is_call {
		return false
	}
	t.temp_counter++
	tmp_name := '_tuple_t${t.temp_counter}'
	tmp_ident := ast.Ident{
		name: tmp_name
	}
	t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(tmp_ident)]
		rhs: [rhs_expr]
		pos: stmt.pos
	}), mut out)
	for i in 0 .. n {
		t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
			op:  stmt.op
			lhs: [tuple_lhs[i]]
			rhs: [
				ast.Expr(ast.SelectorExpr{
					lhs: tmp_ident
					rhs: ast.Ident{
						name: 'arg${i}'
					}
				}),
			]
			pos: stmt.pos
		}), mut out)
	}
	return true
}

// try_expand_return_match_expr_to_flat is the flat-direct mirror of
// `try_expand_return_match_expr` (if.v:608). Body-mirror port (s141,
// body-mirror sweep continuation). Replicates the legacy helper's body
// inline: single-expr ReturnStmt + MatchExpr guard chain, sumtype-return-
// wrap state save/restore, `transform_match_expr` lowering to IfExpr,
// then `expand_return_match_if_expr` to build the stmt list.
//
// Memory win: zero direct slice savings — `expand_return_match_if_expr`
// still allocates the result slice and the dispatcher iterates it.
// Body-mirror benefit is architectural: eliminates the legacy
// `try_expand_return_match_expr` function frame; centralises state
// management (sumtype_return_wrap, preserve_match_branch_value) at the
// flat-direct dispatch site; makes the dispatch surface uniform with
// the s138-s140 body-mirror trio. Real allocation reduction requires
// porting `expand_return_match_if_expr` / `expand_return_if_expr_with_options`
// to direct-emit — multi-session future work.
//
// Bit-equal. Pinned by `fixture_return_match` across all 5 invariants.
pub fn (mut t Transformer) try_expand_return_match_expr_to_flat(stmt ast.ReturnStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if stmt.exprs.len != 1 {
		return false
	}
	match_expr := stmt.exprs[0]
	if match_expr !is ast.MatchExpr {
		return false
	}
	match_expr_ast := match_expr as ast.MatchExpr
	should_wrap_return_sumtype := t.cur_fn_ret_type_name != ''
		&& t.is_sum_type(t.cur_fn_ret_type_name)
	skip_return_sumtype_wrap := (t.cur_fn_returns_option || t.cur_fn_returns_result)
		&& t.return_expr_should_skip_sumtype_wrap(match_expr)
	old_wrap := t.sumtype_return_wrap
	if should_wrap_return_sumtype && !skip_return_sumtype_wrap {
		t.sumtype_return_wrap = t.cur_fn_ret_type_name
	}
	old_preserve_match_branch_value := t.preserve_match_branch_value
	t.preserve_match_branch_value = true
	transformed := t.transform_match_expr(match_expr_ast)
	t.preserve_match_branch_value = old_preserve_match_branch_value
	t.sumtype_return_wrap = old_wrap
	if transformed !is ast.IfExpr {
		return false
	}
	ie := transformed as ast.IfExpr
	expanded := t.expand_return_match_if_expr(ie)
	if t.pending_stmts.len > 0 {
		for ps in t.pending_stmts {
			ids << out.emit_stmt(ps)
		}
		t.pending_stmts.clear()
	}
	for exp_stmt in expanded {
		ids << out.emit_stmt(exp_stmt)
	}
	return true
}

// try_expand_or_expr_stmt_to_flat is the flat-direct mirror of
// `try_expand_or_expr_stmt` (transformer.v:6273). Body-mirror port
// (s138 pattern, first of the body-mirror sweep): replicates the legacy
// helper's body inline and emits each produced stmt directly into the
// FlatBuilder, skipping the outer `[]ast.Stmt` return slice that the
// legacy helper allocates. The inner `extract_or_expr` /
// `transform_or_call_expr` / `transform_expr` calls still produce legacy
// `ast.Stmt` / `ast.Expr` values — porting those to flat-direct is
// multi-session future work — but the outer return slice and the
// inner_pending merge loop are eliminated.
//
// Shape:
//   - Guard via `expr_has_or_expr`; return false if no or-expr/propagation.
//   - If expr is a top-level OrExpr, try the "direct" fast path first
//     (`try_expand_direct_or_expr_stmt` still returns a legacy slice; emit
//     each item via `out.emit_stmt`). Bit-equal to the legacy `if direct_stmts := ...`
//     branch.
//   - Otherwise build `prefix_stmts` via `extract_or_expr` (side-effect
//     pushes), bail if empty, then transform the resulting expression
//     under saved-pending-stmts + saved-skip-if discipline (matches the
//     legacy helper exactly — pending stmts produced by `transform_expr`
//     must hoist AFTER the or-expr prefix stmts, not before).
//   - Emit prefix_stmts → inner_pending → terminating ExprStmt directly
//     into `ids`/`out`, with the outer site-level `pending_stmts` drain
//     applied first (legacy site behavior).
//
// Memory win: one `[]ast.Stmt` outer slice per fired site (small but
// real). Sets up the body-mirror pattern for the remaining or-block
// thin-dispatchers (`try_expand_or_expr_assign_stmts_to_flat`,
// `try_expand_or_expr_return_to_flat`) in subsequent sessions.
//
// Bit-equal. Pinned by `fixture_or_stmt` across all 5 invariants.
pub fn (mut t Transformer) try_expand_or_expr_stmt_to_flat(stmt ast.ExprStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if !t.expr_has_or_expr(stmt.expr) {
		return false
	}
	if stmt.expr is ast.OrExpr {
		if direct_stmts := t.try_expand_direct_or_expr_stmt(stmt.expr) {
			if t.pending_stmts.len > 0 {
				for ps in t.pending_stmts {
					ids << out.emit_stmt(ps)
				}
				t.pending_stmts.clear()
			}
			for exp_stmt in direct_stmts {
				ids << out.emit_stmt(exp_stmt)
			}
			return true
		}
	}
	mut prefix_stmts := []ast.Stmt{}
	new_expr := t.extract_or_expr(stmt.expr, mut prefix_stmts)
	if prefix_stmts.len == 0 {
		return false
	}
	saved_pending := t.pending_stmts
	t.pending_stmts = []ast.Stmt{}
	saved_skip_if := t.skip_if_value_lowering
	t.skip_if_value_lowering = true
	transformed_new := t.transform_expr(new_expr)
	t.skip_if_value_lowering = saved_skip_if
	inner_pending := t.pending_stmts
	t.pending_stmts = saved_pending
	if t.pending_stmts.len > 0 {
		for ps in t.pending_stmts {
			ids << out.emit_stmt(ps)
		}
		t.pending_stmts.clear()
	}
	for ps in prefix_stmts {
		ids << out.emit_stmt(ps)
	}
	for ip in inner_pending {
		ids << out.emit_stmt(ip)
	}
	ids << out.emit_stmt(ast.ExprStmt{
		expr: transformed_new
	})
	return true
}

// try_expand_or_expr_assign_stmts_to_flat is the flat-direct mirror of
// `try_expand_or_expr_assign_stmts` (transformer.v:4375). Body-mirror
// port (s138 pattern, second of the body-mirror sweep). Replicates the
// legacy helper's body inline and emits each produced stmt directly into
// the FlatBuilder, skipping the outer `[]ast.Stmt` return slice.
//
// Shape:
//   - Guard via `stmt.rhs.len == 1` (legacy returns none otherwise).
//   - If RHS is directly an OrExpr → delegate to
//     `expand_direct_or_expr_assign` (still returns legacy `[]ast.Stmt`;
//     direct-emit via `out.emit_stmt` per item). Bit-equal.
//   - Otherwise check `expr_has_or_expr` on RHS; bail if none.
//   - `extract_or_expr` builds prefix_stmts via side effects; bail on
//     empty (no or-expr extracted).
//   - Optional EmptyExpr rewrite via `or_payload_expr_from_prefix_stmts`
//     (recovers payload expression when the or-block has no value).
//   - `transform_expr(new_rhs)` (no save/restore of pending_stmts here —
//     the legacy helper doesn't either; pending_stmts produced inside
//     transform_expr remain in `t.pending_stmts` and are drained at the
//     site as the outer `pending_stmts` drain).
//   - Tuple destructuring branch (a, b := call()?): build temp ident +
//     decl_assign + per-arg selector assigns, all direct-emit.
//   - Otherwise: build final AssignStmt via `transform_stmt` (so map
//     index lowering / string compound assignment fire), direct-emit
//     prefix_stmts then the final transformed assign.
//
// Memory win: one `[]ast.Stmt` outer slice per fired site (plus the
// tuple-destructuring branch saves a second allocation since prefix_stmts
// is no longer returned). Sets up the body-mirror pattern for the
// remaining or-block thin-dispatcher (`try_expand_or_expr_return_to_flat`
// in s140).
//
// Bit-equal. Pinned by `fixture_or_assign` across all 5 invariants.
pub fn (mut t Transformer) try_expand_or_expr_assign_stmts_to_flat(stmt &ast.AssignStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if stmt.rhs.len != 1 {
		return false
	}
	rhs_expr := stmt.rhs[0]
	if rhs_expr is ast.OrExpr {
		direct := t.expand_direct_or_expr_assign(stmt, rhs_expr) or { return false }
		if t.pending_stmts.len > 0 {
			for ps in t.pending_stmts {
				ids << out.emit_stmt(ps)
			}
			t.pending_stmts.clear()
		}
		for exp_stmt in direct {
			ids << out.emit_stmt(exp_stmt)
		}
		return true
	}
	if !t.expr_has_or_expr(rhs_expr) {
		return false
	}
	mut prefix_stmts := []ast.Stmt{}
	mut new_rhs := t.extract_or_expr(rhs_expr, mut prefix_stmts)
	if prefix_stmts.len == 0 {
		return false
	}
	if new_rhs is ast.EmptyExpr {
		payload_type := t.or_assign_expected_payload_type(stmt, rhs_expr) or {
			types.Type(types.voidptr_)
		}
		if payload_expr := t.or_payload_expr_from_prefix_stmts(prefix_stmts, payload_type) {
			new_rhs = payload_expr
		}
	}
	transformed_rhs := t.transform_expr(new_rhs)
	if t.pending_stmts.len > 0 {
		for ps in t.pending_stmts {
			ids << out.emit_stmt(ps)
		}
		t.pending_stmts.clear()
	}
	if tuple_lhs := t.get_tuple_lhs(stmt) {
		if tuple_lhs.len >= 2 {
			t.temp_counter++
			tuple_tmp_name := '_tuple_tmp_${t.temp_counter}'
			tuple_tmp_ident := ast.Ident{
				name: tuple_tmp_name
			}
			for ps in prefix_stmts {
				ids << out.emit_stmt(ps)
			}
			ids << out.emit_stmt(ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(tuple_tmp_ident)]
				rhs: [transformed_rhs]
				pos: stmt.pos
			}))
			for i in 0 .. tuple_lhs.len {
				ids << out.emit_stmt(ast.Stmt(ast.AssignStmt{
					op:  stmt.op
					lhs: [tuple_lhs[i]]
					rhs: [
						ast.Expr(ast.SelectorExpr{
							lhs: tuple_tmp_ident
							rhs: ast.Ident{
								name: 'arg${i}'
							}
						}),
					]
					pos: stmt.pos
				}))
			}
			return true
		}
	}
	final_assign := ast.AssignStmt{
		op:  stmt.op
		lhs: stmt.lhs
		rhs: [transformed_rhs]
		pos: stmt.pos
	}
	for ps in prefix_stmts {
		ids << out.emit_stmt(ps)
	}
	ids << out.emit_stmt(t.transform_stmt(final_assign))
	return true
}

// try_expand_or_expr_return_to_flat is the flat-direct mirror of
// `try_expand_or_expr_return` (transformer.v:6440). Thin-dispatcher port
// with **last-stmt-interleave** drain shape: legacy `try_expand_or_expr_return`
// returns `[prefix_stmts..., final_return_stmt]` and may have left
// `pending_stmts` populated from the final `transform_return_stmt(...)` call.
// Those pending stmts must be emitted BETWEEN the prefix stmts and the
// terminating return — emitting them after would put non-return stmts after
// the return; emitting them before would precede the prefix `_or_tN := ...`
// they depend on.
//
// Site shape preserved exactly: when `pending_stmts.len > 0`, drain happens
// right before emitting the last expanded stmt; otherwise plain in-order emit.
// Output stmts are already transformed (legacy helper internally calls
// `extract_or_expr` + `transform_return_stmt`), so `out.emit_stmt(...)` is
// used (not the appender). Returns `bool` to signal whether the expansion fired.
//
// Memory win: outer `[]ast.Stmt` still allocated by legacy helper; site-level
// consolidation removes the unrolled if/else + for blocks (now one bool
// branch). Full direct-emit (eliminating the legacy outer alloc and the
// inner `prefix_stmts` accumulator) requires porting `extract_or_expr` +
// `transform_return_stmt` to flat-direct — deferred.
//
// Bit-equal. Pinned by `fixture_or_return` across all 5 invariants.
// track_interface_assign_to_flat lifts the inline native-backend
// interface-concrete-type tracker (previously ~9 lines at flat_write.v:1800-1808)
// into a named side-effect helper. The original arm has no `continue` — it
// mutates `t.interface_concrete_types` based on the rhs shape for ALL 1:1
// lhs/rhs assigns under the native backend, then falls through to the next
// arm. Guard chain: native-be + 1:1 lhs/rhs + non-empty lhs name. Behavior:
//   - rhs assigns an interface-castable concrete type → record it.
//   - else if op is plain `=` or `:=` → clear any stale concrete-type
//     mapping (so a re-assign with a non-concrete rhs doesn't leave stale
//     dispatch info).
// No return value (side-effect only).
//
// Memory win: zero. Value: completes driver-body uniformity for the
// interface family — both the tracker (this helper) and the emitter
// (s132's `try_expand_interface_cast_assign_to_flat`) live in named
// helpers; the driver's AssignStmt arm body is now five lines of
// dispatchers + one tracker call.
//
// Bit-equal. Native-backend-only; production coverage via `cmd/v2/v2`
// rebuild.
pub fn (mut t Transformer) track_interface_assign_to_flat(assign_stmt ast.AssignStmt) {
	if !t.is_native_be {
		return
	}
	if assign_stmt.rhs.len != 1 || assign_stmt.lhs.len != 1 {
		return
	}
	lhs_name := t.get_var_name(assign_stmt.lhs[0])
	if lhs_name == '' {
		return
	}
	if concrete := t.get_interface_assignment_concrete_type(assign_stmt.rhs[0]) {
		t.interface_concrete_types[lhs_name] = concrete
	} else if assign_stmt.op in [.assign, .decl_assign] {
		t.interface_concrete_types.delete(lhs_name)
	}
}

// try_expand_interface_cast_assign_to_flat lifts the inline native-backend
// interface-cast assign rewrite (previously ~15 lines at
// flat_write.v:1810-1825) into a named helper. The site recognizes
// `iface = ConcreteType(value)` on the native backend (`is_native_be`),
// records the concrete type in `t.interface_concrete_types[lhs_name]` for
// later method dispatch, and emits a stripped synth AssignStmt where the
// rhs is the inner expression (cast peeled off — native backends don't
// box). Guard chain: native-be + 1:1 lhs/rhs + lhs ident + rhs interface
// cast. Returns `bool`.
//
// Memory win: zero — same synth AssignStmt emitted. Value: driver-body
// uniformity continuation — another multi-line construction block lifted
// out of the driver loop.
//
// Bit-equal. Exercised through the `cmd/v2/v2` native-backend rebuild
// (interface assignments appear when implementing interface types like
// `Walker`/`Drawer` in the v2 self-host); no harness fixture targets it
// directly since the rewrite is native-backend-only.
pub fn (mut t Transformer) try_expand_interface_cast_assign_to_flat(assign_stmt ast.AssignStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if !t.is_native_be {
		return false
	}
	if assign_stmt.rhs.len != 1 || assign_stmt.lhs.len != 1 {
		return false
	}
	if assign_stmt.lhs[0] !is ast.Ident {
		return false
	}
	if !t.is_interface_cast(assign_stmt.rhs[0]) {
		return false
	}
	inner := t.get_interface_cast_inner_expr(assign_stmt.rhs[0]) or { assign_stmt.rhs[0] }
	lhs_name := (assign_stmt.lhs[0] as ast.Ident).name
	if concrete := t.get_expr_type_name(inner) {
		t.interface_concrete_types[lhs_name] = concrete
	}
	t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
		op:  assign_stmt.op
		lhs: assign_stmt.lhs
		rhs: [inner]
		pos: assign_stmt.pos
	}), mut out)
	return true
}

// try_expand_comptime_if_stmt_to_flat lifts the inline comptime-$if-stmt
// rewrite (previously ~20 lines at flat_write.v:1846-1865) into a named
// helper. Sibling of s130's `try_expand_comptime_if_assign_to_flat` for
// the ExprStmt+ComptimeExpr+IfExpr case (not AssignStmt). Same comptime-
// cond probe but different handling:
//   - evaluable cond → recurse the selected stmts through
//     `transform_stmts_to_flat_direct` and splice the resulting FlatNodeIds
//     directly into `ids` (no synth AssignStmt wrapping).
//   - non-evaluable cond → emit a synth ExprStmt wrapping a fresh
//     ComptimeExpr around `transform_comptime_if_bodies(if_expr)` so the
//     backend sees both branches with already-transformed bodies.
//
// Returns `bool` to signal whether the rewrite fired. Returns true on both
// branches.
//
// Memory win: zero — same emission shape. Value: completes driver-body
// uniformity for the comptime $if family.
//
// Bit-equal. Exercised through the `cmd/v2/v2` rebuild.
pub fn (mut t Transformer) try_expand_comptime_if_stmt_to_flat(stmt ast.ExprStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if stmt.expr !is ast.ComptimeExpr {
		return false
	}
	comptime_expr := stmt.expr as ast.ComptimeExpr
	if comptime_expr.expr !is ast.IfExpr {
		return false
	}
	comptime_if := comptime_expr.expr as ast.IfExpr
	if t.can_eval_comptime_cond(comptime_if.cond) {
		selected := t.resolve_comptime_if_stmts(comptime_if)
		nested_ids := t.transform_stmts_to_flat_direct(selected, mut out)
		for id in nested_ids {
			ids << id
		}
		return true
	}
	transformed_comptime := t.transform_comptime_if_bodies(comptime_if)
	ids << out.emit_stmt(ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.ComptimeExpr{
			expr: ast.Expr(transformed_comptime)
		})
	}))
	return true
}

// try_expand_comptime_if_assign_to_flat lifts the inline comptime-$if-assign
// rewrite (previously ~40 lines at flat_write.v:1795-1832) into a named
// helper. Like s129's sincos extraction, this site had no legacy
// `try_expand_*` helper — the entire rewrite logic was inline. The helper
// absorbs the AssignStmt+single-rhs+ComptimeExpr+IfExpr guard chain, the
// comptime-cond evaluability probe (`can_eval_comptime_cond`), the
// `resolve_comptime_if_stmts` lookup, and the last-stmt dispatch (ExprStmt
// vs AssignStmt vs neither). For the false-path (cond not evaluable at
// compile time) the original assign_stmt is emitted as-is. For the true-
// path: all-but-last selected stmts are routed through the appender, and
// the last stmt's rhs (ExprStmt.expr transformed via `transform_expr`, or
// AssignStmt.rhs untransformed) is grafted onto a fresh AssignStmt mirroring
// the original lhs/op/pos.
//
// Returns `bool` to signal whether the rewrite fired. Returns true even
// on the unevaluable-cond branch (since that branch consumes the stmt via
// emit-as-is + continue at the site).
//
// Memory win: zero — same number of synth AssignStmts emitted as before.
// The value is **driver-body uniformity** (continuation of s129's
// milestone): the largest remaining inline construction block in
// `transform_stmts_to_flat_direct` collapses into one bool branch,
// making the driver loop fully uniform.
//
// Bit-equal. Exercised through the `cmd/v2/v2` rebuild (comptime $if-assign
// is common in builtin/os/etc. for backend-conditional consts and inits);
// no harness fixture targets it directly since the rewrite logic depends
// on backend gates not exercised by the fixture set.
pub fn (mut t Transformer) try_expand_comptime_if_assign_to_flat(assign_stmt ast.AssignStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if assign_stmt.rhs.len != 1 {
		return false
	}
	if assign_stmt.rhs[0] !is ast.ComptimeExpr {
		return false
	}
	comptime_expr := assign_stmt.rhs[0] as ast.ComptimeExpr
	if comptime_expr.expr !is ast.IfExpr {
		return false
	}
	comptime_if := comptime_expr.expr as ast.IfExpr
	if !t.can_eval_comptime_cond(comptime_if.cond) {
		t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(assign_stmt), mut out)
		return true
	}
	selected := t.resolve_comptime_if_stmts(comptime_if)
	if selected.len == 0 {
		return false
	}
	for i := 0; i < selected.len - 1; i++ {
		t.append_transformed_stmt_to_flat(mut ids, selected[i], mut out)
	}
	last_stmt := selected[selected.len - 1]
	if last_stmt is ast.ExprStmt {
		last_expr_stmt := last_stmt as ast.ExprStmt
		t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
			op:  assign_stmt.op
			lhs: assign_stmt.lhs
			rhs: [t.transform_expr(last_expr_stmt.expr)]
			pos: assign_stmt.pos
		}), mut out)
		return true
	}
	if last_stmt is ast.AssignStmt {
		last_assign_stmt := last_stmt as ast.AssignStmt
		t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
			op:  assign_stmt.op
			lhs: assign_stmt.lhs
			rhs: last_assign_stmt.rhs
			pos: assign_stmt.pos
		}), mut out)
		return true
	}
	return false
}

// try_expand_sincos_assign_to_flat lifts the inline sincos rewrite
// (previously ~40 lines at flat_write.v:1860) into a named helper. Unlike
// the s121-s128 thin dispatchers, this site had no legacy `try_expand_*`
// helper — the entire rewrite logic was inline. The new helper absorbs the
// `is_native_be` + `lhs.len >= 2` + `rhs.len == 1` guards and the
// `try_extract_sincos_arg` probe (returns false for any failure), then
// emits the two synth AssignStmts (sin + cos) via the s114 appender,
// honoring the `_` blank-binding contract on either lhs slot.
//
// Returns `bool` to signal whether the rewrite fired. Output stmts route
// through the appender because the synth ast.AssignStmts contain
// untransformed sub-exprs (the original lhs idents + freshly-built
// CallExpr around `sincos_arg`) that need a transform pass.
//
// Memory win: zero — same number of synth AssignStmts emitted as before.
// The value is **driver-body uniformity**: every `if ... { ... continue }`
// arm in `transform_stmts_to_flat_direct` is now either a `_to_flat` helper
// call or a single-emit leaf, no more multi-line inline construction blocks
// embedded in the driver. Improves diffability of the driver and prevents
// future per-helper ports from accidentally diverging from this site's
// rewrite logic.
//
// Bit-equal. Exercised through the `cmd/v2/v2` rebuild (native backend
// emits sincos-fused trig for math fixtures); no harness fixture targets
// it directly since the rewrite only fires on the native-backend gate.
pub fn (mut t Transformer) try_expand_sincos_assign_to_flat(assign_stmt ast.AssignStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if !t.is_native_be {
		return false
	}
	if assign_stmt.lhs.len < 2 || assign_stmt.rhs.len != 1 {
		return false
	}
	sincos_arg := t.try_extract_sincos_arg(assign_stmt.rhs[0]) or { return false }
	lhs0 := assign_stmt.lhs[0]
	lhs1 := assign_stmt.lhs[1]
	is_blank0 := lhs0 is ast.Ident && (lhs0 as ast.Ident).name == '_'
	is_blank1 := lhs1 is ast.Ident && (lhs1 as ast.Ident).name == '_'
	if !is_blank0 {
		t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
			op:  assign_stmt.op
			lhs: [lhs0]
			rhs: [
				ast.Expr(ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'sin'
					})
					args: [sincos_arg]
					pos:  assign_stmt.pos
				}),
			]
			pos: assign_stmt.pos
		}), mut out)
	}
	if !is_blank1 {
		t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
			op:  assign_stmt.op
			lhs: [lhs1]
			rhs: [
				ast.Expr(ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'cos'
					})
					args: [sincos_arg]
					pos:  assign_stmt.pos
				}),
			]
			pos: assign_stmt.pos
		}), mut out)
	}
	return true
}

// try_expand_for_in_map_to_flat is the flat-direct mirror of
// `try_expand_for_in_map` (for.v:183). Body-mirror port (s143, follows the
// s142 if-guard body-mirror; completes the punch list of "heavy remaining
// helpers" called out in s141). Replicates the legacy helper's body inline
// and emits each top-level produced stmt directly via the appender (the
// synth stmts inside still need a transform pass to handle smartcast/scope
// and pick up nested expansions — exactly like the s127 thin-dispatcher
// version, but without the outer slice).
//
// Sequence:
//   1. Skip on eval backend; bail if init is not `ForInStmt`; bail if iter
//      type is not a map (after pointer/alias unwrap).
//   2. Resolve key/value var names (Ident or ModifierExpr.expr).
//   3. lvalue vs rvalue iter expr: lvalue uses iter directly; rvalue emits
//      a `map_tmp := <iter>` decl (also registers temp var with iter_type).
//   4. Emit `mut _map_len := map_ref.key_values.len` decl.
//   5. Build the body `loop_body []ast.Stmt` (still legacy-struct since
//      it's the ForStmt.stmts field): _map_delta decl, _map_len update,
//      negative-delta guard, has_index guard, optional key binding + string
//      clone, optional value binding, then the original body stmts.
//   6. Emit the synth `for _map_idx := 0; _map_idx < _map_len; _map_idx++
//      { <loop_body> }`.
//
// Memory win: outer 1-3 stmt accumulator slice eliminated per fired site.
// The inner `loop_body` slice (~8-12 stmts) is the ForStmt's `stmts` field
// and must stay legacy-shape until ForStmt itself is ported to flat-direct
// (multi-session future work). Side effects (`register_temp_var`,
// `scope.insert`) fire in the same order as legacy.
//
// Bit-equal. The full for-in-map body is exercised through
// `fixture_for_in_map` (registered in s128) across all 5 invariants.
pub fn (mut t Transformer) try_expand_for_in_map_to_flat(stmt ast.ForStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if t.is_eval_backend() {
		return false
	}
	if stmt.init !is ast.ForInStmt {
		return false
	}
	for_in := stmt.init as ast.ForInStmt

	iter_type := t.get_expr_type(for_in.expr) or { return false }
	map_type := t.unwrap_map_type(iter_type) or { return false }

	mut key_name := ''
	mut key_is_blank := false
	if for_in.key !is ast.EmptyExpr {
		if for_in.key is ast.Ident {
			key_name = for_in.key.name
			key_is_blank = key_name == '_'
		} else if for_in.key is ast.ModifierExpr {
			if for_in.key.expr is ast.Ident {
				key_name = for_in.key.expr.name
				key_is_blank = key_name == '_'
			}
		}
	}

	mut value_name := ''
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
		}
	}

	key_type_name := t.type_to_c_decl_name(map_type.key_type)
	value_type_name := t.type_to_c_decl_name(map_type.value_type)

	idx_name := t.gen_map_iter_temp_name('idx')
	len_name := t.gen_map_iter_temp_name('len')
	delta_name := t.gen_map_iter_temp_name('delta')

	idx_ident := ast.Ident{
		name: idx_name
	}
	len_ident := ast.Ident{
		name: len_name
	}
	delta_ident := ast.Ident{
		name: delta_name
	}

	smartcast_iter_expr := t.expr_to_string(for_in.expr)
	is_smartcast_iter := smartcast_iter_expr != ''
		&& t.find_smartcast_for_expr(smartcast_iter_expr) != none
	is_lvalue := !is_smartcast_iter && (for_in.expr is ast.Ident || for_in.expr is ast.SelectorExpr)
	mut map_ref := ast.Expr(ast.Ident{})
	if is_lvalue {
		map_ref = for_in.expr
	} else {
		map_tmp_name := t.gen_map_iter_temp_name('map')
		map_tmp_ident := ast.Ident{
			name: map_tmp_name
		}
		map_source := if is_smartcast_iter {
			t.smartcast_map_iter_value_expr(for_in.expr, map_type)
		} else {
			for_in.expr
		}
		t.append_transformed_stmt_to_flat(mut ids, ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(map_tmp_ident)]
			rhs: [map_source]
		}, mut out)
		t.register_temp_var(map_tmp_name, iter_type)
		map_ref = ast.Expr(map_tmp_ident)
	}

	key_values_expr := t.synth_selector(map_ref, 'key_values', types.Type(types.Struct{
		name: 'DenseArray'
	}))

	key_values_len_expr := t.synth_selector(ast.Expr(key_values_expr), 'len',
		types.Type(types.int_))

	t.append_transformed_stmt_to_flat(mut ids, ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.ModifierExpr{
			kind: .key_mut
			expr: len_ident
		})]
		rhs: [ast.Expr(key_values_len_expr)]
	}, mut out)

	mut loop_body := []ast.Stmt{}

	loop_body << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(delta_ident)]
		rhs: [t.make_infix_expr(.minus, key_values_len_expr, ast.Expr(len_ident))]
	}

	loop_body << ast.AssignStmt{
		op:  .assign
		lhs: [ast.Expr(len_ident)]
		rhs: [key_values_len_expr]
	}

	delta_lt_zero := t.make_infix_expr(.lt, ast.Expr(delta_ident), t.make_number_expr('0'))
	loop_body << ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  delta_lt_zero
			stmts: [
				ast.Stmt(ast.AssignStmt{
					op:  .assign
					lhs: [ast.Expr(idx_ident)]
					rhs: [
						ast.Expr(ast.PrefixExpr{
							op:   .minus
							expr: ast.BasicLiteral{
								kind:  .number
								value: '1'
							}
						}),
					]
				}),
				ast.Stmt(ast.FlowControlStmt{
					op: .key_continue
				}),
			]
		}
	}

	has_index_call := ast.CallExpr{
		lhs:  ast.Ident{
			name: 'DenseArray__has_index'
		}
		args: [
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: key_values_expr
			}),
			ast.Expr(idx_ident),
		]
	}
	loop_body << ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  ast.PrefixExpr{
				op:   .not
				expr: has_index_call
			}
			stmts: [ast.Stmt(ast.FlowControlStmt{
				op: .key_continue
			})]
		}
	}

	if !key_is_blank && key_name != '' {
		key_call := ast.CallExpr{
			lhs:  ast.Ident{
				name: 'DenseArray__key'
			}
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: key_values_expr
				}),
				ast.Expr(idx_ident),
			]
		}
		key_cast := ast.CastExpr{
			typ:  ast.Ident{
				name: '${key_type_name}*'
			}
			expr: key_call
		}
		key_deref := ast.PrefixExpr{
			op:   .mul
			expr: key_cast
		}
		loop_body << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: key_name
			})]
			rhs: [ast.Expr(key_deref)]
		}
		if map_type.key_type is types.String {
			loop_body << ast.AssignStmt{
				op:  .assign
				lhs: [ast.Expr(ast.Ident{
					name: key_name
				})]
				rhs: [
					ast.Expr(ast.CallExpr{
						lhs:  ast.Ident{
							name: 'string__clone'
						}
						args: [ast.Expr(ast.Ident{
							name: key_name
						})]
					}),
				]
			}
		}
		t.register_for_in_var_type(key_name, map_type.key_type)
	}

	if value_name != '' && value_name != '_' {
		value_call := ast.CallExpr{
			lhs:  ast.Ident{
				name: 'DenseArray__value'
			}
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: key_values_expr
				}),
				ast.Expr(idx_ident),
			]
		}
		value_cast := ast.CastExpr{
			typ:  ast.Ident{
				name: '${value_type_name}*'
			}
			expr: value_call
		}
		value_deref := ast.PrefixExpr{
			op:   .mul
			expr: value_cast
		}
		loop_body << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: value_name
			})]
			rhs: [ast.Expr(value_deref)]
		}
		t.register_for_in_var_type(value_name, map_type.value_type)
	}

	for body_stmt in stmt.stmts {
		loop_body << body_stmt
	}

	loop_cond := t.make_infix_expr(.lt, ast.Expr(idx_ident), ast.Expr(len_ident))
	next_idx := t.make_infix_expr(.plus, ast.Expr(idx_ident), t.make_number_expr('1'))
	for_stmt := ast.ForStmt{
		init:  ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(idx_ident)]
			rhs: [ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})]
		}
		cond:  loop_cond
		post:  ast.AssignStmt{
			op:  .assign
			lhs: [ast.Expr(idx_ident)]
			rhs: [next_idx]
		}
		stmts: loop_body
	}
	t.append_transformed_stmt_to_flat(mut ids, for_stmt, mut out)

	return true
}

// try_emit_flag_enum_set_clear_to_flat is the flat-direct mirror of the
// flag-enum set/clear single-emit arm in `transform_stmts_to_flat_direct`
// (flat_write.v:1836). Thin-dispatcher port — delegates to the legacy
// `try_transform_flag_enum_set_clear` Optional<Stmt>-returning rewriter,
// then leaf-encodes the result. Returns `bool` to signal whether the
// rewrite fired (driver `continue`s on true).
//
// Output stmt is already a transformed `ast.AssignStmt` (a `|=` / `&= ~`
// flag-enum lowering), so `out.emit_stmt(...)` is used (no re-transform).
//
// Memory win: zero. Value: driver-body uniformity — every arm body in
// `transform_stmts_to_flat_direct` is now either a one-line bool-dispatcher
// `if t.X_to_flat(...) { continue }` or a one-line void-helper call. The
// 3-line "lift result + emit + continue" inline form is gone from the
// driver.
//
// Bit-equal. Production coverage via `cmd/v2/v2` rebuild (no fixture; the
// flag-enum set/clear rewrite path triggers on `flag.set(.x)` / `flag.clear(.x)`
// expressions which production source uses but synthetic fixtures don't).
pub fn (mut t Transformer) try_emit_flag_enum_set_clear_to_flat(stmt ast.ExprStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	flag_stmt := t.try_transform_flag_enum_set_clear(stmt) or { return false }
	ids << out.emit_stmt(flag_stmt)
	return true
}

// try_emit_map_index_push_to_flat is the flat-direct mirror of the
// map-index-push single-emit arm in `transform_stmts_to_flat_direct`
// (flat_write.v:1856). Thin-dispatcher port (s134 wrap pattern). Delegates
// to the legacy `try_transform_map_index_push` Optional<Stmt>-returning
// rewriter (lowers `m[k] << v` to a `map__set` / push-noscan call), then
// leaf-encodes the result. Returns `bool` to signal whether the rewrite
// fired.
//
// Drops the redundant `ast.Stmt(...)` cast at the legacy site — the
// helper's return type is already `?ast.Stmt`, so the cast was a no-op
// (likely an artifact of an earlier signature change). Bit-equal.
//
// Memory win: zero. Value: continued driver-body uniformity (s134
// pattern, second of four single-emit arm wraps).
pub fn (mut t Transformer) try_emit_map_index_push_to_flat(stmt ast.ExprStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	expanded_map_push := t.try_transform_map_index_push(stmt) or { return false }
	ids << out.emit_stmt(expanded_map_push)
	return true
}

// try_emit_map_index_postfix_to_flat is the flat-direct mirror of the
// map-index-postfix single-emit arm in `transform_stmts_to_flat_direct`
// (flat_write.v:1860). Thin-dispatcher port (s134 wrap pattern, third of
// four). Delegates to the legacy `try_transform_map_index_postfix`
// Optional<Stmt>-returning rewriter (lowers `m[k]++` / `m[k]--` to a
// fetch-mutate-set pattern that handles the unique `map[idx]` semantics —
// indexing returns a value, not an lvalue, so postfix needs explicit get
// + arith + set). Returns `bool`.
//
// Memory win: zero. Value: continued driver-body uniformity.
pub fn (mut t Transformer) try_emit_map_index_postfix_to_flat(stmt ast.ExprStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	expanded_map_postfix := t.try_transform_map_index_postfix(stmt) or { return false }
	ids << out.emit_stmt(expanded_map_postfix)
	return true
}

// try_emit_selector_postfix_to_flat is the flat-direct mirror of the
// native-backend selector-postfix single-emit arm in
// `transform_stmts_to_flat_direct` (flat_write.v:1862). Thin-dispatcher
// port (s134 wrap pattern, fourth/last of the series). Lowers
// `obj.field++` / `obj.field--` to a synthesised assign (`obj.field =
// obj.field + 1`) for native backends, which lack the in-place postfix
// codegen path. Absorbs the `if t.is_native_be {}` guard inside the
// helper — the driver site shrinks from 6 lines (incl. guard block) to
// 3 lines.
//
// Returns `bool`. Returns false when native-be is off (no rewrite needed
// — C backend handles postfix on selectors natively) or when the legacy
// helper rejects the shape.
//
// Memory win: zero. Value: completes the s134-s137 single-emit arm wrap
// series — every arm body in `transform_stmts_to_flat_direct` is now
// either a one-line bool-dispatcher (`if t.X_to_flat(...) { continue }`)
// or a one-line void-helper call. No inline `if t.X {}` guard blocks
// remain at the site level; backend-gating lives inside the dispatchers.
pub fn (mut t Transformer) try_emit_selector_postfix_to_flat(stmt ast.ExprStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if !t.is_native_be {
		return false
	}
	postfix_assign := t.try_transform_selector_postfix(stmt) or { return false }
	ids << out.emit_stmt(postfix_assign)
	return true
}

// try_expand_if_guard_stmt_to_flat is the flat-direct mirror of
// `try_expand_if_guard_stmt` (if.v:199). Body-mirror port (s142, follows
// the s138-s141 body-mirror sweep). Replicates the legacy helper's ~280-
// line body inline and emits each produced stmt directly into `ids` via
// `out.emit_stmt(...)` (outputs are already transformed). Four output
// branches preserved verbatim:
//   1. Result/Option guard rhs: emit guard_prefix_stmts → drain
//      pending_stmts (from `transform_expr(rhs)`) → tmp decl assign →
//      terminating `ExprStmt(modified_if)`.
//   2. Map IndexExpr guard rhs: emit `prefix_stmts` (built up via
//      `addr_of_with_prefix_temp` side effects) → temp_assign → terminating
//      `ExprStmt(modified_if)`.
//   3. Array IndexExpr guard rhs: single terminating `ExprStmt(modified_if)`
//      with bounds-check cond.
//   4. Default: bounds/cond check; for map-returns-array, emit temp_assign
//      then terminating `ExprStmt(modified_if)`; otherwise single
//      `ExprStmt(modified_if)`.
//
// Side effects (state save/restore for `skip_if_value_lowering`, the
// `register_synth_type` / `register_temp_var` calls, and the
// `register_if_guard_lhs_payload_type` calls in branch 1) preserved
// verbatim — the legacy `defer`-based restore of `skip_if_value_lowering`
// is replicated inline here, scoped to the helper's lifetime.
//
// Memory win: outer `[]ast.Stmt` accumulator (`stmts` in branch 1,
// `prefix_stmts` in branch 2, the trivial 1-element wrap in branch 3,
// and the 1-or-2-element wrap in branch 4) eliminated per fired site.
// Inner accumulators (`if_stmts`, `new_stmts`) that feed
// `transform_stmts(...)` to produce the IfExpr's inner `stmts` field
// remain — those are part of the IfExpr legacy struct shape and require
// porting `IfExpr` itself to flat-direct to eliminate. Real meaningful
// savings since the s138-s141 body-mirror sweep continued.
//
// Bit-equal. Pinned by `fixture_if_guard` (Option-form). Result, map,
// and array branches exercised by `cmd/v2/v2` self-host paths.
pub fn (mut t Transformer) try_expand_if_guard_stmt_to_flat(stmt ast.ExprStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if stmt.expr !is ast.IfExpr {
		return false
	}
	if_expr := stmt.expr as ast.IfExpr
	if if_expr.cond !is ast.IfGuardExpr {
		return false
	}
	guard := if_expr.cond as ast.IfGuardExpr

	// The entire if-guard expansion is at statement position. Suppress value-
	// position IfExpr hoisting (`_if_t<N> := if ...`) for nested else-if chains
	// so they don't generate mistyped temps for what is really a statement.
	saved_skip := t.skip_if_value_lowering
	t.skip_if_value_lowering = true
	defer {
		t.skip_if_value_lowering = saved_skip
	}

	if guard.stmt.rhs.len == 0 {
		return false
	}

	mut rhs := guard.stmt.rhs[0]
	mut guard_prefix_stmts := []ast.Stmt{}
	if t.expr_has_or_expr(rhs) {
		rhs = t.extract_or_expr(rhs, mut guard_prefix_stmts)
	}
	synth_pos := t.next_synth_pos()

	mut is_result := t.expr_returns_result(rhs)
	mut is_option := t.expr_returns_option(rhs)

	if !is_result && !is_option {
		fn_name := t.get_call_fn_name(rhs)
		is_result = fn_name != '' && t.fn_returns_result(fn_name)
		is_option = fn_name != '' && t.fn_returns_option(fn_name)
	}
	if !is_result && !is_option {
		if ret_type := t.fn_pointer_call_return_type(rhs) {
			is_result = ret_type is types.ResultType || ret_type.name().starts_with('!')
			is_option = ret_type is types.OptionType || ret_type.name().starts_with('?')
		}
	}

	if is_result || is_option {
		// Result/Option if-guard.
		temp_name := t.gen_temp_name()
		temp_pos := synth_pos
		if_pos := t.next_synth_pos()
		temp_ident := ast.Ident{
			name: temp_name
			pos:  temp_pos
		}

		mut data_type := types.Type(types.voidptr_)
		for prefix_stmt in guard_prefix_stmts {
			ids << out.emit_stmt(prefix_stmt)
		}

		if wrapper_type := t.expr_wrapper_type_for_or(rhs) {
			t.register_temp_var(temp_name, wrapper_type)
			t.register_synth_type(temp_pos, wrapper_type)
			if wrapper_type is types.ResultType {
				data_type = wrapper_type.base_type
				is_result = true
				is_option = false
			} else if wrapper_type is types.OptionType {
				data_type = wrapper_type.base_type
				is_result = false
				is_option = true
			}
		} else if wrapper_type := t.get_expr_type(rhs) {
			t.register_temp_var(temp_name, wrapper_type)
			t.register_synth_type(temp_pos, wrapper_type)
			if wrapper_type is types.ResultType {
				data_type = wrapper_type.base_type
			} else if wrapper_type is types.OptionType {
				data_type = wrapper_type.base_type
			}
		} else if wrapper_type := t.fn_pointer_call_return_type(rhs) {
			t.register_temp_var(temp_name, wrapper_type)
			t.register_synth_type(temp_pos, wrapper_type)
			if wrapper_type is types.ResultType {
				data_type = wrapper_type.base_type
			} else if wrapper_type is types.OptionType {
				data_type = wrapper_type.base_type
			}
		}

		transformed_rhs := t.transform_expr(rhs)
		if t.pending_stmts.len > 0 {
			for ps in t.pending_stmts {
				ids << out.emit_stmt(ps)
			}
			t.pending_stmts.clear()
		}
		ids << out.emit_stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(temp_ident)]
			rhs: [transformed_rhs]
			pos: temp_pos
		})

		success_cond := if is_result {
			ast.Expr(ast.PrefixExpr{
				op:   .not
				expr: t.synth_selector(temp_ident, 'is_error', types.Type(types.bool_))
			})
		} else {
			t.make_infix_expr(.eq, t.synth_selector(temp_ident, 'state', types.Type(types.int_)),
				t.make_number_expr('0'))
		}

		mut if_stmts := []ast.Stmt{}
		data_access := t.synth_selector(temp_ident, 'data', data_type)
		t.register_if_guard_lhs_payload_type(guard.stmt.lhs, data_type)
		if_stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: guard.stmt.lhs
			rhs: [data_access]
			pos: guard.stmt.pos
		}
		for s in if_expr.stmts {
			if_stmts << s
		}

		transformed_if_stmts := t.transform_stmts(if_stmts)
		modified_if := ast.IfExpr{
			cond:      success_cond
			stmts:     transformed_if_stmts
			else_expr: t.if_guard_else_expr_with_err(if_expr.else_expr, temp_ident)
			pos:       if_pos
		}
		if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
			t.register_synth_type(if_pos, orig_type)
		}
		ids << out.emit_stmt(ast.ExprStmt{
			expr: modified_if
		})

		return true
	}

	if rhs is ast.IndexExpr {
		if map_expr_typ := t.get_expr_type(rhs.lhs) {
			if map_type := t.unwrap_map_type(map_expr_typ) {
				// Map lookup: use map__get_check pattern.
				temp_pos := synth_pos
				if_pos := t.next_synth_pos()
				temp_name := t.gen_temp_name()
				temp_ident := ast.Ident{
					name: temp_name
					pos:  temp_pos
				}

				pointer_type := types.Type(types.Pointer{
					base_type: map_type.value_type
				})
				t.register_temp_var(temp_name, pointer_type)
				t.register_synth_type(temp_pos, pointer_type)

				mut prefix_stmts := []ast.Stmt{}
				map_arg := if t.is_pointer_type(map_expr_typ) {
					t.transform_expr(rhs.lhs)
				} else {
					t.addr_of_with_prefix_temp(rhs.lhs, map_expr_typ, mut prefix_stmts)
				}
				key_arg := t.addr_of_with_prefix_temp(rhs.expr, map_type.key_type, mut prefix_stmts)

				get_check_call := ast.CallExpr{
					lhs:  ast.Ident{
						name: 'map__get_check'
					}
					args: [
						map_arg,
						t.voidptr_cast(key_arg),
					]
				}
				temp_assign := ast.AssignStmt{
					op:  .decl_assign
					lhs: [ast.Expr(temp_ident)]
					rhs: [ast.Expr(get_check_call)]
					pos: synth_pos
				}

				mut if_stmts := []ast.Stmt{}
				deref_tmp := t.typed_deref(temp_ident, map_type.value_type)
				if_stmts << ast.AssignStmt{
					op:  .decl_assign
					lhs: guard.stmt.lhs
					rhs: [deref_tmp]
					pos: guard.stmt.pos
				}
				for s in if_expr.stmts {
					if_stmts << s
				}

				null_check := t.make_infix_expr(.ne, ast.Expr(temp_ident), ast.Expr(ast.Ident{
					name: 'nil'
				}))

				modified_if := ast.IfExpr{
					cond:      null_check
					stmts:     t.transform_stmts(if_stmts)
					else_expr: t.transform_expr(if_expr.else_expr)
					pos:       if_pos
				}
				if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
					t.register_synth_type(if_pos, orig_type)
				}

				for ps in prefix_stmts {
					ids << out.emit_stmt(ps)
				}
				ids << out.emit_stmt(temp_assign)
				ids << out.emit_stmt(ast.ExprStmt{
					expr: modified_if
				})
				return true
			}
		}

		// Array lookup: bounds check.
		bounds_check := t.make_infix_expr_at(.lt, t.transform_expr(rhs.expr), t.synth_selector(t.transform_expr(rhs.lhs),
			'len', types.Type(types.int_)), rhs.pos)

		mut if_stmts := []ast.Stmt{}
		if_stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: guard.stmt.lhs
			rhs: guard.stmt.rhs
			pos: guard.stmt.pos
		}
		for s in if_expr.stmts {
			if_stmts << s
		}

		modified_if := ast.IfExpr{
			cond:      bounds_check
			stmts:     t.transform_stmts(if_stmts)
			else_expr: t.transform_expr(if_expr.else_expr)
			pos:       synth_pos
		}
		if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
			t.register_synth_type(synth_pos, orig_type)
		}

		ids << out.emit_stmt(ast.ExprStmt{
			expr: modified_if
		})
		return true
	}

	rhs_expr := t.transform_expr(rhs)

	map_returns_array := t.is_map_lookup_returning_array(rhs)

	guard_assign := ast.AssignStmt{
		op:  .decl_assign
		lhs: guard.stmt.lhs
		rhs: guard.stmt.rhs
		pos: guard.stmt.pos
	}
	mut new_stmts := []ast.Stmt{cap: if_expr.stmts.len + 1}
	new_stmts << guard_assign
	for s in if_expr.stmts {
		new_stmts << s
	}

	mut cond_expr := ast.Expr(rhs_expr)
	if map_returns_array {
		mut guard_var_name := ''
		for lhs_expr in guard.stmt.lhs {
			if lhs_expr is ast.Ident {
				guard_var_name = lhs_expr.name
				break
			}
		}
		is_blank := guard_var_name == '_'
		if is_blank {
			guard_var_name = t.gen_temp_name()
		}
		if guard_var_name != '' {
			temp_lhs := if is_blank {
				[
					ast.Expr(ast.Ident{
						name: guard_var_name
						pos:  synth_pos
					}),
				]
			} else {
				guard.stmt.lhs
			}
			temp_assign := ast.AssignStmt{
				op:  .decl_assign
				lhs: temp_lhs
				rhs: guard.stmt.rhs
				pos: guard.stmt.pos
			}
			new_stmts = []ast.Stmt{cap: if_expr.stmts.len}
			for s in if_expr.stmts {
				new_stmts << s
			}
			cond_expr = t.synth_selector(ast.Ident{
				name: guard_var_name
				pos:  synth_pos
			}, 'data', types.Type(types.voidptr_))
			modified_if := ast.IfExpr{
				cond:      cond_expr
				stmts:     t.transform_stmts(new_stmts)
				else_expr: t.transform_expr(if_expr.else_expr)
				pos:       synth_pos
			}
			if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
				t.register_synth_type(synth_pos, orig_type)
			}
			ids << out.emit_stmt(temp_assign)
			ids << out.emit_stmt(ast.ExprStmt{
				expr: modified_if
			})
			return true
		}
	}

	modified_if := ast.IfExpr{
		cond:      cond_expr
		stmts:     t.transform_stmts(new_stmts)
		else_expr: t.transform_expr(if_expr.else_expr)
		pos:       synth_pos
	}
	if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
		t.register_synth_type(synth_pos, orig_type)
	}

	ids << out.emit_stmt(ast.ExprStmt{
		expr: modified_if
	})
	return true
}

// try_expand_or_expr_return_to_flat is the flat-direct mirror of
// `try_expand_or_expr_return` (transformer.v:6440). Body-mirror port
// (s138 pattern, third of the body-mirror sweep — completes the or-block
// trio). Replicates the legacy helper's body inline: scan return exprs
// for any OrExpr, extract via `extract_or_expr` (which pushes into
// `prefix_stmts` as a side effect), and emit `prefix_stmts` followed by
// the transformed final return stmt.
//
// Shape:
//   - Scan `stmt.exprs` for any `expr_has_or_expr`; return false if none.
//   - For each expr: if it's an OrExpr returning `none` AND the function
//     returns option, rewrite to `return none`-bodied OrExpr (matches
//     legacy `return_expr` ternary at transformer.v:6456).
//   - Call `extract_or_expr` per expr to build `prefix_stmts` and
//     `new_exprs`. Bail if prefix_stmts is empty.
//   - Build final `ReturnStmt{exprs: new_exprs}` and pass through
//     `transform_return_stmt` (which may populate `t.pending_stmts`
//     with `_if_tN` temps or similar).
//   - Emit `prefix_stmts` directly, then drain `t.pending_stmts`
//     (last-stmt-interleave: pending stmts go BETWEEN prefix and the
//     terminating return — pending stmts depend on the prefix `_or_tN`
//     decls, and the return must remain last in the block). Finally
//     emit the transformed return stmt.
//
// Memory win: outer `[]ast.Stmt` return slice from legacy helper
// eliminated; `new_exprs` slice (sized to `stmt.exprs.len`) still
// allocated since `transform_return_stmt` needs the full list.
// Completes the or-block body-mirror trio (s138-s140).
//
// Bit-equal. Pinned by `fixture_or_return` across all 5 invariants.
pub fn (mut t Transformer) try_expand_or_expr_return_to_flat(stmt ast.ReturnStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	mut has_or_expr := false
	for expr in stmt.exprs {
		if t.expr_has_or_expr(expr) {
			has_or_expr = true
			break
		}
	}
	if !has_or_expr {
		return false
	}
	mut prefix_stmts := []ast.Stmt{}
	mut new_exprs := []ast.Expr{cap: stmt.exprs.len}
	for expr in stmt.exprs {
		return_expr := if t.cur_fn_returns_option && expr is ast.OrExpr
			&& or_stmts_are_none(expr.stmts) {
			ast.Expr(ast.OrExpr{
				expr:  expr.expr
				stmts: [
					ast.Stmt(ast.ReturnStmt{
						exprs: [ast.Expr(ast.Ident{
							name: 'none'
						})]
					}),
				]
				pos:   expr.pos
			})
		} else {
			expr
		}
		new_expr := t.extract_or_expr(return_expr, mut prefix_stmts)
		new_exprs << new_expr
	}
	if prefix_stmts.len == 0 {
		return false
	}
	final_return := t.transform_return_stmt(ast.ReturnStmt{
		exprs: new_exprs
	})
	for ps in prefix_stmts {
		ids << out.emit_stmt(ps)
	}
	if t.pending_stmts.len > 0 {
		for ps in t.pending_stmts {
			ids << out.emit_stmt(ps)
		}
		t.pending_stmts.clear()
	}
	ids << out.emit_stmt(final_return)
	return true
}

// try_expand_return_if_expr_to_flat is the flat-direct mirror of
// `try_expand_return_if_expr` (if.v:587). Mirrors the same guard chain
// (single-expr ReturnStmt with IfExpr-with-non-empty-else expr) and
// delegates the recursive body expansion to the legacy
// `expand_return_if_expr` helper (which always returns a 1-stmt array
// wrapping the transformed `if cond { return a } else { return b }`). The
// single returned stmt is pushed via `append_transformed_stmt_to_flat`
// directly, skipping the outer `[]ast.Stmt` allocation in the legacy
// helper. Returns `bool` to signal whether the expansion fired.
//
// Memory win: outer 1-element `[]ast.Stmt` allocation from
// `expand_return_if_expr_with_options` eliminated per call (one per matched
// `return if c { a } else { b }` site). The recursive helper still builds
// intermediate `[]ast.Stmt` for then/else branches and nested else-if chains
// — those allocations remain future work. Pinned by `fixture_return_if_expr`
// across all 5 invariants (covers plain `return if c { a } else { b }` and
// chained `return if c1 { a } else if c2 { b } else { c }`).
pub fn (mut t Transformer) try_expand_return_if_expr_to_flat(stmt ast.ReturnStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if stmt.exprs.len != 1 {
		return false
	}
	if_expr := stmt.exprs[0]
	if if_expr !is ast.IfExpr {
		return false
	}
	ie := if_expr as ast.IfExpr
	if ie.else_expr is ast.EmptyExpr {
		return false
	}
	expanded := t.expand_return_if_expr(ie)
	for exp_stmt in expanded {
		t.append_transformed_stmt_to_flat(mut ids, exp_stmt, mut out)
	}
	return true
}

// try_expand_if_expr_assign_stmts_to_flat is the flat-direct mirror of
// `try_expand_if_expr_assign_stmts` (if.v:759). Mirrors the same guard chain
// (op == .assign, single lhs, single rhs that is an IfExpr with non-empty
// else branch) and delegates the recursive expansion to the legacy
// `expand_assign_if_expr` helper (which always returns a 1-stmt array
// `[ExprStmt(IfExpr{...})]`). The single returned stmt is pushed via
// `append_transformed_stmt_to_flat` directly, skipping the outer
// `[]ast.Stmt` allocation in the legacy helper. Returns `bool` to signal
// whether the expansion fired.
//
// Memory win: outer 1-element `[]ast.Stmt` allocation from
// `expand_assign_if_expr` eliminated per call. The recursive helper still
// builds intermediate `[]ast.Stmt` for then/else branches and nested else-if
// chains — those allocations remain future work. Pinned by
// `fixture_if_expr_assign` across all 5 invariants (covers plain
// `x = if c { a } else { b }` and chained `x = if c1 { a } else if c2 { b }
// else { c }`).
pub fn (mut t Transformer) try_expand_if_expr_assign_stmts_to_flat(stmt ast.AssignStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if stmt.op != .assign || stmt.lhs.len != 1 || stmt.rhs.len != 1 {
		return false
	}
	rhs := stmt.rhs[0]
	if rhs !is ast.IfExpr {
		return false
	}
	ie := rhs as ast.IfExpr
	if ie.else_expr is ast.EmptyExpr {
		return false
	}
	expanded := t.expand_assign_if_expr(stmt.lhs[0], ie)
	for exp_stmt in expanded {
		t.append_transformed_stmt_to_flat(mut ids, exp_stmt, mut out)
	}
	return true
}

// try_expand_if_guard_assign_stmts_to_flat is the flat-direct mirror of
// `try_expand_if_guard_assign_stmts` (if.v:46). Mirrors the same guard chain
// (single decl assign with single IfExpr-with-IfGuardExpr rhs, non-empty
// guard.stmt.lhs and rhs, guard rhs not Result/Option/or-wrapped) and the
// same two output branches:
//   - map branch: `x := if key in map { guard_var := map[key]; <orig stmts> } else { default }`
//     (1 stmt total)
//   - non-map branch: `r := expr; x := if r { <orig stmts> } else { default }`
//     (2 stmts total)
// Side effects preserved exactly: one `next_synth_pos()` call, one
// `register_synth_type(synth_pos, orig_type)` per branch (only when
// `get_expr_type` finds a type), same `make_infix_expr_at` call for the
// map-`in` cond. Returns `bool` to signal whether the expansion fired.
//
// Memory win: outer `[]ast.Stmt{}` (non-map branch) + outer `[ast.Stmt(...)]`
// 1-element array (map branch) eliminated per call. Inner `new_then_stmts`
// allocation (map branch) and the AssignStmt/IfExpr/Ident wrappers are still
// built as legacy ast structs and routed through the appender — full
// elimination of those remains future work. Pinned by `fixture_if_guard_assign`
// across all 5 invariants (covers both map and non-map branches).
pub fn (mut t Transformer) try_expand_if_guard_assign_stmts_to_flat(stmt ast.AssignStmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if stmt.rhs.len != 1 || stmt.lhs.len != 1 {
		return false
	}
	rhs_expr := stmt.rhs[0]
	if rhs_expr !is ast.IfExpr {
		return false
	}
	if_expr := rhs_expr as ast.IfExpr
	if if_expr.cond !is ast.IfGuardExpr {
		return false
	}
	guard := if_expr.cond as ast.IfGuardExpr

	mut guard_var_name := ''
	for lhs_expr in guard.stmt.lhs {
		if lhs_expr is ast.Ident {
			guard_var_name = lhs_expr.name
			break
		} else if lhs_expr is ast.ModifierExpr {
			if lhs_expr.expr is ast.Ident {
				guard_var_name = lhs_expr.expr.name
				break
			}
		}
	}
	if guard_var_name == '' || guard.stmt.rhs.len == 0 {
		return false
	}

	guard_rhs := guard.stmt.rhs[0]
	if t.expr_wrapper_type_for_or(guard_rhs) != none || t.expr_returns_result(guard_rhs)
		|| t.expr_returns_option(guard_rhs) {
		return false
	}
	synth_pos := t.next_synth_pos()

	if guard_rhs is ast.IndexExpr {
		if _ := t.get_map_type_for_expr(guard_rhs.lhs) {
			key_in_map := t.make_infix_expr_at(.key_in, guard_rhs.expr, guard_rhs.lhs,
				guard_rhs.pos)

			mut new_then_stmts := []ast.Stmt{cap: if_expr.stmts.len + 1}
			new_then_stmts << ast.AssignStmt{
				op:  .decl_assign
				lhs: guard.stmt.lhs
				rhs: guard.stmt.rhs
				pos: guard.stmt.pos
			}
			for s in if_expr.stmts {
				new_then_stmts << s
			}

			modified_if := ast.IfExpr{
				cond:      key_in_map
				stmts:     new_then_stmts
				else_expr: if_expr.else_expr
				pos:       synth_pos
			}
			if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
				t.register_synth_type(synth_pos, orig_type)
			}

			t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
				op:  stmt.op
				lhs: stmt.lhs
				rhs: [ast.Expr(modified_if)]
				pos: stmt.pos
			}), mut out)
			return true
		}
	}

	t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: guard.stmt.lhs
		rhs: guard.stmt.rhs
		pos: guard.stmt.pos
	}), mut out)

	guard_ident := ast.Ident{
		name: guard_var_name
		pos:  synth_pos
	}
	modified_if := ast.IfExpr{
		cond:      guard_ident
		stmts:     if_expr.stmts
		else_expr: if_expr.else_expr
		pos:       synth_pos
	}
	if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
		t.register_synth_type(synth_pos, orig_type)
	}
	t.append_transformed_stmt_to_flat(mut ids, ast.Stmt(ast.AssignStmt{
		op:  stmt.op
		lhs: stmt.lhs
		rhs: [ast.Expr(modified_if)]
		pos: stmt.pos
	}), mut out)
	return true
}

// expand_lock_expr_to_flat is the flat-direct mirror of `expand_lock_expr`
// (transformer.v:7920). Mirrors the same shape exactly:
//   [sync__RwMutex_lock(&mtx)...,
//    sync__RwMutex_rlock(&mtx)...,
//    <transformed body stmts>,
//    sync__RwMutex_unlock(&mtx)...,
//    sync__RwMutex_runlock(&mtx)...]
// On native backends (arm64/x64), the lock/unlock/rlock/runlock calls are
// suppressed (sync module unavailable). The body is funneled through
// `transform_stmts_to_flat_direct` so the inner `[]ast.Stmt` allocation
// disappears in addition to the outer `[]ast.Stmt{}` result array.
//
// The lock-call CallExpr / PrefixExpr wrappers (and the unlock/rlock/runlock
// equivalents) are still built as legacy ast structs and routed to
// `out.emit_stmt(...)` — matching the legacy site's `out.emit_stmt(exp_stmt)`
// loop (no further transformation). Per-helper direct-emit of those wrappers
// remains future work. Pinned by `fixture_lock_stmt` across all 5 invariants.
pub fn (mut t Transformer) expand_lock_expr_to_flat(expr ast.LockExpr, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	is_native := t.is_native_be
	if !is_native {
		for lock_expr in expr.lock_exprs {
			ids << out.emit_stmt(ast.Stmt(ast.ExprStmt{
				expr: ast.CallExpr{
					lhs:  ast.Ident{
						name: 'sync__RwMutex_lock'
					}
					args: [
						ast.Expr(ast.PrefixExpr{
							op:   .amp
							expr: t.shared_mtx_expr(lock_expr)
						}),
					]
				}
			}))
		}
		for rlock_expr in expr.rlock_exprs {
			ids << out.emit_stmt(ast.Stmt(ast.ExprStmt{
				expr: ast.CallExpr{
					lhs:  ast.Ident{
						name: 'sync__RwMutex_rlock'
					}
					args: [
						ast.Expr(ast.PrefixExpr{
							op:   .amp
							expr: t.shared_mtx_expr(rlock_expr)
						}),
					]
				}
			}))
		}
	}
	body_ids := t.transform_stmts_to_flat_direct(expr.stmts, mut out)
	for body_id in body_ids {
		ids << body_id
	}
	if !is_native {
		for lock_expr in expr.lock_exprs {
			ids << out.emit_stmt(ast.Stmt(ast.ExprStmt{
				expr: ast.CallExpr{
					lhs:  ast.Ident{
						name: 'sync__RwMutex_unlock'
					}
					args: [
						ast.Expr(ast.PrefixExpr{
							op:   .amp
							expr: t.shared_mtx_expr(lock_expr)
						}),
					]
				}
			}))
		}
		for rlock_expr in expr.rlock_exprs {
			ids << out.emit_stmt(ast.Stmt(ast.ExprStmt{
				expr: ast.CallExpr{
					lhs:  ast.Ident{
						name: 'sync__RwMutex_runlock'
					}
					args: [
						ast.Expr(ast.PrefixExpr{
							op:   .amp
							expr: t.shared_mtx_expr(rlock_expr)
						}),
					]
				}
			}))
		}
	}
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
		ast.ImportStmt, ast.InterfaceDecl, ast.ModuleStmt, ast.TypeDecl {
			return out.emit_stmt(stmt)
		}
		ast.StructDecl {
			return out.emit_stmt(t.transform_struct_decl(stmt))
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
				field_ids << out.emit_field_decl_by_ids(field, typ_id, value_id, field_attrs_id)
			}
			fields_list_id := out.emit_aux_list_from_ids(field_ids)
			return out.emit_global_decl_by_ids(stmt.is_public, decl_attrs_id, fields_list_id)
		}
		ast.AssertStmt {
			// AssertStmt port: `transform_stmt`'s arm is the rare fallback path
			// — most assert stmts are expanded into `if !cond { panic(...) }` by
			// `transform_stmts` before they reach the per-stmt dispatch. The
			// fallback rebuilds `AssertStmt{expr: t.transform_expr(stmt.expr)}`
			// without setting `extra`, so the result always has
			// `extra = empty_expr` (the struct default) and `stmt.extra` (any
			// optional `"assert cond, message"` text) is DROPPED. Direct-emit
			// mirrors that exactly: transform `stmt.expr` via
			// `transform_expr_to_flat` and route through
			// `emit_assert_stmt_by_id` which encodes the second edge as
			// `add_expr(empty_expr)` (hits the shared cached `empty_expr_id`).
			// Skips the `ast.AssertStmt` wrapper struct allocation per fallback
			// occurrence; reachability is low but the port completes the
			// stmt-level identity-shape coverage (sessions 27 ExprStmt, 28
			// LabelStmt, 29 BlockStmt, 30 DeferStmt, 31 ComptimeStmt).
			expr_id := t.transform_expr_to_flat(stmt.expr, mut out)
			return out.emit_assert_stmt_by_id(expr_id)
		}
		ast.AssignStmt {
			// AssignStmt port: `transform_stmt`'s arm has a pre-arm dispatch
			// (lines 2958-2966) followed by the match-arm call to
			// `transform_assign_stmt`. The pre-arm dispatch runs only for
			// `stmt is AssignStmt`:
			//   1. `try_expand_or_expr_assign(stmt)` — stub that always
			//      returns `none` (transformer.v:3804). No-op, skip.
			//   2. `try_transform_map_index_assign(stmt)` — may rewrite
			//      `m[key] = val` (and compound forms like `m[k] += v`) into
			//      an `ExprStmt(map__set(&m, &key, &val))` or a `BlockStmt`
			//      hoisting prefix temp stmts before the call. Returned shape
			//      is NOT an AssignStmt — route through leaf `out.emit_stmt`
			//      (the lowered stmt's children are already in their final
			//      form; legacy returns it directly from `transform_stmt`
			//      without re-dispatch, so `add_stmt(returned)` is the right
			//      encoding).
			// 3. else → `transform_assign_stmt(stmt)` returns an AssignStmt
			//    with `lhs`/`rhs` exprs already transformed (the helper
			//    runs `t.transform_expr` on each). Direct-emit encodes them
			//    via leaf `out.emit_expr` (pure `add_expr` — no
			//    re-transform; `transform_expr_to_flat` would
			//    double-transform) and assembles via the new
			//    `emit_assign_stmt_by_ids` helper.
			//
			// Pattern note: sibling of SqlExpr (session 36) at the stmt
			// level — both arms call the helper once then dispatch on the
			// result's shape (lowered vs identity-rebuild). AssignStmt adds
			// a pre-arm guard (`try_transform_map_index_assign`) before the
			// main rewrite, similar to how `transform_stmt`'s dispatch
			// structure has special handling for AssignStmt. The pre-arm
			// helper's lowered shapes (BlockStmt/ExprStmt) don't yet have
			// dedicated flat helpers, so they route through leaf
			// `out.emit_stmt`; when a BlockStmt cross-arm router or
			// ExprStmt deeper port lands, this can switch to dispatched
			// routing.
			//
			// Reachability is very high — assignments appear in nearly
			// every function body. Most reach the identity-rebuild path
			// (only map-index assigns hit the rewrite). The win is the
			// `ast.AssignStmt` wrapper allocation per assign + the
			// `transform_stmt` match dispatch.
			if rewritten := t.try_transform_map_index_assign(stmt) {
				// Session 92 routing refactor (BlockStmt direct-emit fall-
				// through): when `try_transform_map_index_assign`
				// (transformer.v:3851) lowers `m[k] = v` (and compound
				// forms) for non-eval backends, the `addr_of_with_prefix_temp`
				// calls (transformer.v:9756) push key/value temp decl-
				// assigns onto `prefix_stmts` whenever the underlying
				// expression can't take its address directly. With at
				// least one prefix stmt the helper wraps the temp decls
				// + the synthesized `map__set(...)` call into an
				// `ast.BlockStmt{stmts: prefix_stmts + call_stmt}`
				// (transformer.v:3961-3966). Direct-emit via session
				// 39's `emit_block_stmt_by_ids` skips the
				// `ast.BlockStmt` wrapper-struct allocation per map-
				// index assign occurrence. Same template as sessions
				// 73-91 (routing-pass shape extensions). The inner
				// stmts are already-transformed, so leaf-encoding each
				// via `out.emit_stmt` is bit-equal to `add_stmt(BlockStmt)`'s
				// `push_stmt` walk. Reachability is high — most map
				// writes in compiler-style code involve at least one
				// addressable-key/value temp.
				if rewritten is ast.BlockStmt {
					mut stmt_ids := []ast.FlatNodeId{cap: rewritten.stmts.len}
					for s in rewritten.stmts {
						stmt_ids << out.emit_stmt(s)
					}
					return out.emit_block_stmt_by_ids(stmt_ids)
				}
				// Session 93 routing refactor (ExprStmt direct-emit fall-
				// through): when `try_transform_map_index_assign`'s
				// `addr_of_with_prefix_temp` calls don't push any
				// prefix stmts (the key + value are already addressable
				// — common case for `m[ident] = literal` or
				// `m[literal] = ident`), the helper returns just the
				// synthesized `ast.ExprStmt{expr: map__set(...)}`
				// without a BlockStmt wrap (transformer.v:3967).
				// Direct-emit the wrapping ExprStmt via session-prior
				// `emit_expr_stmt_by_id` skips the `ast.ExprStmt`
				// wrapper-struct allocation per such occurrence. The
				// inner CallExpr is already-transformed (args built via
				// `voidptr_cast` over already-transformed key/val temp
				// refs), so leaf-encoding via `out.emit_expr` is bit-
				// equal to `add_stmt(ExprStmt)`'s `push_expr` walk. Same
				// template as session 92 (BlockStmt fall-through in the
				// same pre-arm guard).
				if rewritten is ast.ExprStmt {
					expr_id := out.emit_expr(rewritten.expr)
					return out.emit_expr_stmt_by_id(expr_id)
				}
				// Session 94 routing refactor (AssignStmt direct-emit fall-
				// through): when the pre-arm guard hits the
				// `try_transform_map_selector_assign` branch (lhs is a
				// SelectorExpr over a map index, e.g. `m[k].field =
				// val`) and `addr_of_with_prefix_temp` doesn't push any
				// prefix stmts (rare — the IndexExpr lhs is usually
				// non-addressable so a map temp gets pushed), the
				// helper returns a single `ast.AssignStmt` with the
				// rewritten lhs `(*((ValueType*)map__get_and_set(...))).field`
				// (transformer.v:4045). Direct-emit via session 60's
				// `emit_assign_stmt_by_ids` skips the `ast.AssignStmt`
				// wrapper-struct allocation per such occurrence. The
				// lhs/rhs exprs are already-transformed by the helper,
				// so leaf-encoding via `out.emit_expr` is bit-equal.
				// Same template as sessions 92-93 (sibling fall-throughs
				// in the same pre-arm guard).
				if rewritten is ast.AssignStmt {
					mut lhs_ids := []ast.FlatNodeId{cap: rewritten.lhs.len}
					for e in rewritten.lhs {
						lhs_ids << out.emit_expr(e)
					}
					mut rhs_ids := []ast.FlatNodeId{cap: rewritten.rhs.len}
					for e in rewritten.rhs {
						rhs_ids << out.emit_expr(e)
					}
					return out.emit_assign_stmt_by_ids(rewritten.op, lhs_ids, rhs_ids,
						rewritten.pos)
				}
				return out.emit_stmt(rewritten)
			}
			// Session 60 refactor (wrap-only elision): direct-call the new
			// `transform_assign_stmt_parts(stmt) (op, lhs, rhs, pos)` helper
			// (the body-work driver extracted from `transform_assign_stmt`)
			// and skip the `ast.AssignStmt` wrapper allocation. Legacy
			// `transform_assign_stmt` becomes a thin wrapper that rebuilds
			// the struct for non-flat callers. Same template as session 4
			// (FnDecl_parts) and session 59 (ReturnStmt_parts).
			op, lhs_exprs, rhs_exprs, pos := t.transform_assign_stmt_parts(stmt)
			mut lhs_ids := []ast.FlatNodeId{cap: lhs_exprs.len}
			for e in lhs_exprs {
				lhs_ids << out.emit_expr(e)
			}
			mut rhs_ids := []ast.FlatNodeId{cap: rhs_exprs.len}
			for e in rhs_exprs {
				rhs_ids << out.emit_expr(e)
			}
			return out.emit_assign_stmt_by_ids(op, lhs_ids, rhs_ids, pos)
		}
		ast.ForStmt {
			// ForStmt port: `transform_stmt`'s arm is a single helper call to
			// `transform_for_stmt`, which always returns an `ast.ForStmt`
			// (identity shape) regardless of its internal lowering — for-in
			// loops (range, runes_iterator, fixed array, dynamic array,
			// string, map, untyped) all lower into a ForStmt with
			// transformed init/cond/post/stmts. The helper also opens/closes
			// scopes, sets up smartcast contexts from `is` checks in the
			// condition, and recursively calls `transform_stmts` on the
			// body.
			//
			// Direct-emit calls `transform_for_stmt` once, then encodes the
			// already-transformed children via leaf emitters (no
			// re-transform via `transform_stmt_to_flat` /
			// `transform_expr_to_flat` — would double-transform): init/post
			// stmts via `out.emit_stmt`, cond expr via `out.emit_expr`, body
			// stmts via `out.emit_stmt`. Assembles via the new
			// `emit_for_stmt_by_ids(init_id, cond_id, post_id, stmt_ids)`
			// helper, which mirrors `add_stmt(ForStmt)` encoding exactly
			// (pos = `token.Pos{}`, edges = [init, cond, post, body...]).
			//
			// Pattern note: stmt-level sibling of SqlExpr branch (b)
			// (session 36) and AssignStmt's main-path (session 37) — the
			// "always-lowers via single helper call, identity-shape result"
			// template. Unlike AssignStmt, there's no pre-arm guard;
			// `transform_for_stmt` handles everything internally. Skips the
			// `ast.ForStmt` wrapper struct allocation per occurrence + the
			// `transform_stmt` match dispatch. Reachability is high — `for`
			// loops appear in nearly every non-trivial function body.
			result := t.transform_for_stmt(stmt)
			init_id := out.emit_stmt(result.init)
			cond_id := out.emit_expr(result.cond)
			post_id := out.emit_stmt(result.post)
			mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
			for s in result.stmts {
				stmt_ids << out.emit_stmt(s)
			}
			return out.emit_for_stmt_by_ids(init_id, cond_id, post_id, stmt_ids)
		}
		ast.ForInStmt {
			// ForInStmt port: `transform_stmt`'s arm is a single helper call
			// to `transform_for_in_stmt`, which wraps the ForInStmt as the
			// `init` of a synthetic ForStmt and delegates to
			// `transform_for_stmt`. The return type is `ast.ForStmt` — a
			// DIFFERENT shape from the input (ForInStmt → ForStmt). All the
			// usual for-in lowering (range, runes_iterator, fixed-array,
			// dynamic-array, string, map, untyped) happens inside
			// `transform_for_stmt` once it sees the ForInStmt in `init`.
			//
			// Direct-emit calls `transform_for_in_stmt` once, then encodes
			// the already-transformed ForStmt children via leaf emitters (no
			// re-transform via `transform_stmt_to_flat` /
			// `transform_expr_to_flat` — would double-transform): init/post
			// stmts via `out.emit_stmt`, cond expr via `out.emit_expr`, body
			// stmts via `out.emit_stmt`. Reuses the existing
			// `emit_for_stmt_by_ids` helper from session 38 — no new flat
			// helper needed since the lowered shape is ForStmt.
			//
			// Pattern note: stmt-level sibling of AssocExpr (session 34) and
			// MatchExpr (session 35) — the "always-lowers via single helper
			// to a DIFFERENT shape" template. Distinct from session 38
			// (ForStmt, identity-shape result) because the input arm is
			// ForInStmt but the result encoding is ForStmt. This is also a
			// "cross-arm encoding reuse" — analogous to session 33
			// (GenericArgOrIndexExpr routes to `transform_index_expr_to_flat`
			// for the next stage) but here we reuse the encoding helper
			// rather than re-dispatching through `transform_stmt_to_flat`.
			// We must NOT route through `transform_stmt_to_flat`'s ForStmt
			// arm because that would re-call `transform_for_stmt` on an
			// already-fully-transformed ForStmt — double work and possible
			// smartcast double-application.
			//
			// Skips the `ast.ForStmt` wrapper allocation for the result and
			// the outer `transform_stmt` match dispatch. Reachability is
			// high: `for x in arr` / `for k, v in map` are extremely common
			// (every iterator in the compiler itself uses them).
			result := t.transform_for_in_stmt(stmt)
			init_id := out.emit_stmt(result.init)
			cond_id := out.emit_expr(result.cond)
			post_id := out.emit_stmt(result.post)
			mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
			for s in result.stmts {
				stmt_ids << out.emit_stmt(s)
			}
			return out.emit_for_stmt_by_ids(init_id, cond_id, post_id, stmt_ids)
		}
		ast.BlockStmt {
			// BlockStmt port: `transform_stmt`'s arm is identity-shape —
			// `BlockStmt{stmts: transform_stmts(stmt.stmts)}`. The body
			// driver `transform_stmts` does extra work between sibling stmts
			// (smartcast state snapshot/restore, pending-stmt hoisting, and
			// may expand one input stmt into several outputs) so we can't
			// recurse via `transform_stmt_to_flat` without porting the
			// driver itself — that's a separate (bigger) refactor.
			// Wrap-only port: call legacy `transform_stmts` to materialise
			// the transformed `[]ast.Stmt`, emit each via the legacy
			// `out.emit_stmt(...)` (the stmts are already transformed), and
			// assemble via the new `emit_block_stmt_by_ids` helper. Skips
			// the outer `ast.BlockStmt` wrapper struct allocation per
			// occurrence; the inner `[]ast.Stmt` from `transform_stmts`
			// still materialises (until the driver is ported).
			stmt_ids := t.transform_stmts_to_flat(stmt.stmts, mut out)
			return out.emit_block_stmt_by_ids(stmt_ids)
		}
		ast.ComptimeStmt {
			// ComptimeStmt port: `transform_stmt`'s arm has two branches.
			//   1. `stmt.stmt is ast.ForStmt` ($for branch): rebuild a fresh
			//      ComptimeStmt wrapping a fresh ForStmt with verbatim
			//      `init`/`cond`/`post` and body stmts run through the
			//      `transform_stmts` driver. The init/cond/post are NOT
			//      transformed by the legacy arm (just copied), so direct-emit
			//      synthesises a `ForStmt{init, cond, post, transformed_stmts}`
			//      and wraps via the new `emit_comptime_stmt_by_id` helper.
			//      Skips the outer `ast.ComptimeStmt` wrapper struct
			//      allocation per `$for` occurrence.
			//   2. otherwise (`$if` as stmt, etc.): legacy returns
			//      `transform_stmt(stmt.stmt)` — the ComptimeStmt wrapper is
			//      DROPPED entirely; the result is whatever the inner stmt
			//      transforms into. Direct-emit recurses via
			//      `transform_stmt_to_flat(stmt.stmt, ...)` so any ported
			//      inner stmt also stays on the flat path.
			//
			// Session 61 refactor (inner ForStmt wrapper-struct elision):
			// the $for branch previously synthesised a fresh `ast.ForStmt`
			// wrapper and routed it through legacy `out.emit_stmt(...)`.
			// Direct-emit via session 38's `emit_for_stmt_by_ids` skips
			// that inner wrapper allocation. The init/post stmts and cond
			// expr are verbatim (NOT transformed by legacy — match
			// `transform_stmt` ComptimeStmt arm at transformer.v:2978) so
			// leaf-encode via `out.emit_stmt` / `out.emit_expr`. Body
			// stmts come from `transform_stmts` (already transformed) and
			// leaf-encode via `out.emit_stmt` per stmt. Bit-equal to the
			// legacy ForStmt encoding because both sides use the default
			// `token.Pos{}` (the synth ForStmt struct above had no `pos`
			// field set; `emit_for_stmt_by_ids` also passes `token.Pos{}`).
			//
			// Reachability is reasonable — `$for field in Type.fields { ... }`
			// shows up in compile-time reflection across the compiler.
			if stmt.stmt is ast.ForStmt {
				for_stmt := stmt.stmt as ast.ForStmt
				stmt_ids := t.transform_stmts_to_flat(for_stmt.stmts, mut out)
				init_id := out.emit_stmt(for_stmt.init)
				cond_id := out.emit_expr(for_stmt.cond)
				post_id := out.emit_stmt(for_stmt.post)
				for_id := out.emit_for_stmt_by_ids(init_id, cond_id, post_id, stmt_ids)
				return out.emit_comptime_stmt_by_id(for_id)
			}
			return t.transform_stmt_to_flat(stmt.stmt, mut out)
		}
		ast.DeferStmt {
			// DeferStmt port: `transform_stmt`'s arm is identity-shape —
			// `DeferStmt{mode: stmt.mode, stmts: transform_stmts(stmt.stmts)}`.
			// `mode` (`.scoped` or `.function`) is copied verbatim; the body
			// stmts go through the `transform_stmts` driver (smartcast state
			// snapshot/restore, pending-stmt hoisting, one-to-many expansion)
			// so we can't recurse via `transform_stmt_to_flat` without porting
			// the driver itself — same constraint as the BlockStmt arm.
			// Wrap-only port: call legacy `transform_stmts` to materialise the
			// transformed `[]ast.Stmt`, emit each via legacy `out.emit_stmt(...)`
			// (already transformed), and assemble via the new
			// `emit_defer_stmt_by_ids` helper (mode → flag_defer_func).
			// Skips the outer `ast.DeferStmt` wrapper struct allocation per
			// occurrence; the inner `[]ast.Stmt` still materialises until the
			// driver is ported.
			stmt_ids := t.transform_stmts_to_flat(stmt.stmts, mut out)
			return out.emit_defer_stmt_by_ids(stmt.mode, stmt_ids)
		}
		ast.LabelStmt {
			// LabelStmt port: `transform_stmt`'s arm is identity-shape —
			// `LabelStmt{name: stmt.name, stmt: transform_stmt(stmt.stmt)}`.
			// `name` is copied verbatim; the inner stmt is recursively
			// transformed. Direct-emit recurses into
			// `transform_stmt_to_flat` for the inner stmt (so any ported
			// stmt arm reached inside the label — ExprStmt, ReturnStmt,
			// ForStmt, ... — also stays on the flat path) and assembles
			// via the new `emit_label_stmt_by_id` helper. Skips the
			// `ast.LabelStmt` wrapper struct allocation and the
			// `transform_stmt`/`add_stmt` match-dispatch round-trip per
			// occurrence. Reachability is limited to source-level `label:
			// stmt` syntax (typically `outer:` before loops), but the port
			// is clean — single child stmt and a verbatim name.
			inner_id := t.transform_stmt_to_flat(stmt.stmt, mut out)
			return out.emit_label_stmt_by_id(stmt.name, inner_id)
		}
		ast.ExprStmt {
			// ExprStmt port: `transform_stmt`'s arm is identity-shape — it
			// always returns an `ast.ExprStmt` wrapping the transformed inner
			// expression. The arm flips the `skip_if_value_lowering` flag
			// when the expr is a direct IfExpr (so `transform_if_expr` knows
			// it's in stmt position and doesn't introduce a temp variable),
			// then restores it after the recursive `transform_expr` call.
			// Direct-emit mirrors the flag flip around the recursive
			// `transform_expr_to_flat` call (so any deep-helper-ported expr
			// arm reached inside also benefits from the flat path), then
			// assembles via the new `emit_expr_stmt_by_id` helper. Skips the
			// `ast.ExprStmt` wrapper allocation + the legacy
			// `out.emit_stmt(...)` round-trip's match dispatch on every
			// expression statement.
			is_direct_if := stmt.expr is ast.IfExpr
			saved_skip := t.skip_if_value_lowering
			if is_direct_if {
				t.skip_if_value_lowering = true
			}
			expr_id := t.transform_expr_to_flat(stmt.expr, mut out)
			t.skip_if_value_lowering = saved_skip
			return out.emit_expr_stmt_by_id(expr_id)
		}
		ast.ReturnStmt {
			// ReturnStmt port: `transform_return_stmt` always returns an
			// `ast.ReturnStmt` (never lowers to a different shape). Its body
			// (~110 lines in transformer.v) does per-expression sumtype
			// wrapping, enum-shorthand resolution, smartcast handling, and
			// native-backend pre-wrapping — too much to inline.
			//
			// Session 59 refactor (wrap-only elision): direct-call the new
			// `transform_return_stmt_parts(stmt) []ast.Expr` helper (the
			// body-work driver extracted from `transform_return_stmt`) and
			// skip the `ast.ReturnStmt` wrapper allocation. The legacy
			// `transform_return_stmt` becomes a thin wrapper that calls
			// `_parts` and rebuilds the struct for non-flat callers. Same
			// template as session 4 (FnDecl wrapper-struct elision). Each
			// transformed expr goes through `out.emit_expr(...)` (already
			// transformed — `transform_expr_to_flat` would double-transform).
			exprs := t.transform_return_stmt_parts(stmt)
			mut expr_ids := []ast.FlatNodeId{cap: exprs.len}
			for e in exprs {
				expr_ids << out.emit_expr(e)
			}
			return out.emit_return_stmt_by_ids(expr_ids)
		}
		ast.FnDecl {
			// Direct emit: call `transform_fn_decl_parts_to_flat` (the
			// body-work driver behind `transform_fn_decl`, mirrored through
			// the flat-builder seam) for the two variable parts — final
			// attributes (possibly augmented for `@[live]`) and the final
			// transformed + defer-lowered stmt list already encoded as
			// FlatNodeIds — and assemble the FnDecl flat encoding via the
			// typed builder helpers. Skips the `ast.FnDecl` wrapper struct
			// that `transform_fn_decl` builds in the legacy path. The
			// immutable is_public/is_method/is_static/receiver/language/
			// name/typ/pos fields come straight from `stmt`. Bit-equal to
			// the legacy `out.emit_stmt(t.transform_fn_decl(stmt))`
			// round-trip.
			//
			// Session 112 refactor: switch from inline per-stmt
			// `out.emit_stmt(body_stmt)` loop to the new
			// `transform_fn_decl_parts_to_flat` seam (currently a literal
			// pass-through over the legacy parts helper + leaf-encode).
			// Pure scaffolding — zero memory savings on its own. The
			// purpose is to set up the `transform_stmts` body driver port
			// arc: future sessions replace the pass-through's body with
			// direct-emit logic that skips intermediate `ast.Stmt`
			// wrappers across the expansion sites in `transform_stmts`
			// (comptime $if assign expansion, or-block assign, tuple
			// if/call, lock/rlock, for-in-map, assert, sumtype-init, ...).
			// Each per-site port lands as its own session (s113+) inside
			// the seam without disturbing other call sites of the legacy
			// helper.
			lowered_stmt := t.fn_decl_with_implicit_veb_context_param(stmt)
			attrs, stmt_ids := t.transform_fn_decl_parts_to_flat(lowered_stmt, mut out)
			receiver_id := out.emit_parameter(lowered_stmt.receiver)
			typ_id := out.emit_type(ast.Type(lowered_stmt.typ))
			attrs_id := out.emit_attribute_list(attrs)
			stmts_list_id := out.emit_aux_list_from_ids(stmt_ids)
			return out.emit_fn_decl_by_ids(lowered_stmt.name, lowered_stmt.is_public,
				lowered_stmt.is_method, lowered_stmt.is_static, lowered_stmt.language,
				lowered_stmt.pos, receiver_id, typ_id, attrs_id, stmts_list_id)
		}
		else {
			// Unreachable in practice — the `[]ast.Attribute` variant is
			// handled by the upfront `is` check, and every other variant of
			// `ast.Stmt` has an explicit arm above. V's exhaustiveness
			// checker still requires this arm because `[]ast.Attribute` is
			// part of the match scope's variant set.
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
			// These produce *different* expression shapes — for these branches
			// (`needs_legacy`), session 58 direct-calls `transform_expr` once
			// and dispatches on result shape (identity PrefixExpr via
			// `emit_prefix_expr_by_id`, anything else leaf-emit). For all
			// other operators (`-`, `!`, `~`, `++`, `--`, ...) the legacy arm
			// is a pure wrapper around `transform_expr(expr.expr)`; direct-emit
			// recurses into `transform_expr_to_flat` and assembles via
			// `emit_prefix_expr_by_id`, skipping the `ast.PrefixExpr` struct
			// allocation per occurrence.
			//
			// Session 58 refactor (amp / arrow+OrExpr branches): the common
			// path is identity (`&x` where `x` is not a type cast or
			// AssocExpr) — `transform_expr` returns a PrefixExpr, direct-emit
			// via `emit_prefix_expr_by_id` skips the wrapper allocation. The
			// rare lowered shapes (UnsafeExpr from `lower_assoc_expr`, OrExpr
			// from arrow+OrExpr rewrite, transformed IndexExpr from
			// `amp_type_cast_expr` hit) all leaf-emit so rewrite logic stays
			// single-sourced. Children leaf-encode via `out.emit_expr` (helper
			// already transformed them — `transform_expr_to_flat` would
			// double-transform). Same template as session 41 (ArrayInitExpr).
			needs_legacy := expr.op == .amp || (expr.op == .arrow && expr.expr is ast.OrExpr)
			if needs_legacy {
				result := t.transform_expr(ast.Expr(expr))
				if result is ast.PrefixExpr {
					inner_id := out.emit_expr(result.expr)
					return out.emit_prefix_expr_by_id(result.op, inner_id, result.pos)
				}
				// Session 90 routing refactor (UnsafeExpr direct-emit fall-
				// through): when the `arrow + OrExpr` branch fires
				// (expr.v:49-59), the recursive `transform_expr` of the
				// rewritten `OrExpr{arrow x, stmts}` routes through the
				// OrExpr arm (expr.v:236), which lowers a non-empty
				// `prefix_stmts` set into `ast.UnsafeExpr{stmts:
				// prefix_stmts + ExprStmt{result_expr}, pos}` so codegen
				// emits a GCC compound expression. Direct-emit via
				// session 19's `emit_unsafe_expr_by_ids` skips the
				// `ast.UnsafeExpr` wrapper-struct allocation per
				// `<-x or {...}` occurrence. Same template as sessions
				// 73-89 (routing-pass shape extensions). Reachability is
				// low — channel arrow with or-block is uncommon — but
				// the arm is bit-equal and keeps the dispatch pattern
				// consistent.
				if result is ast.UnsafeExpr {
					mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
					for s in result.stmts {
						stmt_ids << out.emit_stmt(s)
					}
					return out.emit_unsafe_expr_by_ids(stmt_ids, result.pos)
				}
				// Session 95 routing refactor (IndexExpr direct-emit fall-
				// through): when the `& + amp_type_cast_expr` branch fires
				// (expr.v:37-41), `amp_type_cast_expr` rewrites a
				// `&CallOrCastExpr(...)[idx]` into an `ast.IndexExpr{lhs:
				// CastExpr{typ: &Type, expr: inner}, expr: idx, ...}` and
				// the outer arm recursively calls `transform_expr` on it.
				// That recursion routes through `transform_index_expr`
				// (expr.v:436) which, since `expr.expr` is not a RangeExpr
				// and `lhs` is a CastExpr (not a map type), takes the
				// default identity branch returning an `ast.IndexExpr`
				// with already-transformed lhs/expr. Direct-emit via
				// `emit_index_expr_by_ids` (flat.v:943) skips the
				// `ast.IndexExpr` wrapper-struct allocation per
				// `&Type(ptr)[idx]` occurrence. Same template as sessions
				// 73-90 (routing-pass shape extensions). Reachability is
				// low — pointer-cast indexing is uncommon in normal V
				// code — but the dispatch arm is bit-equal and keeps
				// parity with the other PrefixExpr `needs_legacy`
				// downstream shapes.
				if result is ast.IndexExpr {
					lhs_id := out.emit_expr(result.lhs)
					expr_id := out.emit_expr(result.expr)
					return out.emit_index_expr_by_ids(lhs_id, expr_id, result.is_gated, result.pos)
				}
				// Session 98 routing refactor (CallExpr direct-emit fall-
				// through): when the `& + amp_type_cast_expr` branch fires
				// with a slice index (`&Type(ptr)[a..b]`), the rewritten
				// `IndexExpr{lhs: CastExpr{typ: &Type}, expr: RangeExpr, ...}`
				// routes through `transform_index_expr`'s slice branch
				// (expr.v:438-441) which calls `transform_slice_index_expr`
				// (expr.v:576). That helper returns a `CallExpr` shape
				// (`string__substr`, `array__slice`, `array__slice_ni`, or
				// `new_array_from_c_array`) with already-transformed
				// lhs/args. Direct-emit via `emit_call_expr_by_ids`
				// (flat.v:654) skips the `ast.CallExpr` wrapper-struct
				// allocation per `&Type(ptr)[a..b]` occurrence. Same
				// template as sessions 73-97 (routing-pass shape
				// extensions). Reachability is very low — pointer-cast
				// slicing is rare — but the dispatch arm is bit-equal and
				// keeps parity with the other PrefixExpr `needs_legacy`
				// downstream shapes.
				if result is ast.CallExpr {
					lhs_id := out.emit_expr(result.lhs)
					mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
					for a in result.args {
						arg_ids << out.emit_expr(a)
					}
					return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
				}
				return out.emit_expr(result)
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
		ast.LambdaExpr {
			// LambdaExpr is a pure wrapper: the `transform_expr` arm recurses
			// into `expr.expr` and copies `args` (a `[]Ident`) and `pos`
			// verbatim. Direct-emit skips the `ast.LambdaExpr` struct
			// allocation: the inner expression goes through
			// `transform_expr_to_flat` (so nested non-leaf exprs also bypass the
			// legacy round-trip), each arg Ident is emitted via the leaf
			// `out.emit_expr(...)` path (Idents are identity in `transform_expr`
			// already), and the flat node is assembled via the new
			// `emit_lambda_expr_by_ids` helper. Mirrors `add_expr(LambdaExpr)`
			// encoding exactly (edge[0] = inner expr, edge[1..] = args).
			inner_id := t.transform_expr_to_flat(expr.expr, mut out)
			mut arg_ids := []ast.FlatNodeId{cap: expr.args.len}
			for arg in expr.args {
				arg_ids << out.emit_expr(ast.Expr(arg))
			}
			return out.emit_lambda_expr_by_ids(inner_id, arg_ids, expr.pos)
		}
		ast.CastExpr {
			// CastExpr's `transform_expr` arm has one rewrite side case: when
			// `expr.typ` resolves to a known sum-type name and
			// `wrap_sumtype_value(expr.expr, name)` succeeds, the cast is
			// lowered into an explicit sum-type initialization (different
			// expression shape — typically a `CallExpr` to
			// `<SumType>__from_<Variant>` or an InitExpr). Detect that
			// condition up front and fall back to legacy if it might trigger.
			// For non-sumtype casts the arm is a pure wrapper: `typ` is copied
			// verbatim (NOT transformed by `transform_expr`) while the inner
			// `expr` IS transformed. Direct-emit mirrors that exactly — `typ`
			// goes through the leaf `out.emit_expr(...)` (matches
			// `add_expr(typ)` byte-for-byte) and the inner expression goes
			// through the recursive `transform_expr_to_flat`, then assembles
			// via the new `emit_cast_expr_by_ids` helper.
			// Session 54 refactor: direct-call `wrap_sumtype_value` for
			// the sum-type cast lowering branch and leaf-emit the
			// wrapped result (CastExpr/InitExpr/CallExpr depending on
			// the variant shape — see `build_sumtype_init`). When
			// `wrap_sumtype_value` returns none (value already is the
			// sum type, declared receiver matches, generic-param guard,
			// etc.), fall through to the existing default direct-emit
			// path. Skips the outer `transform_expr` match dispatch on
			// every sum-type cast occurrence; matches the routing-pass
			// template used by sessions 47-51.
			//
			// Session 77 refactor: `build_sumtype_init` (the function
			// `wrap_sumtype_value` tail-calls) returns an `ast.InitExpr`
			// on non-eval backends (the dominant case — every
			// `SumType(variant_value)` cast in non-eval builds). The
			// InitExpr's `_tag` field is a BasicLiteral leaf and its
			// `_data._<variant>` field carries an already-transformed
			// boxed value (CastExpr around the inner value, or CastExpr
			// around a `memdup(...)` CallExpr for non-direct variants).
			// Direct-emit the outer InitExpr via session 1's
			// `emit_init_expr_by_ids` + `emit_field_init_by_id` — leaf-
			// encode `typ`/`f.value` since the helper has already
			// transformed them, mirroring `add_expr(InitExpr)` byte-for-
			// byte. Skips the `ast.InitExpr` wrapper-struct allocation
			// per sumtype cast. The eval-backend path passes
			// `transformed_value` through unchanged — that arbitrary
			// shape stays on leaf-emit. Same routing template as
			// sessions 73-76 (third shape after sumtype-lower hit and
			// default direct-emit).
			sumtype_name := t.type_expr_name_full(expr.typ)
			if sumtype_name != '' && t.is_sum_type(sumtype_name) {
				if wrapped := t.wrap_sumtype_value(expr.expr, sumtype_name) {
					if wrapped is ast.InitExpr {
						typ_id := out.emit_expr(wrapped.typ)
						mut field_ids := []ast.FlatNodeId{cap: wrapped.fields.len}
						for f in wrapped.fields {
							value_id := out.emit_expr(f.value)
							field_ids << out.emit_field_init_by_id(f.name, value_id)
						}
						return out.emit_init_expr_by_ids(typ_id, field_ids, wrapped.pos)
					}
					return out.emit_expr(wrapped)
				}
			}
			typ_id := out.emit_expr(expr.typ)
			expr_id := t.transform_expr_to_flat(expr.expr, mut out)
			return out.emit_cast_expr_by_ids(typ_id, expr_id, expr.pos)
		}
		ast.PostfixExpr {
			// PostfixExpr's `transform_expr` arm has lowering side cases when
			// `op in [.not, .question]`: native backends keep the postfix node,
			// other backends rewrite `expr!` / `expr?` into a `CastExpr` over
			// the inner Result/Option base type, plus a string-range check that
			// renames `substr` to `substr_checked`. These produce *different*
			// expression shapes (PostfixExpr on native, CastExpr on
			// lowering hit, raw inner on lowering miss). For the plain
			// postfix ops (`++` / `--`, i.e. `.inc` and `.dec`), the arm is a
			// pure wrapper around `transform_expr(expr.expr)`; direct-emit
			// recurses into `transform_expr_to_flat` and assembles the flat
			// node via `emit_postfix_expr_by_id`.
			//
			// Session 57 refactor (.not/.question branch): direct-call
			// `transform_expr` once and dispatch on result shape — PostfixExpr
			// direct-emits via `emit_postfix_expr_by_id` (session 10);
			// CastExpr direct-emits via `emit_cast_expr_by_ids` (session 12);
			// the unwrapped-inner shape (helper returns just `inner` when no
			// type name resolves) leaf-emits. Children are leaf-encoded via
			// `out.emit_expr` (helper already transformed them — re-routing
			// through `transform_expr_to_flat` would double-transform).
			// Same three-shape dispatch template as session 41 (ArrayInitExpr)
			// extended with an extra CastExpr arm.
			if expr.op == .not || expr.op == .question {
				result := t.transform_expr(ast.Expr(expr))
				if result is ast.PostfixExpr {
					inner_id := out.emit_expr(result.expr)
					return out.emit_postfix_expr_by_id(result.op, inner_id, result.pos)
				}
				if result is ast.CastExpr {
					typ_id := out.emit_expr(result.typ)
					expr_id := out.emit_expr(result.expr)
					return out.emit_cast_expr_by_ids(typ_id, expr_id, result.pos)
				}
				// Session 108 routing refactor (CallExpr direct-emit fall-
				// through): when the PostfixExpr arm's non-native path
				// (expr.v:67-110, `expr.op in [.not, .question]`)
				// rewrites a string range subscript via
				// `rename_substr_to_checked` (transformer.v:11572) and
				// the checker-inferred type name can't be resolved
				// (`type_name == ''`), the arm falls through to
				// `return inner` where `inner` is the rewritten
				// `ast.CallExpr{lhs: Ident{name:
				// 'string__substr_with_check'}, args: expr.args}`
				// (already transformed by the helper's
				// `t.transform_expr(expr.expr)` call). Direct-emit via
				// session 44's `emit_call_expr_by_ids` skips the
				// `ast.CallExpr` wrapper-struct allocation per such
				// occurrence. Same template as sessions 73-107
				// (routing-pass shape extensions). Reachability is very
				// low — requires `expr!` / `expr?` on a string range
				// (`s[a..b]!`) where the inferred type lookup misses —
				// but the dispatch arm is bit-equal and rounds out the
				// PostfixExpr arm's shape coverage alongside the
				// existing PostfixExpr / CastExpr handlers.
				if result is ast.CallExpr {
					lhs_id := out.emit_expr(result.lhs)
					mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
					for a in result.args {
						arg_ids << out.emit_expr(a)
					}
					return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
				}
				return out.emit_expr(result)
			}
			inner_id := t.transform_expr_to_flat(expr.expr, mut out)
			return out.emit_postfix_expr_by_id(expr.op, inner_id, expr.pos)
		}
		ast.IfGuardExpr {
			// IfGuardExpr should only appear as IfExpr condition, handled by
			// `transform_if_expr`. The arm here is the degenerate standalone-
			// reach case: legacy returns `t.transform_expr(expr.stmt.rhs[0])`
			// when rhs is non-empty, otherwise returns `expr` unchanged.
			// Direct-emit mirrors that — recursive `transform_expr_to_flat`
			// for the rhs[0] case, leaf `out.emit_expr(...)` for the fallback.
			if expr.stmt.rhs.len > 0 {
				return t.transform_expr_to_flat(expr.stmt.rhs[0], mut out)
			}
			return out.emit_expr(expr)
		}
		ast.OrExpr {
			// OrExpr in expression position (nested, in for-condition, in call
			// arg, ...) — statement-level or-block sites go through dedicated
			// statement handlers (expand_direct_or_expr_assign,
			// try_expand_or_expr_return, ...) and never reach this arm.
			//
			// `expand_single_or_expr` returns the data-access expression that
			// stands in for the or-block value, and appends any required
			// prefix stmts (typed temp init, optional-state branch, ...) to
			// the supplied `prefix_stmts` accumulator. When `prefix_stmts` is
			// non-empty the legacy arm wraps everything in an `UnsafeExpr`
			// (GCC compound expression): `[prefix_stmts..., ExprStmt(result)]`.
			// When empty, the data-access expression is the answer.
			//
			// Direct-emit mirrors that exactly: emit each prefix stmt via
			// `out.emit_stmt(...)` and assemble via the existing
			// `emit_unsafe_expr_by_ids` helper (the legacy UnsafeExpr wrapper
			// is constructed with no explicit pos, so direct-emit must pass
			// `token.Pos{}` — same as the LockExpr port). When prefix_stmts
			// is empty, the result_expr goes through `out.emit_expr(...)`
			// (leaf). Skips the `ast.UnsafeExpr` wrapper allocation per
			// occurrence in the compound-expression case.
			//
			// Session 62 refactor (inner ExprStmt wrapper-struct elision):
			// the legacy arm appended an `ast.ExprStmt{expr: result_expr}`
			// to prefix_stmts and routed it through `out.emit_stmt(...)`.
			// Direct-emit via `emit_expr_stmt_by_id` (vlib/v2/ast/flat.v:520)
			// over an already-flat `result_expr` id skips that wrapper
			// allocation per compound-expression OrExpr occurrence. Both the
			// legacy `ast.ExprStmt{expr: ...}` (no explicit pos) and
			// `emit_expr_stmt_by_id` use the default `token.Pos{}` — bit-equal.
			mut prefix_stmts := []ast.Stmt{}
			result_expr := t.expand_single_or_expr(expr, mut prefix_stmts)
			if prefix_stmts.len > 0 {
				mut stmt_ids := []ast.FlatNodeId{cap: prefix_stmts.len + 1}
				for s in prefix_stmts {
					stmt_ids << out.emit_stmt(s)
				}
				// Session 105 routing refactor (SelectorExpr direct-emit fall-
				// through inside the unsafe-wrap branch): when
				// `expand_single_or_expr` reaches its final return
				// (transformer.v:7221), it produces
				// `synth_selector(temp_ident, 'data', data_type)` — an
				// `ast.SelectorExpr{lhs: Ident{name: temp_name}, rhs:
				// Ident{name: 'data'}, pos: synth_pos}` (struct.v:28-38).
				// This is the dominant outcome shape for the OrExpr arm
				// (every non-array/non-map/non-sql/non-native-string-range
				// `expr or { ... }` reaches it), and prefix_stmts is always
				// populated by then (the temp decl + IfExpr-on-error are
				// pushed earlier). Direct-emit via session 7's
				// `emit_selector_expr_by_ids` (flat.v:959) skips the
				// `ast.SelectorExpr` wrapper-struct allocation per such
				// occurrence. Both lhs and rhs are simple Idents so leaf-
				// encode via `out.emit_expr` is bit-equal to
				// `add_expr(SelectorExpr)`'s `push_expr` walk. Same template
				// as sessions 97-104 (routing-pass shape extensions).
				// Reachability is high — `expr or { ... }` on any
				// Result/Option (function calls, method calls, ...) hits
				// this path; this is the most common single shape in the
				// OrExpr arm. Non-SelectorExpr shapes (Ident from map-or
				// eval path, PrefixExpr `typed_deref` from map-or control-
				// flow path) leaf-fallback through `out.emit_expr`.
				// Session 106 routing refactor (Ident direct-emit fall-
				// through, sibling of s105 inside the unsafe-wrap branch):
				// when `expand_single_or_expr` routes through
				// `try_expand_map_index_or` (transformer.v:7401), `m[k] or
				// { fallback }` lowers to one of two Ident-returning paths:
				// (a) eval backend (transformer.v:7464) — pushes a decl-
				// assign + IfExpr-on-`key_in_map` and returns the synthesised
				// `temp_ident` (`ast.Ident{name: temp_name}`); (b) non-
				// control-flow fallback (transformer.v:7562) — pushes a get-
				// check call decl, a fallback-value decl, and an IfExpr-on-
				// `_t1 != nil` and returns the `result_ident`
				// (`ast.Ident{name: result_temp}`). Direct-emit via
				// `emit_ident_by_name` (flat.v:784) skips the `ast.Ident`
				// wrapper-struct allocation per `m[k] or { ... }`
				// occurrence. Same template as s105 (SelectorExpr fall-
				// through) and sessions 73-104 (routing-pass shape
				// extensions). Reachability is moderate — map-or patterns
				// are common in compiler-style code for default-on-miss
				// lookups (`scope.lookup_var(name) or { '' }`, etc.).
				// Session 107 routing refactor (PrefixExpr `typed_deref`
				// direct-emit fall-through, sibling of s105/s106 inside the
				// same unsafe-wrap branch): when `expand_single_or_expr`
				// routes through `try_expand_map_index_or` (transformer.v:
				// 7401) and the or-block has control flow (return / break /
				// continue, transformer.v:7567), the helper returns
				// `t.typed_deref(temp_ident, value_type)` — an
				// `ast.PrefixExpr{op: .mul, expr: CastExpr{typ:
				// PrefixExpr{.amp, value_type}, expr: temp_ident}}`
				// (transformer.v:7229-7244). Direct-emit via
				// `emit_prefix_expr_by_id` (flat.v:821) skips the outer
				// `ast.PrefixExpr` wrapper-struct allocation per `m[k] or
				// { return ... }` (or break/continue) occurrence — the
				// inner CastExpr leaf-encodes via `out.emit_expr` exactly
				// as `add_expr(PrefixExpr)`'s `push_expr` walk. Same
				// template as s105/s106. Reachability is moderate — map-
				// or-with-control-flow patterns are common in compiler
				// code for early-exit on missing keys.
				// Session 110 routing refactor (CallExpr direct-emit fall-
				// through, sibling of s105/s106/s107 inside the same unsafe-
				// wrap branch): when `expand_single_or_expr` routes through
				// `expand_string_range_or_native_expr` (transformer.v:11591 —
				// native-backend `s[a..b] or { ... }` path) and the or-block
				// contains side-effect statements (transformer.v:11617-11622
				// pushes `or_side_stmts` onto prefix_stmts), prefix_stmts is
				// non-empty and the helper returns `ast.CallExpr{lhs:
				// Ident{name: 'string__substr_or'}, args: [s, start, end,
				// fallback]}` with already-transformed args. Direct-emit via
				// session 44's `emit_call_expr_by_ids` skips the
				// `ast.CallExpr` wrapper-struct allocation per `s[a..b] or
				// { eprintln('bad'); '' }` (or any side-effect-with-fallback)
				// occurrence on native backends. Same template as session
				// 109 (CallExpr direct-emit in OrExpr's empty-prefix branch)
				// — together they cover both reachable paths for the
				// string-range-or CallExpr shape. Reachability is low —
				// string-range-or with side-effect or-blocks is uncommon,
				// and only fires on native (arm64/x64) backends — but the
				// arm is bit-equal and keeps parity with the empty-prefix
				// sibling.
				result_id := if result_expr is ast.SelectorExpr {
					lhs_id := out.emit_expr(result_expr.lhs)
					rhs_id := out.emit_expr(ast.Expr(result_expr.rhs))
					out.emit_selector_expr_by_ids(lhs_id, rhs_id, result_expr.pos)
				} else if result_expr is ast.Ident {
					out.emit_ident_by_name(result_expr.name, result_expr.pos)
				} else if result_expr is ast.PrefixExpr {
					inner_id := out.emit_expr(result_expr.expr)
					out.emit_prefix_expr_by_id(result_expr.op, inner_id, result_expr.pos)
				} else if result_expr is ast.CallExpr {
					lhs_id := out.emit_expr(result_expr.lhs)
					mut arg_ids := []ast.FlatNodeId{cap: result_expr.args.len}
					for a in result_expr.args {
						arg_ids << out.emit_expr(a)
					}
					out.emit_call_expr_by_ids(lhs_id, arg_ids, result_expr.pos)
				} else {
					out.emit_expr(result_expr)
				}
				stmt_ids << out.emit_expr_stmt_by_id(result_id)
				return out.emit_unsafe_expr_by_ids(stmt_ids, token.Pos{})
			}
			// Session 97 routing refactor (IfExpr direct-emit fall-through):
			// when `expand_single_or_expr` routes through
			// `try_expand_array_index_or` (transformer.v:7261), `arr[idx]
			// or { fallback }` / `str[idx] or { fallback }` lower to
			// `if idx < arr.len { arr[idx] } else { fallback }` — a fully
			// constructed `ast.IfExpr` with already-transformed cond/stmts/
			// else_expr and no prefix_stmts pushed (the helper's
			// `_prefix_stmts` parameter is unused). Direct-emit via
			// session 45's `emit_if_expr_by_ids` skips the `ast.IfExpr`
			// wrapper-struct allocation per `arr[idx] or { ... }`
			// occurrence. Same template as sessions 73-96 (routing-pass
			// shape extensions). Reachability is moderate — array/string
			// index-with-or patterns are common in parser/checker code
			// for bounds-safe lookup with fallback.
			if result_expr is ast.IfExpr {
				cond_id := out.emit_expr(result_expr.cond)
				else_id := out.emit_expr(result_expr.else_expr)
				mut stmt_ids := []ast.FlatNodeId{cap: result_expr.stmts.len}
				for s in result_expr.stmts {
					stmt_ids << out.emit_stmt(s)
				}
				return out.emit_if_expr_by_ids(cond_id, else_id, stmt_ids, result_expr.pos)
			}
			// Session 99 routing refactor (SqlExpr direct-emit fall-through):
			// when `expand_single_or_expr` early-returns the SqlExpr branch
			// (transformer.v:6932-6934 — non-create SqlExpr), the helper
			// returns `transform_expr(SqlExpr)` which routes through
			// `transform_sql_expr` (orm.v:11) and returns an identity-shape
			// `ast.SqlExpr{expr: transform_expr(inner), table_name,
			// is_count, is_create, pos}` — `expr.expr` is already
			// transformed. Direct-emit via `emit_sql_expr_by_id`
			// (flat.v:883) with `out.emit_expr(result_expr.expr)` for the
			// inner skips the `ast.SqlExpr` wrapper-struct allocation per
			// `sql ... { ... } or { ... }` occurrence. Same template as
			// sessions 73-98 (routing-pass shape extensions). Reachability
			// is very low — sql-or-block is rare — but the dispatch arm
			// is bit-equal and keeps parity with the sibling SqlExpr arm
			// (session 47).
			if result_expr is ast.SqlExpr {
				expr_id := out.emit_expr(result_expr.expr)
				return out.emit_sql_expr_by_id(result_expr.table_name, result_expr.is_count,
					result_expr.is_create, expr_id, result_expr.pos)
			}
			// Session 109 routing refactor (CallExpr direct-emit fall-
			// through, empty-prefix branch): when `expand_single_or_expr`
			// routes through `expand_string_range_or_native_expr`
			// (transformer.v:11591 — native-backend `s[a..b] or { ... }`
			// path) and the or-block contains no side-effect statements
			// (only a fallback value, transformer.v:11617-11622), the
			// helper pushes nothing onto prefix_stmts and returns
			// `ast.CallExpr{lhs: Ident{name: 'string__substr_or'}, args:
			// [s, start, end, fallback]}` (transformer.v:11625-11630) with
			// already-transformed args. With prefix_stmts empty the result
			// reaches the post-empty-prefix dispatch. Direct-emit via
			// session 44's `emit_call_expr_by_ids` skips the
			// `ast.CallExpr` wrapper-struct allocation per `s[a..b] or
			// { 'fallback' }` occurrence on native backends. Same
			// template as sessions 73-108 (routing-pass shape
			// extensions). Reachability is low — string-range-or with no
			// side effects is uncommon, and only fires on native
			// (arm64/x64) backends.
			if result_expr is ast.CallExpr {
				lhs_id := out.emit_expr(result_expr.lhs)
				mut arg_ids := []ast.FlatNodeId{cap: result_expr.args.len}
				for a in result_expr.args {
					arg_ids << out.emit_expr(a)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, result_expr.pos)
			}
			return out.emit_expr(result_expr)
		}
		ast.LockExpr {
			// LockExpr's `transform_expr` arm rewrites a `lock x { body }` /
			// `rlock x { body }` into a sequence of mutex lock/unlock calls
			// wrapped in an UnsafeExpr (compound expression). The wrapper
			// `ast.UnsafeExpr` is constructed with the default zero pos in
			// legacy code, so direct-emit must also pass `token.Pos{}` to the
			// `emit_unsafe_expr_by_ids` helper (not `expr.pos` — that would
			// diverge from the legacy encoding).
			//
			// `expand_lock_expr` returns [lock_calls..., body_stmts...,
			// unlock_calls...]. When used as a value expression (GCC compound
			// expr), the last stmt's value is returned — but the unlock calls
			// come after the body, so the compound returns void. The duplicate-
			// last-body-stmt fix (same as legacy) re-inserts the body's tail
			// stmt after the unlock calls. Direct-emit mirrors that exactly,
			// then emits each stmt via `out.emit_stmt(...)` and assembles via
			// `emit_unsafe_expr_by_ids` — the same helper UnsafeExpr session
			// uses, since the legacy code produces an UnsafeExpr here. Skips
			// the `ast.UnsafeExpr` wrapper struct allocation per occurrence.
			mut stmts := t.expand_lock_expr(expr)
			n_unlocks := expr.lock_exprs.len + expr.rlock_exprs.len
			if n_unlocks > 0 && stmts.len > n_unlocks {
				body_end := stmts.len - n_unlocks
				stmts << stmts[body_end - 1]
			}
			mut stmt_ids := []ast.FlatNodeId{cap: stmts.len}
			for s in stmts {
				stmt_ids << out.emit_stmt(s)
			}
			return out.emit_unsafe_expr_by_ids(stmt_ids, token.Pos{})
		}
		ast.UnsafeExpr {
			// UnsafeExpr's `transform_expr` arm has two paths:
			//   1. `unsafe { nil }` is normalised to a plain `nil` Ident — the
			//      backend then handles it as a pointer-null literal. Detect
			//      that via `t.is_unsafe_nil_expr(expr)` and emit a leaf Ident
			//      (identity in `transform_expr`'s `else { expr }` case).
			//   2. otherwise transform `expr.stmts` via `transform_stmts` (the
			//      body driver — still allocates a `[]Stmt`) and rebuild the
			//      wrapper. Direct-emit recurses through `transform_stmts` +
			//      `out.emit_stmt(...)` per result and assembles via the new
			//      `emit_unsafe_expr_by_ids` helper. Mirrors
			//      `add_expr(UnsafeExpr)` encoding exactly: edges are the body
			//      stmts in order. Skips the `ast.UnsafeExpr` struct allocation
			//      per non-nil occurrence.
			//
			// Session 63 refactor: the `unsafe { nil }` branch previously
			// synthesised `ast.Ident{name: 'nil', pos: expr.pos}` and routed
			// through `out.emit_expr(ast.Expr(...))`. Direct-emit via the new
			// `emit_ident_by_name` helper skips the Ident wrapper allocation
			// per `unsafe { nil }` occurrence.
			if t.is_unsafe_nil_expr(expr) {
				return out.emit_ident_by_name('nil', expr.pos)
			}
			stmt_ids := t.transform_stmts_to_flat(expr.stmts, mut out)
			return out.emit_unsafe_expr_by_ids(stmt_ids, expr.pos)
		}
		ast.AsCastExpr {
			// AsCastExpr's `transform_expr` arm mutates Transformer state
			// (smartcast stack snapshot) — the only port so far that does.
			// To keep the `as` cast intact (a same-expression smartcast would
			// otherwise rewrite the operand into a direct sum payload access
			// and hide the original storage type from the backend), the arm
			// snapshots `smartcast_stack` + `smartcast_expr_counts`, drains
			// matching smartcast entries via a bounded `remove_smartcast_for_expr`
			// loop, transforms the inner expression with the drained state, then
			// restores the snapshot. Direct-emit mirrors that exactly — same
			// drain prologue, recursive `transform_expr_to_flat` for the inner,
			// same restore epilogue — then assembles the flat node via the new
			// `emit_as_cast_expr_by_ids` helper. `typ` is copied verbatim (NOT
			// transformed by `transform_expr`) so it goes through the leaf
			// `out.emit_expr(...)`. Skips the `ast.AsCastExpr` struct allocation
			// per occurrence.
			expr_key := t.expr_to_string(expr.expr)
			saved_smartcast_stack := t.smartcast_stack.clone()
			saved_smartcast_expr_counts := t.smartcast_expr_counts.clone()
			for _ in 0 .. smartcast_search_limit {
				if _ := t.remove_smartcast_for_expr(expr_key) {
					continue
				}
				break
			}
			inner_id := t.transform_expr_to_flat(expr.expr, mut out)
			t.smartcast_stack = saved_smartcast_stack.clone()
			t.smartcast_expr_counts = saved_smartcast_expr_counts.clone()
			typ_id := out.emit_expr(expr.typ)
			return out.emit_as_cast_expr_by_ids(inner_id, typ_id, expr.pos)
		}
		ast.AssocExpr {
			// AssocExpr port: `transform_expr`'s arm is a single helper call
			// — `t.lower_assoc_expr(expr, false)` always lowers the struct-
			// update syntax `{base | field: val}` into a sequence of stmts
			// (decl-assign of a typed tmp, field assignments) hoisted into
			// `t.pending_stmts`, returning either the tmp Ident or
			// `&tmp_ident` PrefixExpr as the value expression. The body
			// driver `transform_stmts` drains the pending stmts as prefix
			// stmts before the current stmt.
			//
			// Direct-emit calls `lower_assoc_expr` and routes the result
			// (Ident or `&Ident`) through the leaf `out.emit_expr(...)` —
			// same as the `else` fallback would, minus the `transform_expr`
			// match dispatch on AssocExpr. The win is purely the dispatch
			// skip, but the port removes one fallback path from the `else`
			// branch and demonstrates the "always-lowers via single helper
			// call" template that MatchExpr (lowers to IfExpr via
			// `transform_match_expr`), GenericArgOrIndexExpr (already
			// ported in session 33), and similar arms share. The returned
			// Ident/`&Ident` doesn't benefit from re-routing through
			// `transform_expr_to_flat` — Idents are leaf direct-emit via
			// `out.emit_expr` already, and `&Ident` would hit PrefixExpr's
			// `.amp` legacy fallback (no flat helper applies). Reachability
			// is moderate — struct-update syntax is used across the
			// compiler for AST node copying.
			//
			// Session 96 routing refactor (Ident direct-emit): the AssocExpr
			// arm always passes `take_addr=false` to `lower_assoc_expr`, so
			// the helper always returns the synthesised tmp `ast.Ident{name,
			// pos}` from `typed_temp_ident` (transformer.v:9514). Direct-
			// emit via `emit_ident_by_name` (flat.v:784) skips the
			// `ast.Ident` wrapper-struct allocation per assoc-expr
			// occurrence. Same template as sessions 73-95 (routing-pass
			// shape extensions). The non-Ident fall-through is currently
			// unreachable (take_addr=false branch never hits the `&Ident`
			// PrefixExpr return path) but kept for parity with sibling
			// routing arms — if `lower_assoc_expr` ever grows another
			// outcome shape, this leaf fallback catches it.
			result := t.lower_assoc_expr(expr, false)
			if result is ast.Ident {
				return out.emit_ident_by_name(result.name, result.pos)
			}
			return out.emit_expr(result)
		}
		ast.IfExpr {
			// IfExpr port: `transform_expr`'s arm is a thin wrapper around
			// `transform_if_expr` (expr.v:1268), the largest control-flow
			// helper in the transformer. It has many outcome shapes:
			//   - &&-chain normalisation with embedded `is` checks → recursive
			//     `transform_if_expr(outer_if / inner_if)` rebuild (IfExpr).
			//   - smartcast hoisting (`if x is T && rest { ... }`) → IfExpr
			//     with smartcast applied to the nested then-branch.
			//   - sumtype tag/_type_id rewrite for `is`/`!is` cond → IfExpr.
			//   - matched smartcast / type-narrowed branches → IfExpr with
			//     hoisted decl-assigns + transformed stmts/else.
			//   - direct option/result unwrap (`if x is none`) → IfExpr or
			//     UnsafeExpr lowering with deref + tag check.
			//   - default → `IfExpr{cond: transform(cond), stmts:
			//     transform_stmts(stmts), else_expr: transform(else), pos}`
			//     identity-shape rebuild.
			//   - value-position lowering: `if_expr_is_value(transformed_if)`
			//     and not skipped → `lower_if_expr_value(transformed_if)`
			//     hoists a temp decl-assign per branch, returning an Ident
			//     (NOT an IfExpr). Backends that don't support statement-
			//     expressions get a simpler shape.
			//
			// Two-shape template (sessions 36 SqlExpr, 40 MapInitExpr,
			// 41 ArrayInitExpr, 43 InfixExpr, 44 CallExpr):
			//   (a) result is `ast.IfExpr` → direct-emit via the new
			//       `emit_if_expr_by_ids` helper. Encode the already-
			//       transformed `cond` + `else_expr` via leaf `out.emit_expr`,
			//       and each `stmt` via leaf `out.emit_stmt` (helper already
			//       transformed the body via `transform_stmts` —
			//       `transform_stmt_to_flat` would double-transform and break
			//       parity for hoisted smartcast / pending_stmts ordering).
			//   (b) result is some other shape (lowered value-position
			//       IfExpr → Ident/Block chain, or UnsafeExpr from option
			//       unwrap) → route through leaf `out.emit_expr(...)`.
			//
			// Reachability: high. `if` is one of the two main control-flow
			// constructs in V; every `if cond { ... } else { ... }` passes
			// through this arm. The dispatch-skip win applies on every
			// occurrence; the wrapper-allocation skip applies on the
			// identity branch (statement-position ifs and ifs without value
			// lowering — most of them in compiler-style code).
			//
			// Completes phase 4's expression-arm port sweep — with this
			// landing, every non-leaf arm in `transform_expr_to_flat` has a
			// dedicated port. The `else { return out.emit_expr(
			// t.transform_expr(expr)) }` fallback is dead code (next
			// follow-up: remove it + the related per-arm wrappers in
			// `KeywordOperator`/`GenericArgs` that still route through the
			// legacy round-trip for state-dependent rewrites).
			//
			// Session 78 refactor: extends the dispatch with a third
			// shape — `ast.UnsafeExpr` (from `transform_if_expr`'s
			// option/result if-guard lowering paths at expr.v:1877 and
			// expr.v:1968). Those paths build a 2-stmt UnsafeExpr:
			// `[AssignStmt(decl_assign of tmp = transform(rhs)),
			// ExprStmt(modified_if with transformed body+else)]`. The
			// stmts are already-transformed, so leaf-encoding each via
			// `out.emit_stmt` is bit-equal to `add_stmt(UnsafeExpr)`'s
			// `push_stmt` walk. Direct-emit via session 19's
			// `emit_unsafe_expr_by_ids` skips the `ast.UnsafeExpr`
			// wrapper-struct allocation per if-guard occurrence. Same
			// template as session 77 (sumtype InitExpr fall-through).
			result := t.transform_if_expr(expr)
			if result is ast.IfExpr {
				cond_id := out.emit_expr(result.cond)
				else_id := out.emit_expr(result.else_expr)
				mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
				for s in result.stmts {
					stmt_ids << out.emit_stmt(s)
				}
				return out.emit_if_expr_by_ids(cond_id, else_id, stmt_ids, result.pos)
			}
			if result is ast.UnsafeExpr {
				mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
				for s in result.stmts {
					stmt_ids << out.emit_stmt(s)
				}
				return out.emit_unsafe_expr_by_ids(stmt_ids, result.pos)
			}
			// Session 91 routing refactor (Ident direct-emit fall-
			// through): when `transform_if_expr`'s value-position
			// lowering fires (expr.v:2116), `lower_if_expr_value`
			// (if.v:1083) hoists a `_if_t<N> := if cond { a } else
			// { b }` decl-assign onto `t.pending_stmts` and returns
			// the temp `ast.Ident{name: tmp_name}` (no explicit pos
			// — `token.Pos{}`). Direct-emit via session-prior
			// `emit_ident_by_name` (flat.v:784) skips the
			// `ast.Ident` wrapper-struct allocation per value-
			// position IfExpr occurrence. Same template as sessions
			// 73-90 (routing-pass shape extensions). Reachability is
			// high — every `x := if cond { a } else { b }` outside
			// statement position triggers this path on non-eval
			// backends.
			if result is ast.Ident {
				return out.emit_ident_by_name(result.name, result.pos)
			}
			return out.emit_expr(result)
		}
		ast.MatchExpr {
			// MatchExpr port: `transform_expr`'s arm is a single helper call
			// — `t.transform_match_expr(expr)` always lowers to an IfExpr
			// (sum-type match smartcasts each branch, then chains
			// `is`/`==`/`in` IfBranches; non-sum match chains `==`/`in`
			// IfBranches directly). The lowered IfExpr's children are
			// already transformed inside `transform_match_expr`:
			// `transform_match_branch_stmts(branch.stmts)` per branch +
			// `transform_expr(expr.expr)` on the matched expression which
			// feeds `build_match_branch_cond`. The IfExpr itself is NOT
			// run through `transform_if_expr` (legacy `transform_expr`
			// returns it directly from the MatchExpr arm), so routing
			// through `transform_expr_to_flat`'s IfExpr arm would
			// double-transform via the re-entrant `transform_if_expr`
			// call.
			//
			// Session 46 refactor: now that session 45 landed
			// `emit_if_expr_by_ids`, switch from leaf-encoding the IfExpr
			// (which still allocated the wrapper struct one extra time
			// inside `add_expr(IfExpr)`'s `push_expr` / `push_stmt` walk)
			// to direct-emitting via the helper. The IfExpr result
			// `is ast.IfExpr` guard handles the edge case in
			// `lower_match_expr_to_if` where `branches.len == 0` returns
			// `ast.empty_expr` (no branches → no if; leaf-encode the
			// EmptyExpr instead).
			//
			// Pattern note: sibling of AssocExpr (session 34) — same
			// "always-lowers via single helper call" template, now with a
			// dedicated downstream emit helper available. Same trick can
			// be applied to CallOrCastExpr (session 42, downstream is
			// CallExpr → `emit_call_expr_by_ids`) and SqlExpr's lowered
			// CallExpr branch (session 36) in follow-up routing passes.
			// Reachability is high — `match` is one of the most common
			// constructs in the compiler itself.
			//
			// Session 111 refactor: extract `transform_match_expr_parts`
			// (the matched-expr + transformed-branch-list inputs to
			// `lower_match_expr_to_if`) and add a flat-builder mirror
			// `lower_match_expr_to_if_flat` that emits the nested IfExpr
			// chain straight via `emit_if_expr_by_ids`. The legacy helper
			// allocates one `ast.IfExpr` struct per branch, then the
			// outer-most IfExpr is leaf-routed back here and a single
			// wrapper elision via `emit_if_expr_by_ids` only skipped the
			// OUTERMOST struct — the inner N-1 IfExpr wrappers still
			// allocated. The new path skips ALL of them: zero IfExpr
			// wrapper allocations from the lowering step regardless of
			// branch count. Same `_parts` extraction template as
			// `transform_fn_decl_parts` (session 4), `transform_return_stmt_parts`
			// (session 59), `transform_assign_stmt_parts` (session 60).
			match_expr, branches := t.transform_match_expr_parts(expr)
			return t.lower_match_expr_to_if_flat(match_expr, branches, mut out)
		}
		ast.SqlExpr {
			// SqlExpr port: `transform_expr`'s arm is a thin wrapper around
			// `transform_sql_expr` (orm.v), which has two outcome shapes:
			//   (a) `is_create` AND `lower_sql_create_expr` finds the table
			//       struct → lowers to a CallExpr `expr.create(table_name,
			//       table_fields)`, then runs that CallExpr through
			//       `transform_expr` and returns it. The returned expr is no
			//       longer a SqlExpr — route via leaf `out.emit_expr(...)`
			//       (the lowered CallExpr has already been transformed).
			//   (b) otherwise (read query, count, or create with missing
			//       table) → identity-shape rebuild: `SqlExpr{expr:
			//       transform_expr(expr.expr), table_name, is_count,
			//       is_create, pos}`. Direct-emit transforms `expr.expr`
			//       via `transform_expr_to_flat` and assembles via the new
			//       `emit_sql_expr_by_id(table_name, is_count, is_create,
			//       expr_id, pos)` helper. Skips the `ast.SqlExpr` wrapper
			//       struct allocation per occurrence.
			//
			// To avoid duplicating the table-lookup work between the legacy
			// helper and the direct-emit path (and to preserve the exact
			// "rebuild on lookup miss" semantics), call `transform_sql_expr`
			// once and inspect the result: if it's still a SqlExpr, take the
			// direct-emit path on that (already-transformed) SqlExpr; if
			// it's anything else (the lowered CallExpr from branch (a)),
			// route through the leaf `out.emit_expr(...)`.
			//
			// Pattern note: sibling of AssocExpr (session 34) / MatchExpr
			// (session 35) at the expr level — all three are "always-lowers
			// via single helper call" in legacy `transform_expr`. SqlExpr
			// adds a twist: the helper has two outcome shapes (rewrite-to-
			// CallExpr OR identity rebuild) and we want the direct-emit win
			// on the identity branch. Detecting the shape post-helper keeps
			// the lookup logic single-sourced. Reachability is limited to
			// `sql ... { ... }` blocks; the port removes one fallback path
			// from the `else` branch.
			result := t.transform_sql_expr(expr)
			if result is ast.SqlExpr {
				// `result.expr` is already transformed by `transform_sql_expr`
				// — route through the leaf `out.emit_expr(...)` (pure
				// add_expr encoder, no re-transform) for the inner expr to
				// match `add_expr(SqlExpr{expr: transformed_expr})` exactly.
				expr_id := out.emit_expr(result.expr)
				return out.emit_sql_expr_by_id(result.table_name, result.is_count,
					result.is_create, expr_id, result.pos)
			}
			// Session 48 routing refactor: the lowered branch of
			// `transform_sql_expr` (is_create + table lookup hit) routes
			// through `lower_sql_create_expr`, which calls
			// `t.transform_expr(call)` on a synthesized CallExpr — the
			// result is whatever `transform_call_expr` produces, typically
			// an `ast.CallExpr` with already-transformed args. Direct-emit
			// via `emit_call_expr_by_ids` (session 44) instead of
			// leaf-encoding skips the wrapper allocation. Same template as
			// session 47 (CallOrCastExpr routing refactor). Other lowered
			// shapes still route through leaf `out.emit_expr(result)`.
			if result is ast.CallExpr {
				lhs_id := out.emit_expr(result.lhs)
				mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
				for a in result.args {
					arg_ids << out.emit_expr(a)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
			}
			return out.emit_expr(result)
		}
		ast.InfixExpr {
			// InfixExpr port: `transform_expr`'s arm is a thin wrapper
			// around `transform_infix_expr` (expr.v:2124), which has many
			// outcome shapes that collapse into "identity InfixExpr" vs
			// "lowered different shape":
			//   - `&&`-chain smartcast: `join_and_terms(terms)` rebuilds
			//     an InfixExpr chain — identity shape.
			//   - sum-type tag comparison (`is`/`!is`/`==`/`!=` against a
			//     variant) → `make_infix_expr_at(...)` over `_tag` /
			//     `_type_id` — identity shape (InfixExpr).
			//   - string literal concat folding → StringLiteral.
			//   - string + string → CallExpr to `string__plus`.
			//   - operator overload → CallExpr to the user-defined `<op>`
			//     method.
			//   - alias arithmetic, smartcast-of-lhs/rhs, fixed-array
			//     concat, etc. → various CallExpr / IndexExpr / etc.
			//   - default → `InfixExpr{op, lhs: transform(lhs), rhs:
			//     transform(rhs), pos}` — identity shape.
			//
			// All identity-shape branches return an `ast.InfixExpr`.
			// Direct-emit calls `transform_infix_expr` once and dispatches
			// on result type: if `is ast.InfixExpr` → encode lhs/rhs via
			// leaf `out.emit_expr` (helper already transformed —
			// `transform_expr_to_flat` would double-transform) + assemble
			// via the new `emit_infix_expr_by_ids(op, lhs_id, rhs_id, pos)`
			// helper. Otherwise → route through leaf
			// `out.emit_expr(result)` so the lowered shape's encoding stays
			// in the legacy `add_expr` path (CallExpr/StringLiteral/etc.
			// don't yet have dedicated flat helpers wired at this dispatch
			// surface).
			//
			// Pattern note: extends "always-lowers via helper, two outcome
			// shapes" (sessions 36 SqlExpr, 40 MapInitExpr, 41
			// ArrayInitExpr). The identity branch is reached by the vast
			// majority of arithmetic / boolean / comparison infix
			// operations in V code — every `a + b`, `i < n`, `flag && x`,
			// `xs == ys`, ... — so the direct-emit win is broad.
			//
			// Reachability is extremely high — infix is the workhorse of
			// every function body. The dispatch-skip win applies on every
			// occurrence; the wrapper-allocation skip applies on every
			// identity-branch occurrence.
			//
			// Session 73 routing refactor (CallExpr direct-emit fall-through):
			// when `transform_infix_expr` lowers `string + string`, alias
			// arithmetic, or operator-overload `op` into a CallExpr,
			// direct-emit via `emit_call_expr_by_ids` (session 44) skips the
			// `ast.CallExpr` wrapper-struct allocation. Args/lhs are leaf-
			// encoded (helper already transformed). Same template as session
			// 47 / 48 / 49 / 50 / 51 / 54 / 55 / 56 — extend the result-shape
			// dispatch with a CallExpr arm. Reachability is high — operator-
			// overload calls and `string + string` are common.
			// Session 82 routing refactor (StringLiteral direct-emit
			// fall-through): when `transform_infix_expr`'s string-
			// literal concat folding path (expr.v:2241-2248) fires
			// — `folded_string_literal_concat(expr)` collapses
			// adjacent `.v` StringLiteral operands into one — the
			// return shape is `ast.StringLiteral{kind: .v, value:
			// literal, pos: expr.pos}`. Direct-emit via session 69's
			// `emit_string_literal_by_value` skips the
			// `ast.StringLiteral` wrapper-struct allocation per
			// folded-literal occurrence. Same template as sessions
			// 73-81 (extend result-shape dispatch with another arm).
			// Reachability is moderate — literal-only string concats
			// in V code (compile-time-constant prefixes, error message
			// assembly, generated codepaths).
			result := t.transform_infix_expr(expr)
			if result is ast.InfixExpr {
				lhs_id := out.emit_expr(result.lhs)
				rhs_id := out.emit_expr(result.rhs)
				return out.emit_infix_expr_by_ids(result.op, lhs_id, rhs_id, result.pos)
			}
			if result is ast.CallExpr {
				lhs_id := out.emit_expr(result.lhs)
				mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
				for a in result.args {
					arg_ids << out.emit_expr(a)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
			}
			if result is ast.StringLiteral {
				return out.emit_string_literal_by_value(result.kind, result.value, result.pos)
			}
			// Session 88 routing refactor (PrefixExpr direct-emit fall-
			// through): when `transform_infix_expr` lowers `x not_in
			// [a, b, c]` (expr.v:2470) into a `!`-prefixed chain of
			// `or` equality checks, the helper returns
			// `ast.PrefixExpr{op: .not, expr: chain, pos}` where
			// `chain` is an already-transformed InfixExpr `or`-chain.
			// Direct-emit via session 58's `emit_prefix_expr_by_id`
			// skips the `ast.PrefixExpr` wrapper-struct allocation per
			// `not_in [arr]` occurrence. Same template as sessions
			// 73-87 (routing-pass shape extensions). Reachability is
			// moderate — `not_in` against inline array literals is
			// common in negation-style guards.
			if result is ast.PrefixExpr {
				inner_id := out.emit_expr(result.expr)
				return out.emit_prefix_expr_by_id(result.op, inner_id, result.pos)
			}
			// Session 89 routing refactor (UnsafeExpr direct-emit fall-
			// through): when `transform_infix_expr` lowers `x in {map}`
			// over a non-addressable map literal (expr.v:2439), the
			// helper synthesizes a temp-keyed `map__exists(...)` call
			// wrapped in `ast.UnsafeExpr{stmts: key_stmts, pos}` so
			// codegen emits a GCC compound expression. Direct-emit via
			// session 19's `emit_unsafe_expr_by_ids` skips the
			// `ast.UnsafeExpr` wrapper-struct allocation per `in {map}`
			// occurrence. Same template as sessions 73-88 (routing-pass
			// shape extensions). Reachability is low — inline map
			// literals on the RHS of `in` are uncommon — but the path
			// is still hot in fixtures that exercise `match k in {...}`.
			if result is ast.UnsafeExpr {
				mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
				for s in result.stmts {
					stmt_ids << out.emit_stmt(s)
				}
				return out.emit_unsafe_expr_by_ids(stmt_ids, result.pos)
			}
			return out.emit_expr(result)
		}
		ast.CallExpr {
			// CallExpr port: `transform_call_expr` (fn.v:1881) is the
			// largest single helper in the transformer — it lowers many
			// call shapes into non-CallExpr results (generic-math inline,
			// embed_file lowering, $d resolution, .filter()/.map()/.sort()
			// expansion, flag-enum methods, array contains/index/last_index,
			// smartcast hoisting, BasicLiteral elision, CastExpr for
			// bare-type calls, IfExpr / InitExpr for some intrinsics, etc.).
			// The default tail (fn.v:2603-2610) is identity-shape:
			//   `CallExpr{lhs: transform(lhs), args: [transform_call_arg(...)],
			//    pos}` — wrapped by `lower_missing_call_args` /
			//   `lower_variadic_args` / sumtype-checking arg transforms.
			//
			// Two-shape template (sessions 36 SqlExpr, 40 MapInitExpr,
			// 41 ArrayInitExpr, 43 InfixExpr):
			//   (a) result is `ast.CallExpr` → direct-emit via the new
			//       `emit_call_expr_by_ids` helper. Encode the already-
			//       transformed `lhs` + each `arg` via leaf `out.emit_expr`
			//       (the helper transformed them — re-routing through
			//       `transform_expr_to_flat` would double-transform and
			//       break parity for hoisted smartcast temps + variadic
			//       lowering).
			//   (b) result is some other shape → route through leaf
			//       `out.emit_expr(...)` so the lowering branches stay
			//       single-sourced.
			//
			// Reachability: extremely high. Every function call, method
			// call, intrinsic, and lowered-array/map operation passes
			// through this arm. The dispatch-skip win applies on every
			// occurrence; the wrapper-allocation skip applies on the
			// default-path identity branch (the most common case for
			// ordinary user-code calls).
			// Session 83 routing refactor (CastExpr direct-emit fall-
			// through): when `transform_call_expr`'s flag-enum
			// `zero()` method path (fn.v:2046) fires, the helper
			// returns `ast.CastExpr{typ: Ident{receiver_type}, expr:
			// BasicLiteral{kind: .number, value: '0'}, pos: expr.pos}`
			// — the C/cleanc-backend lowering of `Flags.zero()` into
			// a zero-initialised flag-enum value. Direct-emit via
			// `emit_cast_expr_by_ids` (session 12) skips the
			// `ast.CastExpr` wrapper-struct allocation per `zero()`
			// occurrence. Same template as sessions 73-82 (extend the
			// result-shape dispatch with another arm). Mirror of
			// session 76 (CallOrCastExpr's CastExpr fall-through).
			// Reachability is low — flag enums' `.zero()` is uncommon
			// in compiler-style code — but the dispatch arm is bit-
			// equal and keeps parity with the CallOrCastExpr arm.
			// Session 84 routing refactor (IfExpr direct-emit fall-
			// through): when `transform_call_expr` inlines a generic-
			// math intrinsic via `try_inline_generic_math_call`
			// (fn.v:4895) for `abs(x)` / `math.abs(x)` /
			// `make_inline_if_expr` (max/min), the helper returns an
			// `ast.IfExpr` (cond/stmts/else_expr already transformed
			// via `t.make_infix_expr` + `t.transform_expr(arg)`).
			// Direct-emit via `emit_if_expr_by_ids` (session 45)
			// skips the `ast.IfExpr` wrapper-struct allocation per
			// math-inline occurrence. Same template as sessions 73-83
			// (extend result-shape dispatch with another arm). Mirror
			// of the IfExpr arm's identity-shape direct-emit (session
			// 45). Reachability is moderate — `abs` / `max` / `min`
			// are common in math-heavy code (compiler arithmetic,
			// runtime size math, codegen offset computations).
			result := t.transform_call_expr(expr)
			if result is ast.CallExpr {
				lhs_id := out.emit_expr(result.lhs)
				mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
				for a in result.args {
					arg_ids << out.emit_expr(a)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
			}
			if result is ast.CastExpr {
				typ_id := out.emit_expr(result.typ)
				expr_id := out.emit_expr(result.expr)
				return out.emit_cast_expr_by_ids(typ_id, expr_id, result.pos)
			}
			if result is ast.IfExpr {
				cond_id := out.emit_expr(result.cond)
				else_id := out.emit_expr(result.else_expr)
				mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
				for s in result.stmts {
					stmt_ids << out.emit_stmt(s)
				}
				return out.emit_if_expr_by_ids(cond_id, else_id, stmt_ids, result.pos)
			}
			// Session 85 routing refactor (BasicLiteral direct-emit
			// fall-through): when `transform_call_expr` produces a
			// BasicLiteral result — two paths fire today:
			//   1. elided-fn calls (fn.v:2005): a method whose name
			//      is in `t.elided_fns` (conditionally-compiled
			//      `@[if verbose ?]` fns when the flag is off)
			//      collapses to `BasicLiteral{kind: .number, value:
			//      '0'}`.
			//   2. generic-math constants via
			//      `try_inline_generic_math_call` (fn.v:4895) →
			//      `maxof_constant` / `minof_constant` (fn.v:5150 /
			//      5183) return `BasicLiteral{kind: .number, value:
			//      <type-max/min>}` for `maxof[T]()` / `minof[T]()`
			//      at all numeric types except `i64` minof (which
			//      returns an InfixExpr — falls back to leaf-emit).
			// Both shapes have `pos: token.Pos{}` (no explicit pos
			// field). Direct-emit via session-prior
			// `emit_basic_literal_by_value` (flat.v:793) skips the
			// `ast.BasicLiteral` wrapper-struct allocation per
			// occurrence. Same template as sessions 73-84 (routing-
			// pass shape extensions). Reachability is moderate —
			// `maxof[T]()` / `minof[T]()` are common in compiler-
			// internal numeric overflow guards and ABI lowering.
			if result is ast.BasicLiteral {
				return out.emit_basic_literal_by_value(result.kind, result.value, result.pos)
			}
			// Session 101 routing refactor (Ident direct-emit fall-
			// through): when `transform_call_expr`'s filter/map/
			// any/all/count routing fires (fn.v:1897 →
			// `try_expand_filter_or_map_expr` at transformer.v:4943,
			// `expand_any_or_all_expr` at transformer.v:5226,
			// `expand_count_expr` at transformer.v:5340), each
			// helper hoists the loop expansion onto
			// `t.pending_stmts` and returns a `temp_ident`
			// (`ast.Ident{name: temp_name}`). `try_expand_filter_or_map_expr`
			// also sets `pos: temp_pos`; the any/all/count
			// variants leave pos defaulted (`token.Pos{}`).
			// Direct-emit via session-prior `emit_ident_by_name`
			// (flat.v:784) skips the `ast.Ident` wrapper-struct
			// allocation per `.filter` / `.map` / `.any` / `.all`
			// / `.count` receiver-method occurrence. Same template
			// as session 91 (IfExpr arm Ident fall-through for
			// `lower_if_expr_value` temp). Reachability is high —
			// `.filter` / `.map` / `.any` / `.all` / `.count` are
			// pervasive in stdlib and self-host code.
			if result is ast.Ident {
				return out.emit_ident_by_name(result.name, result.pos)
			}
			// Session 103 routing refactor (InfixExpr direct-emit fall-
			// through): when `try_inline_generic_math_call` (fn.v:4895)
			// fires for `minof[i64]()` / `minof_i64()` calls,
			// `minof_constant` (fn.v:5156-5167) returns the C `LLONG_MIN`
			// pattern as an `ast.InfixExpr{op: .minus, lhs:
			// PrefixExpr{.minus, BasicLiteral{'9223372036854775807'}},
			// rhs: BasicLiteral{'1'}}` to avoid C literal overflow on
			// the most-negative int64. Every other type's
			// `maxof_constant` / `minof_constant` return BasicLiteral
			// (covered by s85). Direct-emit via `emit_infix_expr_by_ids`
			// (session 43) skips the `ast.InfixExpr` wrapper-struct
			// allocation per `minof[i64]()` occurrence. Same template
			// as sessions 73-102 (routing-pass shape extensions).
			// Reachability is very low — `minof[i64]()` is rare — but
			// the dispatch arm is bit-equal and keeps parity with the
			// sibling BasicLiteral handling.
			if result is ast.InfixExpr {
				lhs_id := out.emit_expr(result.lhs)
				rhs_id := out.emit_expr(result.rhs)
				return out.emit_infix_expr_by_ids(result.op, lhs_id, rhs_id, result.pos)
			}
			return out.emit_expr(result)
		}
		ast.CallOrCastExpr {
			// CallOrCastExpr port: `transform_expr`'s arm is a thin wrapper
			// around `transform_call_or_cast_expr` (fn.v:3955), which
			// virtually always lowers a CallOrCastExpr (the parser's
			// ambiguity node for `Type(arg)` vs `func(arg)`) into a
			// different shape:
			//   - CallExpr (most paths: generic-math inline, filter/map
			//     expansion, sort lambda, flag-enum methods, array
			//     contains/index, smartcast methods, all module-prefixed
			//     and method-resolved calls, the "unresolved fallback"
			//     CallExpr at the very end, etc.)
			//   - CastExpr (when lhs is a known type expression)
			//   - BasicLiteral '0' (elided fn or `enum.zero()` for flag
			//     enums)
			//   - any other expression from `lower_call_or_cast_expr` /
			//     `t.transform_expr(t.transform_call_or_cast_expr(...))`
			//     recursion paths
			//
			// There's no identity-shape rebuild — every code path produces
			// a non-CallOrCastExpr. The direct-emit win is purely the
			// dispatch skip on the outer `transform_expr` arm; the lowered
			// result still routes through leaf `out.emit_expr(...)` (no
			// re-transform — `transform_call_or_cast_expr` already
			// transformed receivers/args; double-transforming would break
			// parity for smartcast hoisting and similar mutation).
			//
			// Pattern note: sibling of AssocExpr (session 34), MatchExpr
			// (35) — "always-lowers via single helper to a DIFFERENT
			// shape". Distinct from SqlExpr/MapInitExpr/ArrayInitExpr
			// (sessions 36/40/41) which have an identity branch worth
			// direct-emitting.
			//
			// Session 47 routing refactor: now that session 44 landed
			// `emit_call_expr_by_ids`, the most common downstream shape
			// (CallExpr — generic-math inline, filter/map expansion,
			// flag-enum methods, smartcast methods, all module-prefixed
			// and method-resolved calls, the "unresolved fallback"
			// CallExpr) can be direct-emitted via the helper instead of
			// leaf-encoded. Safety: every return path in
			// `transform_call_or_cast_expr` produces a CallExpr with
			// already-transformed args (`transform_call_arg_with_sumtype_check`,
			// `transform_expr`, etc.) — direct-emit encodes lhs+args via
			// leaf `out.emit_expr`, NO re-transform. Going through
			// `transform_expr_to_flat`'s CallExpr arm would re-run
			// `transform_call_expr` (the fn.v:112 comment "Do NOT route
			// through transform_call_expr because it would..." flags
			// exactly this hazard), so the routing skips the dispatch arm
			// and direct-emits instead.
			//
			// Reachability: CallOrCastExpr is the parser's ambiguous
			// `T(x)`/`f(x)` form, common for single-arg calls and casts. By
			// transformer-exit time every CallOrCastExpr becomes one of the
			// shapes above, so the dispatch-skip win applies broadly. The
			// new direct-emit win applies only to the CallExpr branch
			// (most paths); CastExpr / BasicLiteral / other shapes still
			// route through leaf `out.emit_expr(result)`.
			//
			// Session 76 routing refactor (CastExpr direct-emit fall-through):
			// the CastExpr branch (`transform_call_or_cast_expr` resolved a
			// type name → `Type(arg)`) returns a CastExpr with already-
			// transformed `typ` / `expr`. Direct-emit via
			// `emit_cast_expr_by_ids` (session 12) skips the `ast.CastExpr`
			// wrapper-struct allocation per occurrence. Same routing-pass
			// template as sessions 47-51 / 54 / 55 / 56 / 73 / 74 / 75 —
			// extend the dispatch with a CastExpr arm.
			// Session 86 routing refactor (BasicLiteral direct-emit
			// fall-through): when `transform_call_or_cast_expr`
			// resolves a method whose name is in `t.elided_fns`
			// (fn.v:3995 — conditionally-compiled `@[if verbose ?]`
			// fns when the flag is off), the call collapses to
			// `BasicLiteral{kind: .number, value: '0'}` with default
			// pos. Direct-emit via `emit_basic_literal_by_value`
			// (flat.v:793) skips the `ast.BasicLiteral` wrapper-
			// struct allocation per elided-fn occurrence. Same
			// template as sessions 73-85 (routing-pass shape
			// extensions). Mirror of session 85 (CallExpr arm
			// BasicLiteral fall-through). Reachability is low —
			// elided-fn calls fire only on conditional-compilation
			// hits, but the dispatch arm is bit-equal and keeps
			// parity with the CallExpr arm's BasicLiteral handling.
			result := t.transform_call_or_cast_expr(expr)
			if result is ast.CallExpr {
				lhs_id := out.emit_expr(result.lhs)
				mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
				for a in result.args {
					arg_ids << out.emit_expr(a)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
			}
			if result is ast.CastExpr {
				typ_id := out.emit_expr(result.typ)
				expr_id := out.emit_expr(result.expr)
				return out.emit_cast_expr_by_ids(typ_id, expr_id, result.pos)
			}
			if result is ast.BasicLiteral {
				return out.emit_basic_literal_by_value(result.kind, result.value, result.pos)
			}
			// Session 100 routing refactor (IfExpr direct-emit fall-through):
			// when `transform_call_or_cast_expr` inlines `abs(x)` or
			// `math.abs(x)` via `try_inline_generic_math_coce` (fn.v:5025),
			// the helper returns an `ast.IfExpr` with already-transformed
			// cond/stmts/else_expr (`make_infix_expr` + `transform_expr(arg)`
			// inside the synthesised if branches). Direct-emit via
			// `emit_if_expr_by_ids` (session 45) skips the `ast.IfExpr`
			// wrapper-struct allocation per `abs(x)` occurrence. Same
			// template as session 84 (CallExpr arm's IfExpr fall-through)
			// — mirror for the CallOrCastExpr ambiguity form. Reachability
			// is moderate — `abs` is common in math/arithmetic-heavy code
			// (codegen offset computations, runtime size math).
			if result is ast.IfExpr {
				cond_id := out.emit_expr(result.cond)
				else_id := out.emit_expr(result.else_expr)
				mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
				for s in result.stmts {
					stmt_ids << out.emit_stmt(s)
				}
				return out.emit_if_expr_by_ids(cond_id, else_id, stmt_ids, result.pos)
			}
			// Session 102 routing refactor (Ident direct-emit fall-
			// through): mirror of session 101 (CallExpr arm Ident
			// fall-through) for the CallOrCastExpr ambiguity form.
			// When `transform_call_or_cast_expr`'s filter/map
			// routing fires (fn.v:3961 → `try_expand_filter_or_map_expr`
			// at transformer.v:4943), the helper hoists the loop
			// expansion onto `t.pending_stmts` and returns the
			// synthesised `temp_ident` (`ast.Ident{name: temp_name,
			// pos: temp_pos}`). Direct-emit via `emit_ident_by_name`
			// (flat.v:784) skips the `ast.Ident` wrapper-struct
			// allocation per `.filter` / `.map` receiver-method
			// occurrence routed through the ambiguous form. Same
			// template as sessions 73-101 (routing-pass shape
			// extensions). Reachability is moderate — single-arg
			// `.filter(...)` / `.map(...)` may parse as
			// CallOrCastExpr rather than CallExpr.
			if result is ast.Ident {
				return out.emit_ident_by_name(result.name, result.pos)
			}
			// Session 104 routing refactor (InfixExpr direct-emit fall-
			// through): mirror of session 103 (CallExpr arm InfixExpr
			// fall-through) for the CallOrCastExpr ambiguity form.
			// When `try_inline_generic_math_coce` (fn.v:5025) fires
			// for `minof[i64]()` / `minof_i64()` calls parsed as
			// CallOrCastExpr, `minof_constant` (fn.v:5156-5167)
			// returns the C `LLONG_MIN` pattern as an
			// `ast.InfixExpr{op: .minus, lhs: PrefixExpr{.minus,
			// BasicLiteral{'9223372036854775807'}}, rhs:
			// BasicLiteral{'1'}}` to avoid C literal overflow on
			// the most-negative int64. Direct-emit via
			// `emit_infix_expr_by_ids` (session 43) skips the
			// `ast.InfixExpr` wrapper-struct allocation. Same
			// template as sessions 73-103 (routing-pass shape
			// extensions). Reachability is very low — `minof[i64]()`
			// parsed as CallOrCastExpr is rare — but the dispatch
			// arm is bit-equal and keeps parity with the sibling
			// BasicLiteral handling.
			if result is ast.InfixExpr {
				lhs_id := out.emit_expr(result.lhs)
				rhs_id := out.emit_expr(result.rhs)
				return out.emit_infix_expr_by_ids(result.op, lhs_id, rhs_id, result.pos)
			}
			return out.emit_expr(result)
		}
		ast.MapInitExpr {
			// MapInitExpr port: `transform_map_init_expr` (struct.v:1010) has
			// two outcome shapes:
			//   (a) eval backend → identity-shape rebuild:
			//       `MapInitExpr{typ: transform(typ), keys: [transform(k)...],
			//        vals: [transform(v)...], pos}`. Direct-emit encodes each
			//       child via leaf `out.emit_expr` (the helper already
			//       transformed them — `transform_expr_to_flat` would
			//       double-transform) and assembles via the new
			//       `emit_map_init_expr_by_ids`.
			//   (b) non-eval backend → lowers to a CallExpr (`new_map(...)`
			//       for empty literals, `new_map_init(...)` for keyed
			//       literals). Returned shape is NOT a MapInitExpr — route
			//       through leaf `out.emit_expr(...)`.
			//
			// Pattern note: extends the "always-lowers via helper, two
			// outcome shapes" template established by SqlExpr (session 36),
			// AssignStmt (37). Branch on result type — identity branch picks
			// up the direct-emit win; lowered branch single-sources the
			// `new_map` construction logic in the legacy helper.
			//
			// Reachability: map literals (`{}`, `{'a': 1}`) appear in
			// transformer/checker registries, runtime tests, and codegen
			// tables. Most v2 backend builds use the non-eval path
			// (lowering), so the identity-branch win applies only on the
			// eval backend; the dispatch-skip win applies to both.
			//
			// Session 75 routing refactor (CallExpr direct-emit fall-through):
			// branch (b) — non-eval backend `new_map` / `new_map_init`
			// lowering — returns a CallExpr with already-transformed args.
			// Direct-emit via `emit_call_expr_by_ids` (session 44) skips the
			// `ast.CallExpr` wrapper-struct allocation per occurrence. Same
			// template as sessions 47-51 / 54 / 55 / 56 / 73 / 74 — extend
			// the result-shape dispatch with a CallExpr arm. Reachability is
			// high on non-eval backends (most v2 builds).
			result := t.transform_map_init_expr(expr)
			if result is ast.MapInitExpr {
				typ_id := out.emit_expr(result.typ)
				mut key_ids := []ast.FlatNodeId{cap: result.keys.len}
				for k in result.keys {
					key_ids << out.emit_expr(k)
				}
				mut val_ids := []ast.FlatNodeId{cap: result.vals.len}
				for v in result.vals {
					val_ids << out.emit_expr(v)
				}
				return out.emit_map_init_expr_by_ids(typ_id, key_ids, val_ids, result.pos)
			}
			if result is ast.CallExpr {
				lhs_id := out.emit_expr(result.lhs)
				mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
				for a in result.args {
					arg_ids << out.emit_expr(a)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
			}
			return out.emit_expr(result)
		}
		ast.ArrayInitExpr {
			// ArrayInitExpr port: `transform_array_init_expr` (struct.v:291)
			// has many outcome shapes:
			//   (a) invalid data → identity (`return ast.Expr(expr)`).
			//   (b) fixed-size array (`[1, 2, 3]!`, `[5]int{}`) →
			//       identity-shape rebuild
			//       `ArrayInitExpr{typ: array_typ, exprs, init, cap, len, pos}`.
			//   (c) eval backend → identity-shape rebuild with transformed
			//       typ/init/cap/len/update_expr fields.
			//   (d) spread `[...base, e1, e2]` → CallExpr to
			//       `new_array_from_array_and_c_array(...)`.
			//   (e) empty dynamic array `[]int{}` →
			//       `__new_array_with_default_noscan(len, cap, sizeof(elem), init)`
			//       CallExpr; or a ForStmt expansion when `init` references
			//       `index`.
			//   (f) keyed dynamic array `[1, 2, 3]` →
			//       `new_array_from_c_array(...)` CallExpr.
			//
			// Branches (a)/(b)/(c) all return an `ast.ArrayInitExpr` —
			// direct-emit via the new `emit_array_init_expr_by_ids` helper,
			// skipping the wrapper allocation. The helper already transformed
			// each child, so encode via leaf `out.emit_expr` (no
			// re-transform via `transform_expr_to_flat`). Branches (d)/(e)/
			// (f) return a non-ArrayInitExpr shape — route through leaf
			// `out.emit_expr(...)` so the lowering logic stays
			// single-sourced.
			//
			// Pattern note: extends "always-lowers via helper, two outcome
			// shapes" (sessions 36 SqlExpr, 40 MapInitExpr) — the
			// "two-shape" template handles the multiple identity branches
			// uniformly via post-helper type dispatch. Unlike MapInitExpr
			// (only eval backend keeps the identity shape), ArrayInitExpr
			// keeps identity on every backend for fixed-size literals — so
			// the direct-emit win reaches every backend, not just eval.
			//
			// Reachability: very high — array literals (`[]`, `[1, 2, 3]`,
			// `[]int{}`, `[5]int{init: -1}`, `[1, 2, 3]!`) are ubiquitous in
			// V code. Most call/return-arg array literals on non-eval
			// backends hit branches (d)/(e)/(f), but fixed-size array
			// literals (common in lookup tables and configuration) hit (b)
			// identity-rebuild on every backend.
			//
			// Session 74 routing refactor (CallExpr direct-emit fall-through):
			// branches (d)/(e)/(f) — spread, empty dynamic, keyed dynamic —
			// all lower to a CallExpr (`new_array_from_array_and_c_array`,
			// `__new_array_with_default_noscan`, `new_array_from_c_array`)
			// with already-transformed args. Direct-emit via
			// `emit_call_expr_by_ids` (session 44) skips the `ast.CallExpr`
			// wrapper-struct allocation per occurrence. Same template as
			// sessions 47/48/49/50/51/54/55/56/73 — extend the result-shape
			// dispatch with a CallExpr arm. The Ident/PrefixExpr ForStmt-hoist
			// case (branch (e) when `init` references `index`) falls through
			// to leaf `out.emit_expr` since those are not CallExpr.
			// Reachability is very high — every `[1, 2, 3]` / `[]int{}` /
			// `[]int{len: n}` literal on non-eval backends hits this path.
			// Session 87 routing refactor (Ident direct-emit fall-
			// through): when `transform_array_init_expr` hoists an
			// `init`-with-`index` array literal into a ForStmt
			// (struct.v:2208-2268 — `[1, 2, 3]` where the `init`
			// expression references the synthesised `index`
			// identifier), the helper pushes the init AssignStmt +
			// ForStmt onto `t.pending_stmts` and returns the temp
			// `arr_ident` (an `ast.Ident{name, pos}` produced by
			// `typed_temp_ident`). Direct-emit via
			// `emit_ident_by_name` (flat.v:784) skips the `ast.Ident`
			// wrapper-struct allocation per for-hoist occurrence.
			// Same template as sessions 73-86 (routing-pass shape
			// extensions). Reachability is low — index-referencing
			// init expressions in array literals are rare — but bit-
			// equal and keeps the dispatch pattern consistent.
			result := t.transform_array_init_expr(expr)
			if result is ast.ArrayInitExpr {
				typ_id := out.emit_expr(result.typ)
				init_id := out.emit_expr(result.init)
				cap_id := out.emit_expr(result.cap)
				len_id := out.emit_expr(result.len)
				update_expr_id := out.emit_expr(result.update_expr)
				mut expr_ids := []ast.FlatNodeId{cap: result.exprs.len}
				for e in result.exprs {
					expr_ids << out.emit_expr(e)
				}
				return out.emit_array_init_expr_by_ids(typ_id, init_id, cap_id, len_id,
					update_expr_id, expr_ids, result.pos)
			}
			if result is ast.CallExpr {
				lhs_id := out.emit_expr(result.lhs)
				mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
				for a in result.args {
					arg_ids << out.emit_expr(a)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
			}
			if result is ast.Ident {
				return out.emit_ident_by_name(result.name, result.pos)
			}
			return out.emit_expr(result)
		}
		ast.FieldInit {
			// FieldInit reaches the expression dispatch when it appears as a
			// standalone arg in a struct-shorthand / named-arg call
			// (`foo(name: value)`). The `transform_expr` arm is a pure wrapper:
			// `value` is transformed, `name` is copied verbatim. Direct-emit
			// skips the `ast.FieldInit` struct allocation per occurrence by
			// transforming the value via `transform_expr_to_flat` and assembling
			// via the existing `emit_field_init_by_id` helper. Both the aux
			// (ConstDecl field) and the expression (`add_expr(FieldInit)`)
			// encodings route through `add_field_init` so a single emit helper
			// covers both call sites.
			value_id := t.transform_expr_to_flat(expr.value, mut out)
			return out.emit_field_init_by_id(expr.name, value_id)
		}
		ast.FnLiteral {
			// FnLiteral has three children sets: typ (FnType, verbatim),
			// captured_vars ([]Expr, verbatim), and stmts ([]Stmt, transformed
			// via `transform_stmts` — the body driver that may multi-expand a
			// single stmt into several). Direct-emit:
			//   1. emit typ via the leaf `emit_type` (Type is identity)
			//   2. emit each captured_var via the leaf `emit_expr` (Idents/Expr
			//      are identity in `transform_expr` already)
			//   3. run `transform_stmts(expr.stmts)` (still allocates a `[]Stmt`
			//      — the seam refactor that lets stmt-list expansion emit
			//      directly into the builder is the next big port target) and
			//      emit each result via `out.emit_stmt(...)`
			//   4. assemble via the new `emit_fn_literal_by_ids` helper
			// Skips the `ast.FnLiteral` wrapper struct allocation per
			// occurrence. Mirrors `add_expr(FnLiteral)` encoding exactly
			// (edge[0] = type, edge[1..1+captured.len] = captured_vars,
			// edge[1+captured.len..] = stmts; captured_vars.len in extra).
			typ_id := out.emit_type(ast.Type(expr.typ))
			mut captured_var_ids := []ast.FlatNodeId{cap: expr.captured_vars.len}
			for cv in expr.captured_vars {
				captured_var_ids << out.emit_expr(cv)
			}
			stmt_ids := t.transform_stmts_to_flat(expr.stmts, mut out)
			return out.emit_fn_literal_by_ids(typ_id, captured_var_ids, stmt_ids, expr.pos)
		}
		ast.InitExpr {
			// InitExpr deep helper port: the body of `transform_init_expr`
			// (struct.v ~line 1240) lowers one branch into a non-InitExpr
			// shape — the typed empty map `map[K]V{}` lowers to a CallExpr
			// to `new_map` (or a `MapInitExpr` on the eval backend). That
			// branch routes through `out.emit_expr(t.transform_init_expr(...))`
			// since the result is not an InitExpr. Every other path returns
			// an `ast.InitExpr` with the same `typ` and a per-field
			// transformation that handles sumtype wrapping, ArrayInitExpr
			// special-casing, expected-type resolution, and missing-default
			// fill-in. The default path direct-emits via
			// `emit_init_expr_by_ids` + `emit_field_init_by_id`, skipping the
			// `ast.InitExpr` wrapper allocation on each occurrence.
			return t.transform_init_expr_to_flat(expr, mut out)
		}
		ast.ComptimeExpr {
			// ComptimeExpr deep helper port: `transform_comptime_expr` is
			// mirrored as `transform_comptime_expr_to_flat` below. Most
			// branches lower the comptime form into a non-ComptimeExpr shape
			// (eval_comptime_if produces an IfExpr result, `$res` becomes a
			// BasicLiteral `false`, `$embed_file(...)` becomes an InitExpr or
			// method-chained CallExpr, `@VMODROOT` / `VMODROOT` becomes a
			// StringLiteral, embed_file chains may also produce non-comptime
			// shapes) — those route through `out.emit_expr(...)`. Only the
			// default `ast.ComptimeExpr{expr: transform(inner), pos}` path
			// is direct-emit via `emit_comptime_expr_by_id`.
			return t.transform_comptime_expr_to_flat(expr, mut out)
		}
		ast.IndexExpr {
			// IndexExpr deep helper port: the body of `transform_index_expr`
			// is mirrored as `transform_index_expr_to_flat` below. Three
			// branches produce non-IndexExpr shapes that go through the
			// legacy round-trip (`out.emit_expr(t.transform_expr(...))`):
			// (a) `expr.expr is ast.RangeExpr` → `transform_slice_index_expr`
			//     produces a CallExpr to `array_slice` / `string_substr`.
			// (b) Map index on a non-eval backend → `map__get` lowering
			//     produces an `ast.UnsafeExpr` wrapping prefix temps + cast +
			//     deref.
			// The gated path, the eval-backend map path, and the default
			// path all rebuild an IndexExpr with `lhs` and `expr` recursively
			// transformed and `is_gated` copied verbatim — direct-emit via
			// `emit_index_expr_by_ids`, skipping the `ast.IndexExpr` struct
			// allocation on the common path.
			return t.transform_index_expr_to_flat(expr, mut out)
		}
		ast.SelectorExpr {
			// SelectorExpr is the first deep helper rewrite port: the full body
			// of `transform_selector_expr` is mirrored here as
			// `transform_selector_expr_to_flat`, which direct-emits only the
			// default path (`SelectorExpr{lhs: transform(lhs), rhs, pos}`) via
			// `emit_selector_expr_by_ids`. All special-case branches that
			// rewrite the selector into an Ident / BasicLiteral / CallExpr
			// (typeof.name, sumtype rep fields, smartcast match, os.args,
			// module-qualified enum, same-module enum) go through
			// `out.emit_expr(...)` since their results aren't selectors.
			return t.transform_selector_expr_to_flat(expr, mut out)
		}
		ast.KeywordOperator {
			// KeywordOperator covers comptime/macro-style call shapes:
			// `typeof(x)`, `sizeof(T)`, `isreftype(T)`, `dump(x)`,
			// `likely(x)` / `unlikely(x)`, `offsetof(T, f)`, and `go ...` /
			// `spawn ...`. The `transform_expr` arm has two rewrite
			// branches:
			//   1. `key_typeof` with `exprs.len > 0` → resolves the type
			//      name via `resolve_typeof_expr` and produces a
			//      StringLiteral when the name is non-empty (different
			//      shape).
			//   2. `key_go` with `exprs.len > 0` → `lower_go_call(expr)`
			//      lowers the spawn into a CallExpr to the generated
			//      goroutine wrapper (different shape) — or returns the
			//      original KeywordOperator unchanged when the inner
			//      expression is not a Call/CallOrCast or the function
			//      name can't be resolved.
			// All other ops (`sizeof`, `isreftype`, `dump`, `likely`,
			// `unlikely`, `offsetof`, `spawn`, also `typeof` when
			// `resolve_typeof_expr` returns empty) fall through to the
			// default `ast.Expr(expr)` identity path — direct-emit via
			// the leaf `out.emit_expr(ast.Expr(expr))`, skipping the
			// `transform_expr` dispatch call. Session 51 refactor:
			// direct-call `resolve_typeof_expr` for the typeof branch
			// (skip the `transform_expr` dispatch) and direct-call
			// `lower_go_call` for the go branch then dispatch on the
			// result shape — CallExpr direct-emits via session 44's
			// `emit_call_expr_by_ids`, KeywordOperator identity falls
			// through to leaf-encode. Matches the Ident-port pattern
			// (session 23): leaf-identity default, gated state-dependent
			// special branches.
			if expr.op == .key_typeof && expr.exprs.len > 0 {
				type_name := t.resolve_typeof_expr(expr.exprs[0])
				if type_name != '' {
					return out.emit_string_literal_by_value(.v, quote_v_string_literal(type_name),
						expr.pos)
				}
			}
			if expr.op == .key_go && expr.exprs.len > 0 {
				result := t.lower_go_call(expr)
				if result is ast.CallExpr {
					lhs_id := out.emit_expr(result.lhs)
					mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
					for a in result.args {
						arg_ids << out.emit_expr(a)
					}
					return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
				}
				return out.emit_expr(result)
			}
			return out.emit_expr(ast.Expr(expr))
		}
		ast.GenericArgs {
			// GenericArgs covers the type-parameter shape `lhs[T]` (e.g.
			// `typeof[int]`, `MyGenericFn[string]`, ...). The `transform_expr`
			// arm has three branches:
			//   1. `is_typeof_generic_args(expr)` (the `typeof[T]` shape) →
			//      identity (`return expr`).
			//   2. `args.len == 1` + non-callable lhs type → disambiguates as
			//      an IndexExpr via `transform_index_expr` (different shape).
			//   3. default → `specialize_generic_callable_expr` lowers to an
			//      Ident or CallExpr (different shape).
			// Direct-emit follows sessions 23 (Ident) and 24 (KeywordOperator)
			// — gate the identity branch via the immutable lookup
			// `is_typeof_generic_args` (a `&Transformer` name check, no
			// mutation) and direct-emit via leaf `out.emit_expr(ast.Expr(expr))`.
			// All other paths fall back to the legacy round-trip.
			//
			// Reachability is limited in practice: GenericArgs typically
			// appears as a CallExpr lhs (`typeof[int]()`, `myfn[T](x)`), and
			// CallExpr is unported — so the arm fires only through ported-
			// ancestor recursion (ParenExpr / SelectorExpr deep helper, ...).
			// Same shape as the session 17 IfGuardExpr completeness sweep:
			// keeps the dispatch surface coherent with the legacy
			// `transform_expr` shape without adding a speculative fixture.
			// Session 53 refactor: directly handle branches 2 and 3
			// instead of leaf-encoding via `transform_expr`. Branch 2
			// (single-arg, non-callable lhs) cross-arm routes to
			// `transform_index_expr_to_flat` (same template as
			// GenericArgOrIndexExpr in session 33 — the IndexExpr arm's
			// default direct-emit applies). Branch 3 direct-calls
			// `specialize_generic_callable_expr` (returns Ident or
			// SelectorExpr — both identity in `transform_expr`) and
			// leaf-emits the result. Skips the outer `transform_expr`
			// match dispatch on every non-typeof GenericArgs occurrence.
			if t.is_typeof_generic_args(expr) {
				return out.emit_expr(ast.Expr(expr))
			}
			if expr.args.len == 1 {
				if lhs_type := t.get_expr_type(expr.lhs) {
					if !t.is_callable_type(lhs_type) {
						return t.transform_index_expr_to_flat_parts(expr.lhs, expr.args[0], false,
							expr.pos, mut out)
					}
				}
			}
			return t.specialize_generic_callable_expr_to_flat(expr.lhs, expr.args, expr.pos, mut
				out)
		}
		ast.GenericArgOrIndexExpr {
			// GenericArgOrIndexExpr is the parser ambiguity node for `x[y]` —
			// the legacy arm disambiguates via the lhs type:
			//   1. callable lhs → `specialize_generic_callable_expr(lhs, [expr],
			//      pos)` lowers to an Ident or CallExpr (the generic arg
			//      specialization token, e.g. `myfn_T`) — different shape, route
			//      through the leaf `out.emit_expr(...)`.
			//   2. otherwise → constructs an `ast.IndexExpr{lhs, expr,
			//      is_gated: false, pos}` and runs it through
			//      `transform_index_expr`. Route through the existing
			//      `transform_index_expr_to_flat` helper so the IndexExpr arm's
			//      default-path direct-emit (lhs + expr via
			//      `transform_expr_to_flat`, assembled via
			//      `emit_index_expr_by_ids`) also applies here — skipping the
			//      `transform_expr` dispatch + the IndexExpr arm's
			//      `transform_index_expr` legacy call per occurrence.
			//
			// `is_callable_type` is an immutable lookup (`&Transformer`
			// receiver); `specialize_generic_callable_expr` is `mut`. Same
			// shape as the Ident / KeywordOperator / GenericArgs ports
			// (sessions 23-25): gate the mut-call branch via an upfront
			// immutable probe, direct-emit the default path. Reachability is
			// limited (the parser typically unifies into CallExpr/IndexExpr at
			// parse time), but the port removes one fallback path from the
			// `else` branch and establishes the cross-arm routing pattern
			// (one ported helper calling another).
			if lhs_type := t.get_expr_type(expr.lhs) {
				if t.is_callable_type(lhs_type) {
					return t.specialize_generic_callable_expr_to_flat(expr.lhs, [
						expr.expr,
					], expr.pos, mut out)
				}
			}
			return t.transform_index_expr_to_flat_parts(expr.lhs, expr.expr, false, expr.pos, mut
				out)
		}
		ast.Ident {
			// Ident has two state-dependent rewrite branches in
			// `transform_expr`:
			//   1. `@VMODROOT` → resolves to a StringLiteral via
			//      `vmodroot_string_literal(expr.pos)` (different shape).
			//   2. smartcast match → `find_smartcast_for_expr(expr.name)` hit
			//      lowers to a direct sum payload access via
			//      `apply_smartcast_direct_ctx` (different shape).
			// Both produce non-Ident shapes and fall back to the legacy
			// `out.emit_expr(t.transform_expr(expr))` round-trip. The default
			// path is pure identity (`ast.Expr(expr)`), so direct-emit just
			// leaves the Ident node unchanged via `out.emit_expr(expr)` —
			// skipping the `transform_expr` dispatch call per occurrence.
			// `find_smartcast_for_expr` is an immutable lookup (`&Transformer`
			// receiver), safe to probe up front. Session 52 refactor:
			// direct-call the two helpers and leaf-encode their results
			// (skipping the outer `transform_expr` match dispatch);
			// `vmodroot_string_literal` is immutable and returns
			// StringLiteral (identity in transform_expr), and
			// `apply_smartcast_direct_ctx` is the helper transform_expr
			// would call anyway — same template as session 51 (KeywordOperator).
			// Session 69 follow-up: inline `vmodroot_string_literal` and
			// direct-emit the StringLiteral via session 66's
			// `emit_string_literal_by_value` helper — skipping the
			// `ast.StringLiteral` wrapper-struct allocation per `@VMODROOT`
			// access. Same template as session 66 (ComptimeExpr StringLiteral
			// wrapper elision). Matching site lives inside
			// `transform_comptime_expr_to_flat` for `$VMODROOT` / `$@VMODROOT`.
			if expr.name == '@VMODROOT' {
				return out.emit_string_literal_by_value(.v,
					quote_v_string_literal(t.comptime_vmodroot), expr.pos)
			}
			if ctx := t.find_smartcast_for_expr(expr.name) {
				// Session 79 refactor: `apply_smartcast_direct_ctx`
				// (transformer.v:10288) returns one of five shapes — four
				// of them an outer `ast.ParenExpr` wrapping different inner
				// expressions (pointer-cast hit, interface smartcast,
				// direct-data variant cast, struct/string deref); the
				// fifth is the already-cast `transformed_base` returned
				// verbatim (any shape). Direct-emit the ParenExpr via
				// session 5's `emit_paren_expr_by_id` — encode the
				// already-transformed inner expression via leaf
				// `out.emit_expr` (helper transformed it via
				// `transform_expr(original_expr)`; re-routing through
				// `transform_expr_to_flat` would double-transform). Skips
				// the `ast.ParenExpr` wrapper-struct allocation per
				// smartcast hit on Ident reads (~4-of-5 paths). The
				// already-cast fallback (path 2 — `transformed_base`)
				// still leaf-emits. Same template as sessions 77/78
				// (routing-pass third-shape extensions).
				smartcast_result := t.apply_smartcast_direct_ctx(expr, ctx)
				if smartcast_result is ast.ParenExpr {
					inner_id := out.emit_expr(smartcast_result.expr)
					return out.emit_paren_expr_by_id(inner_id, smartcast_result.pos)
				}
				return out.emit_expr(smartcast_result)
			}
			return out.emit_expr(ast.Expr(expr))
		}
		ast.StringInterLiteral {
			// StringInterLiteral deep helper port: legacy
			// `transform_string_inter_literal` always returns a
			// StringInterLiteral (per-inter smartcast hoist via
			// `hoist_expr_to_temp`, then `transform_sprintf_arg` wrap, then
			// `resolve_sprintf_format` fill-in). `transform_string_inter_literal_to_flat`
			// mirrors the body and emits each inter via
			// `emit_string_inter_by_ids` and the outer literal via
			// `emit_string_inter_literal_by_ids`, skipping the
			// `ast.StringInterLiteral` wrapper, the `[]ast.StringInter` list,
			// and each `ast.StringInter` struct allocation per occurrence.
			return t.transform_string_inter_literal_to_flat(expr, mut out)
		}
	}
}

// transform_comptime_expr_to_flat is the direct-emit port of
// `transform_comptime_expr`. The branches that lower comptime constructs
// into non-ComptimeExpr shapes (eval_comptime_if → IfExpr result;
// `$res(...)` → BasicLiteral false; `$embed_file(...)` → InitExpr or
// chained CallExpr; `@VMODROOT`/`VMODROOT` → StringLiteral; embed-file
// chain lowering) all build the legacy `ast.Expr` and route through
// `out.emit_expr(...)`. Only the default `ComptimeExpr{expr: transform(inner), pos}`
// path is direct-emit via `emit_comptime_expr_by_id` — skipping the
// `ast.ComptimeExpr` struct allocation on the rare default-path occurrence.
fn (mut t Transformer) transform_comptime_expr_to_flat(expr ast.ComptimeExpr, mut out ast.FlatBuilder) ast.FlatNodeId {
	inner := expr.expr
	if inner is ast.IfExpr {
		// Session 70: route through `eval_comptime_if_to_flat` parallel
		// direct-emit helper (selected branch's expr lowers via
		// `transform_expr_to_flat` directly; skip cases share the interned
		// `empty_expr` flat node), skipping one `ast.Expr` allocation per
		// comptime-if evaluation. Legacy `eval_comptime_if` stays in if.v
		// for the non-flat `transform_comptime_expr` caller.
		return t.eval_comptime_if_to_flat(inner, mut out)
	}
	if inner is ast.CallExpr {
		if inner.lhs is ast.Ident && inner.lhs.name == 'res' {
			return out.emit_basic_literal_by_value(.key_false, 'false', expr.pos)
		}
	}
	if inner is ast.CallOrCastExpr {
		if inner.lhs is ast.Ident && inner.lhs.name == 'res' {
			return out.emit_basic_literal_by_value(.key_false, 'false', expr.pos)
		}
	}
	// Session 56 refactor: direct-emit the InitExpr returned by
	// `transform_embed_file_comptime_expr` via session 1/55's
	// `emit_init_expr_by_ids` + `emit_field_init_by_id` (skipping the
	// outer `ast.InitExpr` wrapper). The helper's field values are
	// pure leaves (StringLiteral / BasicLiteral built locally), so
	// leaf-encoding via `out.emit_expr` per field-value is bit-equal
	// to the legacy add_expr path. If `transform_embed_file_comptime_expr`
	// returned the unchanged ComptimeExpr (zero-arg or read failure),
	// fall back to leaf-emit. Same routing template as sessions
	// 47-51/54/55.
	// Two parallel blocks (one per inner shape) because V's type
	// narrowing across `||` doesn't propagate `is` checks — combining
	// them resolves `inner.lhs.name` to the `name()` method instead
	// of the `name` field on the second branch.
	// Session 71: both `embed_file(args)` (CallExpr) and `embed_file(expr)`
	// (CallOrCastExpr) branches direct-emit via the new
	// `transform_embed_file_comptime_expr_to_flat` (shared
	// `embed_file_init_parts` prologue with legacy
	// `transform_embed_file_comptime_expr`) — skipping the intermediate
	// InitExpr + 4 FieldInit + 4 inner literal wrapper structs + 1 typ Ident
	// per `$embed_file(...)` call. The result-dispatch routing pass that
	// session 56 added (InitExpr direct-emit vs leaf-emit fallback) collapses
	// into the helper's own fall-through (legacy ComptimeExpr identity
	// returned via `out.emit_expr(ast.Expr(expr))`).
	if inner is ast.CallExpr {
		if inner.lhs is ast.Ident && inner.lhs.name == 'embed_file' {
			return t.transform_embed_file_comptime_expr_to_flat(expr, inner.args, mut out)
		}
	}
	if inner is ast.CallOrCastExpr {
		if inner.lhs is ast.Ident && inner.lhs.name == 'embed_file' {
			return t.transform_embed_file_comptime_expr_to_flat(expr, [inner.expr], mut out)
		}
	}
	if inner is ast.Ident {
		// Session 69: inline `vmodroot_string_literal` and direct-emit
		// the StringLiteral via session 66's `emit_string_literal_by_value`
		// — skipping the `ast.StringLiteral` wrapper-struct allocation per
		// `$VMODROOT` / `$@VMODROOT` comptime access. Matching site lives
		// in the Ident arm of `transform_expr_to_flat` (for bare `@VMODROOT`).
		if inner.name in ['VMODROOT', '@VMODROOT'] {
			return out.emit_string_literal_by_value(.v,
				quote_v_string_literal(t.comptime_vmodroot), expr.pos)
		}
	}
	// Session 72: route through `transform_embed_file_comptime_chain_to_flat`
	// (parallel direct-emit helper that recursively mirrors
	// `transform_embed_file_comptime_chain` / `transform_embed_file_chain_lhs`
	// via `emit_selector_expr_by_ids` + `emit_call_expr_by_ids` and the
	// session 71 `transform_embed_file_comptime_expr_to_flat`). Skips the
	// chain's outer CallExpr/SelectorExpr/Ident wrapper-struct allocations
	// per `$embed_file('foo').to_string()` and similar nested forms; falls
	// back to the default ComptimeExpr rebuild when no `$embed_file(...)` is
	// found anywhere in the chain. Same template as sessions 47-51/54/55/56.
	if transformed_id := t.transform_embed_file_comptime_chain_to_flat(inner, expr.pos, mut out) {
		return transformed_id
	}
	// Default: rebuild ComptimeExpr with recursively-transformed inner.
	inner_id := t.transform_expr_to_flat(inner, mut out)
	return out.emit_comptime_expr_by_id(inner_id, expr.pos)
}

// transform_init_expr_to_flat is the direct-emit port of
// `transform_init_expr`. The typed empty map InitExpr branch
// (`expr.fields.len == 0` with a `MapType`) lowers into a CallExpr to
// `new_map` (or `MapInitExpr` on the eval backend) — that shape is not an
// InitExpr so it routes through the legacy `out.emit_expr(t.transform_expr(...))`
// round-trip. The default path runs the legacy per-field transform (sumtype
// wrapping, ArrayInitExpr special-casing, expected-type resolution, missing
// default fill-in — too much logic to inline here) and then direct-emits the
// resulting InitExpr via `emit_init_expr_by_ids` + `emit_field_init_by_id`,
// skipping the `ast.InitExpr` outer wrapper allocation on the common path.
fn (mut t Transformer) transform_init_expr_to_flat(expr ast.InitExpr, mut out ast.FlatBuilder) ast.FlatNodeId {
	// Typed empty map InitExpr (`map[K]V{}`) lowers to:
	//   - eval backend → `MapInitExpr{typ, keys: [], vals: [], pos}`
	//     (identity-shape map literal)
	//   - non-eval → CallExpr to `new_map(sizeof(K), sizeof(V), &hash,
	//     &eq, &clone, &free)` with the key-type-specific runtime fns.
	// Session 55 refactor: direct-call `transform_init_expr` (skipping
	// the outer `transform_expr` dispatch) and dispatch on result
	// shape — MapInitExpr direct-emits via session 40's
	// `emit_map_init_expr_by_ids`, CallExpr direct-emits via session
	// 44's `emit_call_expr_by_ids`, anything else falls through to
	// leaf-emit. Same routing template as sessions 47-51/54.
	if expr.fields.len == 0 {
		if expr.typ is ast.Type {
			typ_payload := expr.typ as ast.Type
			if typ_payload is ast.MapType {
				result := t.transform_init_expr(expr)
				if result is ast.MapInitExpr {
					typ_id := out.emit_expr(result.typ)
					mut key_ids := []ast.FlatNodeId{cap: result.keys.len}
					for k in result.keys {
						key_ids << out.emit_expr(k)
					}
					mut val_ids := []ast.FlatNodeId{cap: result.vals.len}
					for v in result.vals {
						val_ids << out.emit_expr(v)
					}
					return out.emit_map_init_expr_by_ids(typ_id, key_ids, val_ids, result.pos)
				}
				if result is ast.CallExpr {
					lhs_id := out.emit_expr(result.lhs)
					mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
					for a in result.args {
						arg_ids << out.emit_expr(a)
					}
					return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
				}
				return out.emit_expr(result)
			}
		}
	}
	// Default path: run legacy field transform, then direct-emit the
	// resulting InitExpr. Defensive guard: if `transform_init_expr` ever
	// produces a non-InitExpr shape for the default path (it currently
	// doesn't), fall back to the round-trip rather than mis-emit.
	transformed := t.transform_init_expr(expr)
	if transformed !is ast.InitExpr {
		return out.emit_expr(transformed)
	}
	init := transformed as ast.InitExpr
	typ_id := out.emit_expr(init.typ)
	mut field_ids := []ast.FlatNodeId{cap: init.fields.len}
	for f in init.fields {
		value_id := out.emit_expr(f.value)
		field_ids << out.emit_field_init_by_id(f.name, value_id)
	}
	return out.emit_init_expr_by_ids(typ_id, field_ids, expr.pos)
}

// typeof_selector_result_to_flat is the direct-emit port of
// `typeof_selector_result` (expr.v:419). Same Option-returning shape but
// emits the BasicLiteral / StringLiteral directly via session 66's helpers
// (`emit_basic_literal_by_value`, `emit_string_literal_by_value`) — skipping
// the wrapper-struct allocation per `typeof(x).idx` / `typeof(x).name` access.
fn typeof_selector_result_to_flat(type_name string, selector string, pos token.Pos, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if type_name == '' {
		return none
	}
	if selector == 'idx' {
		return out.emit_basic_literal_by_value(.number, typeof_type_idx(type_name).str(), pos)
	}
	if selector == 'name' {
		return out.emit_string_literal_by_value(.v, quote_v_string_literal(type_name), pos)
	}
	return none
}

// specialize_generic_callable_expr_to_flat is the direct-emit port of
// `specialize_generic_callable_expr` (types.v). Mirrors the legacy match:
//   - Ident lhs → synth `Ident{name: lhs.name + suffix, pos}` — direct-emit
//     via `emit_ident_by_name`.
//   - SelectorExpr lhs → synth `SelectorExpr{lhs: transform(lhs.lhs), rhs:
//     Ident{name: lhs.rhs.name + suffix, pos: lhs.rhs.pos}, pos}` — direct-
//     emit via `emit_ident_by_name` (for the inner rhs) + `emit_expr` over
//     `transform_expr(lhs.lhs)` (preserves pending_stmts ordering vs the
//     legacy path) + `emit_selector_expr_by_ids`.
//   - else lhs → fallback `Ident{name: lhs.name() + suffix, pos}` — same as
//     the Ident branch.
// Skips one Ident wrapper per Ident/else occurrence; one SelectorExpr +
// one Ident wrapper per SelectorExpr occurrence. Used by the two call sites
// in `transform_expr_to_flat` (GenericArgs default branch, GenericArgOrIndexExpr
// callable-lhs branch).
fn (mut t Transformer) specialize_generic_callable_expr_to_flat(lhs ast.Expr, args []ast.Expr, pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	suffix := t.generic_specialization_suffix(args)
	match lhs {
		ast.Ident {
			return out.emit_ident_by_name(lhs.name + suffix, pos)
		}
		ast.SelectorExpr {
			inner_lhs := t.transform_expr(lhs.lhs)
			lhs_id := out.emit_expr(inner_lhs)
			rhs_id := out.emit_ident_by_name(lhs.rhs.name + suffix, lhs.rhs.pos)
			return out.emit_selector_expr_by_ids(lhs_id, rhs_id, pos)
		}
		else {
			return out.emit_ident_by_name(lhs.name() + suffix, pos)
		}
	}
}

// eval_comptime_if_to_flat is the direct-emit port of `eval_comptime_if`
// (if.v:1210). The legacy helper returns `ast.Expr` (one of:
// `t.transform_expr(stmt.expr)` for the matched then/else branch, a recursive
// `t.eval_comptime_if(else_e)` for $else $if chains, or `ast.empty_expr` for
// skipped / multi-stmt branches). Direct-emit mirrors the same logic but
// routes the matched branch through `transform_expr_to_flat` (one allocation
// saved per evaluation — the intermediate `ast.Expr` from `transform_expr`)
// and emits `ast.empty_expr` via the leaf `out.emit_expr` (which goes through
// `b.empty_expr_id` interning — no allocation). Used by the single call site
// in `transform_comptime_expr_to_flat` (ComptimeExpr arm, inner IfExpr).
// Legacy `eval_comptime_if` stays in if.v with one non-flat-write caller
// (expr.v:3223, inside legacy `transform_comptime_expr`). Same parallel-port
// template as sessions 67/68 (`specialize_generic_callable_expr_to_flat`,
// `typeof_selector_result_to_flat`).
fn (mut t Transformer) eval_comptime_if_to_flat(node ast.IfExpr, mut out ast.FlatBuilder) ast.FlatNodeId {
	cond_result := t.eval_comptime_cond(node.cond)
	if cond_result {
		if node.stmts.len == 1 {
			stmt := node.stmts[0]
			if stmt is ast.ExprStmt {
				return t.transform_expr_to_flat(stmt.expr, mut out)
			}
		}
		return out.emit_expr(ast.empty_expr)
	}
	else_e := node.else_expr
	if else_e !is ast.EmptyExpr {
		if else_e is ast.IfExpr {
			if else_e.cond is ast.EmptyExpr {
				if else_e.stmts.len == 1 {
					stmt := else_e.stmts[0]
					if stmt is ast.ExprStmt {
						return t.transform_expr_to_flat(stmt.expr, mut out)
					}
				}
				return out.emit_expr(ast.empty_expr)
			} else {
				return t.eval_comptime_if_to_flat(else_e, mut out)
			}
		}
	}
	return out.emit_expr(ast.empty_expr)
}

// transform_embed_file_comptime_expr_to_flat is the direct-emit port of
// `transform_embed_file_comptime_expr` (transformer.v:537). Legacy returns
// `ast.Expr` — either an `ast.InitExpr` with 4 `FieldInit`s wrapping
// StringLiteral / BasicLiteral / StringLiteral / StringLiteral leaves
// (StringLiteral content includes the v-quoted file bytes — can be large)
// or the unchanged `ast.ComptimeExpr` (zero-arg / non-string-literal arg /
// `embed_file_string_arg` returning none). Direct-emit calls the shared
// `embed_file_init_parts` prologue for the I/O + state mutation, then on
// success builds the same InitExpr shape via flat helpers
// (`emit_ident_by_name` for the typ Ident, `emit_string_literal_by_value` /
// `emit_basic_literal_by_value` per field value, `emit_field_init_by_id` per
// field, `emit_init_expr_by_ids` for the outer InitExpr) — skipping
// ~10 wrapper structs (1 InitExpr + 4 FieldInit + 4 inner StringLit/BasicLit
// + 1 typ Ident) per `$embed_file(...)` call. The fall-through path emits the
// unchanged ComptimeExpr via leaf `out.emit_expr(ast.Expr(expr))` to match the
// legacy caller's `out.emit_expr(result)` fallback. Used by the two call sites
// in `transform_comptime_expr_to_flat` (CallExpr `embed_file(args)` branch and
// CallOrCastExpr `embed_file(expr)` branch). Same _parts-helper template as
// sessions 4, 59, 60 (FnDecl / ReturnStmt / AssignStmt).
fn (mut t Transformer) transform_embed_file_comptime_expr_to_flat(expr ast.ComptimeExpr, args []ast.Expr, mut out ast.FlatBuilder) ast.FlatNodeId {
	rpath, apath, file_bytes := t.embed_file_init_parts(expr, args) or {
		return out.emit_expr(ast.Expr(expr))
	}
	typ_id := out.emit_ident_by_name(embed_file_helper_type_name, expr.pos)
	data_value_id := out.emit_string_literal_by_value(.v, quote_v_bytes_literal(file_bytes),
		expr.pos)
	data_field_id := out.emit_field_init_by_id('_data', data_value_id)
	len_value_id := out.emit_basic_literal_by_value(.number, file_bytes.len.str(), expr.pos)
	len_field_id := out.emit_field_init_by_id('len', len_value_id)
	path_value_id := out.emit_string_literal_by_value(.v, quote_v_string_literal(rpath), expr.pos)
	path_field_id := out.emit_field_init_by_id('path', path_value_id)
	apath_value_id := out.emit_string_literal_by_value(.v, quote_v_string_literal(apath), expr.pos)
	apath_field_id := out.emit_field_init_by_id('apath', apath_value_id)
	return out.emit_init_expr_by_ids(typ_id, [data_field_id, len_field_id, path_field_id,
		apath_field_id], expr.pos)
}

// transform_embed_file_chain_lhs_to_flat is the direct-emit port of
// `transform_embed_file_chain_lhs` (expr.v:3311). Mirrors the legacy
// helper's outcome shapes:
//   - CallExpr with `embed_file` Ident lhs → embed_file InitExpr via
//     `transform_embed_file_comptime_expr_to_flat`.
//   - CallExpr with SelectorExpr lhs whose base is a transformable chain →
//     CallExpr{SelectorExpr{transformed_base, rhs, sel.pos}, args, pos}
//     via `emit_call_expr_by_ids` + `emit_selector_expr_by_ids`.
//   - CallOrCastExpr with `embed_file` Ident → same as CallExpr embed_file.
//   - SelectorExpr with transformable lhs → SelectorExpr{transformed_lhs,
//     rhs, pos} via `emit_selector_expr_by_ids`.
//   - ComptimeExpr → recurse into `transform_embed_file_comptime_chain_to_flat`.
//   - Anything else → none (legacy fall-through).
// Args/sub-exprs lower via `transform_expr_to_flat` (the leaf-emit
// equivalent of legacy `transform_expr`). All recursive sites probe before
// emitting so failed-match paths never leave orphan flat nodes. Session 72
// pairs this with `transform_embed_file_comptime_chain_to_flat` to eliminate
// the `out.emit_expr(t.transform_embed_file_comptime_chain(...))` leaf-emit
// round-trip in `transform_comptime_expr_to_flat` — the chain unwraps
// `$embed_file('foo').to_string()` and similar nested forms into a single
// CallExpr(SelectorExpr(InitExpr, 'to_string')) shape; direct-emit skips
// the outer ComptimeExpr wrapper, the synth CallExpr/SelectorExpr wrappers,
// and the recursive Ident allocations per chain level.
fn (mut t Transformer) transform_embed_file_chain_lhs_to_flat(expr ast.Expr, comptime_pos token.Pos, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	match expr {
		ast.CallExpr {
			if expr.lhs is ast.Ident && expr.lhs.name == 'embed_file' {
				return t.transform_embed_file_comptime_expr_to_flat(ast.ComptimeExpr{
					expr: ast.Expr(expr)
					pos:  comptime_pos
				}, expr.args, mut out)
			}
			if expr.lhs is ast.SelectorExpr {
				sel := expr.lhs as ast.SelectorExpr
				if transformed_base_id := t.transform_embed_file_chain_lhs_to_flat(sel.lhs,
					comptime_pos, mut out)
				{
					rhs_id := out.emit_ident_by_name(sel.rhs.name, sel.rhs.pos)
					sel_id := out.emit_selector_expr_by_ids(transformed_base_id, rhs_id, sel.pos)
					mut arg_ids := []ast.FlatNodeId{cap: expr.args.len}
					for arg in expr.args {
						arg_ids << t.transform_expr_to_flat(arg, mut out)
					}
					return out.emit_call_expr_by_ids(sel_id, arg_ids, expr.pos)
				}
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.Ident && expr.lhs.name == 'embed_file' {
				return t.transform_embed_file_comptime_expr_to_flat(ast.ComptimeExpr{
					expr: ast.Expr(expr)
					pos:  comptime_pos
				}, [expr.expr], mut out)
			}
		}
		ast.SelectorExpr {
			if transformed_lhs_id := t.transform_embed_file_chain_lhs_to_flat(expr.lhs,
				comptime_pos, mut out)
			{
				rhs_id := out.emit_ident_by_name(expr.rhs.name, expr.rhs.pos)
				return out.emit_selector_expr_by_ids(transformed_lhs_id, rhs_id, expr.pos)
			}
		}
		ast.ComptimeExpr {
			if transformed_inner_id := t.transform_embed_file_comptime_chain_to_flat(expr.expr,
				expr.pos, mut out)
			{
				return transformed_inner_id
			}
		}
		else {}
	}

	return none
}

// transform_embed_file_comptime_chain_to_flat is the direct-emit port of
// `transform_embed_file_comptime_chain` (expr.v:3268). Mirrors the legacy
// helper's three outcome shapes (chain_lhs success direct-return,
// SelectorExpr wrap, CallExpr{SelectorExpr lhs} wrap) via the new
// `transform_embed_file_chain_lhs_to_flat` and the existing
// `emit_selector_expr_by_ids` / `emit_call_expr_by_ids` helpers. Returns
// `none` when no `$embed_file(...)` is found in the chain, matching the
// legacy "fall back to default ComptimeExpr rebuild" behaviour at the call
// site in `transform_comptime_expr_to_flat`.
fn (mut t Transformer) transform_embed_file_comptime_chain_to_flat(expr ast.Expr, comptime_pos token.Pos, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if transformed_id := t.transform_embed_file_chain_lhs_to_flat(expr, comptime_pos, mut out) {
		return transformed_id
	}
	match expr {
		ast.SelectorExpr {
			if transformed_lhs_id := t.transform_embed_file_chain_lhs_to_flat(expr.lhs,
				comptime_pos, mut out)
			{
				rhs_id := out.emit_ident_by_name(expr.rhs.name, expr.rhs.pos)
				return out.emit_selector_expr_by_ids(transformed_lhs_id, rhs_id, expr.pos)
			}
		}
		ast.CallExpr {
			if expr.lhs is ast.SelectorExpr {
				lhs_sel := expr.lhs as ast.SelectorExpr
				if transformed_base_id := t.transform_embed_file_chain_lhs_to_flat(lhs_sel.lhs,
					comptime_pos, mut out)
				{
					rhs_id := out.emit_ident_by_name(lhs_sel.rhs.name, lhs_sel.rhs.pos)
					sel_id := out.emit_selector_expr_by_ids(transformed_base_id, rhs_id,
						lhs_sel.pos)
					mut arg_ids := []ast.FlatNodeId{cap: expr.args.len}
					for arg in expr.args {
						arg_ids << t.transform_expr_to_flat(arg, mut out)
					}
					return out.emit_call_expr_by_ids(sel_id, arg_ids, expr.pos)
				}
			}
		}
		else {}
	}

	return none
}

// transform_index_expr_to_flat is the direct-emit port of
// `transform_index_expr`. The slice lowering (`expr.expr is RangeExpr`) and
// the non-eval-backend map lowering both produce non-IndexExpr shapes
// (CallExpr / UnsafeExpr respectively) — those route through the legacy
// `out.emit_expr(t.transform_expr(...))` round-trip so the rewrite logic
// stays in one place. The gated path, eval-backend map path, and default
// path all rebuild an IndexExpr around recursively-transformed `lhs` and
// `expr` with `is_gated` copied verbatim — direct-emit via
// `emit_index_expr_by_ids`, skipping the `ast.IndexExpr` struct allocation
// on the common path.
//
// Session 65 refactor (parts entry point): the entire body moved into
// `transform_index_expr_to_flat_parts(lhs_expr, idx_expr, is_gated, pos, mut out)`,
// which takes the four IndexExpr fields directly. The wrapper version
// `transform_index_expr_to_flat(expr ast.IndexExpr, mut out)` becomes a thin
// forwarder for the IndexExpr-dispatch arm caller. Two synth call sites that
// previously built `ast.IndexExpr{lhs, expr, is_gated: false, pos}` literals
// (GenericArgs single-arg branch, GenericArgOrIndexExpr default branch) now
// call _parts directly with the four parts — eliding the wrapper-struct
// allocation per synth-IndexExpr occurrence. The map-lowering branch inside
// _parts still allocates a local IndexExpr to call `transform_index_expr(...)`
// — that's only hit when the lhs is a map type, which is unusual for the
// generic-args synth sites (typical lhs is a function/method ident).
fn (mut t Transformer) transform_index_expr_to_flat(expr ast.IndexExpr, mut out ast.FlatBuilder) ast.FlatNodeId {
	return t.transform_index_expr_to_flat_parts(expr.lhs, expr.expr, expr.is_gated, expr.pos, mut
		out)
}

// transform_index_expr_to_flat_parts is the body of `transform_index_expr_to_flat`
// rewritten to take the four IndexExpr fields directly. Synth-IndexExpr call
// sites in flat_write.v use this entry point to skip the `ast.IndexExpr`
// wrapper struct allocation. See `transform_index_expr_to_flat` for the
// per-branch rationale.
fn (mut t Transformer) transform_index_expr_to_flat_parts(lhs_expr ast.Expr, idx_expr ast.Expr, is_gated bool, pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	if idx_expr is ast.RangeExpr {
		lhs := t.transform_expr(lhs_expr)
		result := t.transform_slice_index_expr(lhs, lhs_expr, idx_expr, is_gated)
		if result is ast.CallExpr {
			lhs_id := out.emit_expr(result.lhs)
			mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
			for a in result.args {
				arg_ids << out.emit_expr(a)
			}
			return out.emit_call_expr_by_ids(lhs_id, arg_ids, result.pos)
		}
		return out.emit_expr(result)
	}
	if !is_gated {
		mut map_expr_type_opt := t.map_index_lhs_type(lhs_expr)
		if map_expr_type_opt == none && lhs_expr is ast.Ident {
			map_expr_type_opt = t.lookup_var_type((lhs_expr as ast.Ident).name)
		}
		if map_expr_typ := map_expr_type_opt {
			if _ := t.unwrap_map_type(map_expr_typ) {
				if !t.is_eval_backend() {
					result := t.transform_index_expr(ast.IndexExpr{
						lhs:      lhs_expr
						expr:     idx_expr
						is_gated: is_gated
						pos:      pos
					})
					if result is ast.UnsafeExpr {
						mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
						for s in result.stmts {
							stmt_ids << out.emit_stmt(s)
						}
						return out.emit_unsafe_expr_by_ids(stmt_ids, result.pos)
					}
					return out.emit_expr(result)
				}
			}
		}
	}
	lhs_id := t.transform_expr_to_flat(lhs_expr, mut out)
	index_id := t.transform_expr_to_flat(idx_expr, mut out)
	return out.emit_index_expr_by_ids(lhs_id, index_id, is_gated, pos)
}

// transform_selector_expr_to_flat is the direct-emit port of
// `transform_selector_expr`. Special-case branches that rewrite into
// non-selector shapes (typeof.name, sumtype rep fields, smartcast, os.args,
// module-qualified enum, same-module enum) build the legacy ast.Expr and route
// through `out.emit_expr(...)`. Only the default `x.field` path is direct-emit:
// recurse on lhs via `transform_expr_to_flat`, emit rhs Ident as a leaf, and
// assemble via `emit_selector_expr_by_ids` — skipping the `ast.SelectorExpr`
// struct allocation on the common path.
//
// Session 63 refactor (inner Ident wrapper-struct elision via emit_ident_by_name):
// the four module/enum lookup rewrite branches inside
// `transform_selector_expr_to_flat` (module-qualified enum member,
// `is_module_ident` sub-module call, `is_module_ident` nested-module call,
// same-module enum member) previously synthesised `ast.Ident{name, pos}`
// wrappers and routed them through `out.emit_expr(...)`. Session 63 direct-
// emits via the new `emit_ident_by_name(name, pos)` helper (vlib/v2/ast/flat.v
// — bit-equal to `add_expr(Ident)`: `b.emit(.expr_ident, pos, b.intern(name),
// -1, 0, 0, []FlatEdge{})`) — one wrapper allocation eliminated per occurrence.
//
// Session 64 refactor (os.args inner CallExpr + Ident wrapper-struct elision):
// the `os.args` selector rewrite branch previously synthesised
// `ast.CallExpr{lhs: ast.Ident{name: 'arguments', pos}, pos}` and routed
// through `out.emit_expr(...)` — TWO wrapper allocations per occurrence
// (outer CallExpr, inner Ident). Session 64 direct-emits via
// `emit_ident_by_name('arguments', expr.pos)` + session 44's
// `emit_call_expr_by_ids(lhs_id, [], expr.pos)` — both wrapper allocations
// eliminated per `os.args` access. Cross-helper composition (Ident + Call)
// validates the pattern for other "synthesised CallExpr around synthesised
// child" rewrites scattered through the transformer.
fn (mut t Transformer) transform_selector_expr_to_flat(expr ast.SelectorExpr, mut out ast.FlatBuilder) ast.FlatNodeId {
	// typeof(x).name -> string literal with V type name
	if expr.lhs is ast.KeywordOperator && expr.lhs.op == .key_typeof
		&& expr.rhs.name in ['name', 'idx'] {
		if expr.lhs.exprs.len > 0 {
			type_name := t.resolve_typeof_expr(expr.lhs.exprs[0])
			if result_id := typeof_selector_result_to_flat(type_name, expr.rhs.name, expr.pos, mut
				out)
			{
				return result_id
			}
		}
	}
	// typeof[T]().name -> string literal with V type name
	if expr.rhs.name in ['name', 'idx'] && expr.lhs is ast.CallExpr {
		call := expr.lhs as ast.CallExpr
		type_name := t.resolve_typeof_call_lhs_type_name(call.lhs)
		if result_id := typeof_selector_result_to_flat(type_name, expr.rhs.name, expr.pos, mut out) {
			return result_id
		}
	}
	if expr.rhs.name in ['name', 'idx'] && expr.lhs is ast.CallOrCastExpr {
		call := expr.lhs as ast.CallOrCastExpr
		if call.expr is ast.EmptyExpr {
			type_name := t.resolve_typeof_call_lhs_type_name(call.lhs)
			if result_id := typeof_selector_result_to_flat(type_name, expr.rhs.name, expr.pos, mut
				out)
			{
				return result_id
			}
		}
	}
	// Generated sumtype representation fields already have their base
	// expression lowered. Do not apply smartcasts to them again on a later
	// transform pass.
	if expr.rhs.name in ['_tag', '_data'] || (expr.rhs.name.starts_with('_')
		&& expr.lhs is ast.SelectorExpr && (expr.lhs as ast.SelectorExpr).rhs.name == '_data') {
		return out.emit_expr(ast.Expr(expr))
	}
	// Check for smart cast field access: check ALL contexts in the stack
	if t.has_active_smartcast() {
		full_str := t.expr_to_string(expr)
		if direct_ctx := t.find_smartcast_for_expr(full_str) {
			// Session 80 refactor: same dispatch as session 79's Ident
			// arm — `apply_smartcast_direct_ctx` (transformer.v:10288)
			// returns one of five shapes, four of them an outer
			// `ast.ParenExpr` (pointer-cast hit, interface smartcast,
			// direct-data variant cast, struct/string deref); the
			// fifth path returns `transformed_base` verbatim. Direct-
			// emit the ParenExpr via session 5's
			// `emit_paren_expr_by_id` — inner already-transformed via
			// the helper's recursive `transform_expr(original_expr)`
			// call, so leaf-encode via `out.emit_expr`. Skips the
			// `ast.ParenExpr` wrapper-struct allocation per smartcast
			// hit on SelectorExpr reads (~4-of-5 paths). Same template
			// as session 79 (Ident smartcast ParenExpr fall-through).
			smartcast_result := t.apply_smartcast_direct_ctx(expr, direct_ctx)
			if smartcast_result is ast.ParenExpr {
				inner_id := out.emit_expr(smartcast_result.expr)
				return out.emit_paren_expr_by_id(inner_id, smartcast_result.pos)
			}
			return out.emit_expr(smartcast_result)
		}
		lhs_str := t.expr_to_string(expr.lhs)
		if ctx := t.find_smartcast_for_expr(lhs_str) {
			// Session 81 refactor: `apply_smartcast_field_access_ctx`
			// (struct.v:199) always returns an `ast.SelectorExpr`
			// (via `synth_selector_from_struct` in all five branches:
			// pointer-cast hit, already-cast variant, interface
			// smartcast wrapping ParenExpr(cast), eval-backend direct
			// variant access, default `*((variant*)data_access)`
			// field deref). The lhs of the returned SelectorExpr is
			// the helper's already-constructed cast/paren/data-access
			// chain — leaf-encoded via `out.emit_expr`. The rhs is an
			// `ast.Ident{name: field_name}` with default pos, also
			// leaf-encoded via `out.emit_expr`. Direct-emit via
			// session 7's `emit_selector_expr_by_ids` skips the
			// `ast.SelectorExpr` wrapper-struct allocation per
			// field-access smartcast hit on SelectorExpr reads.
			// Same template as sessions 77/78/79/80 (routing-pass
			// helper-shape extensions).
			result := t.apply_smartcast_field_access_ctx(expr.lhs, expr.rhs.name, ctx)
			if result is ast.SelectorExpr {
				lhs_id := out.emit_expr(result.lhs)
				rhs_id := out.emit_expr(ast.Expr(result.rhs))
				return out.emit_selector_expr_by_ids(lhs_id, rhs_id, result.pos)
			}
			return out.emit_expr(result)
		}
	}
	if expr.lhs is ast.Ident && expr.lhs.name == 'os' && expr.rhs.name == 'args' {
		lhs_id := out.emit_ident_by_name('arguments', expr.pos)
		return out.emit_call_expr_by_ids(lhs_id, []ast.FlatNodeId{}, expr.pos)
	}
	// Handle module-qualified enum value access and nested module references.
	if expr.lhs is ast.SelectorExpr {
		lhs_sel := expr.lhs as ast.SelectorExpr
		if lhs_sel.lhs is ast.Ident {
			module_name := lhs_sel.lhs.name
			type_name := lhs_sel.rhs.name
			qualified := '${module_name}__${type_name}'
			if typ := t.lookup_type(qualified) {
				if typ is types.Enum {
					t.register_synth_type(expr.pos, typ)
					return out.emit_ident_by_name(t.enum_member_ident_for_lookup(qualified, typ,
						expr.rhs.name), expr.pos)
				}
			}
			if t.is_module_ident(module_name) {
				sub_mod := lhs_sel.rhs.name
				fn_name := expr.rhs.name
				if t.get_module_scope(sub_mod) != none {
					return out.emit_ident_by_name('${sub_mod}__${fn_name}', expr.pos)
				}
				full_mod := '${module_name}__${sub_mod}'
				if t.get_module_scope(full_mod) != none {
					return out.emit_ident_by_name('${full_mod}__${fn_name}', expr.pos)
				}
			}
		}
	}
	// Handle same-module enum access.
	if expr.lhs is ast.Ident {
		lhs_name := expr.lhs.name
		qualified := if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !lhs_name.contains('__') {
			'${t.cur_module}__${lhs_name}'
		} else {
			lhs_name
		}
		if typ := t.lookup_type(qualified) {
			if typ is types.Enum {
				t.register_synth_type(expr.pos, typ)
				return out.emit_ident_by_name(t.enum_member_ident_for_lookup(qualified, typ,
					expr.rhs.name), expr.pos)
			}
		}
	}
	// Default transformation — direct-emit.
	lhs_id := t.transform_expr_to_flat(expr.lhs, mut out)
	rhs_id := out.emit_expr(ast.Expr(expr.rhs))
	return out.emit_selector_expr_by_ids(lhs_id, rhs_id, expr.pos)
}

// transform_string_inter_literal_to_flat is the direct-emit port of
// `transform_string_inter_literal`. Always returns a StringInterLiteral (legacy
// rebuilds the wrapper; this port emits the flat node directly). Mirrors the
// legacy body's per-inter loop:
//   1. Smartcast hoist: if the inter expression matches an active smartcast,
//      transform it (legacy `transform_expr` — `transform_expr_to_flat` would
//      detach the inter expr from `pending_stmts` ordering), then
//      `hoist_expr_to_temp` (which mutates `t.pending_stmts` — preserve order
//      vs legacy).
//   2. `transform_sprintf_arg(actual_inter)` produces the final inter
//      expression (type-aware: keeps strings, wraps pointer-with-str, etc.).
//   3. `resolve_sprintf_format(inter)` returns the resolved sprintf format
//      string.
// The per-inter result is emitted via `emit_string_inter_by_ids` and the
// outer StringInterLiteral via `emit_string_inter_literal_by_ids`, skipping
// both the outer `ast.StringInterLiteral` and the inner `[]ast.StringInter`
// allocations per occurrence. The `values []string` slice is passed verbatim
// (legacy copies it by reference into the new struct as well).
fn (mut t Transformer) transform_string_inter_literal_to_flat(expr ast.StringInterLiteral, mut out ast.FlatBuilder) ast.FlatNodeId {
	mut inter_ids := []ast.FlatNodeId{cap: expr.inters.len}
	for inter in expr.inters {
		mut actual_inter := inter
		if t.interpolation_expr_uses_smartcast(inter.expr) {
			if expr_typ := t.get_expr_type(inter.expr) {
				transformed := t.transform_expr(inter.expr)
				hoist_typ := t.get_expr_type(transformed) or {
					t.smartcast_variant_type_for_expr(inter.expr) or { expr_typ }
				}
				hoisted := t.hoist_expr_to_temp(transformed, hoist_typ)
				actual_inter = ast.StringInter{
					...inter
					expr: hoisted
				}
			}
		}
		actual_inter = ast.StringInter{
			...actual_inter
			expr: t.repair_stale_string_str_interpolation_expr(actual_inter.expr)
		}
		inter_expr := t.transform_sprintf_arg(actual_inter)
		expr_id := out.emit_expr(inter_expr)
		format_expr_id := out.emit_expr(inter.format_expr)
		resolved_fmt := t.resolve_sprintf_format(inter)
		inter_ids << out.emit_string_inter_by_ids(inter.format, inter.width, inter.precision,
			expr_id, format_expr_id, resolved_fmt)
	}
	// Legacy `transform_string_inter_literal` does not propagate `expr.pos` to
	// the rebuilt literal (the struct literal omits `pos`, defaulting to
	// `token.Pos{}`). Pass the zero pos here for det/parity.
	return out.emit_string_inter_literal_by_ids(expr.kind, expr.values, inter_ids, token.Pos{})
}
