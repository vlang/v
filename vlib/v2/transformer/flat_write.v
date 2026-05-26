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
		ast.ReturnStmt {
			// ReturnStmt port: `transform_return_stmt` always returns an
			// `ast.ReturnStmt` (never lowers to a different shape). Its body
			// (~110 lines in transformer.v) does per-expression sumtype
			// wrapping, enum-shorthand resolution, smartcast handling, and
			// native-backend pre-wrapping — too much to inline. Wrap-only
			// port: delegate the per-expression transform to legacy, then
			// decompose the resulting ReturnStmt and direct-emit via the new
			// `emit_return_stmt_by_ids` helper. Each transformed expr goes
			// through `out.emit_expr(...)` (it's already transformed). Skips
			// the outer `ast.ReturnStmt` wrapper allocation per occurrence;
			// the `[]ast.Expr` list still materialises (legacy builds it).
			transformed := t.transform_return_stmt(stmt)
			mut expr_ids := []ast.FlatNodeId{cap: transformed.exprs.len}
			for e in transformed.exprs {
				expr_ids << out.emit_expr(e)
			}
			return out.emit_return_stmt_by_ids(expr_ids)
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
			sumtype_name := t.type_expr_name_full(expr.typ)
			if sumtype_name != '' && t.is_sum_type(sumtype_name) {
				return out.emit_expr(t.transform_expr(expr))
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
			// expression shapes (CastExpr, Ident, or even an unwrapped inner)
			// — fall back to legacy so all the rewrite logic stays in one
			// place. For the plain postfix ops (`++` / `--`, i.e. `.inc` and
			// `.dec`), the arm is a pure wrapper around `transform_expr(expr.expr)`;
			// direct-emit recurses into `transform_expr_to_flat` and assembles
			// the flat node via the new `emit_postfix_expr_by_id` helper.
			if expr.op == .not || expr.op == .question {
				return out.emit_expr(t.transform_expr(expr))
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
			// Direct-emit mirrors that exactly: emit each stmt via
			// `out.emit_stmt(...)` and assemble via the existing
			// `emit_unsafe_expr_by_ids` helper (the legacy UnsafeExpr wrapper
			// is constructed with no explicit pos, so direct-emit must pass
			// `token.Pos{}` — same as the LockExpr port). When prefix_stmts
			// is empty, the result_expr goes through `out.emit_expr(...)`
			// (leaf). Skips the `ast.UnsafeExpr` wrapper allocation per
			// occurrence in the compound-expression case.
			mut prefix_stmts := []ast.Stmt{}
			result_expr := t.expand_single_or_expr(expr, mut prefix_stmts)
			if prefix_stmts.len > 0 {
				prefix_stmts << ast.ExprStmt{
					expr: result_expr
				}
				mut stmt_ids := []ast.FlatNodeId{cap: prefix_stmts.len}
				for s in prefix_stmts {
					stmt_ids << out.emit_stmt(s)
				}
				return out.emit_unsafe_expr_by_ids(stmt_ids, token.Pos{})
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
			if t.is_unsafe_nil_expr(expr) {
				return out.emit_expr(ast.Expr(ast.Ident{
					name: 'nil'
					pos:  expr.pos
				}))
			}
			transformed_stmts := t.transform_stmts(expr.stmts)
			mut stmt_ids := []ast.FlatNodeId{cap: transformed_stmts.len}
			for body_stmt in transformed_stmts {
				stmt_ids << out.emit_stmt(body_stmt)
			}
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
			transformed_stmts := t.transform_stmts(expr.stmts)
			mut stmt_ids := []ast.FlatNodeId{cap: transformed_stmts.len}
			for body_stmt in transformed_stmts {
				stmt_ids << out.emit_stmt(body_stmt)
			}
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
			//      goroutine wrapper (different shape).
			// Both fall back to the legacy round-trip. All other ops
			// (`sizeof`, `isreftype`, `dump`, `likely`, `unlikely`,
			// `offsetof`, `spawn`, also `typeof` when `resolve_typeof_expr`
			// returns empty) fall through to the default
			// `ast.Expr(expr)` identity path — direct-emit via the leaf
			// `out.emit_expr(ast.Expr(expr))`, skipping the
			// `transform_expr` dispatch call. Matches the Ident-port
			// pattern (session 23): leaf-identity default, gated
			// state-dependent special branches.
			if (expr.op == .key_typeof || expr.op == .key_go) && expr.exprs.len > 0 {
				return out.emit_expr(t.transform_expr(expr))
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
			if t.is_typeof_generic_args(expr) {
				return out.emit_expr(ast.Expr(expr))
			}
			return out.emit_expr(t.transform_expr(expr))
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
			// receiver), safe to probe up front.
			if expr.name == '@VMODROOT' {
				return out.emit_expr(t.transform_expr(expr))
			}
			if _ := t.find_smartcast_for_expr(expr.name) {
				return out.emit_expr(t.transform_expr(expr))
			}
			return out.emit_expr(ast.Expr(expr))
		}
		else {
			return out.emit_expr(t.transform_expr(expr))
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
		return out.emit_expr(t.eval_comptime_if(inner))
	}
	if inner is ast.CallExpr {
		if inner.lhs is ast.Ident && inner.lhs.name == 'res' {
			return out.emit_expr(ast.Expr(ast.BasicLiteral{
				kind:  .key_false
				value: 'false'
				pos:   expr.pos
			}))
		}
	}
	if inner is ast.CallOrCastExpr {
		if inner.lhs is ast.Ident && inner.lhs.name == 'res' {
			return out.emit_expr(ast.Expr(ast.BasicLiteral{
				kind:  .key_false
				value: 'false'
				pos:   expr.pos
			}))
		}
	}
	if inner is ast.CallExpr {
		if inner.lhs is ast.Ident && inner.lhs.name == 'embed_file' {
			return out.emit_expr(t.transform_embed_file_comptime_expr(expr, inner.args))
		}
	}
	if inner is ast.CallOrCastExpr {
		if inner.lhs is ast.Ident && inner.lhs.name == 'embed_file' {
			return out.emit_expr(t.transform_embed_file_comptime_expr(expr, [
				inner.expr,
			]))
		}
	}
	if inner is ast.Ident {
		if inner.name in ['VMODROOT', '@VMODROOT'] {
			return out.emit_expr(ast.Expr(t.vmodroot_string_literal(expr.pos)))
		}
	}
	if transformed := t.transform_embed_file_comptime_chain(inner, expr.pos) {
		return out.emit_expr(transformed)
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
	// Typed empty map InitExpr lowers to CallExpr / MapInitExpr — different
	// shape, must go through legacy.
	if expr.fields.len == 0 {
		if expr.typ is ast.Type {
			typ_payload := expr.typ as ast.Type
			if typ_payload is ast.MapType {
				return out.emit_expr(t.transform_expr(ast.Expr(expr)))
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
fn (mut t Transformer) transform_index_expr_to_flat(expr ast.IndexExpr, mut out ast.FlatBuilder) ast.FlatNodeId {
	// Slice lowering — produces non-IndexExpr.
	if expr.expr is ast.RangeExpr {
		return out.emit_expr(t.transform_expr(ast.Expr(expr)))
	}
	// Map-index lowering — produces UnsafeExpr on non-eval backends. Mirror
	// the same lookup sequence the legacy arm uses so we never direct-emit
	// a case the legacy code would have rewritten. The eval backend keeps
	// the IndexExpr shape so it falls through to direct-emit.
	if !expr.is_gated {
		mut map_expr_type_opt := t.map_index_lhs_type(expr.lhs)
		if map_expr_type_opt == none && expr.lhs is ast.Ident {
			map_expr_type_opt = t.lookup_var_type((expr.lhs as ast.Ident).name)
		}
		if map_expr_typ := map_expr_type_opt {
			if _ := t.unwrap_map_type(map_expr_typ) {
				if !t.is_eval_backend() {
					return out.emit_expr(t.transform_expr(ast.Expr(expr)))
				}
			}
		}
	}
	// Default + gated + eval-backend-map paths all rebuild IndexExpr.
	lhs_id := t.transform_expr_to_flat(expr.lhs, mut out)
	index_id := t.transform_expr_to_flat(expr.expr, mut out)
	return out.emit_index_expr_by_ids(lhs_id, index_id, expr.is_gated, expr.pos)
}

// transform_selector_expr_to_flat is the direct-emit port of
// `transform_selector_expr`. Special-case branches that rewrite into
// non-selector shapes (typeof.name, sumtype rep fields, smartcast, os.args,
// module-qualified enum, same-module enum) build the legacy ast.Expr and route
// through `out.emit_expr(...)`. Only the default `x.field` path is direct-emit:
// recurse on lhs via `transform_expr_to_flat`, emit rhs Ident as a leaf, and
// assemble via `emit_selector_expr_by_ids` — skipping the `ast.SelectorExpr`
// struct allocation on the common path.
fn (mut t Transformer) transform_selector_expr_to_flat(expr ast.SelectorExpr, mut out ast.FlatBuilder) ast.FlatNodeId {
	// typeof(x).name -> string literal with V type name
	if expr.lhs is ast.KeywordOperator && expr.lhs.op == .key_typeof
		&& expr.rhs.name in ['name', 'idx'] {
		if expr.lhs.exprs.len > 0 {
			type_name := t.resolve_typeof_expr(expr.lhs.exprs[0])
			if result := typeof_selector_result(type_name, expr.rhs.name, expr.pos) {
				return out.emit_expr(result)
			}
		}
	}
	// typeof[T]().name -> string literal with V type name
	if expr.rhs.name in ['name', 'idx'] && expr.lhs is ast.CallExpr {
		call := expr.lhs as ast.CallExpr
		type_name := t.resolve_typeof_call_lhs_type_name(call.lhs)
		if result := typeof_selector_result(type_name, expr.rhs.name, expr.pos) {
			return out.emit_expr(result)
		}
	}
	if expr.rhs.name in ['name', 'idx'] && expr.lhs is ast.CallOrCastExpr {
		call := expr.lhs as ast.CallOrCastExpr
		if call.expr is ast.EmptyExpr {
			type_name := t.resolve_typeof_call_lhs_type_name(call.lhs)
			if result := typeof_selector_result(type_name, expr.rhs.name, expr.pos) {
				return out.emit_expr(result)
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
			return out.emit_expr(t.apply_smartcast_direct_ctx(expr, direct_ctx))
		}
		lhs_str := t.expr_to_string(expr.lhs)
		if ctx := t.find_smartcast_for_expr(lhs_str) {
			return out.emit_expr(t.apply_smartcast_field_access_ctx(expr.lhs, expr.rhs.name, ctx))
		}
	}
	if expr.lhs is ast.Ident && expr.lhs.name == 'os' && expr.rhs.name == 'args' {
		return out.emit_expr(ast.CallExpr{
			lhs: ast.Ident{
				name: 'arguments'
				pos:  expr.pos
			}
			pos: expr.pos
		})
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
					return out.emit_expr(ast.Ident{
						name: t.enum_member_ident_for_lookup(qualified, typ, expr.rhs.name)
						pos:  expr.pos
					})
				}
			}
			if t.is_module_ident(module_name) {
				sub_mod := lhs_sel.rhs.name
				fn_name := expr.rhs.name
				if t.get_module_scope(sub_mod) != none {
					return out.emit_expr(ast.Ident{
						name: '${sub_mod}__${fn_name}'
						pos:  expr.pos
					})
				}
				full_mod := '${module_name}__${sub_mod}'
				if t.get_module_scope(full_mod) != none {
					return out.emit_expr(ast.Ident{
						name: '${full_mod}__${fn_name}'
						pos:  expr.pos
					})
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
				return out.emit_expr(ast.Ident{
					name: t.enum_member_ident_for_lookup(qualified, typ, expr.rhs.name)
					pos:  expr.pos
				})
			}
		}
	}
	// Default transformation — direct-emit.
	lhs_id := t.transform_expr_to_flat(expr.lhs, mut out)
	rhs_id := out.emit_expr(ast.Expr(expr.rhs))
	return out.emit_selector_expr_by_ids(lhs_id, rhs_id, expr.pos)
}
