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
//     FnDecl struct per fn under flat-output paths.
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
//   The active flat-direct paths run post_pass_to_flat and the post-pass tail
//   without materialising a transformed []ast.File. Compatibility entry points
//   still maintain legacy files for the `.v`/eval backends.
//
// Phase 6: drop compatibility materialisation.
//   `transform_files_to_flat` and `_via_driver` still return []ast.File for
//   legacy consumers. Flat-codegen backends use transform_flat_to_flat_direct
//   or the parallel flat-direct path and keep the transformed program in
//   FlatAst only.
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
// session port. It reads one file from `input_flat` through cursors, mirrors
// `transform_file`'s prologue, and emits each top-level stmt through the
// `transform_stmt_to_flat` seam. Returns the FlatNodeId of the appended file
// root, or `ast.invalid_flat_node_id` for an empty / missing source file.
//
// The file-level loop uses the flat stmt-list driver rather than dispatching
// each stmt directly. Top-level comptime `$if` blocks can expand to multiple
// declarations, and codegen expects those declarations to live directly under
// the file root.
//
// Callers must invoke `pre_pass_from_flat(input_flat)` before the per-file
// loop and `post_pass(mut collected_files)` after, mirroring the wedge. The
// per-file API does not run those passes itself so future phases can
// interleave file emissions with pre/post bookkeeping without re-running it.
pub fn (mut t Transformer) transform_file_index_to_flat(input_flat &ast.FlatAst, fi int, mut out ast.FlatBuilder) ast.FlatNodeId {
	return t.transform_flat_file_index_to_flat(input_flat, fi, []ast.Stmt{}, mut out)
}

// transform_file_index_with_extra_to_flat is the pub entry the parallel
// flat-direct transform uses: it forwards the monomorphize-generated `extra`
// stmts for file `fi` (from prepare_flat_for_transform) into the cursor-native
// per-file transform. The plain `transform_file_index_to_flat` above passes no
// extras (sequential generic-free callers).
pub fn (mut t Transformer) transform_file_index_with_extra_to_flat(input_flat &ast.FlatAst, fi int, extra_stmts []ast.Stmt, mut out ast.FlatBuilder) ast.FlatNodeId {
	return t.transform_flat_file_index_to_flat(input_flat, fi, extra_stmts, mut out)
}

fn (mut t Transformer) transform_flat_file_index_to_flat(input_flat &ast.FlatAst, fi int, extra_stmts []ast.Stmt, mut out ast.FlatBuilder) ast.FlatNodeId {
	if fi < 0 || fi >= input_flat.files.len {
		return ast.invalid_flat_node_id
	}
	fc := input_flat.file_cursor(fi)
	t.cur_file_name = fc.name()
	t.cur_module = fc.mod()
	t.cur_import_aliases = flat_import_aliases_for_generic_collect(input_flat, fi)
	if scope := t.get_module_scope(t.cur_module) {
		t.scope = scope
	} else {
		t.scope = unsafe { nil }
	}
	stmt_ids := t.transform_cursor_stmts_to_flat_direct(fc.stmts(), extra_stmts, mut out)
	attrs_id := copy_cursor_aux_list_to_flat(fc.attrs(), mut out)
	imports_id := copy_cursor_aux_list_to_flat(fc.imports(), mut out)
	return out.append_file_with_flat_lists_and_stmt_ids(fc.name(), fc.mod(), fc.selector_names(),
		attrs_id, imports_id, stmt_ids)
}

// transform_file_to_flat transforms one legacy file and appends the transformed
// tree directly to `out`. It is the AST-input counterpart to
// `transform_file_index_to_flat`, used by flat-output pipelines after
// whole-program generic preparation has produced concrete appended stmts.
pub fn (mut t Transformer) transform_file_to_flat(file ast.File, mut out ast.FlatBuilder) ast.FlatNodeId {
	// Mirror transform_file's per-file prologue. transform_stmt and the
	// rewrite sites read these fields to resolve cross-stmt references.
	t.cur_file_name = file.name
	t.cur_module = file.mod
	t.cur_import_aliases = import_aliases_for_generic_collect(file.imports)
	if scope := t.get_module_scope(file.mod) {
		t.scope = scope
	} else {
		t.scope = unsafe { nil }
	}
	// Top-level comptime `$if` blocks can expand to multiple declarations
	// (for example platform-specific worker structs/functions). Use the same
	// stmt-list driver as function bodies so selected branch stmts are spliced
	// into the file root instead of being hidden inside a top-level BlockStmt.
	stmt_ids := t.transform_stmts_to_flat_direct(file.stmts, mut out)
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
		t.restore_flat_stmt_list_smartcast_context(block_smartcast_depth, block_smartcast_stack,
			block_smartcast_counts)
		t.transform_stmt_list_item_to_flat(stmt, mut ids, mut out)
	}
	t.restore_flat_stmt_list_smartcast_context(block_smartcast_depth, block_smartcast_stack,
		block_smartcast_counts)
	return ids
}

pub fn (mut t Transformer) transform_cursor_stmts_to_flat_direct(stmts ast.CursorList, extra_stmts []ast.Stmt, mut out ast.FlatBuilder) []ast.FlatNodeId {
	mut ids := []ast.FlatNodeId{cap: stmts.len() + extra_stmts.len}
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
	for i in 0 .. stmts.len() {
		t.restore_flat_stmt_list_smartcast_context(block_smartcast_depth, block_smartcast_stack,
			block_smartcast_counts)
		t.transform_stmt_list_item_cursor_to_flat(stmts.at(i), mut ids, mut out)
	}
	for stmt in extra_stmts {
		t.restore_flat_stmt_list_smartcast_context(block_smartcast_depth, block_smartcast_stack,
			block_smartcast_counts)
		t.transform_stmt_list_item_to_flat(stmt, mut ids, mut out)
	}
	t.restore_flat_stmt_list_smartcast_context(block_smartcast_depth, block_smartcast_stack,
		block_smartcast_counts)
	return ids
}

fn (mut t Transformer) restore_flat_stmt_list_smartcast_context(block_smartcast_depth int, block_smartcast_stack []SmartcastContext, block_smartcast_counts map[string]int) {
	if t.smartcast_stack.len < block_smartcast_depth {
		t.smartcast_stack = block_smartcast_stack.clone()
		t.smartcast_expr_counts = block_smartcast_counts.clone()
	} else if t.smartcast_stack.len > block_smartcast_depth {
		t.truncate_smartcasts(block_smartcast_depth)
	}
}

fn (mut t Transformer) transform_stmt_list_item_to_flat(stmt ast.Stmt, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	if stmt is ast.AssignStmt {
		assign_stmt := stmt as ast.AssignStmt
		if t.try_expand_comptime_if_assign_to_flat(assign_stmt, mut ids, mut out) {
			return
		}
		t.track_interface_assign_to_flat(assign_stmt)
		if t.try_expand_interface_cast_assign_to_flat(assign_stmt, mut ids, mut out) {
			return
		}
		if t.try_expand_sincos_assign_to_flat(assign_stmt, mut ids, mut out) {
			return
		}
		if t.try_expand_or_expr_assign_stmts_to_flat(&assign_stmt, mut ids, mut out) {
			return
		}
		if t.try_expand_tuple_if_assign_stmts_to_flat(assign_stmt, mut ids, mut out) {
			return
		}
		if t.try_expand_tuple_call_assign_to_flat(assign_stmt, mut ids, mut out) {
			return
		}
		if t.try_expand_if_guard_assign_stmts_to_flat(assign_stmt, mut ids, mut out) {
			return
		}
		if t.try_expand_if_expr_assign_stmts_to_flat(assign_stmt, mut ids, mut out) {
			return
		}
	}
	if stmt is ast.ExprStmt {
		if t.try_expand_comptime_if_stmt_to_flat(stmt, mut ids, mut out) {
			return
		}
	}
	if stmt is ast.ExprStmt {
		if t.try_expand_or_expr_stmt_to_flat(stmt, mut ids, mut out) {
			return
		}
		if t.try_expand_if_guard_stmt_to_flat(stmt, mut ids, mut out) {
			return
		}
	}
	if stmt is ast.ExprStmt {
		if t.try_emit_flag_enum_set_clear_to_flat(stmt, mut ids, mut out) {
			return
		}
	}
	if stmt is ast.ReturnStmt {
		if t.try_expand_return_match_expr_to_flat(stmt, mut ids, mut out) {
			return
		}
		if t.try_expand_or_expr_return_to_flat(stmt, mut ids, mut out) {
			return
		}
		if t.try_expand_return_if_expr_to_flat(stmt, mut ids, mut out) {
			return
		}
	}
	if stmt is ast.ExprStmt {
		if stmt.expr is ast.LockExpr {
			t.expand_lock_expr_to_flat(stmt.expr, mut ids, mut out)
			return
		}
		if t.try_emit_map_index_push_to_flat(stmt, mut ids, mut out) {
			return
		}
		if t.try_emit_map_index_postfix_to_flat(stmt, mut ids, mut out) {
			return
		}
		if t.try_emit_selector_postfix_to_flat(stmt, mut ids, mut out) {
			return
		}
	}
	if stmt is ast.ForStmt {
		if t.try_expand_for_in_map_to_flat(stmt, mut ids, mut out) {
			return
		}
	}
	if stmt is ast.AssertStmt {
		t.expand_assert_stmt_to_flat(stmt, mut ids, mut out)
		return
	}
	t.append_transformed_stmt_to_flat(mut ids, stmt, mut out)
}

// transform_stmt_list_item_cursor_to_flat is the cursor-input mirror of
// `transform_stmt_list_item_to_flat`. It dispatches on the top-level statement's
// FlatNodeKind so converted arms can be handled without routing through the
// legacy guard chain, and unconverted kinds fall back to the proven decode path
// in one line. This is the seam that lets the transform become cursor-native one
// statement kind at a time (eliminating the whole-subtree `.stmt()` decode at
// the `transform_cursor_stmts_to_flat_direct` loop).
//
// First converted set: true-passthrough top-level kinds that carry no
// `try_expand_*` guard and that `transform_stmt_to_flat` emits verbatim. Those
// copy their flat subtrees directly, so they do not route through `c.stmt()`.

// classify_call_fallback_cursor builds a diagnostic key describing WHY a call
// fell back to the legacy decode path. Instrumentation for V2_FLAT_FB_STATS.
fn (t &Transformer) classify_call_fallback_cursor(c ast.Cursor, prefix string) string {
	lhs := c.edge(0)
	if lhs.kind() != .expr_selector {
		return '${prefix}/lhs=${lhs.kind()}'
	}
	receiver := lhs.edge(0)
	method_name := selector_rhs_name_cursor(lhs)
	if t.has_active_smartcast() {
		return '${prefix}/sel/smartcast'
	}
	if method_name_needs_legacy_selector_pipeline(method_name) {
		return '${prefix}/sel/legacy-list:${method_name}'
	}
	recv_type := t.get_expr_type_cursor(receiver) or { return '${prefix}/sel/no-recv-type' }
	base := t.unwrap_alias_and_pointer_type(recv_type)
	if base is types.Struct {
		if !t.type_has_cached_method(base, method_name) {
			return '${prefix}/sel/no-cached-method'
		}
	} else {
		// Mirror the non-struct receiver gate: only its rejects classify as
		// receiver failures; accepted receivers fall through to late gates.
		if method_name in ['filter', 'map', 'any', 'count', 'wait'] {
			return '${prefix}/sel/expansion-method:${method_name}'
		}
		if base is types.Interface || base is types.SumType {
			return '${prefix}/sel/recv-dispatch=${base.name()}'
		}
		if t.resolve_alias_receiver_method_name(recv_type, method_name) == none
			&& !(t.type_is_string(recv_type)
			&& t.lookup_method_cached('string', method_name) != none) {
			return '${prefix}/sel/recv=${typeof(base).name}:${base.name()}:m=${method_name}'
		}
	}
	if t.resolve_static_type_method_call_cursor(receiver, method_name) != none {
		return '${prefix}/sel/static-shadow'
	}
	call_name := t.resolve_method_call_name_cursor(receiver, method_name) or {
		return '${prefix}/sel/no-call-name'
	}
	if call_name in t.elided_fns {
		return '${prefix}/sel/elided'
	}
	info := t.generic_aware_call_fn_info_cursor(c.edge(0), call_name) or {
		return '${prefix}/sel/no-fn-info'
	}
	arg_count := if c.kind() == .expr_call_or_cast {
		arg0 := c.edge(1)
		if arg0.is_valid() && arg0.kind() != .expr_empty {
			1
		} else {
			0
		}
	} else {
		c.edge_count() - 1
	}
	if info.param_types.len != arg_count {
		return '${prefix}/sel/arity:${call_name}:${info.param_types.len}vs${arg_count}'
	}
	if info.is_variadic {
		return '${prefix}/sel/variadic'
	}
	if info.generic_params.len > 0 {
		return '${prefix}/sel/generic'
	}
	for i in 0 .. arg_count {
		arg := c.edge(i + 1)
		if arg.kind() == .aux_field_init {
			return '${prefix}/sel/field-init-arg'
		}
		if !t.identity_call_arg_can_transform_direct(arg, info.param_types[i]) {
			param_base := t.unwrap_alias_type(info.param_types[i])
			param_kind := match param_base {
				types.Struct { 'struct' }
				types.Enum { 'enum' }
				types.Array { 'array' }
				types.ArrayFixed { 'array_fixed' }
				types.Map { 'map' }
				types.SumType { 'sumtype' }
				types.Interface { 'interface' }
				types.FnType { 'fn' }
				types.OptionType { 'option' }
				types.ResultType { 'result' }
				types.Pointer { 'pointer' }
				types.String { 'string' }
				else { 'other' }
			}

			return '${prefix}/sel/arg-not-identity/param=${param_kind}'
		}
	}
	return '${prefix}/sel/struct-late-gate-unknown'
}

fn (mut t Transformer) transform_stmt_list_item_cursor_to_flat(c ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	match c.kind() {
		.stmt_import, .stmt_module, .stmt_directive, .stmt_empty, .stmt_enum_decl,
		.stmt_interface_decl, .stmt_type_decl, .stmt_asm, .stmt_flow_control, .stmt_attributes {
			id := out.copy_subtree_from(c.flat, c.id)
			t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
		}
		.stmt_const_decl {
			id := t.transform_const_decl_cursor_to_flat(c, mut out)
			t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
		}
		.stmt_block {
			id := t.transform_block_stmt_cursor_to_flat(c, mut out)
			t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
		}
		.stmt_defer {
			id := t.transform_defer_stmt_cursor_to_flat(c, mut out)
			t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
		}
		.stmt_label {
			id := t.transform_label_stmt_cursor_to_flat(c, mut out)
			t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
		}
		.stmt_global_decl {
			id := t.transform_global_decl_cursor_to_flat(c, mut out)
			t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
		}
		.stmt_struct_decl {
			t.count_flat_fallback('stmt_struct_decl')
			id := out.emit_stmt(t.transform_struct_decl(c.struct_decl()))
			t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
		}
		.stmt_assert {
			t.expand_assert_stmt_cursor_to_flat(c, mut ids, mut out)
		}
		.stmt_assign {
			if id := t.transform_map_index_assign_cursor_to_flat(c, mut out) {
				t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
				return
			}
			if t.try_expand_if_expr_assign_cursor_to_flat(c, mut ids, mut out) {
				return
			}
			if t.assign_stmt_cursor_needs_legacy_expand(c) {
				t.count_flat_fallback('stmt_assign')
				t.transform_stmt_list_item_to_flat(assign_stmt_from_cursor(c), mut ids, mut out)
			} else {
				id := t.transform_assign_stmt_cursor_to_flat(c, mut out)
				t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
			}
		}
		.stmt_comptime {
			id := t.transform_comptime_stmt_cursor_to_flat(c, mut out)
			t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
		}
		.stmt_expr {
			if t.try_expand_comptime_if_stmt_cursor_to_flat(c, mut ids, mut out) {
				return
			}
			if c.edge(0).kind() == .expr_lock {
				t.expand_lock_expr_cursor_to_flat(c.edge(0), mut ids, mut out)
				return
			}
			if id := t.transform_flag_enum_set_clear_cursor_to_flat(c, mut out) {
				t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
				return
			}
			if t.expr_stmt_cursor_needs_legacy_expand(c) {
				t.count_flat_fallback('stmt_expr')
				t.transform_stmt_list_item_to_flat(expr_stmt_from_cursor(c), mut ids, mut out)
			} else {
				expr := c.edge(0)
				if expr.kind() == .expr_infix
					&& unsafe { token.Token(int(expr.aux())) } == .left_shift {
					if t.try_emit_map_index_push_to_flat(expr_stmt_from_cursor(c), mut ids, mut out) {
						return
					}
				}
				id := t.transform_expr_stmt_cursor_to_flat(c, mut out)
				t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
			}
		}
		.stmt_return {
			if t.try_expand_return_if_expr_cursor_to_flat(c, mut ids, mut out) {
				return
			}
			if t.return_stmt_cursor_needs_legacy_expand(c) {
				t.count_flat_fallback('stmt_return')
				t.transform_stmt_list_item_to_flat(return_stmt_from_cursor(c), mut ids, mut out)
			} else {
				id := t.transform_return_stmt_cursor_to_flat(c, mut out)
				t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
			}
		}
		.stmt_for_in {
			t.count_flat_fallback('stmt_for_in')
			t.transform_stmt_list_item_to_flat(for_in_stmt_from_cursor(c), mut ids, mut out)
		}
		.stmt_fn_decl {
			decl := c.fn_decl_signature()
			if t.omit_backend_generic_decl(decl) {
				return
			}
			// Stream the body cursor-native when it has no defers (defer
			// lowering needs the whole body, so those take the legacy
			// whole-decl decode path).
			if flat_body_has_defer(c.list_at(3)) {
				t.count_flat_fallback('stmt_fn_decl_defer')
				decl_with_body := fn_decl_signature_with_body_cursor(c.fn_decl_signature(), c)
				t.transform_stmt_list_item_to_flat(decl_with_body, mut ids, mut out)
			} else {
				id := t.transform_fn_decl_streaming_to_flat(c, mut out)
				t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
			}
		}
		.stmt_for {
			// Stream plain `for` loops (cond / classic / bare) cursor-native.
			// For-in loops dispatch through cursor-native lowerings for the
			// migrated iterable families, and only unported forms fall back to
			// the legacy whole-loop decode path.
			init_c := c.edge(0)
			if init_c.is_valid() && init_c.kind() == .stmt_for_in {
				if t.try_expand_range_for_in_cursor_to_flat(c, mut ids, mut out) {
					return
				}
				if t.try_expand_array_for_in_cursor_to_flat(c, mut ids, mut out) {
					return
				}
				if t.try_expand_fixed_array_for_in_cursor_to_flat(c, mut ids, mut out) {
					return
				}
				if t.try_expand_for_in_map_cursor_to_flat(c, mut ids, mut out) {
					return
				}
				if t.try_expand_untyped_for_in_cursor_to_flat(c, mut ids, mut out) {
					return
				}
				if t.try_expand_passthrough_for_in_cursor_to_flat(c, mut ids, mut out) {
					return
				}
				panic('unhandled flat for-in lowering')
			} else {
				id := t.transform_for_stmt_streaming_to_flat(c, mut out)
				t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
			}
		}
		else {
			panic('unexpected flat statement kind: ${c.kind()}')
		}
	}
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
	t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
}

// append_transformed_stmt_id_to_flat hoists any `t.pending_stmts` produced
// while building `id` ahead of it (matching the appender's ordering invariant),
// then pushes `id`. Shared by `append_transformed_stmt_to_flat` and the
// cursor-native stmt arms (transform_stmt_list_item_cursor_to_flat) so both
// drain pending side effects identically.
fn (mut t Transformer) append_transformed_stmt_id_to_flat(mut ids []ast.FlatNodeId, id ast.FlatNodeId, mut out ast.FlatBuilder) {
	// Flat-side hoists drain first: arms that push pending_flat_stmt_ids
	// flush the legacy pending_stmts queue into it beforehand, so emitting
	// the flat queue first preserves the legacy chronological order.
	if t.pending_flat_stmt_ids.len > 0 {
		for fid in t.pending_flat_stmt_ids {
			ids << fid
		}
		t.pending_flat_stmt_ids.clear()
	}
	if t.pending_stmts.len > 0 {
		pending := t.pending_stmts.clone()
		t.pending_stmts.clear()
		for ps in pending {
			ids << out.emit_stmt(ps)
		}
	}
	ids << id
}

// transform_const_decl_cursor_to_flat is the cursor-input mirror of the
// `ast.ConstDecl` arm of `transform_stmt_to_flat` (flat_write.v). It reads the
// const decl's is_public flag and field-init list straight from the cursor —
// the ConstDecl wrapper + FieldInit structure are never decoded to legacy — and
// transforms each field value via the existing `transform_expr_to_flat`. Emit
// order matches the flat-output arm exactly (per-field value+field_init, then
// the fields aux_list, then the stmt).
fn (mut t Transformer) transform_const_decl_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	fields := c.list_at(0)
	mut field_ids := []ast.FlatNodeId{cap: fields.len()}
	for i in 0 .. fields.len() {
		fc := fields.at(i)
		value_id := t.transform_expr_cursor_to_flat(fc.edge(0), mut out)
		field_ids << out.emit_field_init_by_id(fc.name(), value_id)
	}
	fields_list_id := out.emit_aux_list_from_ids(field_ids)
	return out.emit_const_decl_by_ids(c.flag(ast.flag_is_public), fields_list_id)
}

// transform_global_decl_cursor_to_flat is the cursor-input mirror of the
// `ast.GlobalDecl` arm of `transform_stmt_to_flat`. It reads the decl
// attributes, field-decl list, and per-field metadata/flags from the cursor and
// transforms each field's typ + value via `transform_expr_to_flat`. Emit order
// matches the flat-output arm (decl attrs, then per-field typ/value/attrs/
// field_decl, then the fields aux_list, then the stmt).
fn (mut t Transformer) transform_global_decl_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	decl_attrs_id := copy_cursor_aux_list_to_flat(c.list_at(0), mut out)
	fields := c.list_at(1)
	mut field_ids := []ast.FlatNodeId{cap: fields.len()}
	for i in 0 .. fields.len() {
		fc := fields.at(i)
		typ_id := t.transform_expr_cursor_to_flat(fc.edge(0), mut out)
		value_id := t.transform_expr_cursor_to_flat(fc.edge(1), mut out)
		field_attrs_id := copy_cursor_aux_list_to_flat(fc.list_at(2), mut out)
		field := ast.FieldDecl{
			name:                fc.name()
			is_public:           fc.flag(ast.flag_is_public)
			is_mut:              fc.flag(ast.flag_is_mut)
			is_module_mut:       fc.flag(ast.flag_field_is_module_mut)
			is_interface_method: fc.flag(ast.flag_field_is_interface_method)
		}
		field_ids << out.emit_field_decl_by_ids(field, typ_id, value_id, field_attrs_id)
	}
	fields_list_id := out.emit_aux_list_from_ids(field_ids)
	return out.emit_global_decl_by_ids(c.flag(ast.flag_is_public), decl_attrs_id, fields_list_id)
}

fn assoc_expr_from_cursor(c ast.Cursor) ast.AssocExpr {
	mut fields := []ast.FieldInit{cap: c.edge_count() - 2}
	for i in 2 .. c.edge_count() {
		field := c.edge(i)
		fields << ast.FieldInit{
			name:  field.name()
			value: field.edge(0).expr()
		}
	}
	return ast.AssocExpr{
		typ:    c.edge(0).expr()
		expr:   c.edge(1).expr()
		fields: fields
		pos:    c.pos()
	}
}

fn or_expr_from_cursor(c ast.Cursor) ast.OrExpr {
	mut stmts := []ast.Stmt{cap: c.edge_count() - 1}
	for i in 1 .. c.edge_count() {
		stmts << c.edge(i).stmt()
	}
	return ast.OrExpr{
		expr:  c.edge(0).expr()
		stmts: stmts
		pos:   c.pos()
	}
}

fn infix_expr_from_cursor(c ast.Cursor) ast.InfixExpr {
	return ast.InfixExpr{
		op:  unsafe { token.Token(int(c.aux())) }
		lhs: c.edge(0).expr()
		rhs: c.edge(1).expr()
		pos: c.pos()
	}
}

fn (t &Transformer) cursor_is_string_concat_operand(c ast.Cursor) bool {
	if !c.is_valid() {
		return false
	}
	if t.is_string_expr_cursor(c) {
		return true
	}
	match c.kind() {
		.expr_call {
			if c.edge_count() == 0 {
				return false
			}
			lhs := c.edge(0)
			return lhs.kind() == .expr_ident && lhs.name().starts_with('string__')
		}
		.expr_paren, .expr_modifier {
			return c.edge_count() > 0 && t.cursor_is_string_concat_operand(c.edge(0))
		}
		.expr_infix {
			op := unsafe { token.Token(int(c.aux())) }
			return op == .plus && (t.cursor_is_string_concat_operand(c.edge(0))
				|| t.cursor_is_string_concat_operand(c.edge(1)))
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) infix_plus_cursor_can_transform_direct(c ast.Cursor) bool {
	if t.cursor_is_string_concat_operand(c.edge(0)) || t.cursor_is_string_concat_operand(c.edge(1)) {
		return false
	}
	if lhs_type := t.get_expr_type_cursor(c.edge(0)) {
		if lhs_type is types.Struct {
			type_name := t.type_to_c_name(lhs_type)
			if type_name == 'time__Time' {
				return false
			}
		}
	}
	return true
}

fn cursor_is_enum_shorthand_selector(c ast.Cursor) bool {
	if !c.is_valid() || c.kind() != .expr_selector || c.edge_count() == 0 {
		return false
	}
	lhs := c.edge(0)
	return lhs.kind() == .expr_empty || (lhs.kind() == .expr_ident && lhs.name() == '')
}

fn (mut t Transformer) enum_shorthand_cursor_to_flat(c ast.Cursor, enum_type string, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if enum_type == '' || !cursor_is_enum_shorthand_selector(c) {
		return none
	}
	member := c.edge(1)
	if typ := t.lookup_type(enum_type) {
		t.register_synth_type(c.pos(), typ)
		if typ is types.Enum {
			return out.emit_ident_by_name(t.enum_member_ident_for_lookup(enum_type, typ,
				member.name()), c.pos())
		}
	}
	return out.emit_ident_by_name(enum_member_ident(enum_type, member.name()), c.pos())
}

fn (mut t Transformer) transform_enum_shorthand_compare_cursor_to_flat(c ast.Cursor, op token.Token, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if op !in [.eq, .ne] || c.edge_count() < 2 {
		return none
	}
	lhs := c.edge(0)
	rhs := c.edge(1)
	if cursor_is_enum_shorthand_selector(rhs) {
		enum_type := t.get_enum_type_name_cursor(lhs)
		rhs_id := t.enum_shorthand_cursor_to_flat(rhs, enum_type, mut out) or { return none }
		lhs_id := t.transform_expr_cursor_to_flat(lhs, mut out)
		return out.emit_infix_expr_by_ids(op, lhs_id, rhs_id, c.pos())
	}
	if cursor_is_enum_shorthand_selector(lhs) {
		enum_type := t.get_enum_type_name_cursor(rhs)
		lhs_id := t.enum_shorthand_cursor_to_flat(lhs, enum_type, mut out) or { return none }
		rhs_id := t.transform_expr_cursor_to_flat(rhs, mut out)
		return out.emit_infix_expr_by_ids(op, lhs_id, rhs_id, c.pos())
	}
	return none
}

fn (t &Transformer) sumtype_check_variant_name_cursor(rhs ast.Cursor) (string, string) {
	if rhs.kind() == .expr_ident {
		return rhs.name(), ''
	}
	if rhs.kind() == .expr_selector {
		mut variant_module := ''
		rhs_lhs := rhs.edge(0)
		if rhs_lhs.kind() == .expr_ident {
			variant_module = rhs_lhs.name()
		}
		return selector_rhs_name_cursor(rhs), variant_module
	}
	return t.type_expr_to_variant_name_cursor(rhs), ''
}

fn (mut t Transformer) transform_interface_self_type_check_cursor_to_flat(lhs ast.Cursor, variant_name string, op token.Token, pos token.Pos, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	lhs_type := t.get_expr_type_cursor(lhs) or { return none }
	lhs_base := t.unwrap_alias_and_pointer_type(lhs_type)
	if lhs_base !is types.Interface {
		return none
	}
	lhs_base_name := lhs_base.name()
	if !lhs_base_name.ends_with(variant_name) && lhs_base_name != variant_name {
		return none
	}
	lhs_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	type_id := t.synth_selector_cursor_to_flat(lhs_id, '_type_id', types.Type(types.int_), mut out)
	zero_id := out.emit_basic_literal_by_value(.number, '0', pos)
	cmp_op := if op in [.key_is, .eq] { token.Token.ne } else { token.Token.eq }
	return out.emit_infix_expr_by_ids(cmp_op, type_id, zero_id, pos)
}

fn (mut t Transformer) transform_sumtype_check_cursor_to_flat(c ast.Cursor, op token.Token, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if op !in [.key_is, .not_is, .eq, .ne] || c.edge_count() < 2 || t.has_active_smartcast() {
		return none
	}
	lhs := c.edge(0)
	rhs := c.edge(1)
	variant_name, variant_module := t.sumtype_check_variant_name_cursor(rhs)
	if variant_name == '' {
		return none
	}
	variant_lookup_name := sum_type_variant_name_with_module(variant_module, variant_name)
	mut sumtype_name := t.generic_scan_sumtype_name_for_expr_cursor(lhs)
	mut lhs_is_enum := t.get_enum_type_name_cursor(lhs) != ''
	if lhs_type := t.get_expr_type_cursor(lhs) {
		lhs_base := t.unwrap_alias_and_pointer_type(lhs_type)
		lhs_is_enum = lhs_is_enum || lhs_base is types.Enum
	}
	rhs_is_type_like := variant_name.len > 0 && variant_name[0] >= `A` && variant_name[0] <= `Z`
	if sumtype_name == '' && !lhs_is_enum && !cursor_is_enum_shorthand_selector(rhs)
		&& (op in [.key_is, .not_is] || rhs_is_type_like) {
		sumtype_name = t.find_sumtype_for_variant(variant_lookup_name)
	}
	if sumtype_name == '' {
		if result_id := t.transform_interface_self_type_check_cursor_to_flat(lhs, variant_name, op,
			c.pos(), mut out)
		{
			return result_id
		}
		return none
	}
	variants := t.get_sum_type_variants(sumtype_name)
	mut tag_value := -1
	for i, variant in variants {
		if sum_type_variant_matches_for_sumtype(sumtype_name, variant, variant_lookup_name) {
			tag_value = i
			break
		}
	}
	if tag_value < 0 {
		return none
	}
	lhs_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	tag_id := t.synth_selector_cursor_to_flat(lhs_id, '_tag', types.Type(types.int_), mut out)
	tag_value_id := out.emit_basic_literal_by_value(.number, tag_value.str(), c.pos())
	cmp_op := if op in [.key_is, .eq] { token.Token.eq } else { token.Token.ne }
	return out.emit_infix_expr_by_ids(cmp_op, tag_id, tag_value_id, c.pos())
}

fn (t &Transformer) scalar_equality_type_can_transform_direct(typ types.Type) bool {
	if t.type_is_string(typ) {
		return false
	}
	base := t.unwrap_alias_type(typ)
	return match base {
		types.Primitive, types.Char, types.ISize, types.Nil, types.Pointer, types.Rune,
		types.USize {
			true
		}
		else {
			false
		}
	}
}

fn (t &Transformer) infix_equality_cursor_can_transform_direct(c ast.Cursor) bool {
	lhs := c.edge(0)
	rhs := c.edge(1)
	if cursor_is_enum_shorthand_selector(lhs) || cursor_is_enum_shorthand_selector(rhs) {
		return false
	}
	lhs_type := t.get_expr_type_cursor(lhs) or { return false }
	rhs_type := t.get_expr_type_cursor(rhs) or { return false }
	return t.scalar_equality_type_can_transform_direct(lhs_type)
		&& t.scalar_equality_type_can_transform_direct(rhs_type)
}

fn (t &Transformer) is_nil_expr_cursor(c ast.Cursor) bool {
	return match c.kind() {
		.expr_ident {
			c.name() == 'nil'
		}
		.expr_keyword {
			unsafe { token.Token(int(c.aux())) } == .key_nil
		}
		.typ_nil {
			true
		}
		.expr_basic_literal {
			unsafe { token.Token(int(c.aux())) } == .number && c.name() == '0'
		}
		.expr_paren, .expr_cast, .expr_modifier {
			c.edge_count() > 0 && t.is_nil_expr_cursor(c.edge(0))
		}
		else {
			false
		}
	}
}

fn cursor_is_c_string_literal(c ast.Cursor) bool {
	return match c.kind() {
		.expr_string {
			unsafe { ast.StringLiteralKind(int(c.aux())) } == .c
		}
		.expr_paren, .expr_modifier {
			c.edge_count() > 0 && cursor_is_c_string_literal(c.edge(0))
		}
		else {
			false
		}
	}
}

fn cursor_is_foldable_v_string_literal(c ast.Cursor) bool {
	return match c.kind() {
		.expr_string {
			unsafe { ast.StringLiteralKind(int(c.aux())) } == .v
		}
		.expr_paren {
			c.edge_count() > 0 && cursor_is_foldable_v_string_literal(c.edge(0))
		}
		else {
			false
		}
	}
}

fn (t &Transformer) is_v_string_expr_cursor(c ast.Cursor) bool {
	return !cursor_is_c_string_literal(c) && t.is_string_expr_cursor(c)
}

fn (t &Transformer) infix_string_compare_cursor_can_transform_direct(c ast.Cursor, op token.Token) bool {
	if op !in [.eq, .ne, .lt, .gt, .le, .ge] || c.edge_count() < 2 {
		return false
	}
	lhs := c.edge(0)
	rhs := c.edge(1)
	return !t.is_nil_expr_cursor(lhs) && !t.is_nil_expr_cursor(rhs)
		&& t.is_v_string_expr_cursor(lhs) && t.is_v_string_expr_cursor(rhs)
}

fn (mut t Transformer) string_compare_call_cursor_to_flat(name string, first ast.Cursor, second ast.Cursor, pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	lhs_id := out.emit_ident_by_name(name, token.Pos{})
	first_id := t.transform_expr_cursor_to_flat(first, mut out)
	second_id := t.transform_expr_cursor_to_flat(second, mut out)
	return out.emit_call_expr_by_ids(lhs_id, [first_id, second_id], pos)
}

fn (mut t Transformer) transform_string_compare_cursor_to_flat(c ast.Cursor, op token.Token, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if !t.infix_string_compare_cursor_can_transform_direct(c, op) {
		return none
	}
	lhs := c.edge(0)
	rhs := c.edge(1)
	match op {
		.eq {
			return t.string_compare_call_cursor_to_flat('string__eq', lhs, rhs, c.pos(), mut out)
		}
		.ne {
			call_id :=
				t.string_compare_call_cursor_to_flat('string__eq', lhs, rhs, c.pos(), mut out)
			return out.emit_prefix_expr_by_id(.not, call_id, c.pos())
		}
		.lt {
			return t.string_compare_call_cursor_to_flat('string__lt', lhs, rhs, c.pos(), mut out)
		}
		.gt {
			return t.string_compare_call_cursor_to_flat('string__lt', rhs, lhs, c.pos(), mut out)
		}
		.le {
			call_id :=
				t.string_compare_call_cursor_to_flat('string__lt', rhs, lhs, c.pos(), mut out)
			return out.emit_prefix_expr_by_id(.not, call_id, c.pos())
		}
		.ge {
			call_id :=
				t.string_compare_call_cursor_to_flat('string__lt', lhs, rhs, c.pos(), mut out)
			return out.emit_prefix_expr_by_id(.not, call_id, c.pos())
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) infix_string_concat_cursor_can_transform_direct(c ast.Cursor, op token.Token) bool {
	if op != .plus || c.edge_count() < 2 {
		return false
	}
	lhs := c.edge(0)
	rhs := c.edge(1)
	if lhs.kind() == .expr_infix || rhs.kind() == .expr_infix {
		return false
	}
	if cursor_is_foldable_v_string_literal(lhs) && cursor_is_foldable_v_string_literal(rhs) {
		return false
	}
	return t.is_v_string_expr_cursor(lhs) && t.is_v_string_expr_cursor(rhs)
}

fn (mut t Transformer) transform_string_concat_cursor_to_flat(c ast.Cursor, op token.Token, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if !t.infix_string_concat_cursor_can_transform_direct(c, op) {
		return none
	}
	lhs_id := out.emit_ident_by_name('string__plus', token.Pos{})
	left_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
	right_id := t.transform_expr_cursor_to_flat(c.edge(1), mut out)
	return out.emit_call_expr_by_ids(lhs_id, [left_id, right_id], c.pos())
}

fn (mut t Transformer) transform_range_membership_cursor_to_flat(c ast.Cursor, op token.Token, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if op !in [.key_in, .not_in] || c.edge_count() < 2 {
		return none
	}
	lhs := c.edge(0)
	range := c.edge(1)
	if !range.is_valid() || range.kind() != .expr_range || range.edge_count() < 2 {
		return none
	}
	lhs_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	start_id := t.transform_expr_cursor_to_flat(range.edge(0), mut out)
	lower_id := out.emit_infix_expr_by_ids(.ge, lhs_id, start_id, c.pos())
	mut range_check_id := lower_id
	end := range.edge(1)
	if end.is_valid() && end.kind() != .expr_empty {
		range_op := unsafe { token.Token(int(range.aux())) }
		upper_op := if range_op == .dotdot { token.Token.lt } else { token.Token.le }
		end_id := t.transform_expr_cursor_to_flat(end, mut out)
		upper_id := out.emit_infix_expr_by_ids(upper_op, lhs_id, end_id, c.pos())
		range_check_id = out.emit_infix_expr_by_ids(.and, lower_id, upper_id, c.pos())
	}
	if op == .not_in {
		return out.emit_prefix_expr_by_id(.not, range_check_id, c.pos())
	}
	return range_check_id
}

fn (mut t Transformer) transform_inline_array_membership_elem_cursor_to_flat(elem ast.Cursor, enum_type string, mut out ast.FlatBuilder) ast.FlatNodeId {
	if enum_type != '' {
		if enum_id := t.enum_shorthand_cursor_to_flat(elem, enum_type, mut out) {
			return enum_id
		}
	}
	return t.transform_expr_cursor_to_flat(elem, mut out)
}

fn (mut t Transformer) transform_inline_array_membership_cursor_to_flat(c ast.Cursor, op token.Token, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if op !in [.key_in, .not_in] || c.edge_count() < 2 {
		return none
	}
	lhs := c.edge(0)
	rhs := c.edge(1)
	if !rhs.is_valid() || rhs.kind() != .expr_array_init || rhs.edge_count() <= 5 {
		return none
	}
	enum_type := t.get_enum_type_name_cursor(lhs)
	lhs_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	first_elem_id := t.transform_inline_array_membership_elem_cursor_to_flat(rhs.edge(5),
		enum_type, mut out)
	mut chain_id := out.emit_infix_expr_by_ids(.eq, lhs_id, first_elem_id, c.pos())
	for i in 6 .. rhs.edge_count() {
		elem_id :=
			t.transform_inline_array_membership_elem_cursor_to_flat(rhs.edge(i), enum_type, mut out)
		elem_cmp_id := out.emit_infix_expr_by_ids(.eq, lhs_id, elem_id, c.pos())
		chain_id = out.emit_infix_expr_by_ids(.logical_or, chain_id, elem_cmp_id, c.pos())
	}
	if op == .not_in {
		return out.emit_prefix_expr_by_id(.not, chain_id, c.pos())
	}
	return chain_id
}

fn (mut t Transformer) transform_map_membership_cursor_to_flat(c ast.Cursor, op token.Token, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if op !in [.key_in, .not_in] || c.edge_count() < 2 {
		return none
	}
	lhs := c.edge(0)
	rhs := c.edge(1)
	rhs_type := t.map_index_lhs_type_cursor(rhs) or { return none }
	map_type := t.unwrap_map_type(rhs_type) or { return none }
	if t.is_eval_backend() {
		lhs_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
		rhs_id := t.transform_expr_cursor_to_flat(c.edge(1), mut out)
		return out.emit_infix_expr_by_ids(op, lhs_id, rhs_id, c.pos())
	}
	map_ptr_id := t.map_expr_to_runtime_ptr_cursor(rhs, rhs_type, mut out) or { return none }
	key_value_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	mut key_ptr_id := ast.FlatNodeId(0)
	mut key_temp_assign_id := ast.FlatNodeId(0)
	has_key_temp := !t.can_take_address_expr_cursor(lhs)
		|| t.is_enum_rvalue_cursor(lhs, map_type.key_type)
	if has_key_temp {
		key_ident := t.typed_temp_ident(t.gen_temp_name(), map_type.key_type)
		key_lhs_id := out.emit_ident_by_name(key_ident.name, key_ident.pos)
		key_temp_assign_id = out.emit_assign_stmt_by_ids(.decl_assign, [key_lhs_id], [
			key_value_id,
		], key_ident.pos)
		key_ref_id := out.emit_ident_by_name(key_ident.name, key_ident.pos)
		key_ptr_id = out.emit_prefix_expr_by_id(.amp, key_ref_id, token.Pos{})
	} else {
		key_ptr_id = out.emit_prefix_expr_by_id(.amp, key_value_id, token.Pos{})
	}
	call_lhs_id := out.emit_ident_by_name('map__exists', token.Pos{})
	call_id := out.emit_call_expr_by_ids(call_lhs_id, [map_ptr_id, key_ptr_id], c.pos())
	result_id := if op == .not_in {
		out.emit_prefix_expr_by_id(.not, call_id, c.pos())
	} else {
		call_id
	}
	if has_key_temp {
		expr_stmt_id := out.emit_expr_stmt_by_id(result_id)
		return out.emit_unsafe_expr_by_ids([key_temp_assign_id, expr_stmt_id], token.Pos{})
	}
	return result_id
}

fn (t &Transformer) map_index_lhs_type_cursor(lhs ast.Cursor) ?types.Type {
	if !lhs.is_valid() {
		return none
	}
	if lhs.kind() == .expr_paren {
		return t.map_index_lhs_type_cursor(lhs.edge(0))
	}
	if lhs.kind() == .expr_prefix {
		op := unsafe { token.Token(int(lhs.aux())) }
		if op == .mul {
			if inner_type := t.get_expr_type_cursor(lhs.edge(0)) {
				if inner_type is types.Pointer {
					return inner_type.base_type
				}
			}
		}
	}
	return t.get_expr_type_cursor(lhs)
}

fn (mut t Transformer) map_expr_to_runtime_ptr_cursor(expr ast.Cursor, typ types.Type, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if t.is_pointer_type(typ) {
		return t.transform_expr_cursor_to_flat(expr, mut out)
	}
	if !t.can_take_address_expr_cursor(expr) {
		return none
	}
	transformed_id := t.transform_expr_cursor_to_flat(expr, mut out)
	return out.emit_prefix_expr_by_id(.amp, transformed_id, token.Pos{})
}

fn (t &Transformer) is_enum_rvalue_cursor(expr ast.Cursor, typ types.Type) bool {
	if expr.kind() !in [.expr_ident, .expr_selector] {
		return false
	}
	mut base := typ
	for _ in 0 .. 64 {
		if base is types.Alias {
			base = (base as types.Alias).base_type
			continue
		}
		if base is types.Pointer {
			base = (base as types.Pointer).base_type
			continue
		}
		break
	}
	if base is types.Enum {
		return true
	}
	if expr_typ := t.get_expr_type_cursor(expr) {
		mut expr_base := expr_typ
		for _ in 0 .. 64 {
			if expr_base is types.Alias {
				expr_base = (expr_base as types.Alias).base_type
				continue
			}
			break
		}
		if expr_base is types.Enum {
			return true
		}
	}
	return false
}

fn (t &Transformer) can_take_address_expr_cursor(expr ast.Cursor) bool {
	if !expr.is_valid() {
		return false
	}
	return match expr.kind() {
		.expr_ident {
			t.ident_is_addressable_lvalue(expr.name())
		}
		.expr_selector {
			t.selector_expr_is_addressable_lvalue_cursor(expr)
		}
		.expr_index {
			t.index_expr_is_addressable_lvalue_cursor(expr)
		}
		.expr_string, .expr_init, .expr_array_init {
			true
		}
		.expr_prefix {
			unsafe { token.Token(int(expr.aux())) } == .mul
		}
		.expr_paren {
			t.can_take_address_expr_cursor(expr.edge(0))
		}
		else {
			false
		}
	}
}

fn (t &Transformer) index_expr_is_addressable_lvalue_cursor(expr ast.Cursor) bool {
	if map_type := t.map_index_lhs_type_cursor(expr.edge(0)) {
		if _ := t.unwrap_map_type(map_type) {
			return false
		}
	}
	return t.selector_lhs_can_take_field_address_cursor(expr.edge(0))
}

fn (t &Transformer) selector_expr_is_addressable_lvalue_cursor(expr ast.Cursor) bool {
	lhs := expr.edge(0)
	if !lhs.is_valid() || lhs.kind() == .expr_empty {
		return false
	}
	if t.selector_resolves_to_non_lvalue_symbol_cursor(expr) {
		return false
	}
	return t.selector_lhs_can_take_field_address_cursor(lhs)
}

fn (t &Transformer) selector_lhs_can_take_field_address_cursor(lhs ast.Cursor) bool {
	match lhs.kind() {
		.expr_ident {
			return t.ident_is_addressable_lvalue(lhs.name())
		}
		.expr_selector {
			return t.selector_expr_is_addressable_lvalue_cursor(lhs)
		}
		.expr_index {
			return t.index_expr_is_addressable_lvalue_cursor(lhs)
		}
		.expr_prefix {
			op := unsafe { token.Token(int(lhs.aux())) }
			return op == .mul || op == .amp
		}
		.expr_paren, .expr_modifier {
			return t.selector_lhs_can_take_field_address_cursor(lhs.edge(0))
		}
		.expr_string, .expr_init, .expr_array_init {
			return true
		}
		else {
			return t.expr_has_pointer_type_cursor(lhs)
		}
	}
}

fn (t &Transformer) expr_has_pointer_type_cursor(expr ast.Cursor) bool {
	if typ := t.get_expr_type_cursor(expr) {
		return t.is_pointer_type(typ)
	}
	return false
}

fn (t &Transformer) selector_resolves_to_non_lvalue_symbol_cursor(expr ast.Cursor) bool {
	lhs := expr.edge(0)
	if lhs.kind() == .expr_ident {
		lhs_name := lhs.name()
		rhs_name := selector_rhs_name_cursor(expr)
		if t.is_module_ident(lhs_name) {
			if obj := t.lookup_module_object(lhs_name, rhs_name) {
				return obj is types.Const || obj is types.Fn || obj is types.Module
					|| obj is types.TypeObject || obj is types.Type
			}
			return true
		}
		qualified := if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !lhs_name.contains('__') {
			'${t.cur_module}__${lhs_name}'
		} else {
			lhs_name
		}
		if _ := t.lookup_type(qualified) {
			return true
		}
	}
	return false
}

fn (t &Transformer) infix_and_cursor_can_transform_direct(c ast.Cursor) bool {
	for term in t.flatten_and_terms_unwrapped_cursor(c) {
		if term.kind() == .expr_infix {
			if _ := t.smartcast_context_from_condition_term_cursor(term) {
				return false
			}
		}
	}
	return true
}

// get_struct_field_type_cursor mirrors get_struct_field_type (struct.v) for a
// cursor selector, minus the smartcast branch — callers gate on
// !has_active_smartcast(), which makes that branch a no-op in the legacy
// pipeline too.
fn (t &Transformer) get_struct_field_type_cursor(sel ast.Cursor) ?types.Type {
	field_name := selector_rhs_name_cursor(sel)
	if field_name == '' {
		return none
	}
	lhs := sel.edge(0)
	mut struct_type_name := ''
	if lhs.kind() == .expr_ident {
		lhs_name := lhs.name()
		if lhs_name == '' {
			return none
		}
		if lhs_type := t.lookup_var_type(lhs_name) {
			if field_typ := t.field_type_from_receiver_type(lhs_type, field_name) {
				return field_typ
			}
			base_type := lhs_type.base_type()
			if base_type is types.Struct {
				if field_typ := t.lookup_struct_field_type(base_type.name, field_name) {
					return field_typ
				}
			}
			struct_type_name = t.type_to_name(base_type)
		}
	}
	if struct_type_name != '' {
		looked_up_type := t.lookup_type(struct_type_name) or { return none }
		looked_base := if looked_up_type is types.Pointer {
			looked_up_type.base_type
		} else {
			looked_up_type
		}
		match looked_base {
			types.Struct {
				if field_typ := t.lookup_struct_field_type(looked_base.name, field_name) {
					return field_typ
				}
			}
			else {}
		}
	}
	struct_type := t.get_expr_type_cursor(lhs) or { return none }
	base_type := if struct_type is types.Pointer {
		struct_type.base_type
	} else {
		struct_type
	}
	match base_type {
		types.Struct {
			if field_typ := t.lookup_struct_field_type(base_type.name, field_name) {
				return field_typ
			}
		}
		else {}
	}

	return none
}

// try_transform_array_append_cursor_to_flat streams the common `arr << value`
// single-element append cursor-native, mirroring the legacy lowering in
// transform_infix_expr (expr.v):
//   builtin__array_push_noscan((array*)&arr, (elem_type[]){ value })
// The gates are deliberately strict so that every accepted shape provably
// takes the same single-push path in the legacy pipeline:
//   - lhs is a plain ident whose scope type resolves to a dynamic array
//     (mirrors get_array_elem_type_str's first, deterministic branch);
//   - the elem type is not a (pointer-to-)sum type (no
//     wrap_array_push_elem_value effect) and not a nested array (no
//     push_many ambiguity);
//   - the rhs has no calls (the legacy path hoists call-bearing values into
//     a pending `_ap_tN` temp), is not enum shorthand, not a bitwise infix
//     (reassociation), not an `_or_tN.data` extract, and its checker type is
//     definitively NOT an array (so push_many cannot apply).
// Everything else returns none and keeps the legacy decode fallback.
fn (mut t Transformer) try_transform_array_append_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if t.is_eval_backend() {
		return none
	}
	lhs := c.edge(0)
	rhs := c.edge(1)
	mut elem_type_name := ''
	mut lhs_is_ptr := false
	if lhs.kind() == .expr_ident {
		lhs_typ := t.lookup_var_type(lhs.name()) or { return none }
		lhs_base := t.unwrap_alias_and_pointer_type(lhs_typ)
		if lhs_base !is types.Array {
			return none
		}
		arr_type := lhs_base as types.Array
		// Mirrors the ident branch of get_array_elem_type_str (which
		// normalizes literal elem type names).
		elem_type_name =
			t.normalize_literal_type(t.array_elem_type_name_for_helpers(arr_type.elem_type))
		lhs_is_ptr = (lhs.name() == t.cur_fn_recv_param && t.cur_fn_recv_is_ptr)
			|| t.is_pointer_type(lhs_typ)
	} else if lhs.kind() == .expr_selector {
		// Mirrors the SelectorExpr branch of get_array_elem_type_str: struct
		// field type first, then the env type of the whole selector. Neither
		// normalizes the elem name. Smartcast scopes stay legacy (the legacy
		// chain consults smartcast_type_for_expr first).
		if t.has_active_smartcast() {
			return none
		}
		if field_typ := t.get_struct_field_type_cursor(lhs) {
			field_base := t.unwrap_alias_and_pointer_type(field_typ)
			if field_base is types.Array {
				elem_type_name = t.array_elem_type_name_for_helpers(field_base.elem_type)
			}
		}
		if elem_type_name == '' {
			typ := t.get_expr_type_cursor(lhs) or { return none }
			base := t.unwrap_alias_and_pointer_type(typ)
			if base !is types.Array {
				return none
			}
			arr_type := base as types.Array
			elem_type_name = t.array_elem_type_name_for_helpers(arr_type.elem_type)
		}
		// is_pointer_type_expr has no selector branch: always address-of.
		lhs_is_ptr = false
	} else {
		return none
	}
	if elem_type_name == '' || elem_type_name == 'void' {
		return none
	}
	if elem_type_name.starts_with('Array_') || elem_type_name == 'array' {
		return none
	}
	if t.sumtype_pointer_base(elem_type_name) != '' {
		return none
	}
	elem_is_sumtype := t.is_sum_type(elem_type_name)
	if rhs.kind() == .expr_selector {
		rhs_lhs := rhs.edge(0)
		if !rhs_lhs.is_valid() || rhs_lhs.kind() == .expr_empty {
			return none
		}
		if selector_rhs_name_cursor(rhs) == 'data' && rhs_lhs.kind() == .expr_ident
			&& rhs_lhs.name().starts_with('_or_t') {
			return none
		}
	}
	if rhs.kind() == .expr_infix {
		rhs_op := unsafe { token.Token(int(rhs.aux())) }
		if rhs_op in [.amp, .pipe, .xor, .left_shift, .right_shift] {
			return none
		}
	}
	rhs_typ := t.get_expr_type_cursor(rhs) or { return none }
	rhs_base := t.unwrap_alias_and_pointer_type(rhs_typ)
	mut push_many := false
	if rhs_base is types.Array {
		// push_many candidate: resolve the rhs elem type the same way the
		// legacy append_rhs_is_array_value_compatible chain does (ident:
		// scope type, normalized; selector: struct field type, then the env
		// type of the whole selector, unnormalized), then require
		// push-compatibility with the lhs elem. Other rhs shapes keep the
		// legacy path.
		mut rhs_elem := ''
		if rhs.kind() == .expr_ident {
			rhs_scope_typ := t.lookup_var_type(rhs.name()) or { return none }
			rhs_scope_base := t.unwrap_alias_and_pointer_type(rhs_scope_typ)
			if rhs_scope_base !is types.Array {
				return none
			}
			rhs_arr := rhs_scope_base as types.Array
			rhs_elem =
				t.normalize_literal_type(t.array_elem_type_name_for_helpers(rhs_arr.elem_type))
		} else if rhs.kind() == .expr_selector {
			if t.has_active_smartcast() {
				return none
			}
			if field_typ := t.get_struct_field_type_cursor(rhs) {
				field_base := t.unwrap_alias_and_pointer_type(field_typ)
				if field_base is types.Array {
					rhs_elem = t.array_elem_type_name_for_helpers(field_base.elem_type)
				}
			}
			if rhs_elem == '' {
				rhs_arr := rhs_base as types.Array
				rhs_elem = t.array_elem_type_name_for_helpers(rhs_arr.elem_type)
			}
		} else {
			return none
		}
		if !t.array_elem_types_compatible(elem_type_name, rhs_elem) {
			return none
		}
		push_many = true
	} else if rhs_base is types.ArrayFixed {
		return none
	} else if rhs.kind() == .expr_ident {
		// The legacy rhs analysis consults the scope before the checker env;
		// require both to agree that the value is not an array.
		if rhs_scope_typ := t.lookup_var_type(rhs.name()) {
			rhs_scope_base := t.unwrap_alias_and_pointer_type(rhs_scope_typ)
			if rhs_scope_base is types.Array || rhs_scope_base is types.ArrayFixed {
				return none
			}
		}
	}
	if elem_is_sumtype {
		// Only the no-wrap case streams: when the pushed value's checker type
		// IS the sum type, wrap_array_push_elem_value provably returns it
		// unchanged (for hoisted call values the legacy path wraps the
		// `_ap_tN` temp ident, whose registered type is the sum type — same
		// no-op). Variant-typed values that need the sumtype init wrap keep
		// the legacy path, as do smartcast scopes.
		if t.has_active_smartcast() {
			return none
		}
		if !t.is_same_sumtype_name(t.type_to_c_name(rhs_typ), elem_type_name) {
			return none
		}
		if rhs.kind() == .expr_ident {
			rhs_scope_typ := t.lookup_var_type(rhs.name()) or { return none }
			if !t.is_same_sumtype_name(t.type_to_c_name(rhs_scope_typ), elem_type_name) {
				return none
			}
		} else if rhs.kind() != .expr_selector && !t.contains_call_expr_cursor(rhs) {
			return none
		}
	}
	// Emission mirrors the legacy tree shape and evaluation order exactly
	// (lhs transform, then rhs transform; synthetic wrapper nodes carry a
	// zero pos like their legacy counterparts).
	lhs_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	cast_inner_id := if lhs_is_ptr {
		lhs_id
	} else {
		out.emit_prefix_expr_by_id(.amp, lhs_id, c.pos())
	}
	array_ptr_typ_id := out.emit_ident_by_name('array*', token.Pos{})
	arr_ptr_id := out.emit_cast_expr_by_ids(array_ptr_typ_id, cast_inner_id, token.Pos{})
	mut rhs_id := t.transform_expr_cursor_to_flat(rhs, mut out)
	if push_many {
		// array__push_many(arr_ptr, rhs.data, rhs.len). Call-bearing values
		// are hoisted into a `_pm_tN` temp first (the legacy path does the
		// same so .data/.len don't evaluate the call twice); ident/selector
		// rhs is never a PrefixExpr, so no paren wrap applies. data/len are
		// typed synth selectors exactly like the legacy synth_selector calls
		// (same synth-pos consumption order).
		if t.contains_call_expr_cursor(rhs) {
			if t.pending_stmts.len > 0 {
				pending := t.pending_stmts.clone()
				t.pending_stmts.clear()
				for ps in pending {
					t.pending_flat_stmt_ids << out.emit_stmt(ps)
				}
			}
			t.temp_counter++
			tmp_name := '_pm_t${t.temp_counter}'
			if rhs_type := t.get_expr_type_cursor(rhs) {
				t.register_temp_var(tmp_name, rhs_type)
			}
			tmp_lhs_id := out.emit_ident_by_name(tmp_name, token.Pos{})
			t.pending_flat_stmt_ids << out.emit_assign_stmt_by_ids(.decl_assign, [
				tmp_lhs_id,
			], [rhs_id], token.Pos{})
			rhs_id = out.emit_ident_by_name(tmp_name, token.Pos{})
		}
		data_id :=
			t.synth_selector_cursor_to_flat(rhs_id, 'data', types.Type(types.voidptr_), mut out)
		len_id := t.synth_selector_cursor_to_flat(rhs_id, 'len', types.Type(types.int_), mut out)
		push_many_lhs_id := out.emit_ident_by_name('array__push_many', token.Pos{})
		return out.emit_call_expr_by_ids(push_many_lhs_id, [arr_ptr_id, data_id, len_id], c.pos())
	}
	if t.contains_call_expr_cursor(rhs) {
		// The legacy path hoists call-bearing values into a `_ap_tN` temp via
		// pending_stmts so the call is evaluated before the push. Flush the
		// chronologically-earlier legacy pendings into the flat queue first,
		// then queue the temp assign as an already-flat stmt.
		if t.pending_stmts.len > 0 {
			pending := t.pending_stmts.clone()
			t.pending_stmts.clear()
			for ps in pending {
				t.pending_flat_stmt_ids << out.emit_stmt(ps)
			}
		}
		t.temp_counter++
		tmp_name := '_ap_t${t.temp_counter}'
		if rhs_type := t.get_expr_type_cursor(rhs) {
			t.register_temp_var(tmp_name, rhs_type)
		}
		tmp_lhs_id := out.emit_ident_by_name(tmp_name, token.Pos{})
		t.pending_flat_stmt_ids << out.emit_assign_stmt_by_ids(.decl_assign, [
			tmp_lhs_id,
		], [rhs_id], token.Pos{})
		rhs_id = out.emit_ident_by_name(tmp_name, token.Pos{})
	}
	value_id := if elem_type_name == 'string' {
		clone_lhs_id := out.emit_ident_by_name('string__clone', token.Pos{})
		out.emit_call_expr_by_ids(clone_lhs_id, [rhs_id], token.Pos{})
	} else {
		rhs_id
	}
	elem_ident_id := out.emit_ident_by_name(elem_type_name, token.Pos{})
	arr_typ_id := out.emit_array_type_by_elem_id(elem_ident_id)
	arr_lit_id := out.emit_array_init_expr_by_ids(arr_typ_id, out.emit_expr(ast.empty_expr),
		out.emit_expr(ast.empty_expr), out.emit_expr(ast.empty_expr),
		out.emit_expr(ast.empty_expr), [value_id], token.Pos{})
	push_lhs_id := out.emit_ident_by_name('builtin__array_push_noscan', token.Pos{})
	return out.emit_call_expr_by_ids(push_lhs_id, [arr_ptr_id, arr_lit_id], c.pos())
}

fn (t &Transformer) infix_left_shift_cursor_can_transform_direct(c ast.Cursor) bool {
	lhs_type := t.get_expr_type_cursor(c.edge(0)) or { return false }
	lhs_base := t.unwrap_alias_and_pointer_type(lhs_type)
	return lhs_base !is types.Array && lhs_base !is types.ArrayFixed
}

fn (t &Transformer) infix_cursor_can_transform_direct(c ast.Cursor, op token.Token) bool {
	match op {
		.and {
			return t.infix_and_cursor_can_transform_direct(c)
		}
		.plus {
			return t.infix_plus_cursor_can_transform_direct(c)
		}
		.eq, .ne {
			return t.infix_equality_cursor_can_transform_direct(c)
		}
		.minus, .mul, .div, .mod {
			if lhs_type := t.get_expr_type_cursor(c.edge(0)) {
				if lhs_type is types.Struct {
					type_name := t.type_to_c_name(lhs_type)
					if type_name == 'time__Time' {
						return false
					}
				}
			}
			return true
		}
		.right_shift, .right_shift_unsigned {
			return true
		}
		.left_shift {
			return t.infix_left_shift_cursor_can_transform_direct(c)
		}
		.amp, .pipe, .xor {
			lhs := c.edge(0)
			if lhs.kind() == .expr_infix {
				lhs_op := unsafe { token.Token(int(lhs.aux())) }
				if lhs_op == .left_shift && !t.infix_left_shift_cursor_can_transform_direct(lhs) {
					return false
				}
			}
			return true
		}
		.logical_or {
			return true
		}
		.lt, .gt, .le, .ge {
			return !t.is_string_expr_cursor(c.edge(0)) && !t.is_string_expr_cursor(c.edge(1))
		}
		else {
			return false
		}
	}
}

fn (mut t Transformer) infix_cursor_result_pos(c ast.Cursor) token.Pos {
	pos := c.pos()
	if pos.id != 0 || pos.offset != 0 {
		return pos
	}
	return t.next_synth_pos()
}

fn call_expr_from_cursor(c ast.Cursor) ast.CallExpr {
	args_cap := if c.edge_count() > 1 { c.edge_count() - 1 } else { 0 }
	mut args := []ast.Expr{cap: args_cap}
	for i in 1 .. c.edge_count() {
		args << c.edge(i).expr()
	}
	return ast.CallExpr{
		lhs:  c.edge(0).expr()
		args: args
		pos:  c.pos()
	}
}

fn (t &Transformer) empty_ident_call_has_identity_arg_pipeline(lhs ast.Cursor) bool {
	if lhs.kind() != .expr_ident || lhs.name() == '' {
		return false
	}
	info := t.generic_aware_call_fn_info_cursor(lhs, lhs.name()) or { return true }
	return info.param_types.len == 0 && !info.is_variadic && info.generic_params.len == 0
}

fn (t &Transformer) ident_call_name_needs_legacy_arg_pipeline(fn_name string) bool {
	return
		fn_name in ['d', 'builtin__new_array_from_c_array_noscan', 'array__has', 'array__all', 'print', 'println', 'eprint', 'eprintln']
		|| fn_name in t.elided_fns
}

fn (t &Transformer) identity_call_arg_can_transform_direct(arg ast.Cursor, param_typ types.Type) bool {
	if t.type_is_string(param_typ) {
		return t.is_string_expr_cursor(arg)
	}
	base := t.unwrap_alias_type(param_typ)
	match base {
		types.Pointer {
			if arg.kind() == .expr_prefix {
				op := unsafe { token.Token(int(arg.aux())) }
				if op == .amp {
					return true
				}
			}
			arg_typ := t.get_expr_type_cursor(arg) or { return false }
			arg_base := t.unwrap_alias_type(arg_typ)
			return arg_base is types.Pointer || arg_base is types.Nil
		}
		types.Primitive, types.Char, types.ISize, types.Nil, types.Rune, types.String, types.USize {
			return true
		}
		types.Struct {
			// An arg whose checker type is EXACTLY the (non-generic) param
			// struct needs no call-boundary coercion (no sumtype wrap, no
			// interface boxing, no auto-deref). Smartcast scopes stay on the
			// legacy path: an active cast can change an ident's effective
			// type between the declared and narrowed form.
			if base.generic_params.len > 0 || t.has_active_smartcast() {
				return false
			}
			arg_typ := t.get_expr_type_cursor(arg) or { return false }
			arg_base := t.unwrap_alias_type(arg_typ)
			if arg_base is types.Struct {
				return arg_base.name == base.name && arg_base.generic_params.len == 0
			}
			return false
		}
		types.SumType {
			// Exact sumtype-to-sumtype: the arg is already the param's sum
			// type, so no variant wrapping applies.
			if t.has_active_smartcast() {
				return false
			}
			arg_typ := t.get_expr_type_cursor(arg) or { return false }
			arg_base := t.unwrap_alias_type(arg_typ)
			if arg_base is types.SumType {
				return arg_base.name == base.name
			}
			return false
		}
		types.Enum {
			// Typed enum values pass through unchanged. Bare `.value`
			// shorthand args need the param's enum context to resolve, so
			// they keep the legacy pipeline (as does anything that is not a
			// plain ident/qualified selector).
			if t.has_active_smartcast() {
				return false
			}
			if arg.kind() == .expr_ident {
				// fine: a typed enum variable resolves without param context
			} else if arg.kind() == .expr_selector {
				lhs := arg.edge(0)
				if !lhs.is_valid() || lhs.kind() == .expr_empty {
					return false
				}
			} else {
				return false
			}
			arg_typ := t.get_expr_type_cursor(arg) or { return false }
			arg_base := t.unwrap_alias_type(arg_typ)
			if arg_base is types.Enum {
				return arg_base.name == base.name
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) call_empty_ident_can_transform_direct(c ast.Cursor) bool {
	if c.kind() != .expr_call || c.edge_count() != 1 || t.has_active_smartcast() {
		return false
	}
	return t.empty_ident_call_has_identity_arg_pipeline(c.edge(0))
}

fn (t &Transformer) call_ident_args_can_transform_direct(c ast.Cursor) bool {
	if c.kind() != .expr_call || c.edge_count() <= 1 {
		return false
	}
	lhs := c.edge(0)
	if lhs.kind() != .expr_ident || lhs.name() == '' {
		return false
	}
	fn_name := lhs.name()
	if t.ident_call_name_needs_legacy_arg_pipeline(fn_name) {
		return false
	}
	info := t.generic_aware_call_fn_info_cursor(lhs, fn_name) or { return false }
	arg_count := c.edge_count() - 1
	if info.param_types.len != arg_count || info.is_variadic || info.generic_params.len > 0 {
		return false
	}
	for i in 0 .. arg_count {
		arg := c.edge(i + 1)
		if arg.kind() == .aux_field_init {
			return false
		}
		if !t.identity_call_arg_can_transform_direct(arg, info.param_types[i]) {
			return false
		}
	}
	return true
}

fn (t &Transformer) resolve_module_call_name_cursor(lhs ast.Cursor) ?string {
	if lhs.kind() != .expr_selector {
		return none
	}
	rhs_name := selector_rhs_name_cursor(lhs)
	if rhs_name == '' {
		return none
	}
	base := lhs.edge(0)
	if base.kind() == .expr_selector {
		inner_base := base.edge(0)
		if inner_base.kind() == .expr_ident && t.is_module_ident(inner_base.name()) {
			sub_mod := selector_rhs_name_cursor(base)
			if sub_mod != '' {
				if t.get_module_scope(sub_mod) != none {
					return '${sub_mod}__${rhs_name}'
				}
				full_mod := '${inner_base.name()}__${sub_mod}'
				if t.get_module_scope(full_mod) != none {
					return '${full_mod}__${rhs_name}'
				}
			}
		}
		return none
	}
	if base.kind() != .expr_ident {
		return none
	}
	lhs_name := base.name()
	if t.lookup_var_type(lhs_name) != none {
		return none
	}
	if resolved_mod := t.resolve_module_name(lhs_name) {
		mut module_names := []string{cap: 2}
		module_names << resolved_mod
		if lhs_name != resolved_mod {
			module_names << lhs_name
		}
		for mod_name in module_names {
			if _ := t.lookup_fn_cached(mod_name, rhs_name) {
				call_mod := if mod_name.contains('.') {
					mod_name.all_after_last('.')
				} else if mod_name.contains('__') {
					mod_name.all_after_last('__')
				} else {
					mod_name
				}
				return '${call_mod}__${rhs_name}'
			}
		}
	}
	if call_prefix := t.resolve_module_call_prefix(lhs_name) {
		if _ := t.lookup_fn_cached(call_prefix, rhs_name) {
			return '${call_prefix}__${rhs_name}'
		}
	}
	return none
}

fn (t &Transformer) call_selector_module_name_can_transform_direct(c ast.Cursor) ?string {
	if c.kind() != .expr_call || c.edge_count() == 0 {
		return none
	}
	lhs := c.edge(0)
	call_name := t.resolve_module_call_name_cursor(lhs) or { return none }
	rhs_name := selector_rhs_name_cursor(lhs)
	if rhs_name in t.elided_fns || call_name in t.elided_fns {
		return none
	}
	info := t.generic_aware_call_fn_info_cursor(lhs, call_name) or { return none }
	arg_count := c.edge_count() - 1
	if info.param_types.len != arg_count || info.is_variadic || info.generic_params.len > 0 {
		return none
	}
	for i in 0 .. arg_count {
		arg := c.edge(i + 1)
		if arg.kind() == .aux_field_init {
			return none
		}
		if !t.identity_call_arg_can_transform_direct(arg, info.param_types[i]) {
			return none
		}
	}
	return call_name
}

fn (t &Transformer) call_selector_static_name_can_transform_direct(c ast.Cursor) ?string {
	if c.kind() != .expr_call || c.edge_count() == 0 {
		return none
	}
	lhs := c.edge(0)
	if lhs.kind() != .expr_selector {
		return none
	}
	method_name := selector_rhs_name_cursor(lhs)
	if method_name == '' || method_name in t.elided_fns {
		return none
	}
	call_name := t.resolve_static_type_method_call_cursor(lhs.edge(0), method_name) or {
		return none
	}
	if call_name in t.elided_fns {
		return none
	}
	info := t.generic_aware_call_fn_info_cursor(lhs, call_name) or { return none }
	arg_count := c.edge_count() - 1
	if info.param_types.len != arg_count || info.is_variadic || info.generic_params.len > 0 {
		return none
	}
	for i in 0 .. arg_count {
		arg := c.edge(i + 1)
		if arg.kind() == .aux_field_init {
			return none
		}
		if !t.identity_call_arg_can_transform_direct(arg, info.param_types[i]) {
			return none
		}
	}
	return call_name
}

fn method_name_needs_legacy_selector_pipeline(method_name string) bool {
	return method_name in ['sort', 'sorted', 'zero', 'has', 'all', 'contains', 'index', 'last_index',
		'str', 'type_name', 'clone', 'insert', 'prepend']
}

fn (t &Transformer) receiver_method_cursor_can_transform_direct(receiver ast.Cursor, method_name string) bool {
	if t.has_active_smartcast() || method_name_needs_legacy_selector_pipeline(method_name) {
		return false
	}
	recv_type := t.get_expr_type_cursor(receiver) or { return false }
	base := t.unwrap_alias_and_pointer_type(recv_type)
	if base is types.Struct {
		return t.type_has_cached_method(base, method_name)
	}
	// Non-struct receivers. Methods with transformer-level expansions
	// (hoisted loops) and dynamic-dispatch receivers stay on the legacy
	// pipeline; the rest resolve to a plain method fn by name.
	if method_name in ['filter', 'map', 'any', 'count', 'wait'] {
		return false
	}
	if base is types.Interface || base is types.SumType {
		return false
	}
	// Alias receiver with its own declared method (e.g. strings.Builder.writeln).
	if t.resolve_alias_receiver_method_name(recv_type, method_name) != none {
		return true
	}
	// Plain string methods (string__starts_with etc.).
	if t.type_is_string(recv_type) {
		return t.lookup_method_cached('string', method_name) != none
	}
	return false
}

// enum_shorthand_arg_can_transform_direct reports whether `arg` is a bare
// `.member` enum shorthand for an enum-typed parameter — the one non-identity
// arg shape the direct call arms resolve themselves (mirroring
// resolve_expr_with_expected_type -> resolve_enum_shorthand).
fn (t &Transformer) enum_shorthand_arg_can_transform_direct(arg ast.Cursor, param_typ types.Type) bool {
	if t.has_active_smartcast() {
		return false
	}
	param_base := t.unwrap_alias_type(param_typ)
	if param_base !is types.Enum {
		return false
	}
	if arg.kind() != .expr_selector {
		return false
	}
	lhs := arg.edge(0)
	return !lhs.is_valid() || lhs.kind() == .expr_empty
}

// transform_call_arg_cursor_to_flat emits one direct-call argument with the
// parameter type as context: bare enum shorthand resolves to the member ident
// exactly like the legacy resolve_enum_shorthand (same synth-type
// registration at the selector pos); everything else transforms normally.
fn (mut t Transformer) transform_call_arg_cursor_to_flat(arg ast.Cursor, param_typ types.Type, mut out ast.FlatBuilder) ast.FlatNodeId {
	if t.enum_shorthand_arg_can_transform_direct(arg, param_typ) {
		param_base := t.unwrap_alias_type(param_typ)
		enum_name := t.type_to_c_name(param_base)
		member := selector_rhs_name_cursor(arg)
		if typ := t.lookup_type(enum_name) {
			t.register_synth_type(arg.pos(), typ)
			if typ is types.Enum {
				return out.emit_ident_by_name(t.enum_member_ident_for_lookup(enum_name, typ, member),
					arg.pos())
			}
		}
		return out.emit_ident_by_name(enum_member_ident(enum_name, member), arg.pos())
	}
	return t.transform_expr_cursor_to_flat(arg, mut out)
}

// direct_method_call_fn_info_cursor returns the resolved method's declared
// receiver-less signature for the direct-call gates. The shared
// generic_aware_call_fn_info_cursor path heuristically strips a "receiver"
// param from checker fn types, which eats the real first param whenever its
// type equals the receiver type (e.g. s.starts_with(prefix) — both string).
// The cached method declaration never includes the receiver in its params,
// so it is the ground truth for arity and per-arg identity checks.
fn (t &Transformer) direct_method_call_fn_info_cursor(lhs ast.Cursor, method_name string, call_name string) ?CallFnInfo {
	recv_key := call_name.all_before_last('__')
	mut lookup_names := []string{cap: 2}
	lookup_names << recv_key
	if recv_key.contains('__') {
		lookup_names << recv_key.all_after_last('__')
	}
	for name in lookup_names {
		if fn_type := t.lookup_method_cached(name, method_name) {
			return call_fn_info_from_fn_type(fn_type)
		}
	}
	return t.generic_aware_call_fn_info_cursor(lhs, call_name)
}

fn (t &Transformer) call_selector_method_name_can_transform_direct(c ast.Cursor) ?string {
	if c.kind() != .expr_call || c.edge_count() == 0 {
		return none
	}
	lhs := c.edge(0)
	if lhs.kind() != .expr_selector {
		return none
	}
	receiver := lhs.edge(0)
	method_name := selector_rhs_name_cursor(lhs)
	if method_name == '' || method_name in t.elided_fns
		|| !t.receiver_method_cursor_can_transform_direct(receiver, method_name) {
		return none
	}
	if t.resolve_static_type_method_call_cursor(receiver, method_name) != none {
		return none
	}
	call_name := t.resolve_method_call_name_cursor(receiver, method_name) or { return none }
	if call_name in t.elided_fns {
		return none
	}
	info := t.direct_method_call_fn_info_cursor(lhs, method_name, call_name) or { return none }
	arg_count := c.edge_count() - 1
	if info.param_types.len != arg_count || info.is_variadic || info.generic_params.len > 0 {
		return none
	}
	for i in 0 .. arg_count {
		arg := c.edge(i + 1)
		if arg.kind() == .aux_field_init {
			return none
		}
		if !t.identity_call_arg_can_transform_direct(arg, info.param_types[i])
			&& !t.enum_shorthand_arg_can_transform_direct(arg, info.param_types[i]) {
			return none
		}
	}
	return call_name
}

fn call_or_cast_expr_from_cursor(c ast.Cursor) ast.CallOrCastExpr {
	return ast.CallOrCastExpr{
		lhs:  c.edge(0).expr()
		expr: c.edge(1).expr()
		pos:  c.pos()
	}
}

fn (t &Transformer) call_or_cast_empty_ident_call_can_transform_direct(c ast.Cursor) bool {
	if c.kind() != .expr_call_or_cast || c.edge_count() < 2 {
		return false
	}
	lhs := c.edge(0)
	arg := c.edge(1)
	if lhs.kind() != .expr_ident || (arg.is_valid() && arg.kind() != .expr_empty) {
		return false
	}
	if t.call_or_cast_lhs_is_type_cursor(lhs) {
		return true
	}
	return t.empty_ident_call_has_identity_arg_pipeline(lhs)
}

fn (t &Transformer) call_or_cast_ident_arg_can_transform_direct(c ast.Cursor) bool {
	if c.kind() != .expr_call_or_cast || c.edge_count() < 2 {
		return false
	}
	lhs := c.edge(0)
	arg := c.edge(1)
	if lhs.kind() != .expr_ident || lhs.name() == '' || !arg.is_valid() || arg.kind() == .expr_empty {
		return false
	}
	fn_name := lhs.name()
	if t.call_or_cast_lhs_is_type_cursor(lhs)
		|| t.ident_call_name_needs_legacy_arg_pipeline(fn_name) {
		return false
	}
	info := t.generic_aware_call_fn_info_cursor(lhs, fn_name) or { return false }
	if info.param_types.len != 1 || info.is_variadic || info.generic_params.len > 0
		|| arg.kind() == .aux_field_init {
		return false
	}
	return t.identity_call_arg_can_transform_direct(arg, info.param_types[0])
}

fn (t &Transformer) call_or_cast_selector_static_name_can_transform_direct(c ast.Cursor) ?string {
	if c.kind() != .expr_call_or_cast || c.edge_count() < 2 {
		return none
	}
	lhs := c.edge(0)
	if lhs.kind() != .expr_selector {
		return none
	}
	method_name := selector_rhs_name_cursor(lhs)
	if method_name == '' || method_name in t.elided_fns {
		return none
	}
	call_name := t.resolve_static_type_method_call_cursor(lhs.edge(0), method_name) or {
		return none
	}
	if call_name in t.elided_fns {
		return none
	}
	arg := c.edge(1)
	arg_count := if arg.is_valid() && arg.kind() != .expr_empty { 1 } else { 0 }
	info := t.generic_aware_call_fn_info_cursor(lhs, call_name) or { return none }
	if info.param_types.len != arg_count || info.is_variadic || info.generic_params.len > 0 {
		return none
	}
	if arg_count == 1 {
		if arg.kind() == .aux_field_init
			|| !t.identity_call_arg_can_transform_direct(arg, info.param_types[0]) {
			return none
		}
	}
	return call_name
}

fn (t &Transformer) call_or_cast_selector_method_name_can_transform_direct(c ast.Cursor) ?string {
	if c.kind() != .expr_call_or_cast || c.edge_count() < 2 {
		return none
	}
	lhs := c.edge(0)
	if lhs.kind() != .expr_selector {
		return none
	}
	receiver := lhs.edge(0)
	method_name := selector_rhs_name_cursor(lhs)
	if method_name == '' || method_name in t.elided_fns
		|| !t.receiver_method_cursor_can_transform_direct(receiver, method_name) {
		return none
	}
	if t.resolve_static_type_method_call_cursor(receiver, method_name) != none {
		return none
	}
	call_name := t.resolve_method_call_name_cursor(receiver, method_name) or { return none }
	if call_name in t.elided_fns {
		return none
	}
	arg := c.edge(1)
	arg_count := if arg.is_valid() && arg.kind() != .expr_empty { 1 } else { 0 }
	info := t.direct_method_call_fn_info_cursor(lhs, method_name, call_name) or { return none }
	if info.param_types.len != arg_count || info.is_variadic || info.generic_params.len > 0 {
		return none
	}
	if arg_count == 1 {
		if arg.kind() == .aux_field_init
			|| (!t.identity_call_arg_can_transform_direct(arg, info.param_types[0])
			&& !t.enum_shorthand_arg_can_transform_direct(arg, info.param_types[0])) {
			return none
		}
	}
	return call_name
}

fn (t &Transformer) call_or_cast_selector_module_name_can_transform_direct(c ast.Cursor) ?string {
	if c.kind() != .expr_call_or_cast || c.edge_count() < 2 {
		return none
	}
	lhs := c.edge(0)
	if lhs.kind() != .expr_selector || t.call_or_cast_lhs_is_type_cursor(lhs) {
		return none
	}
	call_name := t.resolve_module_call_name_cursor(lhs) or { return none }
	rhs_name := selector_rhs_name_cursor(lhs)
	if rhs_name in t.elided_fns || call_name in t.elided_fns {
		return none
	}
	arg := c.edge(1)
	arg_count := if arg.is_valid() && arg.kind() != .expr_empty { 1 } else { 0 }
	info := t.generic_aware_call_fn_info_cursor(lhs, call_name) or { return none }
	if info.param_types.len != arg_count || info.is_variadic || info.generic_params.len > 0 {
		return none
	}
	if arg_count == 1 {
		if arg.kind() == .aux_field_init
			|| !t.identity_call_arg_can_transform_direct(arg, info.param_types[0]) {
			return none
		}
	}
	return call_name
}

fn index_expr_from_cursor(c ast.Cursor) ast.IndexExpr {
	return ast.IndexExpr{
		lhs:      c.edge(0).expr()
		expr:     c.edge(1).expr()
		is_gated: c.flag(ast.flag_is_gated)
		pos:      c.pos()
	}
}

fn selector_expr_from_cursor(c ast.Cursor) ast.SelectorExpr {
	return ast.SelectorExpr{
		lhs: c.edge(0).expr()
		rhs: c.edge(1).ident()
		pos: c.pos()
	}
}

fn if_expr_from_cursor(c ast.Cursor) ast.IfExpr {
	stmts_cap := if c.edge_count() > 2 { c.edge_count() - 2 } else { 0 }
	mut stmts := []ast.Stmt{cap: stmts_cap}
	for i in 2 .. c.edge_count() {
		stmts << c.edge(i).stmt()
	}
	return ast.IfExpr{
		cond:      c.edge(0).expr()
		else_expr: c.edge(1).expr()
		stmts:     stmts
		pos:       c.pos()
	}
}

fn map_init_expr_from_cursor(c ast.Cursor) ast.MapInitExpr {
	keys_len := c.extra_int()
	mut keys := []ast.Expr{cap: keys_len}
	for i in 0 .. keys_len {
		keys << c.edge(1 + i).expr()
	}
	vals_cap := if c.edge_count() > 1 + keys_len {
		c.edge_count() - 1 - keys_len
	} else {
		0
	}
	mut vals := []ast.Expr{cap: vals_cap}
	for i in (1 + keys_len) .. c.edge_count() {
		vals << c.edge(i).expr()
	}
	return ast.MapInitExpr{
		typ:  c.edge(0).expr()
		keys: keys
		vals: vals
		pos:  c.pos()
	}
}

struct MapCursorTypeParts {
mut:
	key_cursor    ast.Cursor
	val_cursor    ast.Cursor
	key_expr      ast.Expr
	val_expr      ast.Expr
	key_type_name string
}

fn (t &Transformer) map_cursor_type_parts(c ast.Cursor) MapCursorTypeParts {
	mut parts := MapCursorTypeParts{
		key_expr:      ast.Expr(ast.Ident{
			name: 'int'
		})
		val_expr:      ast.Expr(ast.Ident{
			name: 'int'
		})
		key_type_name: 'int'
	}
	typ_c := c.edge(0)
	match typ_c.kind() {
		.typ_map {
			parts.key_cursor = typ_c.edge(0)
			parts.val_cursor = typ_c.edge(1)
			parts.key_type_name = t.type_expr_name_full_cursor(parts.key_cursor)
			if parts.key_type_name == '' {
				parts.key_type_name = 'int'
			}
			return parts
		}
		.expr_ident, .expr_selector {
			type_name := if typ_c.kind() == .expr_ident {
				typ_c.name()
			} else {
				t.type_expr_name_full_cursor(typ_c)
			}
			if explicit_map_typ := t.lookup_type(type_name) {
				if explicit_map := t.unwrap_map_type(explicit_map_typ) {
					parts.key_expr = t.type_to_ast_type_expr(explicit_map.key_type)
					parts.val_expr = t.type_to_ast_type_expr(explicit_map.value_type)
					parts.key_type_name = t.type_to_c_name(explicit_map.key_type)
					return parts
				}
			}
		}
		else {}
	}

	if inferred := t.get_expr_type_cursor(c) {
		if inferred_map := t.unwrap_map_type(inferred) {
			parts.key_expr = t.type_to_ast_type_expr(inferred_map.key_type)
			parts.val_expr = t.type_to_ast_type_expr(inferred_map.value_type)
			parts.key_type_name = t.type_to_c_name(inferred_map.key_type)
		}
	}
	if parts.key_type_name == '' {
		parts.key_type_name = 'int'
	}
	return parts
}

fn cursor_is_string_literal(c ast.Cursor) bool {
	if !c.is_valid() {
		return false
	}
	if c.kind() == .expr_string || c.kind() == .expr_string_inter {
		return true
	}
	if c.kind() == .expr_basic_literal {
		kind := unsafe { token.Token(int(c.aux())) }
		return kind == .string
	}
	return false
}

fn map_key_cursor_is_enum_shorthand(c ast.Cursor) bool {
	if !c.is_valid() || c.kind() != .expr_selector {
		return false
	}
	lhs := c.edge(0)
	if lhs.kind() == .expr_empty {
		return true
	}
	return lhs.kind() == .expr_ident && lhs.name() == ''
}

fn emit_map_cursor_type_id(type_cursor ast.Cursor, type_expr ast.Expr, mut out ast.FlatBuilder) ast.FlatNodeId {
	if type_cursor.is_valid() {
		return out.copy_subtree_from(type_cursor.flat, type_cursor.id)
	}
	return out.emit_expr(type_expr)
}

fn emit_addr_of_ident_to_flat(name string, mut out ast.FlatBuilder) ast.FlatNodeId {
	ident_id := out.emit_ident_by_name(name, token.Pos{})
	return out.emit_prefix_expr_by_id(.amp, ident_id, token.Pos{})
}

fn emit_sizeof_type_id_to_flat(type_id ast.FlatNodeId, mut out ast.FlatBuilder) ast.FlatNodeId {
	return out.emit_keyword_operator_by_ids(.key_sizeof, [type_id], token.Pos{})
}

fn (mut t Transformer) transform_map_key_cursor_to_flat(c ast.Cursor, key_type_name string, mut out ast.FlatBuilder) ast.FlatNodeId {
	if key_type_name == '' || key_type_name == 'int' || !map_key_cursor_is_enum_shorthand(c) {
		return t.transform_expr_cursor_to_flat(c, mut out)
	}
	member := c.edge(1)
	if typ := t.lookup_type(key_type_name) {
		t.register_synth_type(c.pos(), typ)
		if typ is types.Enum {
			return out.emit_ident_by_name(t.enum_member_ident_for_lookup(key_type_name, typ,
				member.name()), c.pos())
		}
	}
	return out.emit_ident_by_name(enum_member_ident(key_type_name, member.name()), c.pos())
}

fn array_elem_type_cursor(type_cursor ast.Cursor) ?ast.Cursor {
	if type_cursor.is_valid() && type_cursor.kind() == .typ_array {
		return type_cursor.edge(0)
	}
	return none
}

fn array_elem_type_expr(type_expr ast.Expr) ?ast.Expr {
	if type_expr is ast.Type {
		type_payload := type_expr as ast.Type
		if type_payload is ast.ArrayType {
			array_type := type_payload as ast.ArrayType
			return array_type.elem_type
		}
	}
	return none
}

fn (mut t Transformer) transform_map_value_cursor_to_flat(c ast.Cursor, parts MapCursorTypeParts, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if c.kind() == .expr_array_init {
		typ := c.edge(0)
		if !typ.is_valid() || typ.kind() == .expr_empty {
			if elem_cursor := array_elem_type_cursor(parts.val_cursor) {
				return t.transform_dynamic_array_init_cursor_with_elem_type_to_flat(c, elem_cursor,
					ast.empty_expr, mut out)
			}
			if elem_expr := array_elem_type_expr(parts.val_expr) {
				return t.transform_dynamic_array_init_cursor_with_elem_type_to_flat(c, ast.Cursor{},
					elem_expr, mut out)
			}
			return none
		}
	}
	return t.transform_expr_cursor_to_flat(c, mut out)
}

fn (mut t Transformer) transform_map_init_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	keys_len := c.extra_int()
	mut parts := t.map_cursor_type_parts(c)
	if keys_len > 0 && parts.key_type_name == 'int' {
		first_key := c.edge(1)
		first_val := c.edge(1 + keys_len)
		if cursor_is_string_literal(first_key) {
			parts.key_cursor = ast.Cursor{}
			parts.key_expr = ast.Expr(ast.Ident{
				name: 'string'
			})
			parts.key_type_name = 'string'
		}
		if cursor_is_string_literal(first_val) {
			parts.val_cursor = ast.Cursor{}
			parts.val_expr = ast.Expr(ast.Ident{
				name: 'string'
			})
		}
	}
	mut key_ids := []ast.FlatNodeId{cap: keys_len}
	for i in 0 .. keys_len {
		key_ids << t.transform_map_key_cursor_to_flat(c.edge(1 + i), parts.key_type_name, mut out)
	}
	vals_start := 1 + keys_len
	mut val_ids := []ast.FlatNodeId{cap: c.edge_count() - vals_start}
	for i in vals_start .. c.edge_count() {
		val_ids << t.transform_map_value_cursor_to_flat(c.edge(i), parts, mut out) or {
			return none
		}
	}
	if t.is_eval_backend() {
		typ_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
		return out.emit_map_init_expr_by_ids(typ_id, key_ids, val_ids, c.pos())
	}
	hash_fn, eq_fn, clone_fn, free_fn := map_runtime_key_fns_from_type_name(parts.key_type_name)
	if keys_len == 0 {
		lhs_id := out.emit_ident_by_name('new_map', token.Pos{})
		key_sizeof_id := emit_sizeof_type_id_to_flat(emit_map_cursor_type_id(parts.key_cursor,
			parts.key_expr, mut out), mut out)
		val_sizeof_id := emit_sizeof_type_id_to_flat(emit_map_cursor_type_id(parts.val_cursor,
			parts.val_expr, mut out), mut out)
		return out.emit_call_expr_by_ids(lhs_id, [
			key_sizeof_id,
			val_sizeof_id,
			emit_addr_of_ident_to_flat(hash_fn, mut out),
			emit_addr_of_ident_to_flat(eq_fn, mut out),
			emit_addr_of_ident_to_flat(clone_fn, mut out),
			emit_addr_of_ident_to_flat(free_fn, mut out),
		], c.pos())
	}
	lhs_id := out.emit_ident_by_name('new_map_init_noscan_value', token.Pos{})
	key_size_id := emit_sizeof_type_id_to_flat(emit_map_cursor_type_id(parts.key_cursor,
		parts.key_expr, mut out), mut out)
	val_size_id := emit_sizeof_type_id_to_flat(emit_map_cursor_type_id(parts.val_cursor,
		parts.val_expr, mut out), mut out)
	key_array_typ_id := out.emit_array_type_by_elem_id(emit_map_cursor_type_id(parts.key_cursor,
		parts.key_expr, mut out))
	val_array_typ_id := out.emit_array_type_by_elem_id(emit_map_cursor_type_id(parts.val_cursor,
		parts.val_expr, mut out))
	key_array_id := out.emit_array_init_expr_by_ids(key_array_typ_id,
		out.emit_expr(ast.empty_expr), out.emit_expr(ast.empty_expr),
		out.emit_expr(ast.empty_expr), out.emit_expr(ast.empty_expr), key_ids, token.Pos{})
	val_array_id := out.emit_array_init_expr_by_ids(val_array_typ_id,
		out.emit_expr(ast.empty_expr), out.emit_expr(ast.empty_expr),
		out.emit_expr(ast.empty_expr), out.emit_expr(ast.empty_expr), val_ids, token.Pos{})
	return out.emit_call_expr_by_ids(lhs_id, [
		emit_addr_of_ident_to_flat(hash_fn, mut out),
		emit_addr_of_ident_to_flat(eq_fn, mut out),
		emit_addr_of_ident_to_flat(clone_fn, mut out),
		emit_addr_of_ident_to_flat(free_fn, mut out),
		out.emit_basic_literal_by_value(.number, keys_len.str(), token.Pos{}),
		key_size_id,
		val_size_id,
		key_array_id,
		val_array_id,
	], c.pos())
}

fn array_init_expr_from_cursor(c ast.Cursor) ast.ArrayInitExpr {
	exprs_cap := if c.edge_count() > 5 { c.edge_count() - 5 } else { 0 }
	mut exprs := []ast.Expr{cap: exprs_cap}
	for i in 5 .. c.edge_count() {
		exprs << c.edge(i).expr()
	}
	return ast.ArrayInitExpr{
		typ:         c.edge(0).expr()
		init:        c.edge(1).expr()
		cap:         c.edge(2).expr()
		len:         c.edge(3).expr()
		update_expr: c.edge(4).expr()
		exprs:       exprs
		pos:         c.pos()
	}
}

fn (mut t Transformer) transform_fixed_array_init_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if t.is_native_be {
		return none
	}
	typ := c.edge(0)
	if !typ.is_valid() || typ.kind() != .typ_array_fixed {
		return none
	}
	if c.edge(4).is_valid() && c.edge(4).kind() != .expr_empty {
		return none
	}
	typ_id := out.copy_subtree_from(typ.flat, typ.id)
	init_id := t.transform_expr_cursor_to_flat(c.edge(1), mut out)
	cap_id := t.transform_expr_cursor_to_flat(c.edge(2), mut out)
	len_id := t.transform_expr_cursor_to_flat(c.edge(3), mut out)
	update_id := out.emit_expr(ast.empty_expr)
	mut expr_ids := []ast.FlatNodeId{cap: c.edge_count() - 5}
	for i in 5 .. c.edge_count() {
		expr_ids << t.transform_expr_cursor_to_flat(c.edge(i), mut out)
	}
	return out.emit_array_init_expr_by_ids(typ_id, init_id, cap_id, len_id, update_id, expr_ids,
		c.pos())
}

fn (mut t Transformer) transform_explicit_dynamic_array_init_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	typ := c.edge(0)
	if !typ.is_valid() || typ.kind() != .typ_array {
		return none
	}
	return t.transform_dynamic_array_init_cursor_with_elem_type_to_flat(c, typ.edge(0),
		ast.empty_expr, mut out)
}

fn (mut t Transformer) transform_no_literal_dynamic_array_init_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if t.is_eval_backend() || t.is_native_be {
		return none
	}
	typ := c.edge(0)
	if c.edge_count() != 5 {
		return none
	}
	update_expr := c.edge(4)
	if update_expr.is_valid() && update_expr.kind() != .expr_empty {
		return none
	}
	mut elem_type := ast.Cursor{}
	mut elem_type_expr := ast.Expr(ast.empty_expr)
	mut elem_resolved_type := types.Type(types.void_)
	mut has_elem_resolved_type := false
	if typ.is_valid() && typ.kind() == .typ_array {
		elem_type = typ.edge(0)
	} else if !typ.is_valid() || typ.kind() == .expr_empty {
		array_type := t.get_expr_type_cursor(c) or { return none }
		array_base := t.unwrap_alias_and_pointer_type(array_type)
		if array_base !is types.Array {
			return none
		}
		array_info := array_base as types.Array
		elem_resolved_type = array_info.elem_type
		has_elem_resolved_type = true
		elem_type_expr = t.type_to_ast_type_expr(array_info.elem_type)
	} else {
		return none
	}
	init := c.edge(1)
	init_id := if !init.is_valid() || init.kind() == .expr_empty {
		if elem_type.is_valid() {
			if t.dynamic_array_empty_default_needs_legacy(elem_type) {
				return none
			}
		} else if !has_elem_resolved_type
			|| t.dynamic_array_empty_default_type_needs_legacy(elem_resolved_type) {
			return none
		}
		out.emit_ident_by_name('nil', token.Pos{})
	} else {
		if t.cursor_subtree_has_or_expr(init) || cursor_subtree_has_ident_named(init, 'index')
			|| t.dynamic_array_init_value_needs_legacy(init) {
			return none
		}
		transformed_init_id := t.transform_expr_cursor_to_flat(init, mut out)
		if numeric_literal_cursor(init) {
			typ_id := emit_map_cursor_type_id(elem_type, elem_type_expr, mut out)
			out.emit_cast_expr_by_ids(typ_id, transformed_init_id, token.Pos{})
		} else {
			transformed_init_id
		}
	}
	len_id := t.array_init_bound_cursor_to_flat(c.edge(3), mut out)
	cap_id := t.array_init_bound_cursor_to_flat(c.edge(2), mut out)
	sizeof_id :=
		emit_sizeof_type_id_to_flat(emit_map_cursor_type_id(elem_type, elem_type_expr, mut out), mut out)
	lhs_id := out.emit_ident_by_name('__new_array_with_default_noscan', token.Pos{})
	return out.emit_call_expr_by_ids(lhs_id, [len_id, cap_id, sizeof_id, init_id], c.pos())
}

fn (mut t Transformer) array_init_bound_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	if !c.is_valid() || c.kind() == .expr_empty {
		return out.emit_basic_literal_by_value(.number, '0', token.Pos{})
	}
	return t.transform_expr_cursor_to_flat(c, mut out)
}

fn (t &Transformer) dynamic_array_empty_default_needs_legacy(elem_type ast.Cursor) bool {
	if !elem_type.is_valid() {
		return true
	}
	if elem_type.kind() in [.typ_array, .typ_map] {
		return true
	}
	elem_name := t.type_expr_name_full_cursor(elem_type)
	if elem_name == '' {
		return true
	}
	typ := t.lookup_type(elem_name) or { return false }
	base := t.unwrap_alias_and_pointer_type(typ)
	return t.dynamic_array_empty_default_base_needs_legacy(base)
}

fn (t &Transformer) dynamic_array_empty_default_type_needs_legacy(typ types.Type) bool {
	base := t.unwrap_alias_and_pointer_type(typ)
	return t.dynamic_array_empty_default_base_needs_legacy(base)
}

fn (t &Transformer) dynamic_array_empty_default_base_needs_legacy(base types.Type) bool {
	match base {
		types.Array, types.Map {
			return true
		}
		types.Struct {
			for field in base.fields {
				if field.typ is types.Map {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn (t &Transformer) dynamic_array_init_value_needs_legacy(init ast.Cursor) bool {
	if !init.is_valid() {
		return true
	}
	if init.kind() == .expr_array_init {
		return true
	}
	init_type := t.get_expr_type_cursor(init) or { return false }
	base := t.unwrap_alias_and_pointer_type(init_type)
	return base is types.Array
}

fn numeric_literal_cursor(c ast.Cursor) bool {
	if !c.is_valid() {
		return false
	}
	match c.kind() {
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			return kind == .number
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			return op == .minus && numeric_literal_cursor(c.edge(0))
		}
		.expr_paren {
			return numeric_literal_cursor(c.edge(0))
		}
		else {
			return false
		}
	}
}

fn (mut t Transformer) transform_dynamic_array_init_cursor_with_elem_type_to_flat(c ast.Cursor, elem_type ast.Cursor, elem_type_expr ast.Expr, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if t.is_eval_backend() || t.is_native_be {
		return none
	}
	for i in 1 .. 5 {
		field := c.edge(i)
		if field.is_valid() && field.kind() != .expr_empty {
			return none
		}
	}
	exprs_len := c.edge_count() - 5
	if exprs_len <= 0 {
		return none
	}
	if !elem_type.is_valid() && elem_type_expr is ast.EmptyExpr {
		return none
	}
	mut expr_ids := []ast.FlatNodeId{cap: exprs_len}
	for i in 5 .. c.edge_count() {
		expr_ids << t.transform_expr_cursor_to_flat(c.edge(i), mut out)
	}
	inner_array_typ_id := out.emit_array_type_by_elem_id(emit_map_cursor_type_id(elem_type,
		elem_type_expr, mut out))
	values_id := out.emit_array_init_expr_by_ids(inner_array_typ_id, out.emit_expr(ast.empty_expr),
		out.emit_expr(ast.empty_expr), out.emit_expr(ast.empty_expr),
		out.emit_expr(ast.empty_expr), expr_ids, token.Pos{})
	lhs_id := out.emit_ident_by_name('builtin__new_array_from_c_array_noscan', token.Pos{})
	len_id := out.emit_basic_literal_by_value(.number, exprs_len.str(), token.Pos{})
	cap_id := out.emit_basic_literal_by_value(.number, exprs_len.str(), token.Pos{})
	sizeof_id :=
		emit_sizeof_type_id_to_flat(emit_map_cursor_type_id(elem_type, elem_type_expr, mut out), mut out)
	return out.emit_call_expr_by_ids(lhs_id, [len_id, cap_id, sizeof_id, values_id], c.pos())
}

fn (mut t Transformer) transform_empty_map_init_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if c.edge_count() != 1 {
		return none
	}
	typ := c.edge(0)
	if !typ.is_valid() || typ.kind() != .typ_map {
		return none
	}
	if t.is_eval_backend() {
		typ_id := out.copy_subtree_from(typ.flat, typ.id)
		return out.emit_map_init_expr_by_ids(typ_id, []ast.FlatNodeId{}, []ast.FlatNodeId{},
			c.pos())
	}
	parts := t.map_cursor_type_parts(c)
	hash_fn, eq_fn, clone_fn, free_fn := map_runtime_key_fns_from_type_name(parts.key_type_name)
	lhs_id := out.emit_ident_by_name('new_map', token.Pos{})
	key_sizeof_id := emit_sizeof_type_id_to_flat(emit_map_cursor_type_id(parts.key_cursor,
		parts.key_expr, mut out), mut out)
	val_sizeof_id := emit_sizeof_type_id_to_flat(emit_map_cursor_type_id(parts.val_cursor,
		parts.val_expr, mut out), mut out)
	return out.emit_call_expr_by_ids(lhs_id, [
		key_sizeof_id,
		val_sizeof_id,
		emit_addr_of_ident_to_flat(hash_fn, mut out),
		emit_addr_of_ident_to_flat(eq_fn, mut out),
		emit_addr_of_ident_to_flat(clone_fn, mut out),
		emit_addr_of_ident_to_flat(free_fn, mut out),
	], c.pos())
}

fn init_expr_from_cursor(c ast.Cursor) ast.InitExpr {
	fields_cap := if c.edge_count() > 1 { c.edge_count() - 1 } else { 0 }
	mut fields := []ast.FieldInit{cap: fields_cap}
	for i in 1 .. c.edge_count() {
		field := c.edge(i)
		fields << ast.FieldInit{
			name:  field.name()
			value: field.edge(0).expr()
		}
	}
	return ast.InitExpr{
		typ:    c.edge(0).expr()
		fields: fields
		pos:    c.pos()
	}
}

fn match_branch_from_cursor(c ast.Cursor) ast.MatchBranch {
	conds := c.list_at(0)
	mut cond := []ast.Expr{cap: conds.len()}
	for i in 0 .. conds.len() {
		cond << conds.at(i).expr()
	}
	stmt_list := c.list_at(1)
	mut stmts := []ast.Stmt{cap: stmt_list.len()}
	for i in 0 .. stmt_list.len() {
		stmts << stmt_list.at(i).stmt()
	}
	return ast.MatchBranch{
		cond:  cond
		stmts: stmts
		pos:   c.pos()
	}
}

fn match_expr_from_cursor(c ast.Cursor) ast.MatchExpr {
	mut branches := []ast.MatchBranch{cap: c.edge_count() - 1}
	for i in 1 .. c.edge_count() {
		branches << match_branch_from_cursor(c.edge(i))
	}
	return ast.MatchExpr{
		expr:     c.edge(0).expr()
		branches: branches
		pos:      c.pos()
	}
}

fn (mut t Transformer) transform_expr_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	if !c.is_valid() {
		return out.emit_expr(ast.empty_expr)
	}
	match c.kind() {
		.expr_empty, .expr_keyword, .expr_lifetime, .expr_range, .expr_select, .expr_tuple,
		.typ_anon_struct, .typ_array_fixed, .typ_array, .typ_channel, .typ_fn, .typ_generic,
		.typ_map, .typ_nil, .typ_none, .typ_option, .typ_pointer, .typ_result, .typ_thread,
		.typ_tuple {
			return out.copy_subtree_from(c.flat, c.id)
		}
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			return out.emit_basic_literal_by_value(kind, c.name(), c.pos())
		}
		.expr_ident {
			return t.transform_ident_cursor_to_flat(c, mut out)
		}
		.expr_string {
			kind := unsafe { ast.StringLiteralKind(int(c.aux())) }
			return out.emit_string_literal_by_value(kind, c.name(), c.pos())
		}
		.aux_field_init {
			value_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
			return out.emit_field_init_by_id(c.name(), value_id)
		}
		.expr_if_guard {
			assign := c.edge(0)
			if assign.is_valid() && assign.kind() == .stmt_assign {
				lhs_len := assign.extra_int()
				if assign.edge_count() > lhs_len {
					return t.transform_expr_cursor_to_flat(assign.edge(lhs_len), mut out)
				}
			}
			return out.copy_subtree_from(c.flat, c.id)
		}
		.expr_paren {
			inner_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
			return out.emit_paren_expr_by_id(inner_id, c.pos())
		}
		.expr_modifier {
			kind := unsafe { token.Token(int(c.aux())) }
			inner_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
			return out.emit_modifier_expr_by_id(kind, inner_id, c.pos())
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .amp {
				if id := t.try_transform_amp_type_cast_cursor_to_flat(c.edge(0), mut out) {
					return id
				}
				if !cursor_unwraps_to_assoc_expr(c.edge(0)) {
					inner_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
					return out.emit_prefix_expr_by_id(op, inner_id, c.pos())
				}
			} else if !(op == .arrow && c.edge(0).kind() == .expr_or) {
				inner_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
				return out.emit_prefix_expr_by_id(op, inner_id, c.pos())
			}
			return t.transform_expr_to_flat(ast.Expr(ast.PrefixExpr{
				op:   op
				expr: c.edge(0).expr()
				pos:  c.pos()
			}), mut out)
		}
		.expr_postfix {
			op := unsafe { token.Token(int(c.aux())) }
			if op != .not && op != .question {
				inner_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
				return out.emit_postfix_expr_by_id(op, inner_id, c.pos())
			}
			if id := t.transform_result_postfix_cursor_to_flat(c, op, mut out) {
				return id
			}
			return t.transform_expr_to_flat(ast.Expr(ast.PostfixExpr{
				op:   op
				expr: c.edge(0).expr()
				pos:  c.pos()
			}), mut out)
		}
		.expr_cast {
			sumtype_name := t.type_expr_name_full_cursor(c.edge(0))
			if sumtype_name != '' && t.is_sum_type(sumtype_name) {
				if wrapped_id := t.wrap_sumtype_value_cursor_to_flat(c.edge(1), sumtype_name, mut
					out)
				{
					return wrapped_id
				}
				value_expr := c.edge(1).expr()
				if wrapped := t.wrap_sumtype_value(value_expr, sumtype_name) {
					return t.emit_lowered_expr_result_to_flat(wrapped, mut out)
				}
				transformed_value := t.transform_expr(value_expr)
				if wrapped := t.wrap_sumtype_value_transformed(transformed_value, sumtype_name) {
					return t.emit_lowered_expr_result_to_flat(wrapped, mut out)
				}
			} else {
				typ_id := out.copy_subtree_from(c.edge(0).flat, c.edge(0).id)
				expr_id := t.transform_expr_cursor_to_flat(c.edge(1), mut out)
				return out.emit_cast_expr_by_ids(typ_id, expr_id, c.pos())
			}
		}
		.expr_as_cast {
			expr_key := expr_cursor_to_string(c.edge(0))
			saved_smartcast_stack := t.smartcast_stack.clone()
			saved_smartcast_expr_counts := t.smartcast_expr_counts.clone()
			for _ in 0 .. smartcast_search_limit {
				if _ := t.remove_smartcast_for_expr(expr_key) {
					continue
				}
				break
			}
			expr_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
			t.smartcast_stack = saved_smartcast_stack.clone()
			t.smartcast_expr_counts = saved_smartcast_expr_counts.clone()
			typ_id := out.copy_subtree_from(c.edge(1).flat, c.edge(1).id)
			return out.emit_as_cast_expr_by_ids(expr_id, typ_id, c.pos())
		}
		.expr_sql {
			is_count := c.flag(ast.flag_is_count)
			is_create := c.flag(ast.flag_is_create)
			if !is_create || t.lookup_sql_table_struct(c.name()) == none {
				expr_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
				return out.emit_sql_expr_by_id(c.name(), is_count, is_create, expr_id, c.pos())
			}
			result := t.transform_sql_expr(ast.SqlExpr{
				expr:       c.edge(0).expr()
				table_name: c.name()
				is_count:   is_count
				is_create:  true
				pos:        c.pos()
			})
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_assoc {
			t.count_flat_fallback('expr_assoc')
			result := t.lower_assoc_expr(assoc_expr_from_cursor(c), false)
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_or {
			t.count_flat_fallback('expr_or')
			return t.transform_expr_to_flat(ast.Expr(or_expr_from_cursor(c)), mut out)
		}
		.expr_infix {
			op := unsafe { token.Token(int(c.aux())) }
			if result_id := t.transform_enum_shorthand_compare_cursor_to_flat(c, op, mut out) {
				return result_id
			}
			if result_id := t.transform_sumtype_check_cursor_to_flat(c, op, mut out) {
				return result_id
			}
			if result_id := t.transform_string_concat_cursor_to_flat(c, op, mut out) {
				return result_id
			}
			if result_id := t.transform_string_compare_cursor_to_flat(c, op, mut out) {
				return result_id
			}
			if result_id := t.transform_range_membership_cursor_to_flat(c, op, mut out) {
				return result_id
			}
			if result_id := t.transform_inline_array_membership_cursor_to_flat(c, op, mut out) {
				return result_id
			}
			if result_id := t.transform_map_membership_cursor_to_flat(c, op, mut out) {
				return result_id
			}
			if op == .left_shift {
				if result_id := t.try_transform_array_append_cursor_to_flat(c, mut out) {
					return result_id
				}
			}
			if t.infix_cursor_can_transform_direct(c, op) {
				lhs_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
				rhs_id := t.transform_expr_cursor_to_flat(c.edge(1), mut out)
				return out.emit_infix_expr_by_ids(op, lhs_id, rhs_id, t.infix_cursor_result_pos(c))
			}
			t.count_flat_fallback('expr_infix/op=${op}')
			result := t.transform_infix_expr(infix_expr_from_cursor(c))
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_call {
			if t.call_empty_ident_can_transform_direct(c) {
				lhs := c.edge(0)
				lhs_id := out.emit_ident_by_name(lhs.name(), lhs.pos())
				return out.emit_call_expr_by_ids(lhs_id, []ast.FlatNodeId{}, c.pos())
			}
			if t.call_ident_args_can_transform_direct(c) {
				lhs := c.edge(0)
				lhs_id := out.emit_ident_by_name(lhs.name(), lhs.pos())
				mut arg_ids := []ast.FlatNodeId{cap: c.edge_count() - 1}
				for i in 1 .. c.edge_count() {
					arg_ids << t.transform_expr_cursor_to_flat(c.edge(i), mut out)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, c.pos())
			}
			if call_name := t.call_selector_static_name_can_transform_direct(c) {
				lhs_id := out.emit_ident_by_name(call_name, c.edge(0).pos())
				mut arg_ids := []ast.FlatNodeId{cap: c.edge_count() - 1}
				for i in 1 .. c.edge_count() {
					arg_ids << t.transform_expr_cursor_to_flat(c.edge(i), mut out)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, c.pos())
			}
			if call_name := t.call_selector_module_name_can_transform_direct(c) {
				lhs_id := out.emit_ident_by_name(call_name, c.edge(0).pos())
				mut arg_ids := []ast.FlatNodeId{cap: c.edge_count() - 1}
				for i in 1 .. c.edge_count() {
					arg_ids << t.transform_expr_cursor_to_flat(c.edge(i), mut out)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, c.pos())
			}
			if call_name := t.call_selector_method_name_can_transform_direct(c) {
				lhs := c.edge(0)
				info := t.direct_method_call_fn_info_cursor(lhs, selector_rhs_name_cursor(lhs),
					call_name) or { CallFnInfo{} }
				lhs_id := out.emit_ident_by_name(call_name, lhs.pos())
				mut arg_ids := []ast.FlatNodeId{cap: c.edge_count()}
				arg_ids << t.transform_expr_cursor_to_flat(lhs.edge(0), mut out)
				for i in 1 .. c.edge_count() {
					if i - 1 < info.param_types.len {
						arg_ids << t.transform_call_arg_cursor_to_flat(c.edge(i),
							info.param_types[i - 1], mut out)
					} else {
						arg_ids << t.transform_expr_cursor_to_flat(c.edge(i), mut out)
					}
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, c.pos())
			}
			t.count_flat_fallback(t.classify_call_fallback_cursor(c, 'expr_call'))
			result := t.transform_call_expr(call_expr_from_cursor(c))
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_call_or_cast {
			lhs := c.edge(0)
			arg := c.edge(1)
			if t.call_or_cast_empty_ident_call_can_transform_direct(c) {
				lhs_id := out.emit_ident_by_name(lhs.name(), lhs.pos())
				return out.emit_call_expr_by_ids(lhs_id, []ast.FlatNodeId{}, c.pos())
			}
			if t.call_or_cast_ident_arg_can_transform_direct(c) {
				lhs_id := out.emit_ident_by_name(lhs.name(), lhs.pos())
				arg_id := t.transform_expr_cursor_to_flat(arg, mut out)
				return out.emit_call_expr_by_ids(lhs_id, [arg_id], c.pos())
			}
			if call_name := t.call_or_cast_selector_static_name_can_transform_direct(c) {
				lhs_id := out.emit_ident_by_name(call_name, lhs.pos())
				mut arg_ids := []ast.FlatNodeId{cap: 1}
				if arg.is_valid() && arg.kind() != .expr_empty {
					arg_ids << t.transform_expr_cursor_to_flat(arg, mut out)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, c.pos())
			}
			if call_name := t.call_or_cast_selector_module_name_can_transform_direct(c) {
				lhs_id := out.emit_ident_by_name(call_name, lhs.pos())
				mut arg_ids := []ast.FlatNodeId{cap: 1}
				if arg.is_valid() && arg.kind() != .expr_empty {
					arg_ids << t.transform_expr_cursor_to_flat(arg, mut out)
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, c.pos())
			}
			if call_name := t.call_or_cast_selector_method_name_can_transform_direct(c) {
				info := t.direct_method_call_fn_info_cursor(lhs, selector_rhs_name_cursor(lhs),
					call_name) or { CallFnInfo{} }
				lhs_id := out.emit_ident_by_name(call_name, lhs.pos())
				mut arg_ids := []ast.FlatNodeId{cap: 2}
				arg_ids << t.transform_expr_cursor_to_flat(lhs.edge(0), mut out)
				if arg.is_valid() && arg.kind() != .expr_empty {
					if info.param_types.len > 0 {
						arg_ids << t.transform_call_arg_cursor_to_flat(arg, info.param_types[0], mut out)
					} else {
						arg_ids << t.transform_expr_cursor_to_flat(arg, mut out)
					}
				}
				return out.emit_call_expr_by_ids(lhs_id, arg_ids, c.pos())
			}
			if arg.is_valid() && arg.kind() != .expr_empty && t.call_or_cast_lhs_is_type_cursor(lhs) {
				mut sumtype_name := t.type_expr_name_full_cursor(lhs)
				if (sumtype_name == '' || !t.is_sum_type(sumtype_name)) && c.pos().is_valid() {
					if expr_typ := t.get_expr_type_cursor(c) {
						if expr_typ is types.SumType {
							sumtype_name = expr_typ.get_name()
						}
					}
				}
				if sumtype_name != '' && t.is_sum_type(sumtype_name) {
					if wrapped_id := t.wrap_sumtype_value_cursor_to_flat(arg, sumtype_name, mut out) {
						return wrapped_id
					}
					arg_expr := arg.expr()
					if wrapped := t.wrap_sumtype_value(arg_expr, sumtype_name) {
						return t.emit_lowered_expr_result_to_flat(wrapped, mut out)
					}
					transformed_sum_arg := t.transform_expr(arg_expr)
					if wrapped := t.wrap_sumtype_value_transformed(transformed_sum_arg,
						sumtype_name)
					{
						return t.emit_lowered_expr_result_to_flat(wrapped, mut out)
					}
				} else {
					typ_id := out.copy_subtree_from(lhs.flat, lhs.id)
					expr_id := t.transform_expr_cursor_to_flat(arg, mut out)
					return out.emit_cast_expr_by_ids(typ_id, expr_id, c.pos())
				}
			}
			t.count_flat_fallback(t.classify_call_fallback_cursor(c, 'expr_coc'))
			result := t.transform_call_or_cast_expr(call_or_cast_expr_from_cursor(c))
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_map_init {
			if result_id := t.transform_map_init_cursor_to_flat(c, mut out) {
				return result_id
			}
			t.count_flat_fallback('expr_map_init')
			result := t.transform_map_init_expr(map_init_expr_from_cursor(c))
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_array_init {
			if result_id := t.transform_fixed_array_init_cursor_to_flat(c, mut out) {
				return result_id
			}
			if result_id := t.transform_no_literal_dynamic_array_init_cursor_to_flat(c, mut out) {
				return result_id
			}
			if result_id := t.transform_explicit_dynamic_array_init_cursor_to_flat(c, mut out) {
				return result_id
			}
			t.count_flat_fallback('expr_array_init')
			result := t.transform_array_init_expr(array_init_expr_from_cursor(c))
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_init {
			if result_id := t.transform_empty_map_init_cursor_to_flat(c, mut out) {
				return result_id
			}
			t.count_flat_fallback('expr_init')
			return t.transform_init_expr_to_flat(init_expr_from_cursor(c), mut out)
		}
		.expr_if {
			if t.if_expr_cursor_can_transform_plain(c) {
				return t.transform_plain_if_expr_cursor_to_flat(c, mut out)
			}
			t.count_flat_fallback('expr_if')
			result := t.transform_if_expr(if_expr_from_cursor(c))
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_match {
			t.count_flat_fallback('expr_match')
			match_expr, branches := t.transform_match_expr_parts(match_expr_from_cursor(c))
			return t.lower_match_expr_to_if_flat(match_expr, branches, mut out)
		}
		.expr_comptime {
			return t.transform_comptime_expr_cursor_to_flat(c, mut out)
		}
		.expr_keyword_operator {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .key_typeof && c.edge_count() > 0 {
				type_name := t.resolve_typeof_expr_cursor(c.edge(0))
				if type_name != '' {
					return out.emit_string_literal_by_value(.v, quote_v_string_literal(type_name),
						c.pos())
				}
			}
			if op == .key_go && c.edge_count() > 0 {
				inner := c.edge(0)
				if inner.is_valid() {
					if id := t.lower_go_call_cursor_to_flat(c, inner, mut out) {
						return id
					}
				}
			}
			return out.copy_subtree_from(c.flat, c.id)
		}
		.expr_string_inter {
			return t.transform_string_inter_literal_cursor_to_flat(c, mut out)
		}
		.expr_unsafe {
			body := ast.CursorList{
				flat:      c.flat
				parent_id: c.id
			}
			stmt_ids := t.transform_cursor_stmts_to_flat_direct(body, [], mut out)
			return out.emit_unsafe_expr_by_ids(stmt_ids, c.pos())
		}
		.expr_lock {
			stmt_ids := t.lock_expr_cursor_stmt_ids(c, true, mut out)
			return out.emit_unsafe_expr_by_ids(stmt_ids, token.Pos{})
		}
		.expr_fn_literal {
			typ := c.edge(0)
			typ_id := out.copy_subtree_from(typ.flat, typ.id)
			captured_len := c.extra_int()
			mut captured_ids := []ast.FlatNodeId{cap: captured_len}
			for i in 0 .. captured_len {
				captured := c.edge(1 + i)
				captured_ids << out.copy_subtree_from(captured.flat, captured.id)
			}
			body := ast.CursorList{
				flat:      c.flat
				parent_id: c.id
				offset:    1 + captured_len
			}
			stmt_ids := t.transform_cursor_stmts_to_flat_direct(body, [], mut out)
			return out.emit_fn_literal_by_ids(typ_id, captured_ids, stmt_ids, c.pos())
		}
		.expr_generic_args {
			lhs := c.edge(0)
			if lhs.kind() == .expr_ident && lhs.name() == 'typeof' {
				return out.copy_subtree_from(c.flat, c.id)
			}
			if c.edge_count() == 2 {
				if lhs_type := t.get_expr_type_cursor(lhs) {
					if !t.is_callable_type(lhs_type) {
						arg := c.edge(1)
						if t.generic_index_cursor_can_transform_direct(lhs, arg) {
							return t.transform_generic_index_cursor_to_flat(lhs, arg, c.pos(), mut
								out)
						}
						return t.transform_index_expr_to_flat_parts(lhs.expr(), c.edge(1).expr(),
							false, c.pos(), mut out)
					}
				}
			}
			if suffix := t.generic_specialization_suffix_from_cursor_edges(c, 1) {
				if id := t.specialize_generic_callable_cursor_to_flat(lhs, suffix, c.pos(), mut out) {
					return id
				}
			}
			return out.copy_subtree_from(c.flat, c.id)
		}
		.expr_generic_arg_or_index {
			lhs := c.edge(0)
			arg := c.edge(1)
			if lhs_type := t.get_expr_type_cursor(lhs) {
				if t.is_callable_type(lhs_type) {
					if suffix := t.generic_specialization_suffix_from_cursors([arg]) {
						if id := t.specialize_generic_callable_cursor_to_flat(lhs, suffix, c.pos(), mut
							out)
						{
							return id
						}
					}
					return out.copy_subtree_from(c.flat, c.id)
				}
			}
			if t.generic_index_cursor_can_transform_direct(lhs, arg) {
				return t.transform_generic_index_cursor_to_flat(lhs, arg, c.pos(), mut out)
			}
			return t.transform_index_expr_to_flat_parts(lhs.expr(), arg.expr(), false, c.pos(), mut
				out)
		}
		.expr_selector {
			if result_id := t.transform_selector_cursor_special_to_flat(c, mut out) {
				return result_id
			}
			if t.selector_cursor_can_transform_direct(c) {
				lhs_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
				rhs := c.edge(1)
				rhs_id := out.copy_subtree_from(rhs.flat, rhs.id)
				return out.emit_selector_expr_by_ids(lhs_id, rhs_id, c.pos())
			}
			t.count_flat_fallback('expr_selector')
			result := t.transform_selector_expr(selector_expr_from_cursor(c))
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_index {
			if t.index_cursor_can_transform_direct(c) {
				lhs_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
				index_id := t.transform_expr_cursor_to_flat(c.edge(1), mut out)
				return out.emit_index_expr_by_ids(lhs_id, index_id, c.flag(ast.flag_is_gated),
					c.pos())
			}
			t.count_flat_fallback('expr_index')
			result := t.transform_index_expr(index_expr_from_cursor(c))
			return t.emit_lowered_expr_result_to_flat(result, mut out)
		}
		.expr_lambda {
			inner_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
			mut arg_ids := []ast.FlatNodeId{cap: c.edge_count() - 1}
			for i in 1 .. c.edge_count() {
				arg := c.edge(i)
				arg_ids << out.copy_subtree_from(arg.flat, arg.id)
			}
			return out.emit_lambda_expr_by_ids(inner_id, arg_ids, c.pos())
		}
		else {}
	}

	return out.copy_subtree_from(c.flat, c.id)
}

fn (mut t Transformer) emit_lowered_expr_result_to_flat(result ast.Expr, mut out ast.FlatBuilder) ast.FlatNodeId {
	if result is ast.CallExpr {
		lhs_id := out.emit_expr(result.lhs)
		mut arg_ids := []ast.FlatNodeId{cap: result.args.len}
		for arg in result.args {
			arg_ids << out.emit_expr(arg)
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
	if result is ast.IfExpr {
		cond_id := out.emit_expr(result.cond)
		else_id := out.emit_expr(result.else_expr)
		mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
		for stmt in result.stmts {
			stmt_ids << out.emit_stmt(stmt)
		}
		return out.emit_if_expr_by_ids(cond_id, else_id, stmt_ids, result.pos)
	}
	if result is ast.Ident {
		return out.emit_ident_by_name(result.name, result.pos)
	}
	if result is ast.InfixExpr {
		lhs_id := out.emit_expr(result.lhs)
		rhs_id := out.emit_expr(result.rhs)
		return out.emit_infix_expr_by_ids(result.op, lhs_id, rhs_id, result.pos)
	}
	if result is ast.IndexExpr {
		lhs_id := out.emit_expr(result.lhs)
		expr_id := out.emit_expr(result.expr)
		return out.emit_index_expr_by_ids(lhs_id, expr_id, result.is_gated, result.pos)
	}
	if result is ast.SelectorExpr {
		lhs_id := out.emit_expr(result.lhs)
		rhs_id := out.emit_expr(ast.Expr(result.rhs))
		return out.emit_selector_expr_by_ids(lhs_id, rhs_id, result.pos)
	}
	if result is ast.StringLiteral {
		return out.emit_string_literal_by_value(result.kind, result.value, result.pos)
	}
	if result is ast.PrefixExpr {
		inner_id := out.emit_expr(result.expr)
		return out.emit_prefix_expr_by_id(result.op, inner_id, result.pos)
	}
	if result is ast.PostfixExpr {
		inner_id := out.emit_expr(result.expr)
		return out.emit_postfix_expr_by_id(result.op, inner_id, result.pos)
	}
	if result is ast.SqlExpr {
		expr_id := out.emit_expr(result.expr)
		return out.emit_sql_expr_by_id(result.table_name, result.is_count, result.is_create,
			expr_id, result.pos)
	}
	if result is ast.UnsafeExpr {
		mut stmt_ids := []ast.FlatNodeId{cap: result.stmts.len}
		for stmt in result.stmts {
			stmt_ids << out.emit_stmt(stmt)
		}
		return out.emit_unsafe_expr_by_ids(stmt_ids, result.pos)
	}
	if result is ast.InitExpr {
		typ_id := out.emit_expr(result.typ)
		mut field_ids := []ast.FlatNodeId{cap: result.fields.len}
		for field in result.fields {
			value_id := out.emit_expr(field.value)
			field_ids << out.emit_field_init_by_id(field.name, value_id)
		}
		return out.emit_init_expr_by_ids(typ_id, field_ids, result.pos)
	}
	if result is ast.MapInitExpr {
		typ_id := out.emit_expr(result.typ)
		mut key_ids := []ast.FlatNodeId{cap: result.keys.len}
		for key in result.keys {
			key_ids << out.emit_expr(key)
		}
		mut val_ids := []ast.FlatNodeId{cap: result.vals.len}
		for val in result.vals {
			val_ids << out.emit_expr(val)
		}
		return out.emit_map_init_expr_by_ids(typ_id, key_ids, val_ids, result.pos)
	}
	if result is ast.ArrayInitExpr {
		typ_id := out.emit_expr(result.typ)
		init_id := out.emit_expr(result.init)
		cap_id := out.emit_expr(result.cap)
		len_id := out.emit_expr(result.len)
		update_expr_id := out.emit_expr(result.update_expr)
		mut expr_ids := []ast.FlatNodeId{cap: result.exprs.len}
		for expr in result.exprs {
			expr_ids << out.emit_expr(expr)
		}
		return out.emit_array_init_expr_by_ids(typ_id, init_id, cap_id, len_id, update_expr_id,
			expr_ids, result.pos)
	}
	return out.emit_expr(result)
}

fn (mut t Transformer) transform_selector_cursor_special_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if c.edge_count() < 2 {
		return none
	}
	rhs_name := selector_rhs_name_cursor(c)
	if rhs_name == '' {
		return none
	}
	lhs := c.edge(0)
	if rhs_name in ['name', 'idx'] && lhs.kind() == .expr_keyword_operator {
		op := unsafe { token.Token(int(lhs.aux())) }
		if op == .key_typeof && lhs.edge_count() > 0 {
			type_name := t.resolve_typeof_expr_cursor(lhs.edge(0))
			if result_id := typeof_selector_result_to_flat(type_name, rhs_name, c.pos(), mut out) {
				return result_id
			}
		}
	}
	if rhs_name in ['name', 'idx'] && lhs.kind() == .expr_call {
		type_name := t.resolve_typeof_call_lhs_type_name_cursor(lhs.edge(0))
		if result_id := typeof_selector_result_to_flat(type_name, rhs_name, c.pos(), mut out) {
			return result_id
		}
	}
	if rhs_name in ['name', 'idx'] && lhs.kind() == .expr_call_or_cast {
		arg := lhs.edge(1)
		if !arg.is_valid() || arg.kind() == .expr_empty {
			type_name := t.resolve_typeof_call_lhs_type_name_cursor(lhs.edge(0))
			if result_id := typeof_selector_result_to_flat(type_name, rhs_name, c.pos(), mut out) {
				return result_id
			}
		}
	}
	if rhs_name in ['_tag', '_data']
		|| (rhs_name.starts_with('_') && lhs.kind() == .expr_selector
		&& selector_rhs_name_cursor(lhs) == '_data') {
		return out.copy_subtree_from(c.flat, c.id)
	}
	if t.has_active_smartcast() {
		full_str := expr_cursor_to_string(c)
		if full_str != '' {
			if direct_ctx := t.find_smartcast_for_expr(full_str) {
				if result_id := t.smartcast_direct_cursor_to_flat(c, direct_ctx, mut out) {
					return result_id
				}
				result := t.apply_smartcast_direct_ctx(selector_expr_from_cursor(c), direct_ctx)
				return t.emit_lowered_expr_result_to_flat(result, mut out)
			}
		}
		lhs_str := expr_cursor_to_string(lhs)
		if lhs_str != '' {
			if ctx := t.find_smartcast_for_expr(lhs_str) {
				if result_id := t.smartcast_field_access_cursor_to_flat(lhs, rhs_name, ctx, mut out) {
					return result_id
				}
				result := t.apply_smartcast_field_access_ctx(lhs.expr(), rhs_name, ctx)
				return t.emit_lowered_expr_result_to_flat(result, mut out)
			}
		}
	}
	if lhs.kind() == .expr_ident {
		lhs_name := lhs.name()
		if lhs_name == 'os' && rhs_name == 'args' {
			lhs_id := out.emit_ident_by_name('arguments', c.pos())
			return out.emit_call_expr_by_ids(lhs_id, []ast.FlatNodeId{}, c.pos())
		}
		qualified := if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !lhs_name.contains('__') {
			'${t.cur_module}__${lhs_name}'
		} else {
			lhs_name
		}
		if typ := t.lookup_type(qualified) {
			if typ is types.Enum {
				t.register_synth_type(c.pos(), typ)
				return out.emit_ident_by_name(t.enum_member_ident_for_lookup(qualified, typ,
					rhs_name), c.pos())
			}
		}
	}
	if lhs.kind() == .expr_selector {
		lhs_lhs := lhs.edge(0)
		if lhs_lhs.kind() == .expr_ident {
			module_name := lhs_lhs.name()
			type_name := selector_rhs_name_cursor(lhs)
			qualified := '${module_name}__${type_name}'
			if typ := t.lookup_type(qualified) {
				if typ is types.Enum {
					t.register_synth_type(c.pos(), typ)
					return out.emit_ident_by_name(t.enum_member_ident_for_lookup(qualified, typ,
						rhs_name), c.pos())
				}
			}
			if t.is_module_ident(module_name) {
				sub_mod := type_name
				if t.get_module_scope(sub_mod) != none {
					return out.emit_ident_by_name('${sub_mod}__${rhs_name}', c.pos())
				}
				full_mod := '${module_name}__${sub_mod}'
				if t.get_module_scope(full_mod) != none {
					return out.emit_ident_by_name('${full_mod}__${rhs_name}', c.pos())
				}
			}
		}
	}
	return none
}

struct SmartcastVariantAccessNames {
	variant_simple  string
	mangled_variant string
}

fn smartcast_builtin_variant_name(name string) bool {
	return name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune',
		'f32', 'f64', 'usize', 'isize', 'bool', 'string', 'voidptr', 'charptr', 'byteptr']
}

fn smartcast_direct_data_variant_name(name string) bool {
	return name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'bool', 'rune', 'byte', 'usize', 'isize', 'voidptr', 'charptr', 'byteptr']
}

fn (t &Transformer) smartcast_variant_access_names(ctx SmartcastContext) SmartcastVariantAccessNames {
	variant_short := ctx.variant
	sumtype_module := if ctx.sumtype.contains('__') {
		ctx.sumtype.all_before_last('__')
	} else {
		''
	}
	variant_simple := if variant_short.starts_with('Array_') || variant_short.starts_with('Map_') {
		variant_short
	} else if variant_short.contains('__') {
		mod_prefix := variant_short.all_before_last('__')
		if mod_prefix == sumtype_module {
			variant_short.all_after_last('__')
		} else {
			variant_short
		}
	} else {
		variant_short
	}
	mangled_variant := if ctx.variant_full != '' {
		ctx.variant_full
	} else if smartcast_builtin_variant_name(variant_short) {
		variant_short
	} else if variant_short.contains('__') {
		variant_short
	} else if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		'${t.cur_module}__${variant_short}'
	} else {
		variant_short
	}
	return SmartcastVariantAccessNames{
		variant_simple:  variant_simple
		mangled_variant: mangled_variant
	}
}

fn (mut t Transformer) smartcast_direct_cursor_to_flat(expr ast.Cursor, ctx SmartcastContext, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	names := t.smartcast_variant_access_names(ctx)
	removed_ctxs := t.remove_matching_smartcasts(ctx)
	if t.has_active_smartcast() {
		t.restore_smartcasts(removed_ctxs)
		return none
	}
	transformed_base_id := t.transform_expr_cursor_to_flat(expr, mut out)
	t.restore_smartcasts(removed_ctxs)
	if ctx.sumtype.starts_with('__iface__') {
		object_id := t.synth_selector_cursor_to_flat(transformed_base_id, '_object',
			types.Type(types.voidptr_), mut out)
		typ_id := out.emit_ident_by_name('${names.mangled_variant}*', token.Pos{})
		cast_id := out.emit_cast_expr_by_ids(typ_id, object_id, token.Pos{})
		paren_cast_id := out.emit_paren_expr_by_id(cast_id, token.Pos{})
		deref_id := out.emit_prefix_expr_by_id(.mul, paren_cast_id, token.Pos{})
		return out.emit_paren_expr_by_id(deref_id, token.Pos{})
	}
	data_id := t.synth_selector_cursor_to_flat(transformed_base_id, '_data',
		types.Type(types.voidptr_), mut out)
	is_native_backend := t.pref != unsafe { nil } && t.is_native_be
	variant_id := if is_native_backend {
		data_id
	} else {
		t.synth_selector_cursor_to_flat(data_id, '_${names.variant_simple}',
			types.Type(types.voidptr_), mut out)
	}
	if t.smartcast_variant_data_is_direct(ctx)
		|| smartcast_direct_data_variant_name(names.variant_simple) {
		variant_typ_id := out.emit_ident_by_name(names.mangled_variant, token.Pos{})
		intptr_typ_id := out.emit_ident_by_name('intptr_t', token.Pos{})
		intptr_cast_id := out.emit_cast_expr_by_ids(intptr_typ_id, variant_id, token.Pos{})
		cast_id := out.emit_cast_expr_by_ids(variant_typ_id, intptr_cast_id, token.Pos{})
		return out.emit_paren_expr_by_id(cast_id, token.Pos{})
	}
	typ_id := out.emit_ident_by_name('${names.mangled_variant}*', token.Pos{})
	cast_id := out.emit_cast_expr_by_ids(typ_id, variant_id, token.Pos{})
	deref_id := out.emit_prefix_expr_by_id(.mul, cast_id, token.Pos{})
	return out.emit_paren_expr_by_id(deref_id, token.Pos{})
}

fn (mut t Transformer) synth_selector_from_struct_to_flat(lhs_id ast.FlatNodeId, field_name string, struct_name string, mut out ast.FlatBuilder) ast.FlatNodeId {
	pos := t.next_synth_pos()
	if field_typ := t.lookup_struct_field_type(struct_name, field_name) {
		t.register_synth_type(pos, field_typ)
	}
	rhs_id := out.emit_ident_by_name(field_name, token.Pos{})
	return out.emit_selector_expr_by_ids(lhs_id, rhs_id, pos)
}

fn (mut t Transformer) smartcast_field_access_base_to_flat(transformed_base_id ast.FlatNodeId, field_name string, ctx SmartcastContext, names SmartcastVariantAccessNames, mut out ast.FlatBuilder) ast.FlatNodeId {
	if ctx.sumtype.starts_with('__iface__') {
		object_id := t.synth_selector_cursor_to_flat(transformed_base_id, '_object',
			types.Type(types.voidptr_), mut out)
		typ_id := out.emit_ident_by_name('${names.mangled_variant}*', token.Pos{})
		cast_id := out.emit_cast_expr_by_ids(typ_id, object_id, token.Pos{})
		paren_id := out.emit_paren_expr_by_id(cast_id, token.Pos{})
		return t.synth_selector_from_struct_to_flat(paren_id, field_name, names.mangled_variant, mut
			out)
	}
	is_native_backend := t.pref != unsafe { nil } && t.is_native_be
	data_id := t.synth_selector_cursor_to_flat(transformed_base_id, '_data',
		types.Type(types.voidptr_), mut out)
	variant_id := if is_native_backend {
		data_id
	} else {
		t.synth_selector_cursor_to_flat(data_id, '_${names.variant_simple}',
			types.Type(types.voidptr_), mut out)
	}
	if t.is_eval_backend() {
		return t.synth_selector_from_struct_to_flat(variant_id, field_name, names.mangled_variant, mut
			out)
	}
	typ_id := out.emit_ident_by_name('${names.mangled_variant}*', token.Pos{})
	cast_id := out.emit_cast_expr_by_ids(typ_id, variant_id, token.Pos{})
	return t.synth_selector_from_struct_to_flat(cast_id, field_name, names.mangled_variant, mut out)
}

fn (mut t Transformer) smartcast_field_access_cursor_to_flat(lhs ast.Cursor, field_name string, ctx SmartcastContext, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if lhs.kind() == .expr_ident {
		return t.smartcast_ident_field_access_cursor_to_flat(lhs, field_name, ctx, mut out)
	}
	if lhs.kind() != .expr_selector {
		return none
	}
	names := t.smartcast_variant_access_names(ctx)
	removed_ctxs := t.remove_matching_smartcasts(ctx)
	if t.has_active_smartcast() {
		t.restore_smartcasts(removed_ctxs)
		return none
	}
	transformed_base_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	t.restore_smartcasts(removed_ctxs)
	return t.smartcast_field_access_base_to_flat(transformed_base_id, field_name, ctx, names, mut
		out)
}

fn (mut t Transformer) smartcast_ident_field_access_cursor_to_flat(lhs ast.Cursor, field_name string, ctx SmartcastContext, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if lhs.kind() != .expr_ident {
		return none
	}
	names := t.smartcast_variant_access_names(ctx)
	removed_ctxs := t.remove_matching_smartcasts(ctx)
	if t.find_smartcast_for_expr(lhs.name()) != none {
		t.restore_smartcasts(removed_ctxs)
		return none
	}
	transformed_base_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	t.restore_smartcasts(removed_ctxs)
	return t.smartcast_field_access_base_to_flat(transformed_base_id, field_name, ctx, names, mut
		out)
}

fn (t &Transformer) resolve_typeof_generic_arg_type_name_cursor(arg ast.Cursor) string {
	type_name_from_expr := t.resolve_typeof_expr_cursor(arg)
	if type_name_from_expr != '' {
		return type_name_from_expr
	}
	mut type_name := t.expr_to_type_name_cursor_direct(arg)
	if type_name == '' {
		type_name = t.type_expr_name_full_cursor(arg)
	}
	if type_name == '' {
		return ''
	}
	return c_name_to_v_name(type_name)
}

fn (t &Transformer) resolve_typeof_call_lhs_type_name_cursor(lhs ast.Cursor) string {
	match lhs.kind() {
		.expr_generic_args {
			if lhs.edge_count() > 1 && lhs.edge(0).kind() == .expr_ident
				&& lhs.edge(0).name() == 'typeof' {
				return t.resolve_typeof_generic_arg_type_name_cursor(lhs.edge(1))
			}
		}
		.expr_generic_arg_or_index {
			if lhs.edge(0).kind() == .expr_ident && lhs.edge(0).name() == 'typeof' {
				return t.resolve_typeof_generic_arg_type_name_cursor(lhs.edge(1))
			}
		}
		.expr_ident {
			return t.resolve_specialized_typeof_call_type_name(lhs.name())
		}
		else {}
	}

	return ''
}

fn (t &Transformer) selector_cursor_can_transform_direct(c ast.Cursor) bool {
	if c.edge_count() < 2 {
		return false
	}
	if t.has_active_smartcast()
		&& (expr_cursor_to_string(c) == '' || expr_cursor_to_string(c.edge(0)) == '') {
		return false
	}
	rhs_name := selector_rhs_name_cursor(c)
	if rhs_name == '' {
		return false
	}
	if rhs_name in ['_tag', '_data'] || rhs_name.starts_with('_') {
		return false
	}
	lhs := c.edge(0)
	if rhs_name in ['name', 'idx']
		&& lhs.kind() in [.expr_keyword_operator, .expr_call, .expr_call_or_cast] {
		// typeof(x).name / typeof[T]().idx lower to literals in the legacy
		// pipeline; plain field accesses named `name`/`idx` are ordinary.
		return false
	}
	if lhs.kind() == .expr_ident {
		lhs_name := lhs.name()
		if lhs_name == 'os' && rhs_name == 'args' {
			return false
		}
		qualified := if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !lhs_name.contains('__') {
			'${t.cur_module}__${lhs_name}'
		} else {
			lhs_name
		}
		if typ := t.lookup_type(qualified) {
			if typ is types.Enum {
				return false
			}
		}
		if t.is_module_ident(lhs_name) {
			return false
		}
	}
	if lhs.kind() == .expr_selector {
		lhs_lhs := lhs.edge(0)
		if lhs_lhs.kind() == .expr_ident {
			module_name := lhs_lhs.name()
			type_name := selector_rhs_name_cursor(lhs)
			qualified := '${module_name}__${type_name}'
			if typ := t.lookup_type(qualified) {
				if typ is types.Enum {
					return false
				}
			}
			if t.is_module_ident(module_name) {
				return false
			}
		}
	}
	return true
}

fn cursor_unwraps_to_assoc_expr(expr ast.Cursor) bool {
	mut base := expr
	for base.is_valid() && base.kind() in [.expr_paren, .expr_modifier] {
		base = base.edge(0)
	}
	return base.is_valid() && base.kind() == .expr_assoc
}

fn (mut t Transformer) try_transform_amp_type_cast_cursor_to_flat(expr ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	match expr.kind() {
		.expr_index {
			lhs := expr.edge(0)
			if lhs.kind() != .expr_call_or_cast || !t.call_or_cast_lhs_is_type_cursor(lhs.edge(0)) {
				return none
			}
			cast_id := t.amp_type_cast_lhs_cursor_to_flat(lhs, mut out)
			index_id := t.transform_expr_cursor_to_flat(expr.edge(1), mut out)
			return out.emit_index_expr_by_ids(cast_id, index_id, expr.flag(ast.flag_is_gated),
				expr.pos())
		}
		.expr_selector {
			lhs := expr.edge(0)
			if lhs.kind() != .expr_call_or_cast || !t.call_or_cast_lhs_is_type_cursor(lhs.edge(0)) {
				return none
			}
			cast_id := t.amp_type_cast_lhs_cursor_to_flat(lhs, mut out)
			rhs := expr.edge(1)
			rhs_id := out.copy_subtree_from(rhs.flat, rhs.id)
			return out.emit_selector_expr_by_ids(cast_id, rhs_id, expr.pos())
		}
		else {
			return none
		}
	}
}

fn (mut t Transformer) amp_type_cast_lhs_cursor_to_flat(call_or_cast ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	typ_id := out.copy_subtree_from(call_or_cast.edge(0).flat, call_or_cast.edge(0).id)
	ptr_typ_id := out.emit_prefix_expr_by_id(.amp, typ_id, call_or_cast.pos())
	expr_id := t.transform_expr_cursor_to_flat(call_or_cast.edge(1), mut out)
	return out.emit_cast_expr_by_ids(ptr_typ_id, expr_id, call_or_cast.pos())
}

fn (t &Transformer) index_cursor_can_transform_direct(c ast.Cursor) bool {
	if c.edge_count() < 2 || c.edge(1).kind() == .expr_range {
		return false
	}
	lhs_type := t.get_expr_type_cursor(c.edge(0)) or { return false }
	if _ := t.unwrap_map_type(lhs_type) {
		return t.is_eval_backend()
	}
	return true
}

fn (t &Transformer) generic_index_cursor_can_transform_direct(lhs ast.Cursor, arg ast.Cursor) bool {
	if !lhs.is_valid() || !arg.is_valid() || arg.kind() == .expr_range {
		return false
	}
	lhs_type := t.get_expr_type_cursor(lhs) or { return false }
	if _ := t.unwrap_map_type(lhs_type) {
		return t.is_eval_backend()
	}
	return true
}

fn (mut t Transformer) transform_generic_index_cursor_to_flat(lhs ast.Cursor, arg ast.Cursor, pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	lhs_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	arg_id := t.transform_expr_cursor_to_flat(arg, mut out)
	return out.emit_index_expr_by_ids(lhs_id, arg_id, false, pos)
}

fn (t &Transformer) postfix_cursor_needs_checked_substr(c ast.Cursor) bool {
	inner := c.edge(0)
	return inner.kind() == .expr_index && inner.edge(1).kind() == .expr_range
		&& t.is_string_expr_cursor(inner.edge(0))
}

fn (mut t Transformer) transform_result_postfix_cursor_to_flat(c ast.Cursor, op token.Token, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if c.edge_count() == 0 || c.edge(0).kind() == .expr_empty {
		return none
	}
	if t.postfix_cursor_needs_checked_substr(c) {
		return none
	}
	inner_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
	is_native_backend := t.pref != unsafe { nil } && t.is_native_be
	if is_native_backend {
		return out.emit_postfix_expr_by_id(op, inner_id, c.pos())
	}
	mut type_name := ''
	if inner_type := t.get_expr_type_cursor(c.edge(0)) {
		match inner_type {
			types.ResultType {
				type_name = t.type_to_c_name(inner_type.base_type)
			}
			types.OptionType {
				type_name = t.type_to_c_name(inner_type.base_type)
			}
			else {}
		}
	}
	if type_name == '' {
		if typ := t.get_expr_type_cursor(c) {
			type_name = t.type_to_c_name(typ)
		}
	}
	if type_name != '' {
		typ_id := out.emit_ident_by_name(type_name, token.Pos{})
		return out.emit_cast_expr_by_ids(typ_id, inner_id, c.pos())
	}
	return inner_id
}

fn (t &Transformer) type_expr_uses_current_generic_param_cursor(typ ast.Cursor) bool {
	if !typ.is_valid() {
		return false
	}
	name := t.type_expr_name_cursor(typ)
	if name in t.generic_var_type_params || name in t.cur_fn_generic_params {
		return true
	}
	for i in 0 .. typ.edge_count() {
		if t.type_expr_uses_current_generic_param_cursor(typ.edge(i)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) expr_uses_current_generic_param_cursor(expr ast.Cursor) bool {
	if !expr.is_valid() {
		return false
	}
	match expr.kind() {
		.expr_ident {
			return expr.name() in t.generic_var_type_params
		}
		.expr_index {
			return t.expr_uses_current_generic_param_cursor(expr.edge(0))
		}
		.expr_paren, .expr_modifier {
			return t.expr_uses_current_generic_param_cursor(expr.edge(0))
		}
		.expr_cast {
			if t.type_expr_uses_current_generic_param_cursor(expr.edge(0)) {
				return true
			}
			return t.expr_uses_current_generic_param_cursor(expr.edge(1))
		}
		else {
			return false
		}
	}
}

fn sumtype_expr_needs_variant_inference_cursor(value ast.Cursor) bool {
	return value.kind() in [.expr_init, .expr_array_init, .expr_map_init, .expr_basic_literal,
		.expr_string, .expr_string_inter, .expr_cast, .expr_as_cast]
}

fn (t &Transformer) expr_type_name_hint_cursor(value ast.Cursor) string {
	if !value.is_valid() {
		return ''
	}
	match value.kind() {
		.expr_paren, .expr_modifier {
			return t.expr_type_name_hint_cursor(value.edge(0))
		}
		.expr_cast {
			return t.type_expr_name_full_cursor(value.edge(0))
		}
		.expr_as_cast {
			return t.type_expr_name_full_cursor(value.edge(1))
		}
		.expr_prefix {
			op := unsafe { token.Token(int(value.aux())) }
			if op == .mul {
				inner_type := t.expr_type_name_hint_cursor(value.edge(0))
				base_name := t.pointer_type_name_base(inner_type)
				if base_name != '' {
					return base_name
				}
			}
			return t.expr_type_name_hint_cursor(value.edge(0))
		}
		.expr_call_or_cast {
			if t.call_or_cast_lhs_is_type_cursor(value.edge(0)) {
				return t.type_expr_name_full_cursor(value.edge(0))
			}
		}
		else {}
	}

	return ''
}

fn (t &Transformer) init_expr_sumtype_variant_name_cursor(value ast.Cursor, variants []string, sumtype_name string) string {
	if !value.is_valid() || value.kind() != .expr_init {
		return ''
	}
	init_type_name := t.type_expr_name_full_cursor(value.edge(0))
	if init_type_name == '' || t.is_same_sumtype_name(init_type_name, sumtype_name) {
		return ''
	}
	if variant_name := t.match_variant(init_type_name, variants) {
		return variant_name
	}
	if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		mangled := '${t.cur_module}__${init_type_name}'
		if variant_name := t.match_variant(mangled, variants) {
			return variant_name
		}
	}
	return ''
}

fn (t &Transformer) sumtype_variant_name_from_cursor(value ast.Cursor, sumtype_name string, variants []string) ?string {
	if t.expr_uses_current_generic_param_cursor(value) {
		return none
	}
	mut init_variant_name := ''
	if value.kind() == .expr_init {
		init_variant_name = t.init_expr_sumtype_variant_name_cursor(value, variants, sumtype_name)
	}
	if declared_type := t.declared_expr_type_for_method_receiver_cursor(value) {
		declared_c_name := t.type_to_c_name(declared_type)
		if t.is_same_sumtype_name(declared_c_name, sumtype_name) && init_variant_name == '' {
			return none
		}
	}
	typ := t.get_expr_type_cursor(value) or { return none }
	c_name := t.type_to_c_name(typ)
	input_is_target_sumtype := t.is_same_sumtype_name(c_name, sumtype_name)
	if input_is_target_sumtype && init_variant_name == ''
		&& !sumtype_expr_needs_variant_inference_cursor(value) {
		return none
	}
	if value.kind() == .expr_ident {
		if var_type := t.lookup_var_type(value.name()) {
			var_c_name := t.type_to_c_name(var_type)
			if t.is_same_sumtype_name(var_c_name, sumtype_name) {
				return none
			}
		}
	}
	mut variant_name := init_variant_name
	if c_name != '' && c_name != 'void' && !input_is_target_sumtype {
		variant_name = t.match_variant(c_name, variants) or { '' }
	}
	if variant_name == '' && !input_is_target_sumtype {
		constructor_name := t.type_constructor_name(typ)
		if constructor_name != '' {
			variant_name = t.match_variant(constructor_name, variants) or { '' }
		}
	}
	if variant_name == '' {
		match value.kind() {
			.expr_basic_literal {
				lit_kind := unsafe { token.Token(int(value.aux())) }
				if lit_kind == .number {
					variant_name = if value.name().contains('.') {
						match_sumtype_variant_name('f64', variants)
					} else {
						match_sumtype_variant_name('int', variants)
					}
				} else if lit_kind == .string {
					variant_name = match_sumtype_variant_name('string', variants)
				}
			}
			.expr_string, .expr_string_inter {
				variant_name = match_sumtype_variant_name('string', variants)
			}
			.expr_ident {
				var_type_name := t.get_var_type_name(value.name())
				if var_type_name != '' {
					variant_name = t.match_variant(var_type_name, variants) or { '' }
				}
			}
			.expr_cast {
				variant_name = t.match_variant(t.type_expr_name_full_cursor(value.edge(0)),
					variants) or { '' }
			}
			.expr_as_cast {
				variant_name = t.match_variant(t.type_expr_name_full_cursor(value.edge(1)),
					variants) or { '' }
			}
			else {}
		}
	}
	if variant_name == '' {
		hint_type := t.expr_type_name_hint_cursor(value)
		if hint_type != '' {
			variant_name = t.match_variant(hint_type, variants) or { '' }
		}
	}
	if variant_name == '' {
		return none
	}
	return variant_name
}

fn sumtype_payload_field_short_variant(variant_name string) string {
	if variant_name.starts_with('[]') {
		return 'Array_${variant_name[2..]}'
	}
	if variant_name.starts_with('map[') {
		inner := variant_name[4..]
		if bracket_idx := inner.index(']') {
			key := inner[..bracket_idx]
			val := inner[bracket_idx + 1..]
			return 'Map_${key}_${val}'
		}
		return variant_name
	}
	if variant_name.contains('__') {
		return variant_name.all_after_last('__')
	}
	return variant_name
}

fn (mut t Transformer) build_sumtype_init_cursor_to_flat(value ast.Cursor, transformed_value_id ast.FlatNodeId, variant_name string, sumtype_name string, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	variants := t.get_sum_type_variants(sumtype_name)
	mut tag_value := -1
	for i, variant in variants {
		if variant == variant_name {
			tag_value = i
			break
		}
	}
	if tag_value < 0 {
		return none
	}
	boxed_value_id := if t.is_eval_backend() {
		transformed_value_id
	} else if t.sumtype_variant_init_data_is_direct(variant_name) {
		intptr_typ_id := out.emit_ident_by_name('intptr_t', token.Pos{})
		intptr_cast_id :=
			out.emit_cast_expr_by_ids(intptr_typ_id, transformed_value_id, token.Pos{})
		voidptr_typ_id := out.emit_ident_by_name('voidptr', token.Pos{})
		out.emit_cast_expr_by_ids(voidptr_typ_id, intptr_cast_id, token.Pos{})
	} else {
		payload_type_id := if value.kind() == .expr_init {
			out.copy_subtree_from(value.edge(0).flat, value.edge(0).id)
		} else {
			out.emit_ident_by_name(variant_name, token.Pos{})
		}
		amp_id := out.emit_prefix_expr_by_id(.amp, transformed_value_id, token.Pos{})
		sizeof_id := out.emit_keyword_operator_by_ids(.key_sizeof, [payload_type_id], token.Pos{})
		memdup_lhs_id := out.emit_ident_by_name('memdup', token.Pos{})
		memdup_id := out.emit_call_expr_by_ids(memdup_lhs_id, [amp_id, sizeof_id], token.Pos{})
		voidptr_typ_id := out.emit_ident_by_name('voidptr', token.Pos{})
		out.emit_cast_expr_by_ids(voidptr_typ_id, memdup_id, token.Pos{})
	}
	typ_id := out.emit_ident_by_name(sumtype_name, token.Pos{})
	tag_value_id := out.emit_basic_literal_by_value(.number, tag_value.str(), token.Pos{})
	tag_field_id := out.emit_field_init_by_id('_tag', tag_value_id)
	short_variant := sumtype_payload_field_short_variant(variant_name)
	data_field_id := out.emit_field_init_by_id('_data._${short_variant}', boxed_value_id)
	return out.emit_init_expr_by_ids(typ_id, [tag_field_id, data_field_id], token.Pos{})
}

fn (mut t Transformer) wrap_sumtype_value_cursor_to_flat(value ast.Cursor, sumtype_name string, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 {
		return none
	}
	variant_name := t.sumtype_variant_name_from_cursor(value, sumtype_name, variants) or {
		return none
	}
	transformed_value_id := t.transform_expr_cursor_to_flat(value, mut out)
	return t.build_sumtype_init_cursor_to_flat(value, transformed_value_id, variant_name,
		sumtype_name, mut out)
}

fn (t &Transformer) resolve_go_fn_name_cursor(lhs ast.Cursor) string {
	match lhs.kind() {
		.expr_ident {
			if t.cur_module != '' {
				return '${t.cur_module}__${lhs.name()}'
			}
			return lhs.name()
		}
		.expr_selector {
			base := lhs.edge(0)
			if base.kind() == .expr_ident {
				return '${base.name()}__${lhs.edge(1).name()}'
			}
		}
		else {}
	}

	return ''
}

fn (mut t Transformer) lower_go_call_cursor_to_flat(keyword ast.Cursor, call ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	mut call_lhs := ast.Cursor{}
	mut arg_start := 0
	mut arg_count := 0
	match call.kind() {
		.expr_call {
			call_lhs = call.edge(0)
			arg_start = 1
			arg_count = if call.edge_count() > 1 { call.edge_count() - 1 } else { 0 }
		}
		.expr_call_or_cast {
			call_lhs = call.edge(0)
			arg_start = 1
			arg := call.edge(1)
			arg_count = if arg.is_valid() && arg.kind() != .expr_empty { 1 } else { 0 }
		}
		else {
			return none
		}
	}

	fn_name := t.resolve_go_fn_name_cursor(call_lhs)
	if fn_name == '' {
		return none
	}
	mut transformed_arg_ids := []ast.FlatNodeId{cap: arg_count}
	mut arg_type_names := []string{cap: arg_count}
	for i in 0 .. arg_count {
		arg := call.edge(arg_start + i)
		transformed_arg_ids << t.transform_expr_cursor_to_flat(arg, mut out)
		if typ := t.get_expr_type_cursor(arg) {
			arg_type_names << t.type_to_c_name(typ)
		} else {
			arg_type_names << 'int'
		}
	}
	mut is_method := false
	mut receiver_id := ast.FlatNodeId(-1)
	mut receiver_type_name := ''
	if call_lhs.kind() == .expr_selector {
		sel_lhs := call_lhs.edge(0)
		if !(sel_lhs.kind() == .expr_ident && t.is_module_name(sel_lhs.name())) {
			is_method = true
			receiver_id = t.transform_expr_cursor_to_flat(sel_lhs, mut out)
			if recv_type := t.get_expr_type_cursor(sel_lhs) {
				receiver_type_name = t.type_to_c_name(recv_type)
			} else {
				receiver_type_name = 'voidptr'
			}
		}
	}
	wrapper_name := '__go_wrap_${fn_name}'
	if wrapper_name !in t.needed_go_wrappers {
		mut param_names := []string{}
		mut param_types := []string{}
		if is_method {
			param_names << '_recv'
			param_types << receiver_type_name
		}
		for i, type_name in arg_type_names {
			param_names << '_a${i}'
			param_types << type_name
		}
		t.needed_go_wrappers[wrapper_name] = GoWrapperInfo{
			fn_name:      fn_name
			wrapper_name: wrapper_name
			param_names:  param_names
			param_types:  param_types
		}
	}
	mut wrapper_arg_ids := []ast.FlatNodeId{cap: transformed_arg_ids.len + 1}
	if is_method {
		wrapper_arg_ids << receiver_id
	}
	wrapper_arg_ids << transformed_arg_ids
	lhs_id := out.emit_ident_by_name(wrapper_name, token.Pos{})
	return out.emit_call_expr_by_ids(lhs_id, wrapper_arg_ids, keyword.pos())
}

fn (mut t Transformer) transform_ident_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	name := c.name()
	pos := c.pos()
	if name == '@VMODROOT' {
		return out.emit_string_literal_by_value(.v, quote_v_string_literal(t.comptime_vmodroot),
			pos)
	}
	if ctx := t.find_smartcast_for_expr(name) {
		ident := ast.Ident{
			name: name
			pos:  pos
		}
		smartcast_result := t.apply_smartcast_direct_ctx(ident, ctx)
		if smartcast_result is ast.ParenExpr {
			inner_id := out.emit_expr(smartcast_result.expr)
			return out.emit_paren_expr_by_id(inner_id, smartcast_result.pos)
		}
		return out.emit_expr(smartcast_result)
	}
	return out.emit_ident_by_name(name, pos)
}

fn (t &Transformer) resolve_typeof_expr_cursor(expr ast.Cursor) string {
	if raw_type := t.get_expr_type_cursor(expr) {
		return t.types_type_to_v(raw_type)
	}
	return ''
}

fn (mut t Transformer) transform_comptime_expr_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	inner := c.edge(0)
	if !inner.is_valid() {
		inner_id := out.emit_expr(ast.empty_expr)
		return out.emit_comptime_expr_by_id(inner_id, c.pos())
	}
	if inner.kind() == .expr_ident && inner.name() in ['VMODROOT', '@VMODROOT'] {
		return out.emit_string_literal_by_value(.v, quote_v_string_literal(t.comptime_vmodroot),
			c.pos())
	}
	if inner.kind() in [.expr_call, .expr_call_or_cast] {
		lhs := inner.edge(0)
		if lhs.kind() == .expr_ident {
			if lhs.name() == 'res' {
				return out.emit_basic_literal_by_value(.key_false, 'false', c.pos())
			}
		}
	}
	if inner.kind() == .expr_if {
		return t.eval_comptime_if_cursor_to_flat(inner, mut out)
	}
	if transformed_id := t.transform_embed_file_comptime_chain_cursor_to_flat(inner, c.pos(), mut
		out)
	{
		return transformed_id
	}
	inner_id := t.transform_expr_cursor_to_flat(inner, mut out)
	return out.emit_comptime_expr_by_id(inner_id, c.pos())
}

fn embed_file_string_arg_cursor(expr ast.Cursor) ?string {
	if !expr.is_valid() {
		return none
	}
	if expr.kind() == .expr_string || (expr.kind() == .expr_basic_literal
		&& unsafe { token.Token(int(expr.aux())) } == .string) {
		value := expr.name()
		if value.len >= 2 {
			return value[1..value.len - 1]
		}
		return ''
	}
	return none
}

fn (mut t Transformer) transform_embed_file_comptime_expr_cursor_to_flat(call ast.Cursor, comptime_pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	arg := call.edge(1)
	raw_path := embed_file_string_arg_cursor(arg) or {
		inner_id := out.copy_subtree_from(call.flat, call.id)
		return out.emit_comptime_expr_by_id(inner_id, comptime_pos)
	}
	rpath, apath, file_bytes := t.embed_file_init_parts_from_raw(raw_path, comptime_pos)
	typ_id := out.emit_ident_by_name(embed_file_helper_type_name, comptime_pos)
	data_value_id := out.emit_string_literal_by_value(.v, quote_v_bytes_literal(file_bytes),
		comptime_pos)
	data_field_id := out.emit_field_init_by_id('_data', data_value_id)
	len_value_id := out.emit_basic_literal_by_value(.number, file_bytes.len.str(), comptime_pos)
	len_field_id := out.emit_field_init_by_id('len', len_value_id)
	path_value_id := out.emit_string_literal_by_value(.v, quote_v_string_literal(rpath),
		comptime_pos)
	path_field_id := out.emit_field_init_by_id('path', path_value_id)
	apath_value_id := out.emit_string_literal_by_value(.v, quote_v_string_literal(apath),
		comptime_pos)
	apath_field_id := out.emit_field_init_by_id('apath', apath_value_id)
	return out.emit_init_expr_by_ids(typ_id, [data_field_id, len_field_id, path_field_id,
		apath_field_id], comptime_pos)
}

fn call_expr_arg_ids_from_cursor(mut t Transformer, call ast.Cursor, mut out ast.FlatBuilder) []ast.FlatNodeId {
	mut arg_ids := []ast.FlatNodeId{cap: call.edge_count() - 1}
	for i in 1 .. call.edge_count() {
		arg_ids << t.transform_expr_cursor_to_flat(call.edge(i), mut out)
	}
	return arg_ids
}

fn (mut t Transformer) transform_embed_file_chain_lhs_cursor_to_flat(expr ast.Cursor, comptime_pos token.Pos, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	match expr.kind() {
		.expr_call {
			lhs := expr.edge(0)
			if lhs.kind() == .expr_ident && lhs.name() == 'embed_file' {
				return t.transform_embed_file_comptime_expr_cursor_to_flat(expr, comptime_pos, mut
					out)
			}
			if lhs.kind() == .expr_selector {
				if transformed_base_id := t.transform_embed_file_chain_lhs_cursor_to_flat(lhs.edge(0),
					comptime_pos, mut out)
				{
					rhs := lhs.edge(1)
					rhs_id := out.emit_ident_by_name(rhs.name(), rhs.pos())
					sel_id := out.emit_selector_expr_by_ids(transformed_base_id, rhs_id, lhs.pos())
					arg_ids := call_expr_arg_ids_from_cursor(mut t, expr, mut out)
					return out.emit_call_expr_by_ids(sel_id, arg_ids, expr.pos())
				}
			}
		}
		.expr_call_or_cast {
			lhs := expr.edge(0)
			if lhs.kind() == .expr_ident && lhs.name() == 'embed_file' {
				return t.transform_embed_file_comptime_expr_cursor_to_flat(expr, comptime_pos, mut
					out)
			}
		}
		.expr_selector {
			if transformed_lhs_id := t.transform_embed_file_chain_lhs_cursor_to_flat(expr.edge(0),
				comptime_pos, mut out)
			{
				rhs := expr.edge(1)
				rhs_id := out.emit_ident_by_name(rhs.name(), rhs.pos())
				return out.emit_selector_expr_by_ids(transformed_lhs_id, rhs_id, expr.pos())
			}
		}
		.expr_comptime {
			if transformed_inner_id := t.transform_embed_file_comptime_chain_cursor_to_flat(expr.edge(0),
				expr.pos(), mut out)
			{
				return transformed_inner_id
			}
		}
		else {}
	}

	return none
}

fn (mut t Transformer) transform_embed_file_comptime_chain_cursor_to_flat(expr ast.Cursor, comptime_pos token.Pos, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if transformed_id := t.transform_embed_file_chain_lhs_cursor_to_flat(expr, comptime_pos, mut
		out)
	{
		return transformed_id
	}
	match expr.kind() {
		.expr_selector {
			if transformed_lhs_id := t.transform_embed_file_chain_lhs_cursor_to_flat(expr.edge(0),
				comptime_pos, mut out)
			{
				rhs := expr.edge(1)
				rhs_id := out.emit_ident_by_name(rhs.name(), rhs.pos())
				return out.emit_selector_expr_by_ids(transformed_lhs_id, rhs_id, expr.pos())
			}
		}
		.expr_call {
			lhs := expr.edge(0)
			if lhs.kind() == .expr_selector {
				if transformed_base_id := t.transform_embed_file_chain_lhs_cursor_to_flat(lhs.edge(0),
					comptime_pos, mut out)
				{
					rhs := lhs.edge(1)
					rhs_id := out.emit_ident_by_name(rhs.name(), rhs.pos())
					sel_id := out.emit_selector_expr_by_ids(transformed_base_id, rhs_id, lhs.pos())
					arg_ids := call_expr_arg_ids_from_cursor(mut t, expr, mut out)
					return out.emit_call_expr_by_ids(sel_id, arg_ids, expr.pos())
				}
			}
		}
		else {}
	}

	return none
}

fn (mut t Transformer) eval_comptime_if_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	if !c.is_valid() || c.kind() != .expr_if {
		return out.emit_expr(ast.empty_expr)
	}
	if t.eval_comptime_cond_cursor(c.edge(0)) {
		body := ast.CursorList{
			flat:      c.flat
			parent_id: c.id
			offset:    2
		}
		if body.len() == 1 {
			stmt := body.at(0)
			if stmt.kind() == .stmt_expr {
				return t.transform_expr_cursor_to_flat(stmt.edge(0), mut out)
			}
		}
		return out.emit_expr(ast.empty_expr)
	}
	else_c := c.edge(1)
	if !else_c.is_valid() || else_c.kind() == .expr_empty {
		return out.emit_expr(ast.empty_expr)
	}
	if else_c.kind() == .expr_if {
		if else_c.edge(0).kind() == .expr_empty {
			body := ast.CursorList{
				flat:      else_c.flat
				parent_id: else_c.id
				offset:    2
			}
			if body.len() == 1 {
				stmt := body.at(0)
				if stmt.kind() == .stmt_expr {
					return t.transform_expr_cursor_to_flat(stmt.edge(0), mut out)
				}
			}
			return out.emit_expr(ast.empty_expr)
		}
		return t.eval_comptime_if_cursor_to_flat(else_c, mut out)
	}
	return out.emit_expr(ast.empty_expr)
}

fn lock_cursor_lengths(c ast.Cursor) (int, int) {
	packed := u32(c.extra_int())
	return int(packed & 0xFFFF), int((packed >> 16) & 0xFFFF)
}

fn (mut t Transformer) shared_mtx_expr_cursor_to_flat(locked_expr ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	base := if locked_expr.kind() == .expr_selector && locked_expr.edge_count() > 0 {
		locked_expr.edge(0)
	} else {
		locked_expr
	}
	base_id := out.copy_subtree_from(base.flat, base.id)
	return t.synth_selector_cursor_to_flat(base_id, 'mtx', types.Type(types.Struct{
		name: 'sync__RwMutex'
	}), mut out)
}

fn (mut t Transformer) emit_lock_runtime_stmt_cursor_to_flat(fn_name string, locked_expr ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	lhs_id := out.emit_ident_by_name(fn_name, token.Pos{})
	mtx_id := t.shared_mtx_expr_cursor_to_flat(locked_expr, mut out)
	arg_id := out.emit_prefix_expr_by_id(.amp, mtx_id, token.Pos{})
	call_id := out.emit_call_expr_by_ids(lhs_id, [arg_id], token.Pos{})
	return out.emit_expr_stmt_by_id(call_id)
}

fn (mut t Transformer) lock_expr_cursor_stmt_ids(c ast.Cursor, duplicate_last_body bool, mut out ast.FlatBuilder) []ast.FlatNodeId {
	lock_len, rlock_len := lock_cursor_lengths(c)
	mut ids := []ast.FlatNodeId{}
	if !t.is_native_be {
		for i in 0 .. lock_len {
			ids << t.emit_lock_runtime_stmt_cursor_to_flat('sync__RwMutex_lock', c.edge(i), mut out)
		}
		for i in lock_len .. (lock_len + rlock_len) {
			ids << t.emit_lock_runtime_stmt_cursor_to_flat('sync__RwMutex_rlock', c.edge(i), mut out)
		}
	}
	body := ast.CursorList{
		flat:      c.flat
		parent_id: c.id
		offset:    lock_len + rlock_len
	}
	body_ids := t.transform_cursor_stmts_to_flat_direct(body, [], mut out)
	ids << body_ids
	if !t.is_native_be {
		for i in 0 .. lock_len {
			ids << t.emit_lock_runtime_stmt_cursor_to_flat('sync__RwMutex_unlock', c.edge(i), mut out)
		}
		for i in lock_len .. (lock_len + rlock_len) {
			ids << t.emit_lock_runtime_stmt_cursor_to_flat('sync__RwMutex_runlock', c.edge(i), mut out)
		}
	}
	if duplicate_last_body && body_ids.len > 0 && (lock_len + rlock_len) > 0 {
		ids << out.copy_subtree_from(&out.flat, body_ids[body_ids.len - 1])
	}
	return ids
}

fn (mut t Transformer) expand_lock_expr_cursor_to_flat(c ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	lock_ids := t.lock_expr_cursor_stmt_ids(c, false, mut out)
	for id in lock_ids {
		ids << id
	}
}

fn copy_cursor_aux_list_to_flat(list ast.CursorList, mut out ast.FlatBuilder) ast.FlatNodeId {
	if list.len() == 0 {
		return out.emit_aux_list_from_ids([])
	}
	if list.offset == 0 {
		return out.copy_subtree_from(list.flat, list.parent_id)
	}
	mut ids := []ast.FlatNodeId{cap: list.len()}
	for i in 0 .. list.len() {
		item := list.at(i)
		ids << out.copy_subtree_from(item.flat, item.id)
	}
	return out.emit_aux_list_from_ids(ids)
}

fn (mut t Transformer) transform_block_stmt_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	body := ast.CursorList{
		flat:      c.flat
		parent_id: c.id
	}
	stmt_ids := t.transform_cursor_stmts_to_flat_direct(body, [], mut out)
	return out.emit_block_stmt_by_ids(stmt_ids)
}

fn (mut t Transformer) transform_defer_stmt_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	body := ast.CursorList{
		flat:      c.flat
		parent_id: c.id
	}
	mode := if c.flag(ast.flag_defer_func) {
		ast.DeferMode.function
	} else {
		ast.DeferMode.scoped
	}
	stmt_ids := t.transform_cursor_stmts_to_flat_direct(body, [], mut out)
	return out.emit_defer_stmt_by_ids(mode, stmt_ids)
}

fn (mut t Transformer) transform_comptime_stmt_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	inner := c.edge(0)
	if inner.kind() == .stmt_for {
		stmt_ids := t.transform_cursor_stmts_to_flat_direct(inner.for_body_list(), [], mut out)
		init_id := out.copy_subtree_from(inner.flat, inner.edge(0).id)
		cond_id := out.copy_subtree_from(inner.flat, inner.edge(1).id)
		post_id := out.copy_subtree_from(inner.flat, inner.edge(2).id)
		for_id := out.emit_for_stmt_by_ids(init_id, cond_id, post_id, stmt_ids)
		return out.emit_comptime_stmt_by_id(for_id)
	}
	return t.transform_stmt_cursor_to_flat(inner, mut out)
}

fn (mut t Transformer) try_expand_comptime_if_stmt_cursor_to_flat(c ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if if_expr := t.comptime_if_cursor_from_expr_stmt(c) {
		if !t.can_eval_selected_comptime_if_cursor(if_expr) {
			return false
		}
		selected_ids := t.selected_comptime_if_cursor_stmts_to_flat(if_expr, mut out)
		for id in selected_ids {
			t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
		}
		return true
	}
	return false
}

fn (mut t Transformer) transform_comptime_if_stmt_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if if_expr := t.comptime_if_cursor_from_expr_stmt(c) {
		if !t.can_eval_selected_comptime_if_cursor(if_expr) {
			return none
		}
		selected_ids := t.selected_comptime_if_cursor_stmts_to_flat(if_expr, mut out)
		if selected_ids.len == 1 {
			return selected_ids[0]
		}
		return out.emit_block_stmt_by_ids(selected_ids)
	}
	return none
}

fn (t &Transformer) comptime_if_cursor_from_expr_stmt(c ast.Cursor) ?ast.Cursor {
	if !c.is_valid() || c.kind() != .stmt_expr || c.edge_count() == 0 {
		return none
	}
	expr := c.edge(0)
	if !expr.is_valid() || expr.kind() != .expr_comptime {
		return none
	}
	inner := expr.edge(0)
	if !inner.is_valid() || inner.kind() != .expr_if {
		return none
	}
	return inner
}

fn (t &Transformer) can_eval_selected_comptime_if_cursor(c ast.Cursor) bool {
	if !c.is_valid() || c.kind() != .expr_if {
		return false
	}
	cond := c.edge(0)
	if !t.can_eval_comptime_cond_cursor(cond) {
		return false
	}
	if t.eval_comptime_cond_cursor(cond) {
		return true
	}
	else_expr := c.edge(1)
	if !else_expr.is_valid() || else_expr.kind() == .expr_empty {
		return true
	}
	if else_expr.kind() != .expr_if {
		return true
	}
	if expr_cursor_is_empty(else_expr.edge(0)) {
		return true
	}
	return t.can_eval_selected_comptime_if_cursor(else_expr)
}

fn (mut t Transformer) selected_comptime_if_cursor_stmts_to_flat(c ast.Cursor, mut out ast.FlatBuilder) []ast.FlatNodeId {
	if !c.is_valid() || c.kind() != .expr_if {
		return []ast.FlatNodeId{}
	}
	if t.eval_comptime_cond_cursor(c.edge(0)) {
		return t.transform_cursor_stmts_to_flat_direct(ast.CursorList{
			flat:      c.flat
			parent_id: c.id
			offset:    2
		}, [], mut out)
	}
	else_expr := c.edge(1)
	if !else_expr.is_valid() || else_expr.kind() == .expr_empty {
		return []ast.FlatNodeId{}
	}
	if else_expr.kind() == .expr_if {
		if expr_cursor_is_empty(else_expr.edge(0)) {
			return t.transform_cursor_stmts_to_flat_direct(ast.CursorList{
				flat:      else_expr.flat
				parent_id: else_expr.id
				offset:    2
			}, [], mut out)
		}
		return t.selected_comptime_if_cursor_stmts_to_flat(else_expr, mut out)
	}
	expr_id := t.transform_expr_cursor_to_flat(else_expr, mut out)
	return [out.emit_expr_stmt_by_id(expr_id)]
}

fn (mut t Transformer) transform_label_stmt_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	inner_id := t.transform_stmt_cursor_to_flat(c.edge(0), mut out)
	return out.emit_label_stmt_by_id(c.name(), inner_id)
}

fn (mut t Transformer) expand_assert_stmt_cursor_to_flat(c ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	expr := c.edge(0)
	if !expr.is_valid() || expr.kind() == .expr_empty {
		return
	}
	expr_str := expr_cursor_to_v_string(expr)
	pos := expr.pos()
	line := t.get_line_for_pos(pos)
	fn_name := t.cur_fn_name_str
	header := '${t.cur_file_name}:${line}: fn ${fn_name}'
	expr_msg := '   > assert ${expr_str}'
	mut body_ids := []ast.FlatNodeId{}
	body_ids << t.make_eprintln_str_to_flat(header, pos, mut out)
	body_ids << t.make_eprintln_str_to_flat(expr_msg, pos, mut out)
	if assert_expr_cursor_is_comparison(expr) {
		lhs := expr.edge(0)
		rhs := expr.edge(1)
		if assert_expr_cursor_is_simple(lhs) && assert_expr_cursor_is_simple(rhs) {
			body_ids << t.make_eprintln_inter_cursor_to_flat('    Left value: `', lhs, '`', pos, mut out)
			body_ids << t.make_eprintln_inter_cursor_to_flat('   Right value: `', rhs, '`', pos, mut out)
		}
	}
	body_ids << make_exit_one_stmt_to_flat(pos, mut out)
	cond_expr_id := t.transform_expr_cursor_to_flat(expr, mut out)
	cond_id := out.emit_prefix_expr_by_id(.not, cond_expr_id, pos)
	else_id := out.emit_expr(ast.empty_expr)
	if_id := out.emit_if_expr_by_ids(cond_id, else_id, body_ids, pos)
	stmt_id := out.emit_expr_stmt_by_id(if_id)
	t.append_transformed_stmt_id_to_flat(mut ids, stmt_id, mut out)
}

fn expr_cursor_to_v_string(expr ast.Cursor) string {
	if !expr.is_valid() {
		return '...'
	}
	return match expr.kind() {
		.expr_ident, .expr_basic_literal, .expr_string {
			expr.name()
		}
		.expr_infix {
			op := unsafe { token.Token(int(expr.aux())) }
			lhs := expr_cursor_to_v_string(expr.edge(0))
			rhs := expr_cursor_to_v_string(expr.edge(1))
			'${lhs} ${op.str()} ${rhs}'
		}
		.expr_prefix {
			op := unsafe { token.Token(int(expr.aux())) }
			inner := expr_cursor_to_v_string(expr.edge(0))
			'${op.str()}${inner}'
		}
		.expr_selector {
			lhs := expr_cursor_to_v_string(expr.edge(0))
			'${lhs}.${expr.edge(1).name()}'
		}
		.expr_call {
			if expr.edge_count() == 0 {
				'...'
			} else {
				name := expr_cursor_to_v_string(expr.edge(0))
				mut args := []string{cap: expr.edge_count() - 1}
				for i in 1 .. expr.edge_count() {
					args << expr_cursor_to_v_string(expr.edge(i))
				}
				'${name}(${args.join(', ')})'
			}
		}
		.expr_paren {
			inner := expr_cursor_to_v_string(expr.edge(0))
			'(${inner})'
		}
		.expr_index {
			lhs := expr_cursor_to_v_string(expr.edge(0))
			idx := expr_cursor_to_v_string(expr.edge(1))
			'${lhs}[${idx}]'
		}
		.expr_cast {
			typ := expr_cursor_to_v_string(expr.edge(0))
			inner := expr_cursor_to_v_string(expr.edge(1))
			'${typ}(${inner})'
		}
		.expr_modifier {
			kind := unsafe { token.Token(int(expr.aux())) }
			inner := expr_cursor_to_v_string(expr.edge(0))
			'${kind.str()} ${inner}'
		}
		else {
			'...'
		}
	}
}

fn assert_expr_cursor_is_simple(expr ast.Cursor) bool {
	if !expr.is_valid() {
		return false
	}
	return expr.kind() in [.expr_ident, .expr_basic_literal, .expr_string, .expr_selector]
}

fn assert_expr_cursor_is_comparison(expr ast.Cursor) bool {
	if !expr.is_valid() || expr.kind() != .expr_infix {
		return false
	}
	op := unsafe { token.Token(int(expr.aux())) }
	return op in [.eq, .ne, .lt, .gt, .le, .ge]
}

fn (mut t Transformer) make_eprintln_str_to_flat(value string, pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	lhs_id := out.emit_ident_by_name('eprintln', token.Pos{})
	arg_id := out.emit_string_literal_by_value(.v, quote_v_string_literal(value), pos)
	call_id := out.emit_call_expr_by_ids(lhs_id, [arg_id], pos)
	return out.emit_expr_stmt_by_id(call_id)
}

fn (mut t Transformer) make_eprintln_inter_cursor_to_flat(prefix string, expr ast.Cursor, suffix string, pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	lhs_id := out.emit_ident_by_name('eprintln', token.Pos{})
	inter_expr_id := t.transform_expr_cursor_to_flat(expr, mut out)
	format_expr_id := out.emit_expr(ast.empty_expr)
	inter_id := out.emit_string_inter_by_ids(.unformatted, 0, 0, inter_expr_id, format_expr_id, '')
	arg_id := out.emit_string_inter_literal_by_ids(.v, [prefix, suffix], [inter_id], pos)
	call_id := out.emit_call_expr_by_ids(lhs_id, [arg_id], pos)
	return out.emit_expr_stmt_by_id(call_id)
}

fn make_exit_one_stmt_to_flat(pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	lhs_id := out.emit_ident_by_name('exit', token.Pos{})
	arg_id := out.emit_basic_literal_by_value(.number, '1', pos)
	call_id := out.emit_call_expr_by_ids(lhs_id, [arg_id], pos)
	return out.emit_expr_stmt_by_id(call_id)
}

fn (mut t Transformer) transform_assign_stmt_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	t.remember_decl_assign_cursor_type(c)
	lhs_len := c.extra_int()
	mut lhs_ids := []ast.FlatNodeId{cap: lhs_len}
	for i in 0 .. lhs_len {
		lhs_ids << t.transform_assign_lhs_cursor_to_flat(c.edge(i), mut out)
	}
	mut rhs_ids := []ast.FlatNodeId{cap: c.edge_count() - lhs_len}
	for i in lhs_len .. c.edge_count() {
		rhs_ids << t.transform_expr_cursor_to_flat(c.edge(i), mut out)
	}
	op := unsafe { token.Token(int(c.aux())) }
	return out.emit_assign_stmt_by_ids(op, lhs_ids, rhs_ids, c.pos())
}

fn (mut t Transformer) transform_assign_lhs_cursor_to_flat(lhs ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	if lhs.kind() == .expr_ident {
		return out.emit_ident_by_name(lhs.name(), lhs.pos())
	}
	return t.transform_expr_cursor_to_flat(lhs, mut out)
}

fn (mut t Transformer) remember_decl_assign_cursor_type(c ast.Cursor) {
	if c.kind() != .stmt_assign || unsafe { token.Token(int(c.aux())) } != .decl_assign
		|| c.extra_int() != 1 || c.edge_count() != 2 {
		return
	}
	lhs := c.edge(0)
	rhs := c.edge(1)
	lhs_name := t.get_var_name_cursor(lhs)
	if lhs_name == '' || lhs_name == '_' {
		return
	}
	if decl_type := t.decl_assign_storage_type_cursor(lhs, rhs) {
		t.remember_decl_assign_lhs_type_cursor(lhs, decl_type)
	}
	if rhs_type := t.fn_pointer_call_return_type_cursor(rhs) {
		t.register_temp_var(lhs_name, rhs_type)
	} else if rhs_type := t.smartcast_type_for_expr_cursor(rhs) {
		t.register_local_var_type(lhs_name, rhs_type)
	} else if rhs_type := t.rune_arithmetic_expr_type_cursor(rhs) {
		t.register_local_var_type(lhs_name, rhs_type)
	} else if rhs.kind() == .expr_array_init {
		if rhs_type := t.get_array_init_expr_type_cursor(rhs) {
			t.register_local_var_type(lhs_name, rhs_type)
		}
	} else if rhs.kind() == .expr_map_init {
		typ_cursor := rhs.edge(0)
		if typ_cursor.is_valid() && typ_cursor.kind() != .expr_empty {
			if rhs_type := t.type_from_param_type_expr(typ_cursor.type_expr(), []) {
				t.register_local_var_type(lhs_name, rhs_type)
			}
		}
	} else if rhs.kind() in [.expr_call, .expr_call_or_cast, .expr_init, .expr_ident, .expr_selector] {
		if rhs_type := t.get_expr_type_cursor(rhs) {
			t.register_local_var_type(lhs_name, rhs_type)
		}
	}
	if bindings := t.generic_bindings_from_generic_call_expr_cursor(rhs) {
		t.local_receiver_generic_bindings[lhs_name] = bindings.clone()
	}
}

fn (mut t Transformer) remember_decl_assign_lhs_type_cursor(lhs ast.Cursor, typ types.Type) {
	match lhs.kind() {
		.expr_ident {
			if lhs.name() == '_' {
				return
			}
			t.remember_local_decl_type(lhs.name(), typ)
			t.register_local_var_type(lhs.name(), typ)
			if lhs.pos().id != 0 {
				t.register_synth_type(lhs.pos(), typ)
			}
		}
		.expr_modifier {
			t.remember_decl_assign_lhs_type_cursor(lhs.edge(0), typ)
		}
		else {}
	}
}

fn (mut t Transformer) transform_map_index_assign_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if t.is_eval_backend() || c.kind() != .stmt_assign {
		return none
	}
	lhs_len := c.extra_int()
	rhs_len := c.edge_count() - lhs_len
	op := unsafe { token.Token(int(c.aux())) }
	if op !in [.assign, .decl_assign] || lhs_len != 1 || rhs_len != 1 {
		return none
	}
	lhs := c.edge(0)
	if lhs.kind() != .expr_index {
		return none
	}
	rhs := c.edge(lhs_len)
	if t.cursor_subtree_has_or_expr(rhs) || rhs.kind() in [.expr_if, .expr_if_guard] {
		return none
	}
	if rhs.kind() == .expr_comptime && rhs.edge(0).kind() == .expr_if {
		return none
	}
	map_expr := lhs.edge(0)
	key_expr := lhs.edge(1)
	map_expr_typ := t.map_index_lhs_type_cursor(map_expr) or { return none }
	map_type := t.unwrap_map_type(map_expr_typ) or { return none }
	map_ptr_id := t.map_expr_to_runtime_ptr_cursor(map_expr, map_expr_typ, mut out) or {
		return none
	}

	key_ident := t.typed_temp_ident(t.gen_temp_name(), map_type.key_type)
	key_lhs_id := out.emit_ident_by_name(key_ident.name, key_ident.pos)
	key_value_id := t.transform_map_key_value_cursor_to_flat(key_expr, map_type.key_type, mut out)
	key_assign_id := out.emit_assign_stmt_by_ids(.decl_assign, [key_lhs_id], [
		key_value_id,
	], key_ident.pos)
	key_ref_id := out.emit_ident_by_name(key_ident.name, key_ident.pos)
	key_ptr_id := out.emit_prefix_expr_by_id(.amp, key_ref_id, token.Pos{})

	val_ident := t.typed_temp_ident(t.gen_temp_name(), map_type.value_type)
	val_lhs_id := out.emit_ident_by_name(val_ident.name, val_ident.pos)
	val_value_id := t.transform_map_assign_value_cursor_to_flat(rhs, map_type.value_type, mut out)
	val_assign_id := out.emit_assign_stmt_by_ids(.decl_assign, [val_lhs_id], [
		val_value_id,
	], val_ident.pos)
	val_ref_id := out.emit_ident_by_name(val_ident.name, val_ident.pos)
	val_ptr_id := out.emit_prefix_expr_by_id(.amp, val_ref_id, token.Pos{})

	call_lhs_id := out.emit_ident_by_name('map__set', token.Pos{})
	call_id := out.emit_call_expr_by_ids(call_lhs_id, [
		map_ptr_id,
		emit_voidptr_cast_id(key_ptr_id, mut out),
		emit_voidptr_cast_id(val_ptr_id, mut out),
	], c.pos())
	call_stmt_id := out.emit_expr_stmt_by_id(call_id)
	return out.emit_block_stmt_by_ids([key_assign_id, val_assign_id, call_stmt_id])
}

fn emit_voidptr_cast_id(expr_id ast.FlatNodeId, mut out ast.FlatBuilder) ast.FlatNodeId {
	typ_id := out.emit_ident_by_name('voidptr', token.Pos{})
	return out.emit_cast_expr_by_ids(typ_id, expr_id, token.Pos{})
}

fn (mut t Transformer) transform_map_key_value_cursor_to_flat(c ast.Cursor, key_type types.Type, mut out ast.FlatBuilder) ast.FlatNodeId {
	enum_type := t.type_to_c_name(t.unwrap_alias_and_pointer_type(key_type))
	if enum_type != '' {
		if enum_id := t.enum_shorthand_cursor_to_flat(c, enum_type, mut out) {
			return enum_id
		}
	}
	return t.transform_expr_cursor_to_flat(c, mut out)
}

fn (mut t Transformer) transform_map_assign_value_cursor_to_flat(c ast.Cursor, value_type types.Type, mut out ast.FlatBuilder) ast.FlatNodeId {
	base := t.unwrap_alias_and_pointer_type(value_type)
	if base is types.Enum {
		enum_type := t.type_to_c_name(base)
		if enum_id := t.enum_shorthand_cursor_to_flat(c, enum_type, mut out) {
			return enum_id
		}
	}
	if base is types.SumType {
		sumtype_name := t.type_to_c_name(base)
		if wrapped_id := t.wrap_sumtype_value_cursor_to_flat(c, sumtype_name, mut out) {
			return wrapped_id
		}
	}
	return t.transform_expr_cursor_to_flat(c, mut out)
}

fn (t &Transformer) assign_stmt_cursor_needs_legacy_expand(c ast.Cursor) bool {
	if t.is_native_be {
		return true
	}
	lhs_len := c.extra_int()
	rhs_len := c.edge_count() - lhs_len
	if lhs_len != 1 || rhs_len != 1 {
		return true
	}
	lhs := c.edge(0)
	if t.assign_lhs_cursor_needs_legacy_rewrite(lhs) {
		return true
	}
	rhs := c.edge(lhs_len)
	if t.cursor_subtree_has_or_expr(rhs) {
		return true
	}
	if rhs.kind() == .expr_comptime {
		return rhs.edge(0).kind() == .expr_if
	}
	return rhs.kind() in [.expr_if, .expr_if_guard]
}

fn (mut t Transformer) try_expand_if_expr_assign_cursor_to_flat(c ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if c.kind() != .stmt_assign || t.is_native_be {
		return false
	}
	lhs_len := c.extra_int()
	rhs_len := c.edge_count() - lhs_len
	op := unsafe { token.Token(int(c.aux())) }
	if op != .assign || lhs_len != 1 || rhs_len != 1 {
		return false
	}
	lhs := c.edge(0)
	rhs := c.edge(lhs_len)
	if t.assign_lhs_cursor_needs_legacy_rewrite(lhs) {
		return false
	}
	if rhs.kind() != .expr_if || expr_cursor_is_empty(rhs.edge(1)) {
		return false
	}
	if t.cursor_subtree_has_or_expr(rhs) || !t.assign_if_expr_cursor_can_lower(rhs) {
		return false
	}
	if_id := t.emit_assign_if_expr_cursor_to_flat(lhs, rhs, mut out)
	stmt_id := out.emit_expr_stmt_by_id(if_id)
	t.append_transformed_stmt_id_to_flat(mut ids, stmt_id, mut out)
	return true
}

fn (t &Transformer) assign_lhs_cursor_needs_legacy_rewrite(lhs ast.Cursor) bool {
	match lhs.kind() {
		.expr_index {
			return t.index_cursor_targets_map_value(lhs)
		}
		.expr_generic_arg_or_index {
			if lhs_type := t.get_expr_type_cursor(lhs.edge(0)) {
				if t.is_callable_type(lhs_type) {
					return false
				}
			}
			return t.generic_index_cursor_targets_map_value(lhs)
		}
		.expr_selector {
			return t.selector_cursor_targets_map_index(lhs)
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) assign_if_expr_cursor_can_lower(c ast.Cursor) bool {
	if !c.is_valid() || c.kind() != .expr_if {
		return false
	}
	cond := c.edge(0)
	if !expr_cursor_is_empty(cond) && !t.if_expr_cursor_condition_can_transform_plain(c) {
		return false
	}
	body := ast.CursorList{
		flat:      c.flat
		parent_id: c.id
		offset:    2
	}
	if !t.assign_if_body_cursor_can_lower(body) {
		return false
	}
	else_expr := c.edge(1)
	if expr_cursor_is_empty(else_expr) {
		return false
	}
	if else_expr.kind() == .expr_if {
		else_cond := else_expr.edge(0)
		else_body := ast.CursorList{
			flat:      else_expr.flat
			parent_id: else_expr.id
			offset:    2
		}
		if expr_cursor_is_empty(else_cond) {
			return t.assign_if_body_cursor_can_lower(else_body)
		}
		return t.assign_if_expr_cursor_can_lower(else_expr)
	}
	return t.assign_if_branch_expr_cursor_can_lower(else_expr)
}

fn (t &Transformer) assign_if_body_cursor_can_lower(body ast.CursorList) bool {
	if body.len() == 0 {
		return false
	}
	last := body.at(body.len() - 1)
	if last.kind() != .stmt_expr {
		return false
	}
	return t.assign_if_branch_expr_cursor_can_lower(last.edge(0))
}

fn (t &Transformer) assign_if_branch_expr_cursor_can_lower(expr ast.Cursor) bool {
	if !expr.is_valid() {
		return false
	}
	if expr.kind() == .expr_if {
		return t.assign_if_expr_cursor_can_lower(expr)
	}
	if expr.kind() in [.expr_call, .expr_call_or_cast] {
		typ := t.get_expr_type_cursor(expr) or { return false }
		type_name := typ.name()
		return type_name != '' && type_name != 'void' && type_name != 'Void'
	}
	return true
}

fn (mut t Transformer) emit_assign_if_expr_cursor_to_flat(lhs ast.Cursor, c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	body := ast.CursorList{
		flat:      c.flat
		parent_id: c.id
		offset:    2
	}
	body_ids := t.emit_assign_if_body_cursor_to_flat(lhs, body, mut out)
	else_id := t.emit_assign_if_else_cursor_to_flat(lhs, c.edge(1), mut out)
	cond_id := if expr_cursor_is_empty(c.edge(0)) {
		out.emit_expr(ast.empty_expr)
	} else {
		t.transform_expr_cursor_to_flat(c.edge(0), mut out)
	}
	return out.emit_if_expr_by_ids(cond_id, else_id, body_ids, token.Pos{})
}

fn (mut t Transformer) emit_assign_if_else_cursor_to_flat(lhs ast.Cursor, c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	if c.kind() == .expr_if {
		if expr_cursor_is_empty(c.edge(0)) {
			body := ast.CursorList{
				flat:      c.flat
				parent_id: c.id
				offset:    2
			}
			body_ids := t.emit_assign_if_body_cursor_to_flat(lhs, body, mut out)
			return out.emit_if_expr_by_ids(out.emit_expr(ast.empty_expr),
				out.emit_expr(ast.empty_expr), body_ids, token.Pos{})
		}
		return t.emit_assign_if_expr_cursor_to_flat(lhs, c, mut out)
	}
	mut body_ids := []ast.FlatNodeId{}
	t.append_assign_if_branch_expr_cursor_to_flat(lhs, c, mut body_ids, mut out)
	return out.emit_if_expr_by_ids(out.emit_expr(ast.empty_expr), out.emit_expr(ast.empty_expr),
		body_ids, token.Pos{})
}

fn (mut t Transformer) emit_assign_if_body_cursor_to_flat(lhs ast.Cursor, body ast.CursorList, mut out ast.FlatBuilder) []ast.FlatNodeId {
	mut ids := []ast.FlatNodeId{cap: body.len()}
	if body.len() == 0 {
		return ids
	}
	for i in 0 .. body.len() - 1 {
		t.transform_stmt_list_item_cursor_to_flat(body.at(i), mut ids, mut out)
	}
	last := body.at(body.len() - 1)
	t.append_assign_if_branch_expr_cursor_to_flat(lhs, last.edge(0), mut ids, mut out)
	return ids
}

fn (mut t Transformer) append_assign_if_branch_expr_cursor_to_flat(lhs ast.Cursor, expr ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	if expr.kind() == .expr_if {
		if_id := t.emit_assign_if_expr_cursor_to_flat(lhs, expr, mut out)
		stmt_id := out.emit_expr_stmt_by_id(if_id)
		t.append_transformed_stmt_id_to_flat(mut ids, stmt_id, mut out)
		return
	}
	lhs_id := t.transform_expr_cursor_to_flat(lhs, mut out)
	rhs_id := t.transform_expr_cursor_to_flat(expr, mut out)
	assign_id := out.emit_assign_stmt_by_ids(.assign, [lhs_id], [rhs_id], token.Pos{})
	t.append_transformed_stmt_id_to_flat(mut ids, assign_id, mut out)
}

fn assign_stmt_from_cursor(c ast.Cursor) ast.AssignStmt {
	lhs_len := c.extra_int()
	mut lhs := []ast.Expr{cap: lhs_len}
	for i in 0 .. lhs_len {
		lhs << c.edge(i).expr()
	}
	mut rhs := []ast.Expr{cap: c.edge_count() - lhs_len}
	for i in lhs_len .. c.edge_count() {
		rhs << c.edge(i).expr()
	}
	return ast.AssignStmt{
		op:  unsafe { token.Token(int(c.aux())) }
		lhs: lhs
		rhs: rhs
		pos: c.pos()
	}
}

fn for_in_stmt_from_cursor(c ast.Cursor) ast.ForInStmt {
	return ast.ForInStmt{
		key:   c.edge(0).expr()
		value: c.edge(1).expr()
		expr:  c.edge(2).expr()
	}
}

fn for_in_cursor_var_name(c ast.Cursor) string {
	if !c.is_valid() {
		return ''
	}
	if c.kind() == .expr_ident {
		return c.name()
	}
	if c.kind() == .expr_modifier {
		inner := c.edge(0)
		if inner.is_valid() && inner.kind() == .expr_ident {
			return inner.name()
		}
	}
	return ''
}

fn for_in_cursor_binding(c ast.Cursor, fallback string) (string, ast.Expr, bool) {
	if !c.is_valid() {
		return fallback, ast.Expr(ast.Ident{
			name: fallback
		}), false
	}
	if c.kind() == .expr_ident {
		return c.name(), ast.Expr(c.ident()), false
	}
	if c.kind() == .expr_modifier {
		inner := c.edge(0)
		if inner.is_valid() && inner.kind() == .expr_ident {
			return inner.name(), ast.Expr(inner.ident()), unsafe { token.Token(int(c.aux())) } == .key_mut
		}
	}
	return fallback, ast.Expr(ast.Ident{
		name: fallback
	}), false
}

fn for_in_cursor_index_name(c ast.Cursor, value_name string) string {
	if c.is_valid() {
		if c.kind() == .expr_ident && c.name() != '_' {
			return c.name()
		}
		if c.kind() == .expr_modifier {
			inner := c.edge(0)
			if inner.is_valid() && inner.kind() == .expr_ident && inner.name() != '_' {
				return inner.name()
			}
		}
	}
	return '_idx_${value_name}'
}

fn for_stmt_from_cursor(c ast.Cursor) ast.ForStmt {
	return ast.ForStmt{
		init:  c.edge(0).stmt()
		cond:  c.edge(1).expr()
		post:  c.edge(2).stmt()
		stmts: c.for_body_list().stmts()
	}
}

fn expr_stmt_from_cursor(c ast.Cursor) ast.ExprStmt {
	return ast.ExprStmt{
		expr: c.edge(0).expr()
	}
}

fn return_stmt_from_cursor(c ast.Cursor) ast.ReturnStmt {
	mut exprs := []ast.Expr{cap: c.edge_count()}
	for i in 0 .. c.edge_count() {
		exprs << c.edge(i).expr()
	}
	return ast.ReturnStmt{
		exprs: exprs
	}
}

fn (mut t Transformer) transform_expr_stmt_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	expr := c.edge(0)
	is_direct_if := expr.kind() == .expr_if
	saved_skip := t.skip_if_value_lowering
	if is_direct_if {
		t.skip_if_value_lowering = true
	}
	expr_id := t.transform_expr_cursor_to_flat(expr, mut out)
	t.skip_if_value_lowering = saved_skip
	return out.emit_expr_stmt_by_id(expr_id)
}

fn (t &Transformer) expr_stmt_cursor_needs_legacy_expand(c ast.Cursor) bool {
	expr := c.edge(0)
	if !expr.is_valid() {
		return true
	}
	if t.cursor_subtree_has_or_expr(expr) {
		return true
	}
	if expr.kind() == .expr_postfix {
		return t.postfix_expr_stmt_cursor_needs_legacy_expand(expr)
	}
	return expr.kind() in [.expr_if_guard, .expr_lock]
}

fn (t &Transformer) postfix_expr_stmt_cursor_needs_legacy_expand(expr ast.Cursor) bool {
	op := unsafe { token.Token(int(expr.aux())) }
	if op !in [.inc, .dec] {
		return false
	}
	target := expr.edge(0)
	match target.kind() {
		.expr_index {
			return t.index_cursor_targets_map_value(target)
		}
		.expr_generic_arg_or_index {
			if lhs_type := t.get_expr_type_cursor(target.edge(0)) {
				if t.is_callable_type(lhs_type) {
					return false
				}
			}
			return t.generic_index_cursor_targets_map_value(target)
		}
		.expr_selector {
			if t.is_native_be {
				return true
			}
			return t.selector_cursor_targets_map_index(target)
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) index_cursor_targets_map_value(index ast.Cursor) bool {
	if !index.is_valid() || index.kind() != .expr_index {
		return false
	}
	return t.cursor_expr_is_map_container(index.edge(0))
}

fn (t &Transformer) generic_index_cursor_targets_map_value(index ast.Cursor) bool {
	if !index.is_valid() || index.kind() != .expr_generic_arg_or_index {
		return false
	}
	return t.cursor_expr_is_map_container(index.edge(0))
}

fn (t &Transformer) selector_cursor_targets_map_index(sel ast.Cursor) bool {
	if !sel.is_valid() || sel.kind() != .expr_selector {
		return false
	}
	lhs := sel.edge(0)
	match lhs.kind() {
		.expr_index {
			return t.index_cursor_targets_map_value(lhs)
		}
		.expr_generic_arg_or_index {
			return t.generic_index_cursor_targets_map_value(lhs)
		}
		.expr_selector {
			return t.selector_cursor_targets_map_index(lhs)
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) cursor_expr_is_map_container(expr ast.Cursor) bool {
	typ := t.get_expr_type_cursor(expr) or { return false }
	if _ := t.unwrap_map_type(typ) {
		return true
	}
	return false
}

fn (mut t Transformer) transform_return_stmt_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	mut expr_ids := []ast.FlatNodeId{cap: c.edge_count()}
	for i in 0 .. c.edge_count() {
		expr_ids << t.transform_return_expr_cursor_to_flat(c.edge(i), mut out)
	}
	return out.emit_return_stmt_by_ids(expr_ids)
}

fn (mut t Transformer) transform_return_expr_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	if t.cur_fn_ret_type_name != '' && return_expr_cursor_is_enum_shorthand(c) {
		member_name := selector_rhs_name_cursor(c)
		if typ := t.lookup_type(t.cur_fn_ret_type_name) {
			t.register_synth_type(c.pos(), typ)
			if typ is types.Enum {
				return out.emit_ident_by_name(t.enum_member_ident_for_lookup(t.cur_fn_ret_type_name,
					typ, member_name), c.pos())
			}
		}
		return out.emit_ident_by_name(enum_member_ident(t.cur_fn_ret_type_name, member_name),
			c.pos())
	}
	return t.transform_expr_cursor_to_flat(c, mut out)
}

fn (t &Transformer) return_stmt_cursor_needs_legacy_expand(c ast.Cursor) bool {
	if t.return_stmt_needs_return_expr_rewrite() {
		return true
	}
	if c.edge_count() == 1 {
		expr := c.edge(0)
		if expr.kind() == .expr_match {
			return true
		}
		if expr.kind() == .expr_if && !expr_cursor_is_empty(expr.edge(1)) {
			return true
		}
	}
	for i in 0 .. c.edge_count() {
		if t.cursor_subtree_has_or_expr(c.edge(i)) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) try_expand_return_if_expr_cursor_to_flat(c ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if c.kind() != .stmt_return || c.edge_count() != 1 {
		return false
	}
	if t.return_stmt_needs_return_expr_rewrite() {
		return false
	}
	if_expr := c.edge(0)
	if if_expr.kind() != .expr_if || expr_cursor_is_empty(if_expr.edge(1)) {
		return false
	}
	if t.cursor_subtree_has_or_expr(if_expr) || !t.return_if_expr_cursor_can_lower(if_expr) {
		return false
	}
	if_id := t.emit_return_if_expr_cursor_to_flat(if_expr, mut out)
	stmt_id := out.emit_expr_stmt_by_id(if_id)
	t.append_transformed_stmt_id_to_flat(mut ids, stmt_id, mut out)
	return true
}

fn (t &Transformer) return_if_expr_cursor_can_lower(c ast.Cursor) bool {
	if !c.is_valid() || c.kind() != .expr_if {
		return false
	}
	cond := c.edge(0)
	if !expr_cursor_is_empty(cond) && !t.if_expr_cursor_condition_can_transform_plain(c) {
		return false
	}
	body := ast.CursorList{
		flat:      c.flat
		parent_id: c.id
		offset:    2
	}
	if !t.return_if_body_cursor_can_lower(body) {
		return false
	}
	else_expr := c.edge(1)
	if expr_cursor_is_empty(else_expr) {
		return false
	}
	if else_expr.kind() == .expr_if {
		else_cond := else_expr.edge(0)
		else_body := ast.CursorList{
			flat:      else_expr.flat
			parent_id: else_expr.id
			offset:    2
		}
		if expr_cursor_is_empty(else_cond) {
			return t.return_if_body_cursor_can_lower(else_body)
		}
		return t.return_if_expr_cursor_can_lower(else_expr)
	}
	return t.return_if_branch_expr_cursor_can_lower(else_expr)
}

fn (t &Transformer) return_if_body_cursor_can_lower(body ast.CursorList) bool {
	if body.len() == 0 {
		return false
	}
	last := body.at(body.len() - 1)
	if last.kind() != .stmt_expr {
		return false
	}
	return t.return_if_branch_expr_cursor_can_lower(last.edge(0))
}

fn (t &Transformer) return_if_branch_expr_cursor_can_lower(expr ast.Cursor) bool {
	if !expr.is_valid() || expr.kind() in [.expr_match, .expr_unsafe] {
		return false
	}
	if expr.kind() == .expr_if {
		return t.return_if_expr_cursor_can_lower(expr)
	}
	if expr.kind() in [.expr_call, .expr_call_or_cast] {
		typ := t.get_expr_type_cursor(expr) or { return false }
		type_name := typ.name()
		return type_name != '' && type_name != 'void' && type_name != 'Void'
	}
	return true
}

fn (mut t Transformer) emit_return_if_expr_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	body := ast.CursorList{
		flat:      c.flat
		parent_id: c.id
		offset:    2
	}
	body_ids := t.emit_return_if_body_cursor_to_flat(body, mut out)
	else_id := t.emit_return_if_else_cursor_to_flat(c.edge(1), mut out)
	cond_id := if expr_cursor_is_empty(c.edge(0)) {
		out.emit_expr(ast.empty_expr)
	} else {
		t.transform_expr_cursor_to_flat(c.edge(0), mut out)
	}
	return out.emit_if_expr_by_ids(cond_id, else_id, body_ids, token.Pos{})
}

fn (mut t Transformer) emit_return_if_else_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	if c.kind() == .expr_if {
		if expr_cursor_is_empty(c.edge(0)) {
			body := ast.CursorList{
				flat:      c.flat
				parent_id: c.id
				offset:    2
			}
			body_ids := t.emit_return_if_body_cursor_to_flat(body, mut out)
			return out.emit_if_expr_by_ids(out.emit_expr(ast.empty_expr),
				out.emit_expr(ast.empty_expr), body_ids, token.Pos{})
		}
		return t.emit_return_if_expr_cursor_to_flat(c, mut out)
	}
	mut body_ids := []ast.FlatNodeId{}
	t.append_return_if_branch_expr_cursor_to_flat(c, mut body_ids, mut out)
	return out.emit_if_expr_by_ids(out.emit_expr(ast.empty_expr), out.emit_expr(ast.empty_expr),
		body_ids, token.Pos{})
}

fn (mut t Transformer) emit_return_if_body_cursor_to_flat(body ast.CursorList, mut out ast.FlatBuilder) []ast.FlatNodeId {
	mut ids := []ast.FlatNodeId{cap: body.len()}
	if body.len() == 0 {
		return ids
	}
	for i in 0 .. body.len() - 1 {
		t.transform_stmt_list_item_cursor_to_flat(body.at(i), mut ids, mut out)
	}
	last := body.at(body.len() - 1)
	t.append_return_if_branch_expr_cursor_to_flat(last.edge(0), mut ids, mut out)
	return ids
}

fn (mut t Transformer) append_return_if_branch_expr_cursor_to_flat(expr ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	if expr.kind() == .expr_if {
		if_id := t.emit_return_if_expr_cursor_to_flat(expr, mut out)
		stmt_id := out.emit_expr_stmt_by_id(if_id)
		t.append_transformed_stmt_id_to_flat(mut ids, stmt_id, mut out)
		return
	}
	expr_id := t.transform_return_expr_cursor_to_flat(expr, mut out)
	return_id := out.emit_return_stmt_by_ids([expr_id])
	t.append_transformed_stmt_id_to_flat(mut ids, return_id, mut out)
}

fn (t &Transformer) return_stmt_needs_return_expr_rewrite() bool {
	if t.is_native_be || t.cur_fn_returns_option || t.cur_fn_returns_result {
		return true
	}
	if t.cur_fn_ret_type_name == '' {
		return false
	}
	if t.is_sum_type(t.cur_fn_ret_type_name) {
		return true
	}
	return false
}

fn return_expr_cursor_is_enum_shorthand(expr ast.Cursor) bool {
	if !expr.is_valid() || expr.kind() != .expr_selector {
		return false
	}
	lhs := expr.edge(0)
	return !lhs.is_valid() || lhs.kind() == .expr_empty
}

fn (t &Transformer) cursor_subtree_has_or_expr(c ast.Cursor) bool {
	if !c.is_valid() {
		return false
	}
	if c.kind() == .expr_or {
		return true
	}
	if c.kind() == .expr_postfix {
		op := unsafe { token.Token(int(c.aux())) }
		if op in [.not, .question] {
			return true
		}
	}
	for i in 0 .. c.edge_count() {
		if t.cursor_subtree_has_or_expr(c.edge(i)) {
			return true
		}
	}
	return false
}

fn cursor_subtree_has_kind(c ast.Cursor, kind ast.FlatNodeKind) bool {
	if !c.is_valid() {
		return false
	}
	if c.kind() == kind {
		return true
	}
	for i in 0 .. c.edge_count() {
		if cursor_subtree_has_kind(c.edge(i), kind) {
			return true
		}
	}
	return false
}

fn cursor_subtree_has_ident_named(c ast.Cursor, name string) bool {
	if !c.is_valid() {
		return false
	}
	if c.kind() == .expr_ident && c.name() == name {
		return true
	}
	for i in 0 .. c.edge_count() {
		if cursor_subtree_has_ident_named(c.edge(i), name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) if_expr_cursor_condition_can_transform_plain(c ast.Cursor) bool {
	cond := c.edge(0)
	if !cond.is_valid() || cursor_subtree_has_kind(cond, .expr_if_guard) {
		return false
	}
	for term in t.flatten_and_terms_unwrapped_cursor(cond) {
		if term.kind() == .expr_infix {
			if _ := t.smartcast_context_from_condition_term_cursor(term) {
				return false
			}
		}
	}
	return true
}

fn (t &Transformer) if_expr_cursor_can_transform_plain(c ast.Cursor) bool {
	if !t.skip_if_value_lowering {
		return false
	}
	return t.if_expr_cursor_condition_can_transform_plain(c)
}

fn (mut t Transformer) transform_plain_if_expr_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	outer_smartcast_stack := t.smartcast_stack.clone()
	outer_smartcast_counts := t.smartcast_expr_counts.clone()

	saved_pending := t.pending_stmts
	t.pending_stmts = []ast.Stmt{}
	body := ast.CursorList{
		flat:      c.flat
		parent_id: c.id
		offset:    2
	}
	body_ids := t.transform_cursor_stmts_to_flat_direct(body, [], mut out)
	inner_pending_then := t.pending_stmts
	t.pending_stmts = saved_pending
	for ip in inner_pending_then {
		t.pending_stmts << ip
	}
	t.smartcast_stack = outer_smartcast_stack.clone()
	t.smartcast_expr_counts = outer_smartcast_counts.clone()

	saved_pending_else := t.pending_stmts
	t.pending_stmts = []ast.Stmt{}
	else_id := t.transform_expr_cursor_to_flat(c.edge(1), mut out)
	inner_pending_else := t.pending_stmts
	t.pending_stmts = saved_pending_else
	for ip in inner_pending_else {
		t.pending_stmts << ip
	}
	t.smartcast_stack = outer_smartcast_stack.clone()
	t.smartcast_expr_counts = outer_smartcast_counts.clone()

	cond_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
	t.smartcast_stack = outer_smartcast_stack.clone()
	t.smartcast_expr_counts = outer_smartcast_counts.clone()
	return out.emit_if_expr_by_ids(cond_id, else_id, body_ids, c.pos())
}

fn (mut t Transformer) transform_stmt_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	if !c.is_valid() {
		return out.emit_stmt(ast.empty_stmt)
	}
	match c.kind() {
		.stmt_import, .stmt_module, .stmt_directive, .stmt_empty, .stmt_enum_decl,
		.stmt_interface_decl, .stmt_type_decl, .stmt_asm, .stmt_flow_control, .stmt_attributes {
			return out.copy_subtree_from(c.flat, c.id)
		}
		.stmt_const_decl {
			return t.transform_const_decl_cursor_to_flat(c, mut out)
		}
		.stmt_block {
			return t.transform_block_stmt_cursor_to_flat(c, mut out)
		}
		.stmt_defer {
			return t.transform_defer_stmt_cursor_to_flat(c, mut out)
		}
		.stmt_label {
			return t.transform_label_stmt_cursor_to_flat(c, mut out)
		}
		.stmt_global_decl {
			return t.transform_global_decl_cursor_to_flat(c, mut out)
		}
		.stmt_struct_decl {
			return out.emit_stmt(t.transform_struct_decl(c.struct_decl()))
		}
		.stmt_assert {
			expr_id := t.transform_expr_cursor_to_flat(c.edge(0), mut out)
			return out.emit_assert_stmt_by_id(expr_id)
		}
		.stmt_assign {
			if id := t.transform_map_index_assign_cursor_to_flat(c, mut out) {
				return id
			}
			if t.assign_stmt_cursor_needs_legacy_expand(c) {
				t.count_flat_fallback('nested_assign')
				return t.transform_stmt_to_flat(assign_stmt_from_cursor(c), mut out)
			}
			return t.transform_assign_stmt_cursor_to_flat(c, mut out)
		}
		.stmt_comptime {
			return t.transform_comptime_stmt_cursor_to_flat(c, mut out)
		}
		.stmt_expr {
			if id := t.transform_comptime_if_stmt_cursor_to_flat(c, mut out) {
				return id
			}
			if id := t.transform_flag_enum_set_clear_cursor_to_flat(c, mut out) {
				return id
			}
			if t.expr_stmt_cursor_needs_legacy_expand(c) {
				t.count_flat_fallback('nested_expr')
				return t.transform_stmt_to_flat(expr_stmt_from_cursor(c), mut out)
			}
			return t.transform_expr_stmt_cursor_to_flat(c, mut out)
		}
		.stmt_return {
			return t.transform_return_stmt_cursor_to_flat(c, mut out)
		}
		.stmt_for_in {
			t.count_flat_fallback('nested_for_in')
			return t.transform_stmt_to_flat(for_in_stmt_from_cursor(c), mut out)
		}
		.stmt_fn_decl {
			if flat_body_has_defer(c.list_at(3)) {
				t.count_flat_fallback('nested_fn_decl_defer')
				decl := fn_decl_signature_with_body_cursor(c.fn_decl_signature(), c)
				return t.transform_stmt_to_flat(decl, mut out)
			}
			return t.transform_fn_decl_streaming_to_flat(c, mut out)
		}
		.stmt_for {
			init_c := c.edge(0)
			if init_c.is_valid() && init_c.kind() == .stmt_for_in {
				t.count_flat_fallback('nested_for')
				return t.transform_stmt_to_flat(for_stmt_from_cursor(c), mut out)
			}
			return t.transform_for_stmt_streaming_to_flat(c, mut out)
		}
		else {
			panic('unexpected flat statement kind: ${c.kind()}')
		}
	}
}

// emit_fn_decl_flat assembles a stmt_fn_decl flat node from the immutable
// header of `decl` plus the already-transformed (and flat-emitted) attribute
// list and body stmt ids. Shared by the legacy `ast.FnDecl` arm of
// transform_stmt_to_flat and the cursor-native streaming path so both encode
// the FnDecl identically.
fn (mut t Transformer) emit_fn_decl_flat(decl ast.FnDecl, attrs []ast.Attribute, stmt_ids []ast.FlatNodeId, mut out ast.FlatBuilder) ast.FlatNodeId {
	receiver := if decl.is_method {
		decl.receiver
	} else {
		ast.Parameter{
			typ: ast.empty_expr
		}
	}
	receiver_id := out.emit_parameter(receiver)
	typ_id := out.emit_type(ast.Type(decl.typ))
	attrs_id := out.emit_attribute_list(attrs)
	stmts_list_id := out.emit_aux_list_from_ids(stmt_ids)
	return out.emit_fn_decl_by_ids(decl.name, decl.is_public, decl.is_method, decl.is_static,
		decl.language, decl.pos, receiver_id, typ_id, attrs_id, stmts_list_id)
}

// flat_subtree_has_defer reports whether the cursor or any of its descendants
// is a `stmt_defer` node. Used to detect function-body defers without decoding
// the body. Conservative by construction: it also flags defers nested inside
// closures (which lower_defer_stmts would not lower), but over-detection only
// costs a fall-back to the whole-body decode path, never correctness.
fn flat_subtree_has_defer(c ast.Cursor) bool {
	if !c.is_valid() {
		return false
	}
	if c.kind() == .stmt_defer {
		return true
	}
	for i in 0 .. c.edge_count() {
		if flat_subtree_has_defer(c.edge(i)) {
			return true
		}
	}
	return false
}

// flat_body_has_defer reports whether any statement in a function body (a
// stmt CursorList) contains a defer anywhere in its subtree.
fn flat_body_has_defer(body ast.CursorList) bool {
	for i in 0 .. body.len() {
		if flat_subtree_has_defer(body.at(i)) {
			return true
		}
	}
	return false
}

// transform_fn_decl_streaming_to_flat is the cursor-native FnDecl path: it reads
// the signature from the cursor (c.fn_decl_signature, body-less — no whole-decl
// decode), runs the shared transform prologue (enter_fn_body_transform), streams
// the body straight from the cursor's stmt list (c.list_at(3)) via the cursor
// body driver — so the body is decoded one statement at a time instead of the
// whole function at once — then runs the shared epilogue and emits the FnDecl.
// The caller guarantees the body has no defers (defer lowering needs the whole
// body, so defer functions take the legacy whole-decl path).
fn (mut t Transformer) transform_fn_decl_streaming_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	lowered := t.fn_decl_with_implicit_veb_context_param(c.fn_decl_signature())
	mut ctx := t.enter_fn_body_transform(lowered) or {
		// Uninstantiated-generic skip / comptime-elided: empty body.
		return t.emit_fn_decl_flat(lowered, lowered.attributes, []ast.FlatNodeId{}, mut out)
	}
	body_ids := t.transform_cursor_stmts_to_flat_direct(c.list_at(3), [], mut out)
	t.restore_fn_body_transform_state(mut ctx)
	attrs := t.finish_fn_body_transform(lowered, mut ctx)
	return t.emit_fn_decl_flat(lowered, attrs, body_ids, mut out)
}

// transform_for_stmt_streaming_to_flat is the cursor-native counterpart to
// `transform_stmt_to_flat`'s ForStmt arm for plain (non-for-in) loops. It
// mirrors `transform_for_stmt`'s clean tail (for.v, the path taken when
// `stmt.init` is NOT a ForInStmt): open a loop scope, push smartcasts derived
// from `is` checks in the condition for the body, transform the body, pop the
// smartcasts, then transform init/cond/post and close the scope. The one
// difference is the body is STREAMED from the cursor
// (`transform_cursor_stmts_to_flat_direct`) instead of decoded to `[]ast.Stmt`
// and re-emitted — so a `for` loop's body (and any loops nested inside it,
// recursively) never materialises as legacy AST.
//
// The body is transformed FIRST, preserving the legacy transform order so the
// synthetic-position counter advances identically; the for-node's edges are
// still assembled as [init, cond, post, body...], so the structural signature
// and generated C are bit-identical to the decode path (arena order differs but
// is irrelevant — both consumers follow edges, not node-id order).
//
// Precondition (the dispatcher checks it): `c.edge(0)` is not a stmt_for_in.
// Migrated for-in families use their own cursor-native lowering helpers.
fn (mut t Transformer) transform_for_stmt_streaming_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	// Open a child scope for loop variables (transform_for_stmt:562).
	t.open_scope()
	cond := c.edge(1)
	// `for x is Type { ... }`: push smartcast contexts from `is` terms in the
	// (untransformed) condition for the body, exactly like transform_for_stmt.
	mut loop_smartcasts := []SmartcastContext{}
	for term in t.flatten_and_terms_unwrapped_cursor(cond) {
		if term.kind() == .expr_infix {
			if ctx := t.smartcast_context_from_condition_term_cursor(term) {
				loop_smartcasts << ctx
			}
		}
	}
	for ctx in loop_smartcasts {
		t.push_smartcast_full(ctx.expr, ctx.variant, ctx.variant_full, ctx.sumtype)
	}
	body_ids := t.transform_cursor_stmts_to_flat_direct(c.for_body_list(), [], mut out)
	for _ in loop_smartcasts {
		t.pop_smartcast()
	}
	// init/cond/post transform after the body (matches the struct-literal field
	// evaluation order in transform_for_stmt:701-706: init, cond, post).
	init_id := t.transform_stmt_cursor_to_flat(c.edge(0), mut out)
	cond_id := t.transform_expr_cursor_to_flat(cond, mut out)
	post_id := t.transform_stmt_cursor_to_flat(c.edge(2), mut out)
	t.close_scope()
	return out.emit_for_stmt_by_ids(init_id, cond_id, post_id, body_ids)
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
	return_sumtype_info := t.current_return_sumtype_wrap_info() or { ConcreteSumtypeWrapInfo{} }
	should_wrap_return_sumtype := return_sumtype_info.name != ''
	skip_return_sumtype_wrap := (t.cur_fn_returns_option || t.cur_fn_returns_result)
		&& t.return_expr_should_skip_sumtype_wrap(match_expr)
	old_wrap := t.sumtype_return_wrap
	if should_wrap_return_sumtype && !skip_return_sumtype_wrap {
		t.sumtype_return_wrap = return_sumtype_info.name
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
//   - Otherwise: build final AssignStmt via `transform_stmt_to_flat` (so map
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
	ids << t.transform_stmt_to_flat(final_assign, mut out)
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

fn (mut t Transformer) try_expand_range_for_in_cursor_to_flat(stmt ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	for_in_c := stmt.edge(0)
	if !for_in_c.is_valid() || for_in_c.kind() != .stmt_for_in {
		return false
	}
	range_c := for_in_c.edge(2)
	if !range_c.is_valid() || range_c.kind() != .expr_range {
		return false
	}
	mut value_name := for_in_cursor_var_name(for_in_c.edge(1))
	if value_name == '' {
		value_name = '_i'
	}

	t.open_scope()
	if int_obj := t.scope.lookup_parent('int', 0) {
		t.register_for_in_var_type(value_name, int_obj.typ())
	}
	body_ids := t.transform_cursor_stmts_to_flat_direct(stmt.for_body_list(), [], mut out)

	range_op := unsafe { token.Token(int(range_c.aux())) }
	cmp_op := if range_op == .ellipsis { token.Token.le } else { token.Token.lt }
	t.close_scope()

	start_id := t.transform_expr_cursor_to_flat(range_c.edge(0), mut out)
	end_id := t.transform_expr_cursor_to_flat(range_c.edge(1), mut out)
	init_lhs_id := out.emit_ident_by_name(value_name, token.Pos{})
	init_id := out.emit_assign_stmt_by_ids(.decl_assign, [init_lhs_id], [start_id], token.Pos{})
	cond_lhs_id := out.emit_ident_by_name(value_name, token.Pos{})
	cond_id := out.emit_infix_expr_by_ids(cmp_op, cond_lhs_id, end_id, token.Pos{})
	post_lhs_id := out.emit_ident_by_name(value_name, token.Pos{})
	one_id := out.emit_basic_literal_by_value(.number, '1', token.Pos{})
	post_id := out.emit_assign_stmt_by_ids(.plus_assign, [post_lhs_id], [one_id], token.Pos{})
	id := out.emit_for_stmt_by_ids(init_id, cond_id, post_id, body_ids)
	t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
	return true
}

fn runes_iterator_base_expr_cursor(expr ast.Cursor) ?ast.Cursor {
	if !expr.is_valid() {
		return none
	}
	if expr.kind() == .expr_call && expr.edge_count() == 1 {
		lhs := expr.edge(0)
		if lhs.is_valid() && lhs.kind() == .expr_selector && lhs.edge(1).name() == 'runes_iterator' {
			return lhs.edge(0)
		}
	}
	if expr.kind() == .expr_call_or_cast {
		lhs := expr.edge(0)
		arg := expr.edge(1)
		if lhs.is_valid() && lhs.kind() == .expr_selector && lhs.edge(1).name() == 'runes_iterator'
			&& (!arg.is_valid() || arg.kind() == .expr_empty) {
			return lhs.edge(0)
		}
	}
	return none
}

fn (mut t Transformer) try_expand_fixed_array_for_in_cursor_to_flat(stmt ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	for_in_c := stmt.edge(0)
	if !for_in_c.is_valid() || for_in_c.kind() != .stmt_for_in {
		return false
	}
	iter_type := t.for_in_iter_expr_type_cursor(for_in_c.edge(2)) or { return false }
	mut iter_base_type := iter_type
	for {
		if iter_base_type is types.Pointer {
			ptr := iter_base_type as types.Pointer
			iter_base_type = ptr.base_type
			continue
		}
		if iter_base_type is types.Alias {
			alias_t := iter_base_type as types.Alias
			iter_base_type = alias_t.base_type
			continue
		}
		break
	}
	if iter_base_type !is types.ArrayFixed {
		return false
	}
	arr_fixed := iter_base_type as types.ArrayFixed

	t.open_scope()
	value_name, _, _ := for_in_cursor_binding(for_in_c.edge(1), '_elem')
	key_name := for_in_cursor_index_name(for_in_c.edge(0), value_name)

	idx_pos := t.next_synth_pos()
	key_type := types.Type(arr_fixed).key_type()
	value_type := types.Type(arr_fixed).value_type()
	t.register_for_in_var_type(key_name, key_type)
	t.register_for_in_var_type(value_name, value_type)
	t.register_synth_type(idx_pos, key_type)

	iter_expr_c := for_in_c.edge(2)
	iter_pos := t.next_synth_pos()
	raw_iter_id := t.transform_expr_cursor_to_flat(iter_expr_c, mut out)
	transformed_expr_id := t.iter_value_expr_cursor_to_flat(iter_expr_c, raw_iter_id, iter_pos,
		types.Type(arr_fixed), mut out)
	iter_pending := t.pending_stmts
	t.pending_stmts = []ast.Stmt{}

	index_pos := t.next_synth_pos()
	t.register_synth_type(index_pos, value_type)
	index_key_id := out.emit_ident_by_name(key_name, idx_pos)
	index_expr_id := out.emit_index_expr_by_ids(transformed_expr_id, index_key_id, false, index_pos)

	value_decl_pos := t.next_synth_pos()
	value_lhs_id := out.emit_ident_by_name(value_name, value_decl_pos)
	t.register_synth_type(value_decl_pos, value_type)
	t.register_for_in_lhs_type_cursor(for_in_c.edge(1), value_type)

	mut body_ids := []ast.FlatNodeId{cap: 1 + stmt.for_body_list().len()}
	body_ids << out.emit_assign_stmt_by_ids(.decl_assign, [value_lhs_id], [
		index_expr_id,
	], token.Pos{})
	body_ids << t.transform_cursor_stmts_to_flat_direct(stmt.for_body_list(), [], mut out)
	body_pending := t.pending_stmts
	t.pending_stmts = []ast.Stmt{}
	t.pending_stmts << iter_pending
	t.pending_stmts << body_pending
	t.close_scope()

	init_lhs_id := out.emit_ident_by_name(key_name, idx_pos)
	init_rhs_id := out.emit_basic_literal_by_value(.number, '0', token.Pos{})
	init_id := out.emit_assign_stmt_by_ids(.decl_assign, [init_lhs_id], [init_rhs_id], token.Pos{})
	cond_lhs_id := out.emit_ident_by_name(key_name, idx_pos)
	cond_rhs_id := out.emit_basic_literal_by_value(.number, arr_fixed.len.str(), token.Pos{})
	cond_id := out.emit_infix_expr_by_ids(.lt, cond_lhs_id, cond_rhs_id, token.Pos{})
	post_lhs_id := out.emit_ident_by_name(key_name, idx_pos)
	post_rhs_id := out.emit_basic_literal_by_value(.number, '1', token.Pos{})
	post_id := out.emit_assign_stmt_by_ids(.plus_assign, [post_lhs_id], [post_rhs_id], token.Pos{})
	id := out.emit_for_stmt_by_ids(init_id, cond_id, post_id, body_ids)
	t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
	return true
}

fn (mut t Transformer) try_expand_array_for_in_cursor_to_flat(stmt ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	for_in_c := stmt.edge(0)
	if !for_in_c.is_valid() || for_in_c.kind() != .stmt_for_in {
		return false
	}
	base_iter_expr_c := for_in_c.edge(2)
	mut iter_expr_c := base_iter_expr_c
	mut iter_type := types.Type(types.void_)
	mut value_type := types.Type(types.void_)
	mut matched := false

	if iter_base := runes_iterator_base_expr_cursor(base_iter_expr_c) {
		if base_type := t.for_in_iter_expr_type_cursor(iter_base) {
			iter_expr_c = iter_base
			iter_type = base_type
			value_type = runes_iter_value_type()
			matched = true
		}
	}
	if !matched {
		if sc := t.find_smartcast_for_expr(expr_cursor_to_string(base_iter_expr_c)) {
			if orig_type := t.for_in_iter_expr_type_cursor(base_iter_expr_c) {
				if orig_type is types.SumType {
					for variant in orig_type.variants {
						variant_name := t.type_to_c_name(variant)
						if variant_name == sc.variant_full || variant_name == sc.variant {
							if variant is types.Array || variant is types.String {
								iter_type = variant
								value_type = t.for_in_value_type(variant)
								matched = true
							}
							break
						}
					}
				}
			}
		}
	}
	if !matched {
		if raw_iter_type := t.for_in_iter_expr_type_cursor(base_iter_expr_c) {
			mut iter_base_type := raw_iter_type
			for {
				if iter_base_type is types.Pointer {
					ptr := iter_base_type as types.Pointer
					iter_base_type = ptr.base_type
					continue
				}
				if iter_base_type is types.Alias {
					alias_t := iter_base_type as types.Alias
					iter_base_type = alias_t.base_type
					continue
				}
				break
			}
			if iter_base_type is types.Array {
				iter_type = iter_base_type
				value_type = t.for_in_value_type(iter_base_type)
				matched = true
			} else if iter_base_type is types.String || t.is_string_iterable_type(iter_base_type) {
				iter_type = iter_base_type
				value_type = t.for_in_value_type(iter_base_type)
				matched = true
			}
		}
	}
	if !matched {
		return false
	}

	t.open_scope()
	t.emit_array_for_in_cursor_to_flat(stmt, for_in_c, iter_expr_c, iter_type, value_type, mut ids, mut
		out)
	t.close_scope()
	return true
}

fn (mut t Transformer) emit_array_for_in_cursor_to_flat(stmt ast.Cursor, for_in_c ast.Cursor, iter_expr_c ast.Cursor, iter_type types.Type, value_type types.Type, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	value_name, _, is_mut_value := for_in_cursor_binding(for_in_c.edge(1), '_elem')
	key_name := for_in_cursor_index_name(for_in_c.edge(0), value_name)

	idx_pos := t.next_synth_pos()

	key_type := iter_type.key_type()
	t.register_for_in_var_type(key_name, key_type)
	t.register_for_in_var_type(value_name, value_type)
	had_generic_value := value_name in t.generic_var_type_params
	old_generic_value := t.generic_var_type_params[value_name] or { '' }
	if placeholder := t.generic_iter_value_placeholder_cursor(iter_expr_c) {
		t.generic_var_type_params[value_name] = placeholder
	}
	t.register_synth_type(idx_pos, key_type)

	iter_pos := t.next_synth_pos()
	raw_iter_id := t.transform_expr_cursor_to_flat(iter_expr_c, mut out)
	transformed_expr_id := t.iter_value_expr_cursor_to_flat(iter_expr_c, raw_iter_id, iter_pos,
		iter_type, mut out)
	iter_pending := t.pending_stmts
	t.pending_stmts = []ast.Stmt{}

	index_pos := t.next_synth_pos()
	t.register_synth_type(index_pos, value_type)
	index_key_id := out.emit_ident_by_name(key_name, idx_pos)
	index_expr := if t.is_string_iterable_type(iter_type)
		|| t.for_in_value_uses_array_index(value_type) {
		out.emit_index_expr_by_ids(transformed_expr_id, index_key_id, false, index_pos)
	} else {
		t.array_data_index_expr_cursor_to_flat(transformed_expr_id, index_key_id, value_type,
			index_pos, mut out)
	}
	value_is_ptr := value_type is types.Pointer
	value_lhs_type := if is_mut_value && !value_is_ptr {
		types.Type(types.Pointer{
			base_type: value_type
		})
	} else {
		value_type
	}
	value_decl_pos := t.next_synth_pos()
	value_decl_lhs_id := out.emit_ident_by_name(value_name, value_decl_pos)
	t.register_synth_type(value_decl_pos, value_lhs_type)
	t.register_for_in_lhs_type_cursor(for_in_c.edge(1), value_lhs_type)
	value_rhs := if is_mut_value && !value_is_ptr {
		ptr_pos := t.next_synth_pos()
		t.register_synth_type(ptr_pos, value_lhs_type)
		out.emit_prefix_expr_by_id(.amp, index_expr, ptr_pos)
	} else {
		index_expr
	}

	mut body_ids := []ast.FlatNodeId{cap: stmt.for_body_list().len() + 1}
	body_ids << out.emit_assign_stmt_by_ids(.decl_assign, [value_decl_lhs_id], [
		value_rhs,
	], token.Pos{})
	body_ids << t.transform_cursor_stmts_to_flat_direct(stmt.for_body_list(), [], mut out)
	if had_generic_value {
		t.generic_var_type_params[value_name] = old_generic_value
	} else {
		t.generic_var_type_params.delete(value_name)
	}
	body_pending := t.pending_stmts
	t.pending_stmts = []ast.Stmt{}
	t.pending_stmts << iter_pending
	t.pending_stmts << body_pending

	init_lhs_id := out.emit_ident_by_name(key_name, idx_pos)
	init_rhs_id := out.emit_basic_literal_by_value(.number, '0', token.Pos{})
	init_id := out.emit_assign_stmt_by_ids(.decl_assign, [init_lhs_id], [init_rhs_id], token.Pos{})
	cond_lhs_id := out.emit_ident_by_name(key_name, idx_pos)
	len_id :=
		t.synth_selector_cursor_to_flat(transformed_expr_id, 'len', types.Type(types.int_), mut out)
	cond_id := out.emit_infix_expr_by_ids(.lt, cond_lhs_id, len_id, token.Pos{})
	post_lhs_id := out.emit_ident_by_name(key_name, idx_pos)
	post_rhs_id := out.emit_basic_literal_by_value(.number, '1', token.Pos{})
	post_id := out.emit_assign_stmt_by_ids(.plus_assign, [post_lhs_id], [post_rhs_id], token.Pos{})
	id := out.emit_for_stmt_by_ids(init_id, cond_id, post_id, body_ids)
	t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
}

fn (t &Transformer) generic_iter_value_placeholder_cursor(expr ast.Cursor) ?string {
	if !expr.is_valid() {
		return none
	}
	if expr.kind() == .expr_ident {
		return t.generic_var_type_params[expr.name()] or { none }
	}
	if expr.kind() in [.expr_paren, .expr_modifier] {
		return t.generic_iter_value_placeholder_cursor(expr.edge(0))
	}
	return none
}

fn (t &Transformer) iter_expr_needs_deref_cursor(expr ast.Cursor) bool {
	if !expr.is_valid() {
		return false
	}
	if expr.kind() == .expr_ident {
		name := expr.name()
		if name == t.cur_fn_recv_param && t.cur_fn_recv_is_ptr {
			return true
		}
		if typ := t.lookup_var_type(name) {
			return t.is_pointer_type(typ)
		}
		var_type_name := t.get_var_type_name(name)
		if var_type_name != '' {
			return var_type_name.ends_with('*') || var_type_name.starts_with('&')
		}
	}
	if expr.kind() == .expr_prefix {
		op := unsafe { token.Token(int(expr.aux())) }
		if op == .amp {
			return true
		}
	}
	if typ := t.get_expr_type_cursor(expr) {
		return t.is_pointer_type(typ)
	}
	return false
}

fn (mut t Transformer) iter_value_expr_cursor_to_flat(orig ast.Cursor, transformed_id ast.FlatNodeId, pos token.Pos, iter_type types.Type, mut out ast.FlatBuilder) ast.FlatNodeId {
	t.register_synth_type(pos, iter_type)
	if t.iter_expr_needs_deref_cursor(orig) {
		base_id := out.emit_paren_expr_by_id(transformed_id, pos)
		deref_id := out.emit_prefix_expr_by_id(.mul, base_id, pos)
		return out.emit_paren_expr_by_id(deref_id, pos)
	}
	return transformed_id
}

fn (mut t Transformer) synth_selector_cursor_to_flat(lhs_id ast.FlatNodeId, field_name string, typ types.Type, mut out ast.FlatBuilder) ast.FlatNodeId {
	pos := t.next_synth_pos()
	t.register_synth_type(pos, typ)
	rhs_id := out.emit_ident_by_name(field_name, token.Pos{})
	return out.emit_selector_expr_by_ids(lhs_id, rhs_id, pos)
}

fn (mut t Transformer) array_data_index_expr_cursor_to_flat(array_expr_id ast.FlatNodeId, index_expr_id ast.FlatNodeId, value_type types.Type, pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	t.register_synth_type(pos, value_type)
	data_id :=
		t.synth_selector_cursor_to_flat(array_expr_id, 'data', types.Type(types.voidptr_), mut out)
	ptr_pos := t.next_synth_pos()
	t.register_synth_type(ptr_pos, types.Type(types.Pointer{
		base_type: value_type
	}))
	elem_type_name := t.type_to_c_decl_name(value_type)
	typ_id := out.emit_ident_by_name('${elem_type_name}*', token.Pos{})
	typed_data_id := out.emit_cast_expr_by_ids(typ_id, data_id, ptr_pos)
	return out.emit_index_expr_by_ids(typed_data_id, index_expr_id, false, pos)
}

fn (mut t Transformer) smartcast_map_iter_value_expr_cursor_to_flat(iter_expr ast.Cursor, map_type types.Map, mut out ast.FlatBuilder) ast.FlatNodeId {
	iter_id := t.transform_expr_cursor_to_flat(iter_expr, mut out)
	map_c_name := t.type_to_c_name(types.Type(map_type))
	if map_c_name == '' {
		return iter_id
	}
	data_id :=
		t.synth_selector_cursor_to_flat(iter_id, '_data', types.Type(types.voidptr_), mut out)
	is_native_backend := t.pref != unsafe { nil } && t.is_native_be
	variant_id := if is_native_backend {
		data_id
	} else {
		t.synth_selector_cursor_to_flat(data_id, '_${map_c_name}', types.Type(types.voidptr_), mut out)
	}
	typ_id := out.emit_ident_by_name('${map_c_name}*', token.Pos{})
	cast_id := out.emit_cast_expr_by_ids(typ_id, variant_id, token.Pos{})
	deref_id := out.emit_prefix_expr_by_id(.mul, cast_id, token.Pos{})
	return out.emit_paren_expr_by_id(deref_id, token.Pos{})
}

fn (mut t Transformer) register_for_in_lhs_type_cursor(lhs ast.Cursor, typ types.Type) {
	if !lhs.is_valid() {
		return
	}
	if lhs.kind() == .expr_ident {
		pos := lhs.pos()
		if pos.id != 0 {
			t.register_synth_type(pos, typ)
		}
	} else if lhs.kind() == .expr_modifier {
		t.register_for_in_lhs_type_cursor(lhs.edge(0), typ)
	}
}

fn (mut t Transformer) try_expand_untyped_for_in_cursor_to_flat(stmt ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	for_in_c := stmt.edge(0)
	if !for_in_c.is_valid() || for_in_c.kind() != .stmt_for_in {
		return false
	}
	iter_expr_c := for_in_c.edge(2)
	if _ := t.for_in_iter_expr_type_cursor(iter_expr_c) {
		if !t.is_native_be {
			return false
		}
	}

	t.open_scope()
	t.emit_untyped_for_in_cursor_to_flat(stmt, for_in_c, iter_expr_c, mut ids, mut out)
	t.close_scope()
	return true
}

fn (mut t Transformer) emit_untyped_for_in_cursor_to_flat(stmt ast.Cursor, for_in_c ast.Cursor, iter_expr_c ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) {
	value_name, _, is_mut_value := for_in_cursor_binding(for_in_c.edge(1), '_elem')
	key_name := for_in_cursor_index_name(for_in_c.edge(0), value_name)

	idx_pos := t.next_synth_pos()
	if int_obj := t.scope.lookup_parent('int', 0) {
		int_type := int_obj.typ()
		t.register_for_in_var_type(key_name, int_type)
		t.register_synth_type(idx_pos, int_type)
	}

	iter_typ := t.get_expr_type_cursor(iter_expr_c)
	iter_pos := t.next_synth_pos()
	raw_iter_id := t.transform_expr_cursor_to_flat(iter_expr_c, mut out)
	transformed_expr_id := out.emit_paren_expr_by_id(raw_iter_id, iter_pos)
	if typ := iter_typ {
		t.register_synth_type(iter_pos, typ)
	}
	iter_pending := t.pending_stmts
	t.pending_stmts = []ast.Stmt{}

	value_expr_c := for_in_c.edge(1)
	index_pos := t.next_synth_pos()
	if typ := t.get_expr_type_cursor(value_expr_c) {
		t.register_synth_type(index_pos, typ)
	}

	index_key_id := out.emit_ident_by_name(key_name, idx_pos)
	index_expr := out.emit_index_expr_by_ids(transformed_expr_id, index_key_id, false, index_pos)
	value_rhs := if is_mut_value {
		ptr_pos := t.next_synth_pos()
		if typ := t.get_expr_type_cursor(value_expr_c) {
			t.register_synth_type(ptr_pos, types.Type(types.Pointer{
				base_type: typ
			}))
		}
		out.emit_prefix_expr_by_id(.amp, index_expr, ptr_pos)
	} else {
		index_expr
	}
	value_lhs_id := for_in_cursor_binding_lhs_to_flat(for_in_c.edge(1), value_name, mut out)

	mut body_ids := []ast.FlatNodeId{cap: stmt.for_body_list().len() + 1}
	body_ids << out.emit_assign_stmt_by_ids(.decl_assign, [value_lhs_id], [value_rhs], token.Pos{})
	body_ids << t.transform_cursor_stmts_to_flat_direct(stmt.for_body_list(), [], mut out)
	body_pending := t.pending_stmts
	t.pending_stmts = []ast.Stmt{}
	t.pending_stmts << iter_pending
	t.pending_stmts << body_pending

	init_lhs_id := out.emit_ident_by_name(key_name, idx_pos)
	init_rhs_id := out.emit_basic_literal_by_value(.number, '0', token.Pos{})
	init_id := out.emit_assign_stmt_by_ids(.decl_assign, [init_lhs_id], [init_rhs_id], token.Pos{})
	cond_lhs_id := out.emit_ident_by_name(key_name, idx_pos)
	len_id :=
		t.synth_selector_cursor_to_flat(transformed_expr_id, 'len', types.Type(types.int_), mut out)
	cond_id := out.emit_infix_expr_by_ids(.lt, cond_lhs_id, len_id, token.Pos{})
	post_lhs_id := out.emit_ident_by_name(key_name, idx_pos)
	post_rhs_id := out.emit_basic_literal_by_value(.number, '1', token.Pos{})
	post_id := out.emit_assign_stmt_by_ids(.plus_assign, [post_lhs_id], [post_rhs_id], token.Pos{})
	id := out.emit_for_stmt_by_ids(init_id, cond_id, post_id, body_ids)
	t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
}

fn for_in_cursor_binding_lhs_to_flat(c ast.Cursor, fallback string, mut out ast.FlatBuilder) ast.FlatNodeId {
	if c.is_valid() {
		if c.kind() == .expr_ident {
			return out.emit_ident_by_name(c.name(), c.pos())
		}
		if c.kind() == .expr_modifier {
			inner := c.edge(0)
			if inner.is_valid() && inner.kind() == .expr_ident {
				return out.emit_ident_by_name(inner.name(), inner.pos())
			}
		}
	}
	return out.emit_ident_by_name(fallback, token.Pos{})
}

fn (mut t Transformer) try_expand_passthrough_for_in_cursor_to_flat(stmt ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	for_in_c := stmt.edge(0)
	if !for_in_c.is_valid() || for_in_c.kind() != .stmt_for_in {
		return false
	}
	iter_expr_c := for_in_c.edge(2)
	iter_type := t.for_in_iter_expr_type_cursor(iter_expr_c) or { return false }
	if t.is_native_be {
		return false
	}

	t.open_scope()
	value_name := for_in_cursor_var_name(for_in_c.edge(1))
	if value_name != '' && value_name != '_' {
		t.register_for_in_var_type(value_name, t.for_in_value_type(iter_type))
	}
	key_name := for_in_cursor_var_name(for_in_c.edge(0))
	if key_name != '' && key_name != '_' {
		t.register_for_in_var_type(key_name, iter_type.key_type())
	}

	body_ids := t.transform_cursor_stmts_to_flat_direct(stmt.for_body_list(), [], mut out)
	key_id := out.copy_subtree_from(for_in_c.edge(0).flat, for_in_c.edge(0).id)
	value_id := out.copy_subtree_from(for_in_c.edge(1).flat, for_in_c.edge(1).id)
	iter_id := t.transform_expr_cursor_to_flat(iter_expr_c, mut out)
	init_id := out.emit_for_in_stmt_by_ids(key_id, value_id, iter_id)
	cond_id := t.transform_expr_cursor_to_flat(stmt.edge(1), mut out)
	post_id := t.transform_stmt_cursor_to_flat(stmt.edge(2), mut out)
	t.close_scope()
	id := out.emit_for_stmt_by_ids(init_id, cond_id, post_id, body_ids)
	t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)
	return true
}

fn (mut t Transformer) try_expand_for_in_map_cursor_to_flat(stmt ast.Cursor, mut ids []ast.FlatNodeId, mut out ast.FlatBuilder) bool {
	if t.is_eval_backend() {
		return false
	}
	for_in_c := stmt.edge(0)
	if !for_in_c.is_valid() || for_in_c.kind() != .stmt_for_in {
		return false
	}
	iter_expr_c := for_in_c.edge(2)
	iter_type := t.get_expr_type_cursor(iter_expr_c) or { return false }
	map_type := t.unwrap_map_type(iter_type) or { return false }

	key_name := for_in_cursor_var_name(for_in_c.edge(0))
	key_is_blank := key_name == '_'
	value_name := for_in_cursor_var_name(for_in_c.edge(1))

	key_type_name := t.type_to_c_decl_name(map_type.key_type)
	value_type_name := t.type_to_c_decl_name(map_type.value_type)

	idx_name := t.gen_map_iter_temp_name('idx')
	len_name := t.gen_map_iter_temp_name('len')
	delta_name := t.gen_map_iter_temp_name('delta')

	smartcast_iter_expr := expr_cursor_to_string(iter_expr_c)
	is_smartcast_iter := smartcast_iter_expr != ''
		&& t.find_smartcast_for_expr(smartcast_iter_expr) != none
	is_lvalue := !is_smartcast_iter && iter_expr_c.kind() in [.expr_ident, .expr_selector]
	mut map_ref_id := ast.FlatNodeId(0)
	if is_lvalue {
		map_ref_id = t.transform_expr_cursor_to_flat(iter_expr_c, mut out)
	} else {
		map_tmp_name := t.gen_map_iter_temp_name('map')
		map_source_id := if is_smartcast_iter {
			t.smartcast_map_iter_value_expr_cursor_to_flat(iter_expr_c, map_type, mut out)
		} else {
			t.transform_expr_cursor_to_flat(iter_expr_c, mut out)
		}
		map_tmp_lhs_id := out.emit_ident_by_name(map_tmp_name, token.Pos{})
		map_tmp_assign_id := out.emit_assign_stmt_by_ids(.decl_assign, [
			map_tmp_lhs_id,
		], [map_source_id], token.Pos{})
		t.append_transformed_stmt_id_to_flat(mut ids, map_tmp_assign_id, mut out)
		t.register_temp_var(map_tmp_name, iter_type)
		map_ref_id = out.emit_ident_by_name(map_tmp_name, token.Pos{})
	}

	key_values_id := t.synth_selector_cursor_to_flat(map_ref_id, 'key_values', types.Type(types.Struct{
		name: 'DenseArray'
	}), mut out)

	key_values_len_id := t.synth_selector_cursor_to_flat(key_values_id, 'len',
		types.Type(types.int_), mut out)

	len_lhs_id := out.emit_modifier_expr_by_id(.key_mut,
		out.emit_ident_by_name(len_name, token.Pos{}), token.Pos{})
	len_init_id := out.emit_assign_stmt_by_ids(.decl_assign, [len_lhs_id], [
		key_values_len_id,
	], token.Pos{})
	t.append_transformed_stmt_id_to_flat(mut ids, len_init_id, mut out)

	mut stmt_ids := []ast.FlatNodeId{cap: stmt.for_body_list().len() + 8}

	delta_lhs_id := out.emit_ident_by_name(delta_name, token.Pos{})
	delta_rhs_id := out.emit_infix_expr_by_ids(.minus, key_values_len_id,
		out.emit_ident_by_name(len_name, token.Pos{}), token.Pos{})
	stmt_ids << out.emit_assign_stmt_by_ids(.decl_assign, [delta_lhs_id], [
		delta_rhs_id,
	], token.Pos{})

	len_update_lhs_id := out.emit_ident_by_name(len_name, token.Pos{})
	stmt_ids << out.emit_assign_stmt_by_ids(.assign, [len_update_lhs_id], [
		key_values_len_id,
	], token.Pos{})

	delta_lt_zero_id := out.emit_infix_expr_by_ids(.lt,
		out.emit_ident_by_name(delta_name, token.Pos{}), out.emit_basic_literal_by_value(.number,
		'0', token.Pos{}), token.Pos{})
	minus_one_id := out.emit_prefix_expr_by_id(.minus, out.emit_basic_literal_by_value(.number,
		'1', token.Pos{}), token.Pos{})
	reset_idx_id := out.emit_assign_stmt_by_ids(.assign, [
		out.emit_ident_by_name(idx_name, token.Pos{}),
	], [minus_one_id], token.Pos{})
	continue_id := out.emit_flow_control_stmt(.key_continue, '')
	delta_guard_id := out.emit_if_expr_by_ids(delta_lt_zero_id, out.emit_expr(ast.empty_expr), [
		reset_idx_id,
		continue_id,
	], token.Pos{})
	stmt_ids << out.emit_expr_stmt_by_id(delta_guard_id)

	has_index_lhs_id := out.emit_ident_by_name('DenseArray__has_index', token.Pos{})
	has_index_key_values_id := out.emit_prefix_expr_by_id(.amp, key_values_id, token.Pos{})
	has_index_idx_id := out.emit_ident_by_name(idx_name, token.Pos{})
	has_index_call_id := out.emit_call_expr_by_ids(has_index_lhs_id, [
		has_index_key_values_id,
		has_index_idx_id,
	], token.Pos{})
	has_index_not_id := out.emit_prefix_expr_by_id(.not, has_index_call_id, token.Pos{})
	has_index_guard_id := out.emit_if_expr_by_ids(has_index_not_id, out.emit_expr(ast.empty_expr), [
		out.emit_flow_control_stmt(.key_continue, ''),
	], token.Pos{})
	stmt_ids << out.emit_expr_stmt_by_id(has_index_guard_id)

	if !key_is_blank && key_name != '' {
		key_call_lhs_id := out.emit_ident_by_name('DenseArray__key', token.Pos{})
		key_values_ref_id := out.emit_prefix_expr_by_id(.amp, key_values_id, token.Pos{})
		key_call_id := out.emit_call_expr_by_ids(key_call_lhs_id, [key_values_ref_id,
			out.emit_ident_by_name(idx_name, token.Pos{})], token.Pos{})
		key_cast_type_id := out.emit_ident_by_name('${key_type_name}*', token.Pos{})
		key_cast_id := out.emit_cast_expr_by_ids(key_cast_type_id, key_call_id, token.Pos{})
		key_deref_id := out.emit_prefix_expr_by_id(.mul, key_cast_id, token.Pos{})
		key_lhs_id := out.emit_ident_by_name(key_name, token.Pos{})
		stmt_ids << out.emit_assign_stmt_by_ids(.decl_assign, [key_lhs_id], [
			key_deref_id,
		], token.Pos{})
		if map_type.key_type is types.String {
			clone_lhs_id := out.emit_ident_by_name('string__clone', token.Pos{})
			clone_arg_id := out.emit_ident_by_name(key_name, token.Pos{})
			clone_call_id := out.emit_call_expr_by_ids(clone_lhs_id, [clone_arg_id], token.Pos{})
			stmt_ids << out.emit_assign_stmt_by_ids(.assign, [
				out.emit_ident_by_name(key_name, token.Pos{}),
			], [clone_call_id], token.Pos{})
		}
		t.register_for_in_var_type(key_name, map_type.key_type)
	}

	if value_name != '' && value_name != '_' {
		value_call_lhs_id := out.emit_ident_by_name('DenseArray__value', token.Pos{})
		value_values_ref_id := out.emit_prefix_expr_by_id(.amp, key_values_id, token.Pos{})
		value_call_id := out.emit_call_expr_by_ids(value_call_lhs_id, [
			value_values_ref_id,
			out.emit_ident_by_name(idx_name, token.Pos{}),
		], token.Pos{})
		value_cast_type_id := out.emit_ident_by_name('${value_type_name}*', token.Pos{})
		value_cast_id := out.emit_cast_expr_by_ids(value_cast_type_id, value_call_id, token.Pos{})
		value_deref_id := out.emit_prefix_expr_by_id(.mul, value_cast_id, token.Pos{})
		value_lhs_id := out.emit_ident_by_name(value_name, token.Pos{})
		stmt_ids << out.emit_assign_stmt_by_ids(.decl_assign, [value_lhs_id], [
			value_deref_id,
		], token.Pos{})
		t.register_for_in_var_type(value_name, map_type.value_type)
	}

	t.open_scope()
	stmt_ids << t.transform_cursor_stmts_to_flat_direct(stmt.for_body_list(), [], mut out)
	init_id := out.emit_assign_stmt_by_ids(.decl_assign, [
		out.emit_ident_by_name(idx_name, token.Pos{}),
	], [out.emit_basic_literal_by_value(.number, '0', token.Pos{})], token.Pos{})
	cond_id := out.emit_infix_expr_by_ids(.lt, out.emit_ident_by_name(idx_name, token.Pos{}),
		out.emit_ident_by_name(len_name, token.Pos{}), token.Pos{})
	next_idx_id := out.emit_infix_expr_by_ids(.plus, out.emit_ident_by_name(idx_name, token.Pos{}), out.emit_basic_literal_by_value(.number,
		'1', token.Pos{}), token.Pos{})
	post_id := out.emit_assign_stmt_by_ids(.assign, [
		out.emit_ident_by_name(idx_name, token.Pos{}),
	], [next_idx_id], token.Pos{})
	t.close_scope()
	id := out.emit_for_stmt_by_ids(init_id, cond_id, post_id, stmt_ids)
	t.append_transformed_stmt_id_to_flat(mut ids, id, mut out)

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

fn (mut t Transformer) transform_flag_enum_set_clear_cursor_to_flat(stmt ast.Cursor, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if !stmt.is_valid() || stmt.kind() != .stmt_expr || stmt.edge_count() == 0 {
		return none
	}
	expr := stmt.edge(0)
	mut receiver := ast.Cursor{}
	mut arg := ast.Cursor{}
	mut method_name := ''
	match expr.kind() {
		.expr_call {
			if expr.edge_count() == 2 {
				lhs := expr.edge(0)
				if lhs.kind() != .expr_selector {
					return none
				}
				method_name = selector_rhs_name_cursor(lhs)
				receiver = lhs.edge(0)
				arg = expr.edge(1)
			} else if expr.edge_count() == 3 {
				lhs := expr.edge(0)
				if lhs.kind() != .expr_ident {
					return none
				}
				name := lhs.name()
				if name.ends_with('__set') {
					method_name = 'set'
				} else if name.ends_with('__clear') {
					method_name = 'clear'
				} else {
					return none
				}
				receiver = expr.edge(1)
				arg = expr.edge(2)
			} else {
				return none
			}
		}
		.expr_call_or_cast {
			if expr.edge_count() < 2 {
				return none
			}
			lhs := expr.edge(0)
			if lhs.kind() != .expr_selector || t.call_or_cast_lhs_is_type_cursor(lhs) {
				return none
			}
			method_name = selector_rhs_name_cursor(lhs)
			receiver = lhs.edge(0)
			arg = expr.edge(1)
		}
		else {
			return none
		}
	}

	if method_name !in ['set', 'clear'] || !receiver.is_valid() || !arg.is_valid() {
		return none
	}
	enum_type := t.get_enum_type_name_cursor(receiver)
	if enum_type == '' || !t.is_flag_enum(enum_type) {
		return none
	}
	lhs_id := t.transform_expr_cursor_to_flat(receiver, mut out)
	arg_id := t.transform_flag_enum_arg_cursor_to_flat(arg, enum_type, mut out)
	rhs_id := if method_name == 'clear' {
		out.emit_prefix_expr_by_id(.bit_not, arg_id, token.Pos{})
	} else {
		arg_id
	}
	op := if method_name == 'set' { token.Token.or_assign } else { token.Token.and_assign }
	return out.emit_assign_stmt_by_ids(op, [lhs_id], [rhs_id], expr.pos())
}

fn (mut t Transformer) transform_flag_enum_arg_cursor_to_flat(arg ast.Cursor, enum_type string, mut out ast.FlatBuilder) ast.FlatNodeId {
	if id := t.enum_shorthand_cursor_to_flat(arg, enum_type, mut out) {
		return id
	}
	match arg.kind() {
		.expr_infix {
			op := unsafe { token.Token(int(arg.aux())) }
			lhs_id := t.transform_flag_enum_arg_cursor_to_flat(arg.edge(0), enum_type, mut out)
			rhs_id := t.transform_flag_enum_arg_cursor_to_flat(arg.edge(1), enum_type, mut out)
			return out.emit_infix_expr_by_ids(op, lhs_id, rhs_id, arg.pos())
		}
		.expr_paren {
			inner_id := t.transform_flag_enum_arg_cursor_to_flat(arg.edge(0), enum_type, mut out)
			return out.emit_paren_expr_by_id(inner_id, arg.pos())
		}
		else {
			return t.transform_expr_cursor_to_flat(arg, mut out)
		}
	}
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
			return t.emit_fn_decl_flat(lowered_stmt, attrs, stmt_ids, mut out)
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

fn (mut t Transformer) specialize_generic_callable_cursor_to_flat(lhs ast.Cursor, suffix string, pos token.Pos, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	match lhs.kind() {
		.expr_ident {
			return out.emit_ident_by_name(lhs.name() + suffix, pos)
		}
		.expr_selector {
			inner_id := t.transform_expr_cursor_to_flat(lhs.edge(0), mut out)
			rhs := lhs.edge(1)
			rhs_id := out.emit_ident_by_name(rhs.name() + suffix, rhs.pos())
			return out.emit_selector_expr_by_ids(inner_id, rhs_id, pos)
		}
		else {
			mut name := expr_cursor_to_string(lhs)
			if name == '' {
				name = expr_cursor_to_v_string(lhs)
			}
			if name == '' {
				return none
			}
			return out.emit_ident_by_name(name + suffix, pos)
		}
	}
}

fn (t &Transformer) generic_specialization_suffix_from_cursor_edges(c ast.Cursor, start int) ?string {
	if start >= c.edge_count() {
		return ''
	}
	mut args := []ast.Cursor{cap: c.edge_count() - start}
	for i in start .. c.edge_count() {
		args << c.edge(i)
	}
	return t.generic_specialization_suffix_from_cursors(args)
}

fn (t &Transformer) generic_specialization_suffix_from_cursors(args []ast.Cursor) ?string {
	if args.len == 0 {
		return ''
	}
	mut all_placeholders := true
	mut placeholder_parts := []string{cap: args.len}
	for arg in args {
		if name := generic_placeholder_name_cursor(arg) {
			placeholder_parts << name
		} else {
			all_placeholders = false
			break
		}
	}
	if all_placeholders {
		return '_' + placeholder_parts.join('_')
	}
	mut parts := []string{cap: args.len}
	for arg in args {
		token_part := t.generic_specialization_token_cursor(arg) or { 'Type' }
		parts << token_part
	}
	return '_T_' + parts.join('_')
}

fn generic_placeholder_name_cursor(expr ast.Cursor) ?string {
	if expr.kind() == .expr_ident {
		name := expr.name()
		if name in ['T', 'U', 'V', 'K', 'W'] {
			return name
		}
	}
	return none
}

fn (t &Transformer) generic_specialization_token_cursor(expr ast.Cursor) ?string {
	if expr.kind() == .expr_ident {
		if concrete := t.cur_monomorphized_fn_bindings[expr.name()] {
			return t.generic_specialization_token_from_type(concrete)
		}
	}
	if concrete := t.get_expr_type_cursor(expr) {
		return t.generic_specialization_token_from_type(concrete)
	}
	match expr.kind() {
		.expr_ident {
			return sanitize_generic_token_part(expr.name())
		}
		.expr_selector {
			name := generic_specialization_cursor_name(expr)
			if name == '' {
				return none
			}
			return sanitize_generic_token_part(name)
		}
		.expr_prefix {
			op := unsafe { token.Token(int(expr.aux())) }
			if op == .amp {
				base := t.generic_specialization_token_cursor(expr.edge(0)) or { 'Type' }
				return base + 'ptr'
			}
			name := generic_specialization_cursor_name(expr)
			if name == '' {
				return none
			}
			return sanitize_generic_token_part(name)
		}
		.typ_array {
			base := t.generic_specialization_token_cursor(expr.edge(0)) or { 'Type' }
			return 'Array_' + base
		}
		.typ_array_fixed {
			base := t.generic_specialization_token_cursor(expr.edge(1)) or { 'Type' }
			len_expr := expr.edge(0)
			len_str := if len_expr.kind() == .expr_basic_literal { len_expr.name() } else { '0' }
			return 'Array_fixed_' + base + '_${len_str}'
		}
		.typ_map {
			key := t.generic_specialization_token_cursor(expr.edge(0)) or { 'Type' }
			val := t.generic_specialization_token_cursor(expr.edge(1)) or { 'Type' }
			return 'Map_' + key + '_' + val
		}
		.typ_option {
			base := t.generic_specialization_token_cursor(expr.edge(0)) or { 'Type' }
			return 'Option_' + base
		}
		.typ_result {
			base := t.generic_specialization_token_cursor(expr.edge(0)) or { 'Type' }
			return 'Result_' + base
		}
		.typ_pointer {
			base := t.generic_specialization_token_cursor(expr.edge(0)) or { 'Type' }
			return base + 'ptr'
		}
		else {
			name := generic_specialization_cursor_name(expr)
			if name != '' {
				return sanitize_generic_token_part(name)
			}
			return none
		}
	}
}

fn generic_specialization_cursor_name(expr ast.Cursor) string {
	mut name := expr_cursor_to_string(expr)
	if name != '' {
		return name
	}
	name = expr.name()
	if name != '' {
		return name
	}
	name = expr_cursor_to_v_string(expr)
	if name != '' && name != '...' {
		return name
	}
	return ''
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

fn string_values_from_cursor(list ast.CursorList) []string {
	mut values := []string{cap: list.len()}
	for i in 0 .. list.len() {
		value := list.at(i)
		if value.is_valid() {
			values << value.name()
		}
	}
	return values
}

fn string_inter_width_precision_from_cursor(inter ast.Cursor) (int, int) {
	if inter.edge_count() >= 4 {
		return inter.edge(2).extra_int(), inter.edge(3).extra_int()
	}
	packed := inter.extra_int()
	mut width := (packed >> 16) & 0xffff
	mut precision := packed & 0xffff
	if precision & 0x8000 != 0 {
		precision |= ~0xffff
	}
	if width & 0x8000 != 0 {
		width |= ~0xffff
	}
	return width, precision
}

fn (mut t Transformer) transform_string_inter_cursor_to_flat(inter_c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	format := unsafe { ast.StringInterFormat(int(inter_c.aux())) }
	width, precision := string_inter_width_precision_from_cursor(inter_c)
	expr_c := inter_c.edge(0)
	format_expr_c := inter_c.edge(1)
	if format != .unformatted {
		expr_id := t.transform_expr_cursor_to_flat(expr_c, mut out)
		format_expr_id := copy_string_inter_format_expr_cursor_to_flat(format_expr_c, mut out)
		resolved_fmt := t.resolve_explicit_sprintf_format_cursor(format, width, precision,
			format_expr_c, expr_c)
		return out.emit_string_inter_by_ids(format, width, precision, expr_id, format_expr_id,
			resolved_fmt)
	}
	if !t.interpolation_expr_cursor_uses_smartcast(expr_c)
		&& string_inter_cursor_needs_stale_str_repair(expr_c) {
		if repaired_id := t.transform_stale_string_str_inter_cursor_to_flat(expr_c, format_expr_c,
			format, width, precision, mut out)
		{
			return repaired_id
		}
	}
	if !t.interpolation_expr_cursor_uses_smartcast(expr_c)
		&& !string_inter_cursor_needs_stale_str_repair(expr_c) {
		if typ := t.string_inter_arg_type_cursor(expr_c) {
			if expr_id := t.transform_sprintf_arg_cursor_to_flat(expr_c, typ, mut out) {
				format_expr_id :=
					copy_string_inter_format_expr_cursor_to_flat(format_expr_c, mut out)
				resolved_fmt := t.resolve_unformatted_sprintf_format_cursor(expr_c)
				return out.emit_string_inter_by_ids(format, width, precision, expr_id,
					format_expr_id, resolved_fmt)
			}
		}
	}
	mut actual_inter := ast.StringInter{
		format:       format
		width:        width
		precision:    precision
		expr:         expr_c.expr()
		format_expr:  format_expr_c.expr()
		resolved_fmt: inter_c.name()
	}
	if t.interpolation_expr_uses_smartcast(actual_inter.expr) {
		if expr_typ := t.get_expr_type(actual_inter.expr) {
			transformed := t.transform_expr(actual_inter.expr)
			hoist_typ := t.get_expr_type(transformed) or {
				t.smartcast_variant_type_for_expr(actual_inter.expr) or { expr_typ }
			}
			hoisted := t.hoist_expr_to_temp(transformed, hoist_typ)
			actual_inter = ast.StringInter{
				...actual_inter
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
	format_expr_id := out.emit_expr(actual_inter.format_expr)
	resolved_fmt := t.resolve_sprintf_format(actual_inter)
	return out.emit_string_inter_by_ids(format, width, precision, expr_id, format_expr_id,
		resolved_fmt)
}

fn copy_string_inter_format_expr_cursor_to_flat(format_expr_c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	if format_expr_c.is_valid() {
		return out.copy_subtree_from(format_expr_c.flat, format_expr_c.id)
	}
	return out.emit_expr(ast.empty_expr)
}

fn (t &Transformer) string_inter_arg_type_cursor(expr ast.Cursor) ?types.Type {
	if declared := t.declared_expr_type_for_method_receiver_cursor(expr) {
		declared_type := t.normalize_type(declared)
		if actual := t.get_expr_type_cursor(expr) {
			actual_type := t.normalize_type(actual)
			if t.should_prefer_declared_interpolation_type(actual_type, declared_type) {
				return declared_type
			}
			return actual_type
		}
		return declared_type
	}
	return t.get_expr_type_cursor(expr)
}

fn (mut t Transformer) transform_stale_string_str_inter_cursor_to_flat(expr ast.Cursor, format_expr ast.Cursor, format ast.StringInterFormat, width int, precision int, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	if !string_inter_cursor_needs_stale_str_repair(expr) {
		return none
	}
	call := expr.edge(0)
	arg := call.edge(1)
	arg_type := t.string_inter_arg_type_cursor(arg) or { return none }
	match arg_type {
		types.String {
			return none
		}
		types.Alias {
			if arg_type.base_type is types.String {
				return none
			}
		}
		else {}
	}

	str_fn_name := t.get_str_fn_name_for_type(arg_type) or { return none }
	t.needed_str_fns[str_fn_name] = ''
	if arg_type is types.Enum {
		t.needed_enum_str_fns[str_fn_name] = arg_type
	}
	arg_id := t.transform_expr_cursor_to_flat(arg, mut out)
	expr_id := t.emit_cursor_str_call_selector_to_flat(str_fn_name, [arg_id], call.pos(), mut out)
	format_expr_id := copy_string_inter_format_expr_cursor_to_flat(format_expr, mut out)
	resolved_fmt := t.get_sprintf_format_for_type(arg_type)
	return out.emit_string_inter_by_ids(format, width, precision, expr_id, format_expr_id,
		resolved_fmt)
}

fn (t &Transformer) interpolation_expr_cursor_uses_smartcast(expr ast.Cursor) bool {
	if !t.has_active_smartcast() {
		return false
	}
	expr_str := expr_cursor_to_string(expr)
	if expr_str == '' {
		return false
	}
	for ctx in t.smartcast_stack {
		if ctx.expr == '' {
			continue
		}
		if expr_str == ctx.expr || expr_str.starts_with(ctx.expr + '.')
			|| expr_str.starts_with(ctx.expr + '[') {
			return true
		}
	}
	return false
}

fn string_inter_cursor_needs_stale_str_repair(expr ast.Cursor) bool {
	if !expr.is_valid() || expr.kind() != .expr_selector || expr.edge(1).name() != 'str' {
		return false
	}
	call := expr.edge(0)
	if call.kind() != .expr_call || call.edge_count() < 1 {
		return false
	}
	lhs := call.edge(0)
	return lhs.kind() == .expr_ident && lhs.name() == 'string__str' && call.edge_count() == 2
}

fn (mut t Transformer) emit_cursor_str_call_selector_to_flat(str_fn_name string, arg_ids []ast.FlatNodeId, pos token.Pos, mut out ast.FlatBuilder) ast.FlatNodeId {
	lhs_id := out.emit_ident_by_name(str_fn_name, token.Pos{})
	call_id := out.emit_call_expr_by_ids(lhs_id, arg_ids, pos)
	return t.synth_selector_cursor_to_flat(call_id, 'str', types.Type(types.voidptr_), mut out)
}

fn (mut t Transformer) transform_sprintf_arg_cursor_to_flat(expr ast.Cursor, typ types.Type, mut out ast.FlatBuilder) ?ast.FlatNodeId {
	transformed_id := t.transform_expr_cursor_to_flat(expr, mut out)
	match typ {
		types.String {
			return transformed_id
		}
		types.Pointer {
			if str_fn_name := t.get_str_fn_name_for_type(typ.base_type) {
				t.needed_str_fns[str_fn_name] = ''
				deref_id := out.emit_prefix_expr_by_id(.mul, transformed_id, expr.pos())
				return t.emit_cursor_str_call_selector_to_flat(str_fn_name, [deref_id], expr.pos(), mut
					out)
			}
			return transformed_id
		}
		types.Primitive {
			if typ.props.has(types.Properties.float) {
				str_fn_name := if typ.size == 32 { 'f32__str' } else { 'f64__str' }
				return t.emit_cursor_str_call_selector_to_flat(str_fn_name, [
					transformed_id,
				], expr.pos(), mut out)
			}
			return transformed_id
		}
		types.Rune, types.Char {
			return transformed_id
		}
		types.Enum {
			if str_fn_name := t.get_str_fn_name_for_type(typ) {
				t.needed_str_fns[str_fn_name] = ''
				t.needed_enum_str_fns[str_fn_name] = typ
				return t.emit_cursor_str_call_selector_to_flat(str_fn_name, [
					transformed_id,
				], expr.pos(), mut out)
			}
			return transformed_id
		}
		types.Alias {
			if typ.base_type is types.String {
				return transformed_id
			}
			base := t.live_alias_base_type(typ) or { return transformed_id }
			match base {
				types.String, types.Primitive {
					return transformed_id
				}
				else {
					return transformed_id
				}
			}
		}
		else {
			str_fn_name := t.register_auto_str_dependency(typ)
			if str_fn_name != '' {
				return t.emit_cursor_str_call_selector_to_flat(str_fn_name, [
					transformed_id,
				], expr.pos(), mut out)
			}
			return transformed_id
		}
	}

	return none
}

fn (t &Transformer) resolve_unformatted_sprintf_format_cursor(expr ast.Cursor) string {
	if typ := t.string_inter_arg_type_cursor(expr) {
		return t.get_sprintf_format_for_type(typ)
	}
	return '%d'
}

fn (t &Transformer) resolve_explicit_sprintf_format_cursor(format ast.StringInterFormat, in_width int, in_precision int, format_expr ast.Cursor, expr ast.Cursor) string {
	mut fmt := '%'
	mut width := in_width
	mut precision := in_precision
	mut arg_typ := types.Type(types.Primitive{})
	mut has_arg_typ := false
	if typ := t.string_inter_arg_type_cursor(expr) {
		arg_typ = typ
		has_arg_typ = true
	}
	if width == 0 && precision == 0 && format_expr.is_valid() && format_expr.kind() != .expr_empty {
		if format_expr.kind() == .expr_basic_literal {
			val := format_expr.name()
			if val.contains('.') {
				parts := val.split('.')
				if parts.len == 2 {
					if parts[0].len > 0 && parts[0] != '0' {
						width = parts[0].int()
					}
					precision = parts[1].int()
				}
			} else {
				if val.starts_with('0') && val.len > 1 {
					fmt += '0'
				}
				width = val.int()
			}
		} else if format_expr.kind() == .expr_prefix
			&& unsafe { token.Token(int(format_expr.aux())) } == .minus
			&& format_expr.edge(0).kind() == .expr_basic_literal {
			fmt += '-'
			width = format_expr.edge(0).name().int()
		}
	}
	if width > 0 {
		fmt += '${width}'
	}
	if precision > 0 {
		fmt += '.${precision}'
	}
	match format {
		.decimal {
			fmt += if has_arg_typ { t.sprintf_int_format_suffix(arg_typ, 'd', 'u') } else { 'd' }
		}
		.float {
			fmt += 'f'
		}
		.hex {
			fmt += if has_arg_typ { t.sprintf_int_format_suffix(arg_typ, 'x', 'x') } else { 'x' }
		}
		.octal {
			fmt += if has_arg_typ { t.sprintf_int_format_suffix(arg_typ, 'o', 'o') } else { 'o' }
		}
		.character {
			fmt += 'c'
		}
		.exponent {
			fmt += 'e'
		}
		.exponent_short {
			fmt += 'g'
		}
		.binary {
			fmt += if has_arg_typ { t.sprintf_int_format_suffix(arg_typ, 'd', 'u') } else { 'd' }
		}
		.pointer_address {
			fmt += 'p'
		}
		.string {
			fmt += 's'
		}
		.unformatted {
			fmt += if has_arg_typ { t.sprintf_int_format_suffix(arg_typ, 'd', 'u') } else { 'd' }
		}
	}

	return fmt
}

fn (mut t Transformer) transform_string_inter_literal_cursor_to_flat(c ast.Cursor, mut out ast.FlatBuilder) ast.FlatNodeId {
	values := string_values_from_cursor(c.list_at(0))
	inters := c.list_at(1)
	mut inter_ids := []ast.FlatNodeId{cap: inters.len()}
	for i in 0 .. inters.len() {
		inter_ids << t.transform_string_inter_cursor_to_flat(inters.at(i), mut out)
	}
	kind := unsafe { ast.StringLiteralKind(int(c.aux())) }
	return out.emit_string_inter_literal_by_ids(kind, values, inter_ids, token.Pos{})
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
