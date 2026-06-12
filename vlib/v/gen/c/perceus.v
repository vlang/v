// perceus.v — compiler-resident Perceus drop-placement analysis (P1).
// Ported from a validated standalone CFG/last-use analysis prototype:
// builds a per-function CFG over V's AST, runs backward liveness to a fixpoint,
// classifies uniqueness (escape/capture/spawn), and returns a DROP MAP keyed by
// statement position: stmt.pos -> the unique, non-returned values whose last
// read is at/within that statement (so a `drop` goes right after it).
//
// Analysis-only: it computes WHERE drops would go; emission is wired separately,
// behind `-d perceus`, gated by G-DIFF/G-LEAK. All symbols are `pcs_`/`Pcs`
// prefixed to avoid collisions in the large `c` (cgen) module. Coverage is the
// structured subset (Block/Assign/If/Match/For{,In,C}/Return/Branch/ExprStmt);
// unhandled nodes fall back to the GC residual (sound).
module c

import v.ast

struct PcsStep {
mut:
	pos int      // originating stmt.pos.pos (drop insertion key)
	def []string
	use []string
}

struct PcsBB {
mut:
	succ     []int
	steps    []PcsStep
	use      []string
	def      []string
	live_in  []string
	live_out []string
}

struct PcsCfg {
mut:
	blocks      []PcsBB
	exit_id     int
	loop_stack  [][2]int
	shared_vars []string
	returned    []string
	heap_vars   []string // locally-defined, heap-owning (array/map/string/has free()) -> the only drop candidates
	spine       []int    // block ids on the always-executed-exactly-once "spine" (top-level, before any early exit) — the only blocks drops may be EMITTED in
	spine_loop_pending bool // set by the spine walker just before lowering a spine-position loop whose body is all-simple — the loop arm then records its body block as per-iteration spine
	table       &ast.Table = unsafe { nil }
	// Interprocedural escape state. When `interproc` is true the call rule consults
	// `escapes` (whole-program `fkey() -> per-param escape` summaries); when false
	// it falls back to the conservative "every call argument escapes" rule. A
	// callee absent from `escapes` is treated as fully escaping in both modes.
	interproc bool
	escapes   map[string][]bool
	// Deep-drop analysis (for freeing nested heap fields of a dropped `&Foo`, not
	// just the struct allocation). A var is deep-droppable iff it is born from a
	// fresh `&Foo{...}` whose every heap field is freshly allocated (deep_fresh)
	// AND no conflicting non-fresh assignment to it exists (deep_nonfresh) AND no
	// heap field is ever selected through it (deep_field_exposed — which would mean
	// a field buffer was read-aliased out or reassigned, making a deep free a UAF).
	// Populated during the exhaustive pcs_scan_share pass, so it cannot miss a use.
	deep_fresh         []string
	deep_nonfresh      []string
	deep_field_exposed []string
}

// PcsEscapeEnv drives the whole-program parameter-escape fixpoint. `fns` maps a
// function's `fkey()` to a per-parameter escape vector; for methods `vec[0]` is
// the receiver and the explicit parameters follow. A parameter "escapes" when the
// callee may retain its heap buffer beyond the call (it is returned, aliased into
// something returned, address-taken, appended, stored in an aggregate, captured,
// sent to a thread/channel, or passed on to another escaping slot).
struct PcsEscapeEnv {
mut:
	fns   map[string][]bool
	table &ast.Table = unsafe { nil }
}

// heap-owning predicate, mirroring autofree_variable's dispatch: only these
// kinds carry a heap allocation worth a deterministic drop. Params (never
// assigned) and value types (int/bool/struct-without-free) are excluded.
fn (c &PcsCfg) pcs_is_heap_owning(typ ast.Type) bool {
	if c.table == unsafe { nil } || typ == 0 {
		return false
	}
	sym := c.table.sym(typ)
	if sym.kind in [ast.Kind.array, .map, .string] {
		return true
	}
	if sym.has_method('free') {
		return true
	}
	// User-reference pointer types (`&Foo`): a heap-allocated reference to a
	// user/struct type — exactly autofree's `-experimental`-gated case (see
	// autofree_variable's `is_user_ref`). They leak today because autofree cannot
	// tell unique from shared; Perceus uniqueness is precisely what makes freeing
	// the unique ones sound, so they become drop candidates here. (Pointers to
	// array/map/string are already handled by the kind check above.)
	if typ.is_ptr() {
		n := sym.name.after('.')
		if n.len > 0 && n[0].is_capital() {
			return true
		}
	}
	return false
}

// pcs_rhs_is_fresh_string reports whether `rhs` (bound to a `string`-typed
// target) provably yields a NEWLY allocated string buffer that shares storage
// with none of the variables it reads. In V, string concatenation (`+`) and
// string interpolation always allocate fresh; substrings/slices (IndexExpr with
// a RangeExpr) may share and are deliberately NOT included.
fn (c &PcsCfg) pcs_rhs_is_fresh_string(rhs ast.Expr, result_typ ast.Type) bool {
	if c.table == unsafe { nil } || result_typ == 0 {
		return false
	}
	if c.table.sym(result_typ).kind != .string {
		return false
	}
	mut e := rhs
	for e is ast.ParExpr {
		e = (e as ast.ParExpr).expr
	}
	return match e {
		ast.StringInterLiteral { true }
		ast.InfixExpr { e.op == .plus }
		else { false }
	}
}

// pcs_is_store_target reports whether an assignment LHS writes THROUGH an existing
// object (a struct field, a collection element, or a pointer dereference) rather
// than (re)binding a plain local. Storing into such a target retains the stored
// value: for arrays/strings/maps V clones on the store (no alias), but a stored
// POINTER (`&Foo`) genuinely aliases, so it escapes the current binding.
fn pcs_is_store_target(l ast.Expr) bool {
	return match l {
		ast.SelectorExpr { true }
		ast.IndexExpr { true }
		ast.PrefixExpr { l.op == .mul } // `*p = ...` dereference store
		else { false }
	}
}

// pcs_rhs_is_fresh_ref reports whether `rhs` is a FRESH heap allocation that this
// binding uniquely OWNS — i.e. `&Foo{...}` (parsed as `PrefixExpr(.amp, StructInit)`,
// the `is_amp` merge). Only an owning pointer may be freed; a borrowed pointer
// (`p := other`, `p := obj.field`, `p := f()`) points to memory it does not own, so
// freeing it would corrupt the real owner. This is the ownership half of the proof
// that lets Perceus retire autofree's `is_auto_heap`/`-experimental` pointer gate
// (the uniqueness half is the share classifier).
fn pcs_rhs_is_fresh_ref(rhs ast.Expr) bool {
	mut e := rhs
	for e is ast.ParExpr {
		e = (e as ast.ParExpr).expr
	}
	if e is ast.PrefixExpr {
		return e.op == .amp && e.right is ast.StructInit
	}
	return false
}

// pcs_field_init_is_fresh reports whether a struct-field initializer allocates
// FRESH memory uniquely owned by the struct (so deep-freeing it when the struct is
// dropped is sound), vs. borrowing/aliasing external memory (ident/selector/call/
// index — which would make a deep free a UAF on the real owner).
fn (c &PcsCfg) pcs_field_init_is_fresh(e ast.Expr, ftyp ast.Type) bool {
	mut ex := e
	for ex is ast.ParExpr {
		ex = (ex as ast.ParExpr).expr
	}
	return match ex {
		ast.ArrayInit { true } // []T{...}
		ast.MapInit { true } // {...}
		ast.StringInterLiteral { true } // '...${x}...'
		ast.StringLiteral { true } // literal: no owned heap buffer to alias
		ast.IntegerLiteral, ast.FloatLiteral, ast.BoolLiteral, ast.CharLiteral { true }
		ast.PrefixExpr { ex.op == .amp && ex.right is ast.StructInit } // &Bar{...}
		ast.InfixExpr { ftyp == ast.string_type && ex.op == .plus } // fresh string concat
		else { false } // ident / selector / call / index: may alias external memory
	}
}

// pcs_struct_init_all_fields_fresh reports whether every HEAP-owning field of a
// `&Foo{...}` is initialized with a fresh allocation, so the resulting struct
// uniquely owns all its heap fields and deep-freeing them on drop is sound.
fn (c &PcsCfg) pcs_struct_init_all_fields_fresh(rhs ast.Expr) bool {
	mut e := rhs
	for e is ast.ParExpr {
		e = (e as ast.ParExpr).expr
	}
	if e !is ast.PrefixExpr {
		return false
	}
	pe := e as ast.PrefixExpr
	if pe.op != .amp || pe.right !is ast.StructInit {
		return false
	}
	si := pe.right as ast.StructInit
	if si.has_update_expr {
		return false // `&Foo{...other}` may copy heap fields from `other` (aliased)
	}
	for f in si.init_fields {
		if c.pcs_is_heap_owning(f.expected_type) {
			if !c.pcs_field_init_is_fresh(f.expr, f.expected_type) {
				return false
			}
		}
	}
	return true
}

// pcs_deep_drop_set returns the names of dropped `&Foo` locals that are SOUND to
// DEEP-free (free nested heap fields too). Computed from the exhaustive scan state.
fn (c &PcsCfg) pcs_deep_drop_set() map[string]bool {
	mut out := map[string]bool{}
	for name in c.deep_fresh {
		if name !in c.deep_nonfresh && name !in c.deep_field_exposed {
			out[name] = true
		}
	}
	return out
}

// pcs_is_fresh_array_call reports whether a call is a fresh-array-producing builtin
// (`map`/`filter`) whose result shares NO heap storage with its receiver — so the
// receiver neither escapes into the callee nor is aliased by the result, and stays
// uniquely owned (droppable at the call). This holds ONLY for PRIMITIVE element
// types: `[]int.map(it*2)` yields a brand-new buffer of scalar copies, but
// `[]string.filter(...)` copies string *headers* that share the receiver's element
// buffers, so freeing the receiver would corrupt the result. The element-non-heap
// guard is exactly that distinction. (P2 reuse precondition.)
fn (c &PcsCfg) pcs_is_fresh_array_call(e ast.CallExpr) bool {
	if e.kind != .map && e.kind != .filter {
		return false
	}
	if c.table == unsafe { nil } || e.left_type == 0 {
		return false
	}
	sym := c.table.final_sym(e.left_type)
	if sym.kind != .array {
		return false
	}
	if sym.info is ast.Array {
		return !c.pcs_is_heap_owning(sym.info.elem_type)
	}
	return false
}

fn pcs_uniq_push(mut list []string, name string) {
	if name != '' && name !in list {
		list << name
	}
}

fn pcs_set_eq(a []string, b []string) bool {
	if a.len != b.len {
		return false
	}
	for v in a {
		if v !in b {
			return false
		}
	}
	return true
}

// pcs_collect gathers every identifier read inside an expression. It is
// EXHAUSTIVE BY CONSTRUCTION: every `ast.Expr` variant has an explicit arm and
// there is NO `else`, so V's match-exhaustiveness check fails the build if a
// future compiler adds an expr kind — forcing us to decide how to traverse it
// rather than silently treating it as a leaf. A use this function fails to
// record would make a live variable look dead and license an unsound early
// drop, so totality here is the correctness foundation; the CFG's control-flow
// precision (elsewhere) is only a performance dial.
fn pcs_collect(e ast.Expr, mut out []string) {
	match e {
		ast.Ident { pcs_uniq_push(mut out, e.name) }
		ast.InfixExpr {
			pcs_collect(e.left, mut out)
			pcs_collect(e.right, mut out)
		}
		ast.PrefixExpr { pcs_collect(e.right, mut out) }
		ast.PostfixExpr { pcs_collect(e.expr, mut out) }
		ast.ParExpr { pcs_collect(e.expr, mut out) }
		ast.SelectorExpr { pcs_collect(e.expr, mut out) }
		ast.ArrayDecompose { pcs_collect(e.expr, mut out) }
		ast.IfGuardExpr { pcs_collect(e.expr, mut out) }
		ast.IsRefType { pcs_collect(e.expr, mut out) }
		ast.Likely { pcs_collect(e.expr, mut out) }
		ast.SizeOf { pcs_collect(e.expr, mut out) }
		ast.TypeOf { pcs_collect(e.expr, mut out) }
		ast.DumpExpr { pcs_collect(e.expr, mut out) }
		ast.UnsafeExpr { pcs_collect(e.expr, mut out) }
		ast.LambdaExpr { pcs_collect(e.expr, mut out) }
		ast.AsCast { pcs_collect(e.expr, mut out) }
		ast.CTempVar { pcs_collect(e.orig, mut out) }
		ast.ChanInit { pcs_collect(e.cap_expr, mut out) }
		ast.SpawnExpr { pcs_collect(e.call_expr, mut out) }
		ast.GoExpr { pcs_collect(e.call_expr, mut out) }
		ast.TypeNode { pcs_collect_stmt(e.stmt, mut out) }
		ast.IndexExpr {
			pcs_collect(e.left, mut out)
			pcs_collect(e.index, mut out)
			for ix in e.indices {
				pcs_collect(ix, mut out)
			}
			for s in e.or_expr.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.CallExpr {
			pcs_collect(e.left, mut out)
			for a in e.args {
				pcs_collect(a.expr, mut out)
			}
			for s in e.or_block.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.ComptimeCall {
			pcs_collect(e.left, mut out)
			for a in e.args {
				pcs_collect(a.expr, mut out)
			}
			for s in e.or_block.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.ComptimeSelector {
			pcs_collect(e.left, mut out)
			pcs_collect(e.field_expr, mut out)
		}
		ast.CastExpr {
			pcs_collect(e.expr, mut out)
			pcs_collect(e.arg, mut out)
		}
		ast.RangeExpr {
			pcs_collect(e.low, mut out)
			pcs_collect(e.high, mut out)
		}
		ast.ConcatExpr {
			for v in e.vals {
				pcs_collect(v, mut out)
			}
		}
		ast.Assoc {
			for v in e.exprs {
				pcs_collect(v, mut out)
			}
		}
		ast.ArrayInit {
			for v in e.exprs {
				pcs_collect(v, mut out)
			}
			pcs_collect(e.len_expr, mut out)
			pcs_collect(e.cap_expr, mut out)
			pcs_collect(e.init_expr, mut out)
			pcs_collect(e.elem_type_expr, mut out)
			pcs_collect(e.update_expr, mut out)
		}
		ast.MapInit {
			for k in e.keys {
				pcs_collect(k, mut out)
			}
			for v in e.vals {
				pcs_collect(v, mut out)
			}
			pcs_collect(e.update_expr, mut out)
		}
		ast.StructInit {
			pcs_collect(e.typ_expr, mut out)
			pcs_collect(e.update_expr, mut out)
			for f in e.init_fields {
				pcs_collect(f.expr, mut out)
			}
		}
		ast.StringInterLiteral {
			for v in e.exprs {
				pcs_collect(v, mut out)
			}
			for v in e.fwidth_exprs {
				pcs_collect(v, mut out)
			}
			for v in e.precision_exprs {
				pcs_collect(v, mut out)
			}
		}
		ast.IfExpr {
			pcs_collect(e.left, mut out)
			for br in e.branches {
				pcs_collect(br.cond, mut out)
				for s in br.stmts {
					pcs_collect_stmt(s, mut out)
				}
			}
		}
		ast.MatchExpr {
			pcs_collect(e.cond, mut out)
			for br in e.branches {
				for ex in br.exprs {
					pcs_collect(ex, mut out)
				}
				for s in br.stmts {
					pcs_collect_stmt(s, mut out)
				}
			}
		}
		ast.SelectExpr {
			for br in e.branches {
				pcs_collect_stmt(br.stmt, mut out)
				for s in br.stmts {
					pcs_collect_stmt(s, mut out)
				}
			}
		}
		ast.LockExpr {
			for l in e.lockeds {
				pcs_collect(l, mut out)
			}
			for s in e.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.OrExpr {
			for s in e.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.AnonFn {
			for s in e.decl.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.SqlExpr {
			pcs_collect(e.db_expr, mut out)
			pcs_collect(e.where_expr, mut out)
			pcs_collect(e.order_expr, mut out)
			pcs_collect(e.limit_expr, mut out)
			pcs_collect(e.offset_expr, mut out)
		}
		// Pure leaves — carry no enclosing-scope variable references.
		ast.NodeError, ast.AtExpr, ast.BoolLiteral, ast.CharLiteral, ast.Comment,
		ast.ComptimeType, ast.EmptyExpr, ast.EnumVal, ast.FloatLiteral,
		ast.IntegerLiteral, ast.Nil, ast.None, ast.OffsetOf, ast.SqlQueryDataExpr,
		ast.StringLiteral {}
	}
}

// pcs_collect_stmt gathers every identifier read inside a statement. Like
// pcs_collect it is EXHAUSTIVE BY CONSTRUCTION (every `ast.Stmt` variant, no
// `else`). Used for nested stmts in if/match/lock/or arms and for the
// conservative pin sweep over statement kinds the CFG does not model precisely.
fn pcs_collect_stmt(st ast.Stmt, mut out []string) {
	match st {
		ast.ExprStmt { pcs_collect(st.expr, mut out) }
		ast.AssignStmt {
			for r in st.right {
				pcs_collect(r, mut out)
			}
			for l in st.left {
				pcs_collect(l, mut out)
			}
		}
		ast.Return {
			for e in st.exprs {
				pcs_collect(e, mut out)
			}
		}
		ast.AssertStmt {
			pcs_collect(st.expr, mut out)
			pcs_collect(st.extra, mut out)
		}
		ast.Block {
			for s in st.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.ForStmt {
			pcs_collect(st.cond, mut out)
			for s in st.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.ForInStmt {
			pcs_collect(st.cond, mut out)
			pcs_collect(st.high, mut out)
			for s in st.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.ForCStmt {
			pcs_collect_stmt(st.init, mut out)
			pcs_collect(st.cond, mut out)
			pcs_collect_stmt(st.inc, mut out)
			for s in st.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.DeferStmt {
			for s in st.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.ComptimeFor {
			pcs_collect(st.expr, mut out)
			for s in st.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.FnDecl {
			for s in st.stmts {
				pcs_collect_stmt(s, mut out)
			}
		}
		ast.AsmStmt {
			for io in st.output {
				pcs_collect(io.expr, mut out)
			}
			for io in st.input {
				pcs_collect(io.expr, mut out)
			}
		}
		ast.HashStmt {
			for c in st.ct_conds {
				pcs_collect(c, mut out)
			}
		}
		ast.SqlStmt { pcs_collect(st.db_expr, mut out) }
		// Pure leaves / declaration statements with no local-variable uses.
		ast.BranchStmt, ast.ConstDecl, ast.DebuggerStmt, ast.EmptyStmt,
		ast.EnumDecl, ast.GlobalDecl, ast.GotoLabel, ast.GotoStmt, ast.Import,
		ast.InterfaceDecl, ast.Module, ast.NodeError, ast.SemicolonStmt,
		ast.StructDecl, ast.TypeDecl {}
	}
}

fn (mut c PcsCfg) pcs_nb() int {
	id := c.blocks.len
	c.blocks << PcsBB{}
	return id
}

fn (mut c PcsCfg) pcs_edge(from int, to int) {
	if from >= 0 && from < c.blocks.len {
		c.blocks[from].succ << to
	}
}

fn (mut c PcsCfg) pcs_emit(b int, pos int, def []string, use []string) {
	if b < 0 || b >= c.blocks.len {
		return
	}
	for u in use {
		if u !in c.blocks[b].def {
			pcs_uniq_push(mut c.blocks[b].use, u)
		}
	}
	for d in def {
		pcs_uniq_push(mut c.blocks[b].def, d)
	}
	c.blocks[b].steps << PcsStep{
		pos: pos
		def: def.clone()
		use: use.clone()
	}
}

fn (mut c PcsCfg) pcs_mark_shared(name string) {
	pcs_uniq_push(mut c.shared_vars, name)
}

// pcs_share_idents marks EVERY identifier in `e` as shared (conservatively
// not-uniquely-owned -> never deterministically dropped). Used at "retaining"
// positions where a value's heap buffer may be captured by something that
// outlives the current binding.
fn (mut c PcsCfg) pcs_share_idents(e ast.Expr) {
	mut ids := []string{}
	pcs_collect(e, mut ids)
	for id in ids {
		c.pcs_mark_shared(id)
	}
}

// pcs_call_escape decides, for a single call, whether the receiver and each
// positional argument escape into the callee (i.e. the callee may retain the
// value's heap buffer beyond the call). It consults the whole-program escape
// summaries keyed by `fkey()`. Conservative defaults — returning escape=true —
// apply whenever the callee cannot be resolved to a summary: no escape env at
// all, an indirect/fn-variable/field call, or a callee with no recorded summary
// (external C/JS, generic, no-body). This makes the worst case observationally
// identical to the pre-interprocedural "pin every call argument" rule, so the
// change can only ever REMOVE pins, never license an unsound early drop.
fn (c &PcsCfg) pcs_call_escape(e ast.CallExpr) (bool, []bool) {
	if !c.interproc || e.is_fn_var || e.is_field {
		// receiver escapes (true) is irrelevant for free fns; harmless for methods.
		return true, []bool{len: e.args.len, init: true}
	}
	summary := c.escapes[e.fkey()] or {
		return true, []bool{len: e.args.len, init: true}
	}
	// summary[0] is the receiver for methods; explicit parameters follow.
	off := if e.is_method { 1 } else { 0 }
	recv_esc := if e.is_method { if summary.len > 0 { summary[0] } else { true } } else { false }
	mut arg_esc := []bool{len: e.args.len}
	for i in 0 .. e.args.len {
		si := i + off
		// Beyond the declared parameters (variadic spread, or any arity mismatch)
		// fall back to escaping — sound, and the spill is rare on hot paths.
		arg_esc[i] = if si < summary.len { summary[si] } else { true }
	}
	return recv_esc, arg_esc
}

// pcs_scan_share is the uniqueness classifier (Perceus layer 3, conservative).
// It is EXHAUSTIVE BY CONSTRUCTION (every ast.Expr arm, no `else`). A heap value
// is treated as SHARED (ineligible for deterministic drop) whenever its buffer
// may be aliased or retained:
//   - address taken (`&x`),
//   - captured by a closure / passed to spawn / sent on a channel,
//   - appended into another collection (`a << x`),
//   - passed as a call argument or call receiver — the callee may store it, and
//     without interprocedural escape analysis we must assume it might (V strings
//     /arrays share their heap buffer across the call; this is exactly the
//     `files.filter()`-aliases-`files` class of bug).
// Assignment-level aliasing (`y := f(x)` where both are heap) is handled in
// pcs_scan_share_stmt. Imprecision here only ever KEEPS a value alive (smaller
// optimization, never a use-after-free) — the spec's "GC residual" principle.
fn (mut c PcsCfg) pcs_scan_share(e ast.Expr) {
	match e {
		ast.PrefixExpr {
			if e.op == .amp {
				c.pcs_share_idents(e.right)
			}
			c.pcs_scan_share(e.right)
		}
		ast.InfixExpr {
			if e.op == .left_shift {
				// `a << x` — x's buffer may be retained by a; a is mutated.
				c.pcs_share_idents(e.left)
				c.pcs_share_idents(e.right)
			}
			c.pcs_scan_share(e.left)
			c.pcs_scan_share(e.right)
		}
		ast.CallExpr {
			// Interprocedural escape: pin ONLY the receiver/arguments whose value
			// the callee may retain. With no summary (external/generic/indirect)
			// pcs_call_escape returns all-true, reproducing the conservative
			// pre-interprocedural behaviour exactly.
			recv_esc, arg_esc := c.pcs_call_escape(e)
			// Fresh-producing array builtins (`map`/`filter`) over primitive elements
			// neither retain nor alias their receiver — the result is a brand-new
			// buffer of scalar copies — so the receiver does not escape the call.
			if recv_esc && !c.pcs_is_fresh_array_call(e) {
				c.pcs_share_idents(e.left)
			}
			for i, a in e.args {
				if i < arg_esc.len && arg_esc[i] {
					c.pcs_share_idents(a.expr)
				}
			}
			// Always recurse: nested calls inside the receiver/args have their own
			// (independent) escape decisions; a non-escaping outer arg can still
			// contain an inner call that escapes some inner variable.
			c.pcs_scan_share(e.left)
			for a in e.args {
				c.pcs_scan_share(a.expr)
			}
			for s in e.or_block.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.ComptimeCall {
			c.pcs_share_idents(e.left)
			for a in e.args {
				c.pcs_share_idents(a.expr)
			}
			for s in e.or_block.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.SpawnExpr {
			c.pcs_share_idents(e.call_expr.left)
			for a in e.call_expr.args {
				c.pcs_share_idents(a.expr)
			}
		}
		ast.GoExpr {
			c.pcs_share_idents(e.call_expr.left)
			for a in e.call_expr.args {
				c.pcs_share_idents(a.expr)
			}
		}
		ast.AnonFn {
			for v in e.inherited_vars {
				c.pcs_mark_shared(v.name)
			}
		}
		ast.ParExpr { c.pcs_scan_share(e.expr) }
		ast.PostfixExpr { c.pcs_scan_share(e.expr) }
		ast.SelectorExpr {
			// Deep-drop guard: selecting a HEAP field through a var (`p.buf`, read
			// or write) means that field's buffer may be aliased out or reassigned,
			// so the var is NOT safe to deep-free. (Scalar field reads like `p.a`
			// are heap-owning=false and ignored.)
			base := e.expr
			if base is ast.Ident {
				if c.pcs_is_heap_owning(e.typ) {
					pcs_uniq_push(mut c.deep_field_exposed, base.name)
				}
			}
			c.pcs_scan_share(e.expr)
		}
		ast.ArrayDecompose { c.pcs_scan_share(e.expr) }
		ast.IfGuardExpr { c.pcs_scan_share(e.expr) }
		ast.IsRefType { c.pcs_scan_share(e.expr) }
		ast.Likely { c.pcs_scan_share(e.expr) }
		ast.SizeOf { c.pcs_scan_share(e.expr) }
		ast.TypeOf { c.pcs_scan_share(e.expr) }
		ast.DumpExpr { c.pcs_scan_share(e.expr) }
		ast.UnsafeExpr {
			// `unsafe { … }` can launder a pointer past the structured rules
			// (pointer arithmetic, raw deref stores). Conservatively pin everything
			// it mentions so no value used inside unsafe is ever dropped early.
			c.pcs_share_idents(e.expr)
			c.pcs_scan_share(e.expr)
		}
		ast.LambdaExpr { c.pcs_scan_share(e.expr) }
		ast.AsCast {
			// A cast can move a pointer into an opaque/other-typed binding the
			// classifier no longer tracks (e.g. `&Foo` -> voidptr). Pin the operand.
			c.pcs_share_idents(e.expr)
			c.pcs_scan_share(e.expr)
		}
		ast.CTempVar { c.pcs_scan_share(e.orig) }
		ast.ChanInit { c.pcs_scan_share(e.cap_expr) }
		ast.ComptimeSelector {
			c.pcs_scan_share(e.left)
			c.pcs_scan_share(e.field_expr)
		}
		ast.CastExpr {
			// See AsCast — a cast can launder a pointer into an opaque binding.
			c.pcs_share_idents(e.expr)
			c.pcs_scan_share(e.expr)
			c.pcs_scan_share(e.arg)
		}
		ast.RangeExpr {
			c.pcs_scan_share(e.low)
			c.pcs_scan_share(e.high)
		}
		ast.IndexExpr {
			c.pcs_scan_share(e.left)
			c.pcs_scan_share(e.index)
			for ix in e.indices {
				c.pcs_scan_share(ix)
			}
		}
		ast.ConcatExpr {
			for v in e.vals {
				c.pcs_scan_share(v)
			}
		}
		ast.Assoc {
			for v in e.exprs {
				c.pcs_scan_share(v)
			}
		}
		ast.ArrayInit {
			// elements placed into a new aggregate -> their buffers are retained.
			for v in e.exprs {
				c.pcs_share_idents(v)
			}
			c.pcs_scan_share(e.len_expr)
			c.pcs_scan_share(e.cap_expr)
			c.pcs_scan_share(e.init_expr)
			c.pcs_scan_share(e.elem_type_expr)
			c.pcs_scan_share(e.update_expr)
		}
		ast.MapInit {
			for k in e.keys {
				c.pcs_share_idents(k)
			}
			for v in e.vals {
				c.pcs_share_idents(v)
			}
			c.pcs_scan_share(e.update_expr)
		}
		ast.StructInit {
			c.pcs_scan_share(e.typ_expr)
			c.pcs_scan_share(e.update_expr)
			for f in e.init_fields {
				// field value stored into the struct -> retained.
				c.pcs_share_idents(f.expr)
			}
		}
		ast.StringInterLiteral {
			for v in e.exprs {
				c.pcs_scan_share(v)
			}
			for v in e.fwidth_exprs {
				c.pcs_scan_share(v)
			}
			for v in e.precision_exprs {
				c.pcs_scan_share(v)
			}
		}
		ast.IfExpr {
			c.pcs_scan_share(e.left)
			for br in e.branches {
				c.pcs_scan_share(br.cond)
				for s in br.stmts {
					c.pcs_scan_share_stmt(s)
				}
			}
		}
		ast.MatchExpr {
			c.pcs_scan_share(e.cond)
			for br in e.branches {
				for ex in br.exprs {
					c.pcs_scan_share(ex)
				}
				for s in br.stmts {
					c.pcs_scan_share_stmt(s)
				}
			}
		}
		ast.SelectExpr {
			for br in e.branches {
				c.pcs_scan_share_stmt(br.stmt)
				for s in br.stmts {
					c.pcs_scan_share_stmt(s)
				}
			}
		}
		ast.LockExpr {
			for l in e.lockeds {
				c.pcs_scan_share(l)
			}
			for s in e.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.OrExpr {
			for s in e.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.SqlExpr {
			c.pcs_scan_share(e.db_expr)
			c.pcs_scan_share(e.where_expr)
			c.pcs_scan_share(e.order_expr)
			c.pcs_scan_share(e.limit_expr)
			c.pcs_scan_share(e.offset_expr)
		}
		ast.TypeNode { c.pcs_scan_share_stmt(e.stmt) }
		// Pure leaves — no sub-expressions that can carry a heap reference.
		ast.Ident, ast.NodeError, ast.AtExpr, ast.BoolLiteral, ast.CharLiteral,
		ast.Comment, ast.ComptimeType, ast.EmptyExpr, ast.EnumVal,
		ast.FloatLiteral, ast.IntegerLiteral, ast.Nil, ast.None, ast.OffsetOf,
		ast.SqlQueryDataExpr, ast.StringLiteral {}
	}
}

// pcs_scan_share_stmt walks statements applying the sharing classifier to every
// contained expression, plus the assignment-aliasing rule: in `lhs := rhs`, if a
// heap-owning variable is defined AND the rhs reads any heap-owning variable, the
// result may alias the read value (V filter/slice/map share buffers), so BOTH the
// defined and the read heap vars are pinned shared. Exhaustive, no `else`.
fn (mut c PcsCfg) pcs_scan_share_stmt(st ast.Stmt) {
	match st {
		ast.AssignStmt {
			// Deep-drop classification: a `name := &Foo{...}` whose every heap field
			// is fresh makes `name` a deep-free candidate; any OTHER assignment to a
			// name (reassignment, non-fresh init, or a field-store base) disqualifies
			// it. (`name.f = x` has a SelectorExpr LHS → caught by deep_field_exposed
			// in pcs_scan_share above; here we cover plain `name = ...` rebinds.)
			if st.left.len == 1 && st.right.len == 1 {
				l0 := st.left[0]
				if l0 is ast.Ident {
					if c.pcs_struct_init_all_fields_fresh(st.right[0]) {
						pcs_uniq_push(mut c.deep_fresh, l0.name)
					} else {
						pcs_uniq_push(mut c.deep_nonfresh, l0.name)
					}
				}
			}
			for r in st.right {
				c.pcs_scan_share(r)
			}
			for l in st.left {
				c.pcs_scan_share(l)
			}
			mut rhs_heap := []string{}
			for r in st.right {
				mut ids := []string{}
				pcs_collect(r, mut ids)
				for id in ids {
					if id in c.heap_vars {
						pcs_uniq_push(mut rhs_heap, id)
					}
				}
			}
			mut lhs_heap := []string{}
			for i, l in st.left {
				if l is ast.Ident && i < st.left_types.len && c.pcs_is_heap_owning(st.left_types[i]) {
					pcs_uniq_push(mut lhs_heap, l.name)
				}
			}
			// Exempt provably buffer-FRESH single assignments: a string built by
			// concatenation (`a + b`) or interpolation (`'${a}${b}'`) always
			// allocates a new buffer in V — the result aliases neither operand, so
			// no aliasing pin is warranted (the operands stay droppable iff their
			// OTHER uses are non-retaining; the result is uniquely owned). This is
			// unconditionally sound: it does not touch slices/substrings (IndexExpr)
			// or array/map concat, which DO share buffers and remain pinned.
			mut fresh := st.left.len == 1 && st.right.len == 1 && st.left_types.len >= 1
				&& c.pcs_rhs_is_fresh_string(st.right[0], st.left_types[0])
			// `b := a.map(...)` / `a.filter(...)` over primitive elements yields a
			// fresh buffer that does not alias `a`, so the assign-aliasing pin must
			// not fire (`a` stays droppable; `b` is uniquely owned). See
			// pcs_is_fresh_array_call.
			if !fresh && st.right.len == 1 {
				mut e := st.right[0]
				for e is ast.ParExpr {
					e = (e as ast.ParExpr).expr
				}
				if e is ast.CallExpr && c.pcs_is_fresh_array_call(e) {
					fresh = true
				}
			}
			if !fresh && lhs_heap.len > 0 && rhs_heap.len > 0 {
				for n in lhs_heap {
					c.pcs_mark_shared(n)
				}
				for n in rhs_heap {
					c.pcs_mark_shared(n)
				}
			}
			// Field/element/pointee stores retain the RHS (a stored pointer aliases;
			// see pcs_is_store_target). Pin every heap ident on the RHS of such a
			// store — paired positionally when arities match, else all of them.
			for i, l in st.left {
				if pcs_is_store_target(l) {
					if st.left.len == st.right.len {
						c.pcs_share_idents(st.right[i])
					} else {
						for r in st.right {
							c.pcs_share_idents(r)
						}
					}
				}
			}
		}
		ast.ExprStmt { c.pcs_scan_share(st.expr) }
		ast.Return {
			for e in st.exprs {
				c.pcs_scan_share(e)
			}
		}
		ast.AssertStmt {
			c.pcs_scan_share(st.expr)
			c.pcs_scan_share(st.extra)
		}
		ast.Block {
			for s in st.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.ForStmt {
			c.pcs_scan_share(st.cond)
			for s in st.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.ForInStmt {
			// the iterated collection is borrowed across the loop and may bind
			// loop vars that alias its elements -> conservatively shared.
			c.pcs_share_idents(st.cond)
			c.pcs_scan_share(st.high)
			for s in st.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.ForCStmt {
			c.pcs_scan_share_stmt(st.init)
			c.pcs_scan_share(st.cond)
			c.pcs_scan_share_stmt(st.inc)
			for s in st.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.DeferStmt {
			for s in st.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.ComptimeFor {
			c.pcs_scan_share(st.expr)
			for s in st.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.FnDecl {
			for s in st.stmts {
				c.pcs_scan_share_stmt(s)
			}
		}
		ast.AsmStmt {
			for io in st.output {
				c.pcs_scan_share(io.expr)
			}
			for io in st.input {
				c.pcs_scan_share(io.expr)
			}
		}
		ast.HashStmt {
			for cc in st.ct_conds {
				c.pcs_scan_share(cc)
			}
		}
		ast.SqlStmt { c.pcs_scan_share(st.db_expr) }
		// Declaration / leaf statements with no local-variable uses.
		ast.BranchStmt, ast.ConstDecl, ast.DebuggerStmt, ast.EmptyStmt,
		ast.EnumDecl, ast.GlobalDecl, ast.GotoLabel, ast.GotoStmt, ast.Import,
		ast.InterfaceDecl, ast.Module, ast.NodeError, ast.SemicolonStmt,
		ast.StructDecl, ast.TypeDecl {}
	}
}

fn (mut c PcsCfg) pcs_lower_stmts(stmts []ast.Stmt, entry int) int {
	mut cur := entry
	for st in stmts {
		cur = c.pcs_lower_stmt(st, cur)
	}
	return cur
}

fn (mut c PcsCfg) pcs_lower_stmt(st ast.Stmt, entry int) int {
	mut cur := entry
	match st {
		ast.Block {
			cur = c.pcs_lower_stmts(st.stmts, cur)
		}
		ast.BranchStmt {
			if c.loop_stack.len > 0 {
				top := c.loop_stack[c.loop_stack.len - 1]
				c.pcs_edge(cur, if st.kind.str().contains('break') { top[1] } else { top[0] })
			}
			cur = c.pcs_nb()
		}
		ast.ForStmt {
			header := c.pcs_nb()
			c.pcs_edge(cur, header)
			body := c.pcs_nb()
			if c.spine_loop_pending {
				c.spine << body
				c.spine_loop_pending = false
			}
			exit := c.pcs_nb()
			c.pcs_edge(header, body)
			c.pcs_edge(header, exit)
			c.loop_stack << [header, exit]!
			be := c.pcs_lower_stmts(st.stmts, body)
			c.pcs_edge(be, header)
			c.loop_stack.pop()
			cur = exit
		}
		ast.ForInStmt {
			// Record the iterable read at the loop header so the iterated value
			// stays live across the loop region (a missing use here would let a
			// straight-line predecessor drop it early — the original soundness
			// hole). `key_var`/`val_var` are loop-local refs, not heap owners.
			mut iu := []string{}
			pcs_collect(st.cond, mut iu)
			pcs_collect(st.high, mut iu)
			header := c.pcs_nb()
			c.pcs_edge(cur, header)
			c.pcs_emit(header, st.pos.pos, [], iu)
			body := c.pcs_nb()
			if c.spine_loop_pending {
				c.spine << body
				c.spine_loop_pending = false
			}
			exit := c.pcs_nb()
			c.pcs_edge(header, body)
			c.pcs_edge(header, exit)
			c.loop_stack << [header, exit]!
			be := c.pcs_lower_stmts(st.stmts, body)
			c.pcs_edge(be, header)
			c.loop_stack.pop()
			cur = exit
		}
		ast.AssertStmt {
			mut uses := []string{}
			pcs_collect(st.expr, mut uses)
			pcs_collect(st.extra, mut uses)
			c.pcs_emit(cur, st.pos.pos, [], uses)
		}
		ast.ForCStmt {
			if st.has_init {
				cur = c.pcs_lower_stmt(st.init, cur)
			}
			header := c.pcs_nb()
			c.pcs_edge(cur, header)
			if st.has_cond {
				mut cu := []string{}
				pcs_collect(st.cond, mut cu)
				c.pcs_emit(header, st.pos.pos, [], cu)
			}
			body := c.pcs_nb()
			if c.spine_loop_pending {
				c.spine << body
				c.spine_loop_pending = false
			}
			exit := c.pcs_nb()
			c.pcs_edge(header, body)
			c.pcs_edge(header, exit)
			c.loop_stack << [header, exit]!
			mut be := c.pcs_lower_stmts(st.stmts, body)
			if st.has_inc {
				be = c.pcs_lower_stmt(st.inc, be)
			}
			c.pcs_edge(be, header)
			c.loop_stack.pop()
			cur = exit
		}
		ast.ExprStmt {
			mut uses := []string{}
			pcs_collect(st.expr, mut uses)
			c.pcs_emit(cur, st.pos.pos, [], uses)
			cur = c.pcs_lower_expr(st.expr, cur)
		}
		ast.AssignStmt {
			mut uses := []string{}
			for r in st.right {
				pcs_collect(r, mut uses)
			}
			if st.op != .decl_assign && st.op != .assign {
				for l in st.left {
					pcs_collect(l, mut uses)
				}
			}
			mut defs := []string{}
			for i, l in st.left {
				if l is ast.Ident {
					defs << l.name
					// record heap-owning locals (the only drop candidates)
					if i < st.left_types.len && c.pcs_is_heap_owning(st.left_types[i]) {
						// Pointer-typed candidates (`&Foo`) are admitted ONLY when this
						// binding owns fresh memory (`p := &Foo{...}`); a borrowed pointer
						// must never be freed. By-value heap types (array/map/string/struct
						// with a free method) carry their own ownership and are admitted
						// unconditionally — V's clone-on-store semantics keep them unaliased.
						if st.left_types[i].is_ptr() {
							if st.left.len == st.right.len && pcs_rhs_is_fresh_ref(st.right[i]) {
								pcs_uniq_push(mut c.heap_vars, l.name)
							}
						} else {
							pcs_uniq_push(mut c.heap_vars, l.name)
						}
					}
				}
			}
			c.pcs_emit(cur, st.pos.pos, defs, uses)
			for r in st.right {
				cur = c.pcs_lower_expr(r, cur)
			}
		}
		ast.Return {
			mut uses := []string{}
			for e in st.exprs {
				pcs_collect(e, mut uses)
			}
			for u in uses {
				pcs_uniq_push(mut c.returned, u)
			}
			c.pcs_emit(cur, st.pos.pos, [], uses)
			c.pcs_edge(cur, c.exit_id)
			cur = c.pcs_nb()
		}
		else {
			// Any statement kind the CFG does not model precisely: pin every
			// variable it mentions (treat as shared) so it can never be dropped
			// early. Sound by construction — unmodeled code only ever REMOVES
			// drop candidates, never licenses an unsound one.
			mut ids := []string{}
			pcs_collect_stmt(st, mut ids)
			for id in ids {
				c.pcs_mark_shared(id)
			}
		}
	}
	return cur
}

fn (mut c PcsCfg) pcs_lower_expr(ex ast.Expr, entry int) int {
	mut cur := entry
	match ex {
		ast.IfExpr {
			mut cu := []string{}
			for br in ex.branches {
				pcs_collect(br.cond, mut cu)
			}
			c.pcs_emit(cur, ex.pos.pos, [], cu)
			join := c.pcs_nb()
			for br in ex.branches {
				arm := c.pcs_nb()
				c.pcs_edge(cur, arm)
				ae := c.pcs_lower_stmts(br.stmts, arm)
				c.pcs_edge(ae, join)
			}
			if !ex.has_else {
				c.pcs_edge(cur, join)
			}
			cur = join
		}
		ast.MatchExpr {
			mut cu := []string{}
			pcs_collect(ex.cond, mut cu)
			c.pcs_emit(cur, ex.pos.pos, [], cu)
			join := c.pcs_nb()
			for br in ex.branches {
				arm := c.pcs_nb()
				c.pcs_edge(cur, arm)
				ae := c.pcs_lower_stmts(br.stmts, arm)
				c.pcs_edge(ae, join)
			}
			cur = join
		}
		else {}
	}
	return cur
}

fn (mut c PcsCfg) pcs_liveness() {
	for _ in 0 .. 10000 {
		mut changed := false
		for i := c.blocks.len - 1; i >= 0; i-- {
			mut new_out := []string{}
			for s in c.blocks[i].succ {
				for v in c.blocks[s].live_in {
					pcs_uniq_push(mut new_out, v)
				}
			}
			mut new_in := []string{}
			for v in c.blocks[i].use {
				pcs_uniq_push(mut new_in, v)
			}
			for v in new_out {
				if v !in c.blocks[i].def {
					pcs_uniq_push(mut new_in, v)
				}
			}
			if !pcs_set_eq(new_out, c.blocks[i].live_out)
				|| !pcs_set_eq(new_in, c.blocks[i].live_in) {
				changed = true
				c.blocks[i].live_out = new_out
				c.blocks[i].live_in = new_in
			}
		}
		if !changed {
			break
		}
	}
}

// pcs_drop_map: per-statement backward walk -> stmt.pos -> drop-eligible vars
// (unique, non-returned) whose last read is at that step.
fn (c &PcsCfg) pcs_drop_map() map[int][]string {
	mut dm := map[int][]string{}
	for b in c.blocks {
		mut live := b.live_out.clone()
		for i := b.steps.len - 1; i >= 0; i-- {
			st := b.steps[i]
			for u in st.use {
				if u in c.heap_vars && u !in live && u !in c.shared_vars && u !in c.returned {
					dm[st.pos] << u
				}
			}
			for d in st.def {
				if d in c.heap_vars && d !in st.use && d !in live && d !in c.shared_vars
					&& d !in c.returned {
					dm[st.pos] << d
				}
			}
			mut nl := []string{}
			for v in live {
				if v !in st.def {
					nl << v
				}
			}
			for u in st.use {
				pcs_uniq_push(mut nl, u)
			}
			live = nl.clone()
		}
	}
	return dm
}

fn pcs_or_is_propagate(o ast.OrExpr) bool {
	return o.kind == .propagate_option || o.kind == .propagate_result
}

// pcs_expr_has_exit reports whether evaluating `e` may transfer control OUT of the
// enclosing function before the next top-level statement runs: a `?`/`!` error
// propagation, or a `return`/`goto` inside an if/match used as an expression.
// Closures (AnonFn/LambdaExpr) are deliberately NOT descended — a `return` inside
// one leaves the closure, not us. This is used only to bound the always-executed
// "spine"; a conservative miss can at worst leak on the exit path (we only ever
// SUPPRESS a scope-exit free, never add one), it can never cause a UAF/double-free.
fn pcs_expr_has_exit(e ast.Expr) bool {
	match e {
		ast.CallExpr {
			if pcs_or_is_propagate(e.or_block) {
				return true
			}
			if pcs_expr_has_exit(e.left) {
				return true
			}
			for a in e.args {
				if pcs_expr_has_exit(a.expr) {
					return true
				}
			}
			for s in e.or_block.stmts {
				if pcs_stmt_has_early_exit(s) {
					return true
				}
			}
			return false
		}
		ast.SelectorExpr {
			return pcs_or_is_propagate(e.or_block) || pcs_expr_has_exit(e.expr)
		}
		ast.Ident {
			return pcs_or_is_propagate(e.or_expr)
		}
		ast.IndexExpr {
			if pcs_or_is_propagate(e.or_expr) {
				return true
			}
			if pcs_expr_has_exit(e.left) || pcs_expr_has_exit(e.index) {
				return true
			}
			for s in e.or_expr.stmts {
				if pcs_stmt_has_early_exit(s) {
					return true
				}
			}
			return false
		}
		ast.InfixExpr {
			return pcs_or_is_propagate(e.or_block) || pcs_expr_has_exit(e.left)
				|| pcs_expr_has_exit(e.right)
		}
		ast.PrefixExpr {
			return pcs_or_is_propagate(e.or_block) || pcs_expr_has_exit(e.right)
		}
		ast.ComptimeCall {
			return pcs_or_is_propagate(e.or_block)
		}
		ast.ComptimeSelector {
			return pcs_or_is_propagate(e.or_block)
		}
		ast.ParExpr { return pcs_expr_has_exit(e.expr) }
		ast.PostfixExpr { return pcs_expr_has_exit(e.expr) }
		ast.UnsafeExpr { return pcs_expr_has_exit(e.expr) }
		ast.AsCast { return pcs_expr_has_exit(e.expr) }
		ast.CastExpr { return pcs_expr_has_exit(e.expr) || pcs_expr_has_exit(e.arg) }
		ast.RangeExpr { return pcs_expr_has_exit(e.low) || pcs_expr_has_exit(e.high) }
		ast.ArrayDecompose { return pcs_expr_has_exit(e.expr) }
		ast.ConcatExpr {
			for v in e.vals {
				if pcs_expr_has_exit(v) {
					return true
				}
			}
			return false
		}
		ast.ArrayInit {
			for v in e.exprs {
				if pcs_expr_has_exit(v) {
					return true
				}
			}
			return pcs_expr_has_exit(e.len_expr) || pcs_expr_has_exit(e.cap_expr)
				|| pcs_expr_has_exit(e.init_expr)
		}
		ast.MapInit {
			for v in e.vals {
				if pcs_expr_has_exit(v) {
					return true
				}
			}
			for k in e.keys {
				if pcs_expr_has_exit(k) {
					return true
				}
			}
			return false
		}
		ast.StructInit {
			for f in e.init_fields {
				if pcs_expr_has_exit(f.expr) {
					return true
				}
			}
			return pcs_expr_has_exit(e.update_expr)
		}
		ast.StringInterLiteral {
			for v in e.exprs {
				if pcs_expr_has_exit(v) {
					return true
				}
			}
			return false
		}
		ast.IfExpr {
			if pcs_expr_has_exit(e.left) {
				return true
			}
			for br in e.branches {
				if pcs_expr_has_exit(br.cond) {
					return true
				}
				for s in br.stmts {
					if pcs_stmt_has_early_exit(s) {
						return true
					}
				}
			}
			return false
		}
		ast.MatchExpr {
			if pcs_expr_has_exit(e.cond) {
				return true
			}
			for br in e.branches {
				for ex in br.exprs {
					if pcs_expr_has_exit(ex) {
						return true
					}
				}
				for s in br.stmts {
					if pcs_stmt_has_early_exit(s) {
						return true
					}
				}
			}
			return false
		}
		ast.IfGuardExpr { return pcs_expr_has_exit(e.expr) }
		// Leaves and constructs that cannot carry a function-exit (closures excluded
		// by design): treat as exit-free. A miss here can only leak, never corrupt.
		else { return false }
	}
}

// pcs_stmt_has_early_exit reports whether `st` may exit the enclosing function
// before the following top-level statement runs (`return`/`goto`/`?`/`!`). It
// recurses through control flow (loops, blocks, defers' triggers are ignored) but
// not into closures. `break`/`continue` are NOT exits: at function scope they are
// always loop-local (a bare one is a compile error), so they cannot skip a sibling
// top-level statement. See pcs_expr_has_exit for the soundness/leak note.
fn pcs_stmt_has_early_exit(st ast.Stmt) bool {
	match st {
		ast.Return { return true }
		ast.GotoStmt { return true }
		ast.BranchStmt { return false }
		ast.ExprStmt { return pcs_expr_has_exit(st.expr) }
		ast.AssignStmt {
			for r in st.right {
				if pcs_expr_has_exit(r) {
					return true
				}
			}
			for l in st.left {
				if pcs_expr_has_exit(l) {
					return true
				}
			}
			return false
		}
		ast.AssertStmt {
			return pcs_expr_has_exit(st.expr) || pcs_expr_has_exit(st.extra)
		}
		ast.Block {
			for s in st.stmts {
				if pcs_stmt_has_early_exit(s) {
					return true
				}
			}
			return false
		}
		ast.ForStmt {
			if pcs_expr_has_exit(st.cond) {
				return true
			}
			for s in st.stmts {
				if pcs_stmt_has_early_exit(s) {
					return true
				}
			}
			return false
		}
		ast.ForInStmt {
			if pcs_expr_has_exit(st.cond) || pcs_expr_has_exit(st.high) {
				return true
			}
			for s in st.stmts {
				if pcs_stmt_has_early_exit(s) {
					return true
				}
			}
			return false
		}
		ast.ForCStmt {
			if pcs_stmt_has_early_exit(st.init) || pcs_expr_has_exit(st.cond)
				|| pcs_stmt_has_early_exit(st.inc) {
				return true
			}
			for s in st.stmts {
				if pcs_stmt_has_early_exit(s) {
					return true
				}
			}
			return false
		}
		ast.ComptimeFor {
			if pcs_expr_has_exit(st.expr) {
				return true
			}
			for s in st.stmts {
				if pcs_stmt_has_early_exit(s) {
					return true
				}
			}
			return false
		}
		// DeferStmt runs at scope exit (not an early exit of subsequent statements);
		// declaration/leaf statements cannot exit. Else => exit-free (leak-safe).
		else { return false }
	}
}

// pcs_is_simple_spine_stmt reports whether `st` is a straight-line statement that
// always runs to completion in place — a plain assignment or expression/assert with
// NO `?`/`!` propagation and no control transfer. Such statements neither split the
// basic block nor divert the enclosing loop body's per-iteration flow, so a run of
// them keeps the body's entry block always-executed-exactly-once.
// pcs_stmt_top_is_branch_expr: a top-level `if`/`match` expression as a statement's
// value splits the basic block (pcs_lower_expr creates arm/join blocks), so it is
// NOT single-block straight-line even when it cannot exit the function.
fn pcs_stmt_top_is_branch_expr(e ast.Expr) bool {
	mut x := e
	for x is ast.ParExpr {
		x = (x as ast.ParExpr).expr
	}
	return x is ast.IfExpr || x is ast.MatchExpr
}

fn pcs_is_simple_spine_stmt(st ast.Stmt) bool {
	match st {
		ast.AssignStmt {
			for r in st.right {
				if pcs_expr_has_exit(r) || pcs_stmt_top_is_branch_expr(r) {
					return false
				}
			}
			for l in st.left {
				if pcs_expr_has_exit(l) {
					return false
				}
			}
			return true
		}
		ast.ExprStmt {
			return !pcs_expr_has_exit(st.expr) && !pcs_stmt_top_is_branch_expr(st.expr)
		}
		ast.AssertStmt {
			return !pcs_expr_has_exit(st.expr) && !pcs_expr_has_exit(st.extra)
		}
		else {
			return false
		}
	}
}

// pcs_is_simple_body_loop reports whether `st` is a loop whose body is ENTIRELY
// divert-free simple statements (pcs_is_simple_spine_stmt). For such a loop the
// whole body lands in one basic block that executes exactly once per iteration with
// no early exit, so a heap local defined and last-used within an iteration (dead
// across the back-edge, per the liveness fixpoint) may be dropped each iteration —
// its per-iteration scope-exit free is suppressed 1:1. This is the minimal sound
// loop-body coverage; bodies containing branches/inner loops/breaks are left out
// (the body block would then carry conditional or post-divert steps).
fn pcs_is_simple_body_loop(st ast.Stmt) bool {
	body := match st {
		ast.ForStmt { st.stmts }
		ast.ForInStmt { st.stmts }
		ast.ForCStmt { st.stmts }
		else { return false }
	}
	if body.len == 0 {
		return false
	}
	for s in body {
		if !pcs_is_simple_spine_stmt(s) {
			return false
		}
	}
	return true
}

// pcs_lower_body_spine lowers the function-body statements like pcs_lower_stmts but
// also records the "spine": the chain of top-level blocks that always execute
// exactly once. Recording stops at the first top-level statement that may exit the
// function early — after that, no later top-level block is unconditional, so no
// drop emitted there could be matched 1:1 against a suppressed scope-exit free.
fn (mut c PcsCfg) pcs_lower_body_spine(stmts []ast.Stmt, entry int) int {
	mut cur := entry
	mut open := true
	for st in stmts {
		if open && pcs_stmt_has_early_exit(st) {
			open = false
		}
		if open && cur !in c.spine {
			c.spine << cur
		}
		// A loop reached on the spine whose body is all-simple runs its body block
		// exactly once per iteration: flag it so the loop arm records that block as
		// per-iteration spine (where the in-loop reuse / drops fire).
		if open && pcs_is_simple_body_loop(st) {
			c.spine_loop_pending = true
		}
		cur = c.pcs_lower_stmt(st, cur)
		c.spine_loop_pending = false
	}
	return cur
}

// compute_drop_map is the full-coverage analysis (all basic blocks). Kept for
// analysis/verification; emission uses the spine-restricted variant below.
pub fn compute_drop_map(fnd ast.FnDecl, mut table ast.Table) map[int][]string {
	mut c := PcsCfg{
		table: table
	}
	entry := c.pcs_nb()
	c.exit_id = c.pcs_nb()
	last := c.pcs_lower_stmts(fnd.stmts, entry)
	c.pcs_edge(last, c.exit_id)
	c.pcs_liveness()
	return c.pcs_drop_map()
}

// compute_emittable_drop_map returns the SAFE subset of drops to actually emit
// under `-d perceus -autofree`: only drops in the entry basic block (block 0),
// i.e. the function's straight-line prefix before any branch/loop/return. Every
// step in block 0 executes unconditionally and exactly once, so emitting the
// free at the drop site (and suppressing the matching scope-exit free) is
// observationally identical to scope-exit free — no double free, and no
// path-dependent leak (the failure mode of suppressing an unconditional
// scope-exit free while only dropping on some branches). Branch/loop coverage is
// future work, each widening separately re-gated (G-DIFF + G-LEAK).
pub fn compute_emittable_drop_map(fnd ast.FnDecl, mut table ast.Table, escapes map[string][]bool) (map[int][]string, map[string]bool) {
	mut c := PcsCfg{
		table:     table
		interproc: true
		escapes:   escapes
	}
	entry := c.pcs_nb()
	c.exit_id = c.pcs_nb()
	last := c.pcs_lower_body_spine(fnd.stmts, entry)
	c.pcs_edge(last, c.exit_id)
	// Uniqueness/aliasing classifier over the whole body. Runs AFTER lowering so
	// `heap_vars` is populated (the assign-aliasing rule needs it). Marks shared
	// every heap value whose buffer may be aliased/retained; only the residual
	// (provably uniquely owned) heap locals remain drop candidates. The same
	// exhaustive pass also records the deep-drop classification (see PcsCfg).
	for st in fnd.stmts {
		c.pcs_scan_share_stmt(st)
	}
	c.pcs_liveness()
	return c.pcs_spine_drop_map(), c.pcs_deep_drop_set()
}

// pcs_spine_drop_map: pcs_drop_map restricted to the always-executed spine blocks
// (entry + the top-level blocks before any early exit). Every step in a spine block
// runs exactly once on every execution, so a drop emitted there (and its matching
// scope-exit free suppressed) is observationally identical to the scope-exit free —
// no double free, and no path-dependent leak (the failure mode of suppressing an
// unconditional scope-exit free while only dropping on some branches). A variable
// whose last use lands in a non-spine block (inside a branch/loop, or after an early
// exit) is simply left to scope-exit autofree — sound, never dropped early.
fn (c &PcsCfg) pcs_spine_drop_map() map[int][]string {
	mut dm := map[int][]string{}
	for bi, b in c.blocks {
		if bi !in c.spine {
			continue
		}
		mut live := b.live_out.clone()
		for i := b.steps.len - 1; i >= 0; i-- {
			st := b.steps[i]
			for u in st.use {
				if u in c.heap_vars && u !in live && u !in c.shared_vars && u !in c.returned {
					if u !in dm[st.pos] {
						dm[st.pos] << u
					}
				}
			}
			for d in st.def {
				if d in c.heap_vars && d !in st.use && d !in live && d !in c.shared_vars
					&& d !in c.returned {
					if d !in dm[st.pos] {
						dm[st.pos] << d
					}
				}
			}
			mut nl := []string{}
			for v in live {
				if v !in st.def {
					nl << v
				}
			}
			for u in st.use {
				pcs_uniq_push(mut nl, u)
			}
			live = nl.clone()
		}
	}
	return dm
}

fn pcs_bool_eq(a []bool, b []bool) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// pcs_collect_fn_decls gathers every top-level FnDecl across all files. Methods
// are top-level FnDecls (so they are included); anonymous fns are expression-level
// and are handled by the local classifier's AnonFn arm (their captures are pinned),
// so they are not separately summarised here.
fn pcs_collect_fn_decls(files []&ast.File) []ast.FnDecl {
	mut out := []ast.FnDecl{}
	for f in files {
		for st in f.stmts {
			if st is ast.FnDecl {
				out << st
			}
		}
	}
	return out
}

// pcs_fn_param_escape computes one function's parameter-escape vector under the
// CURRENT round's summaries (read through `env`). A parameter escapes iff, after
// the local sharing classifier has run over the whole body, its name is marked
// shared (its buffer may be aliased/retained) OR it appears in a return
// expression. Heap-owning parameters are seeded into `heap_vars` so the
// assignment-aliasing rule fires on them too — this is what closes the
// `y := param; return y` alias-return hole (the assignment pins `param` the moment
// it is copied into a heap local that may later escape).
fn (env &PcsEscapeEnv) pcs_fn_param_escape(fnd ast.FnDecl) []bool {
	mut c := PcsCfg{
		table:     env.table
		interproc: true
		escapes:   env.fns
	}
	for p in fnd.params {
		if c.pcs_is_heap_owning(p.typ) {
			pcs_uniq_push(mut c.heap_vars, p.name)
		}
	}
	entry := c.pcs_nb()
	c.exit_id = c.pcs_nb()
	last := c.pcs_lower_stmts(fnd.stmts, entry)
	c.pcs_edge(last, c.exit_id)
	for st in fnd.stmts {
		c.pcs_scan_share_stmt(st)
	}
	mut esc := []bool{len: fnd.params.len}
	for i, p in fnd.params {
		esc[i] = p.name in c.shared_vars || p.name in c.returned
	}
	return esc
}

// build_escape_summaries runs the interprocedural escape fixpoint over the whole
// program and returns the final `fkey() -> per-parameter escape` map consumed by
// the call rule (pcs_call_escape) during per-function drop analysis.
//
// The iteration is ASCENDING from "nothing escapes": a parameter flips false->true
// only when evidence appears (a local escaping use, or being passed to a callee
// slot that is itself escaping under the current summaries), and never flips back.
// That makes it monotone over a finite domain, so it is the LEAST — i.e. most
// precise — sound fixpoint, and it terminates. (Mutual recursion such as
// `a(x){b(x)}` / `b(y){a(y)}` correctly settles with both params non-escaping,
// which a descending/greatest-fixpoint formulation would miss.)
//
// Functions we cannot soundly summarise — non-V (C/JS), no-body, or generic — are
// simply omitted, so their callers see "no summary" and conservatively pin every
// argument. The round cap is a pure safety backstop: real programs converge in a
// few rounds; if it is ever exhausted we fall back to all-escaping (sound).
pub fn build_escape_summaries(files []&ast.File, mut table ast.Table) map[string][]bool {
	decls := pcs_collect_fn_decls(files)
	mut summarizable := []ast.FnDecl{}
	for fnd in decls {
		if fnd.language != .v || fnd.no_body || fnd.generic_names.len > 0 {
			continue
		}
		summarizable << fnd
	}
	mut env := PcsEscapeEnv{
		table: table
	}
	// Seed every summarizable function with all-parameters-non-escaping (false).
	for fnd in summarizable {
		env.fns[fnd.fkey()] = []bool{len: fnd.params.len}
	}
	max_rounds := 200
	for round in 0 .. max_rounds {
		mut changed := false
		mut next := env.fns.clone()
		for fnd in summarizable {
			key := fnd.fkey()
			esc := env.pcs_fn_param_escape(fnd)
			if !pcs_bool_eq(esc, next[key]) {
				next[key] = esc
				changed = true
			}
		}
		env.fns = next.clone()
		if !changed {
			break
		}
		if round == max_rounds - 1 {
			// Pathological non-convergence: drop to the conservative top.
			for fnd in summarizable {
				env.fns[fnd.fkey()] = []bool{len: fnd.params.len, init: true}
			}
		}
	}
	return env.fns
}
