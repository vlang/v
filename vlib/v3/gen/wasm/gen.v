module wasm

import os
import v3.flat
import v3.types

// gen.v is the v3 WebAssembly backend. Like the C backend's FlatGen it walks
// the flat AST directly (WASM has structured control flow, so no SSA/relooping
// is needed), and like the arm64 backend it emits a binary module. Generics,
// strings-as-values, structs, arrays and maps are out of scope for now; the
// backend targets the integer/float core plus WASI `print`/`println`.

// Fixed low-memory scratch layout (bytes):
//   [0..3]   nwritten result for fd_write
//   [4..11]  iovec (ptr @4, len @8)
//   [12]     newline byte '\n'
//   [16..47] itoa scratch buffer (BUF_END = 48, filled back-to-front)
//   [64..]   interned string-literal data
const nwritten_ptr = 0
const iov_ptr = 4
const nl_ptr = 12
const buf_end = 48
const data_base = 64

pub enum WType {
	void
	i32
	i64
	f32
	f64
}

struct FnInfo {
	name    string
	node_id flat.NodeId
	module  string
	file    string
	params  []WType
	ret     WType
	index   int
}

struct Frame {
	tag   FrameTag
	label string // loop label for `break label` / `continue label`, else ''
}

// VarScope snapshots the local-name bindings so an inner block or for-loop can
// shadow an outer local and have the outer binding restored on scope exit.
struct VarScope {
	index    map[string]int
	wtype    map[string]WType
	unsigned map[string]bool
	widths   map[string]int
}

enum FrameTag {
	plain
	brk
	cont
}

@[heap]
pub struct Gen {
mut:
	a        &flat.FlatAst      = unsafe { nil }
	tc       &types.TypeChecker = unsafe { nil }
	used_fns map[string]bool
	mod      &Module = unsafe { nil }
	// per-function state
	cur           Code
	local_types   []u8
	nparams       int
	var_index     map[string]int
	var_wtype     map[string]WType
	var_unsigned  map[string]bool
	var_widths    map[string]int // sub-32-bit int locals: 8 or 16; else 32
	frames        []Frame
	pending_label string // label of a preceding `label:` marker, for the next loop
	cur_ret       WType
	cur_fn_module string
	cur_fn_file   string
	// module-wide; keyed by qualified function name (see qualified_fn_key)
	fn_index        map[string]int
	fn_ret          map[string]WType
	fn_params       map[string][]WType
	file_aliases    map[string]map[string]string // file -> (import alias -> full import path)
	import_paths    []string                     // every `import a.b.c` path (full)
	module_imports  map[string][]string          // module path -> imported module paths ('' = main)
	global_index    map[string]int               // __global name -> wasm global index
	global_wtype    map[string]WType
	global_unsigned map[string]bool
	global_widths   map[string]int
	data_pool       []u8
	data_off        map[string]int
	uses_print      bool
	write_idx       int
	print_int_idx   int
	has_main        bool
	init_fns        []int // function indices of init() entry points, in call order
	warnings        []string
}

pub fn Gen.new(a &flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) &Gen {
	return &Gen{
		a:        a
		tc:       tc
		used_fns: used_fns
		mod:      Module.new()
	}
}

// gen builds the whole module from the flat AST.
pub fn (mut g Gen) gen() {
	g.collect_imports()
	g.collect_globals()
	user_fns := g.collect_user_fns()
	g.uses_print = g.detect_print(user_fns)

	// 1. function-index assignment (imports first, then helpers, then user fns).
	mut fd_write_idx := -1
	if g.uses_print {
		ft := g.mod.add_type([valtype_i32, valtype_i32, valtype_i32, valtype_i32], [
			valtype_i32,
		])
		fd_write_idx = g.mod.add_import_func('wasi_snapshot_preview1', 'fd_write', ft)
	}
	mut defined := 0
	if g.uses_print {
		g.write_idx = g.mod.reserve_func_index(defined)
		defined++
		g.print_int_idx = g.mod.reserve_func_index(defined)
		defined++
	}
	mut module_init := map[string]string{} // module path -> its init fn key
	for i, f in user_fns {
		idx := g.mod.reserve_func_index(defined + i)
		key := qualified_fn_key(f.module, f.name)
		g.fn_index[key] = idx
		g.fn_ret[key] = f.ret
		g.fn_params[key] = f.params
		if f.name == 'main' && (f.module == '' || f.module == 'main') {
			g.has_main = true
		} else if f.name == 'init' && f.params.len == 0 {
			module_init[f.module] = key
		}
	}
	// Order init calls by the import graph (dependencies first), so a module's
	// init observes its imports' startup; main's init runs last.
	mut visited := map[string]bool{}
	g.order_module_inits('', module_init, mut visited)

	// 2. helper bodies (must be added in the same order as reserved above).
	if g.uses_print {
		g.emit_write_helper(fd_write_idx)
		g.emit_print_int_helper()
	}

	// 3. user function bodies.
	for f in user_fns {
		g.emit_user_fn(f)
	}

	// 4. WASI `_start` entry that calls main.
	mut start_idx := -1
	if g.has_main {
		start_idx = g.emit_start()
	}

	// 5. exports + memory sizing + data. WASM export names must be unique, so
	//    reserve the runtime names and skip any user function that would clash.
	g.mod.add_export('memory', export_mem, 0)
	mut used_exports := map[string]bool{}
	used_exports['memory'] = true
	used_exports['_start'] = true
	for f in user_fns {
		ename := export_fn_name(f.module, f.name)
		if ename in used_exports {
			g.warn('not exporting ${f.name}: export name `${ename}` is already in use')
			continue
		}
		used_exports[ename] = true
		g.mod.add_export(ename, export_func, g.fn_index[qualified_fn_key(f.module, f.name)])
	}
	if start_idx >= 0 {
		g.mod.add_export('_start', export_func, start_idx)
	}
	if g.data_pool.len > 0 {
		g.mod.add_data(data_base, g.data_pool)
	}
	if g.uses_print {
		g.mod.add_data(nl_ptr, [u8(0x0a)])
	}
	total := data_base + g.data_pool.len
	pages := if total <= 0x10000 { 2 } else { (total + 0xffff) / 0x10000 + 1 }
	g.mod.set_mem_min(pages)
}

pub fn (g &Gen) warnings_list() []string {
	return g.warnings
}

// write serializes the module to `path`.
pub fn (mut g Gen) write(path string) ! {
	bytes := g.mod.compile()
	os.write_file_array(path, bytes)!
}

// ---- function collection ----

fn (mut g Gen) collect_user_fns() []FnInfo {
	// 1. Gather every candidate (primitive-signature, non-generic) function in
	//    user code, keyed by qualified name, preserving source order.
	mut candidates := map[string]FnInfo{}
	mut order := []string{}
	mut cur_module := ''
	mut cur_file := ''
	mut cur_module_path := ''
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		match node.kind {
			.file {
				cur_module = ''
				cur_file = node.value
				cur_module_path = ''
			}
			.module_decl {
				cur_module = node.value
				// Only files that declare a non-main module are imported modules;
				// resolve their import path. Main files (no decl or `module main`)
				// must never be classified by a directory-name coincidence.
				cur_module_path = if cur_module != '' && cur_module != 'main' {
					g.module_path_for_file(cur_file)
				} else {
					''
				}
			}
			.fn_decl {
				if i < g.a.user_code_start {
					continue
				}
				if node.generic_params.len > 0 || node.value == '' {
					continue
				}
				// Prefer the import path (distinguishes same-leaf modules); fall
				// back to the module declaration name if no import matched.
				mod_key := if cur_module_path != '' {
					cur_module_path
				} else if cur_module != '' && cur_module != 'main' {
					cur_module
				} else {
					''
				}
				if fi := g.fn_info(node, flat.NodeId(i), mod_key, cur_file) {
					key := qualified_fn_key(mod_key, node.value)
					if key !in candidates {
						candidates[key] = fi
						order << key
					}
				}
			}
			else {}
		}
	}

	// 2. Seed with every main-module candidate (all of them are compiled and
	//    exported), then BFS-follow direct calls into imported-module
	//    candidates. This deliberately ignores markused reachability: builtins
	//    such as strconv__format_int are intercepted by the print path and
	//    never emitted as real calls, so their numeric helpers must not be
	//    pulled in.
	mut reached := map[string]bool{}
	mut work := []string{}
	for key in order {
		f := candidates[key]
		if f.module == '' || f.module == 'main' {
			reached[key] = true
			work << key
		}
	}
	// `init` functions are entry points (run before main), like the C path's
	// _vinit. Every imported module runs its init regardless of whether any of
	// its other functions are called, so seed the init of main and of every
	// imported module, then follow the calls those inits make.
	mut active := map[string]bool{}
	active[''] = true
	for p in g.import_paths {
		active[p] = true
	}
	for key in order {
		f := candidates[key]
		if key !in reached && f.name == 'init' && f.params.len == 0 && f.module in active {
			reached[key] = true
			work << key
		}
	}
	for work.len > 0 {
		f := candidates[work.pop()]
		for callee_key in g.called_candidate_keys(f, candidates) {
			if callee_key !in reached {
				reached[callee_key] = true
				work << callee_key
			}
		}
	}

	// 3. Emit reached functions in source order for deterministic indices.
	mut out := []FnInfo{}
	for key in order {
		if reached[key] {
			out << candidates[key]
		}
	}
	return out
}

// called_candidate_keys returns the qualified keys of the candidate functions
// directly called inside f's body (calls to non-candidates such as intercepted
// builtins are ignored).
fn (g &Gen) called_candidate_keys(f FnInfo, candidates map[string]FnInfo) []string {
	mut keys := []string{}
	g.collect_call_keys(g.a.nodes[int(f.node_id)], f.module, f.file, candidates, mut keys)
	return keys
}

fn (g &Gen) collect_call_keys(node flat.Node, cur_mod string, cur_file string, candidates map[string]FnInfo, mut keys []string) {
	if node.kind == .call && node.children_count > 0 {
		for key in g.resolve_call_keys(g.a.child_node(&node, 0), cur_mod, cur_file) {
			if key in candidates {
				keys << key
				break
			}
		}
	}
	for i in 0 .. node.children_count {
		cid := g.a.child(&node, i)
		if int(cid) >= 0 {
			g.collect_call_keys(g.a.nodes[int(cid)], cur_mod, cur_file, candidates, mut keys)
		}
	}
}

// fn_info extracts a compilable signature, or none if the function uses types
// the backend does not handle yet (strings, structs, arrays, pointers, ...).
fn (mut g Gen) fn_info(node flat.Node, id flat.NodeId, module_ string, file string) ?FnInfo {
	mut params := []WType{}
	for i in 0 .. node.children_count {
		p := g.a.child_node(&node, i)
		if p.kind != .param || p.value.len == 0 {
			continue
		}
		pt := g.tc.parse_type(p.typ)
		w := prim_wtype(pt) or { return none }
		params << w
	}
	mut ret := WType.void
	if node.typ.len > 0 && node.typ != 'void' {
		rt := g.tc.parse_type(node.typ)
		ret = prim_wtype(rt) or { return none }
	}
	return FnInfo{
		name:    node.value
		node_id: id
		module:  module_
		file:    file
		params:  params
		ret:     ret
	}
}

fn (mut g Gen) detect_print(fns []FnInfo) bool {
	for f in fns {
		node := g.a.nodes[int(f.node_id)]
		if g.subtree_has_print(node) {
			return true
		}
	}
	return false
}

fn (g &Gen) subtree_has_print(node flat.Node) bool {
	if node.kind == .call {
		callee := g.a.child_node(&node, 0)
		if callee.kind == .ident && callee.value in ['println', 'print', 'eprintln', 'eprint'] {
			return true
		}
	}
	for i in 0 .. node.children_count {
		child_id := g.a.child(&node, i)
		if int(child_id) < 0 {
			continue
		}
		if g.subtree_has_print(g.a.nodes[int(child_id)]) {
			return true
		}
	}
	return false
}

// ---- user function emission ----

fn (mut g Gen) emit_user_fn(f FnInfo) {
	node := g.a.nodes[int(f.node_id)]
	g.cur = Code{}
	g.local_types = []u8{}
	g.var_index = map[string]int{}
	g.var_wtype = map[string]WType{}
	g.var_unsigned = map[string]bool{}
	g.var_widths = map[string]int{}
	g.frames = []Frame{}
	g.pending_label = ''
	g.cur_ret = f.ret
	g.cur_fn_module = f.module
	g.cur_fn_file = f.file

	// register params as locals 0..n-1
	mut pi := 0
	mut wparams := []u8{}
	for i in 0 .. node.children_count {
		p := g.a.child_node(&node, i)
		if p.kind != .param || p.value.len == 0 {
			continue
		}
		w := f.params[pi]
		pt := g.tc.parse_type(p.typ)
		g.var_index[p.value] = pi
		g.var_wtype[p.value] = w
		g.var_unsigned[p.value] = type_is_unsigned(pt)
		g.var_widths[p.value] = narrow_width(pt)
		wparams << wt_valtype(w)
		pi++
	}
	g.nparams = pi

	// body: non-param children
	for i in 0 .. node.children_count {
		child := g.a.child_node(&node, i)
		if child.kind == .param {
			continue
		}
		g.gen_stmt(g.a.child(&node, i))
	}
	// A non-void function returns via explicit `return`s; if control still
	// reaches the end (e.g. the body is `if c { return ... } else { return
	// ... }`), the fall-through is unreachable. `unreachable` keeps the stack
	// type-valid without inventing a bogus result value.
	if f.ret != .void {
		g.cur.raw(0x00) // unreachable
	}
	g.cur.end()

	mut results := []u8{}
	if f.ret != .void {
		results << wt_valtype(f.ret)
	}
	type_idx := g.mod.add_type(wparams, results)
	g.mod.add_func(type_idx, g.local_types, g.cur.bytes)
}

fn (mut g Gen) emit_start() int {
	mut c := Code{}
	// Run init() entry points before main, matching the C path's _vinit.
	for init_idx in g.init_fns {
		c.call(init_idx)
	}
	if main_idx := g.fn_index['main'] {
		c.call(main_idx)
	}
	c.end()
	type_idx := g.mod.add_type([], [])
	return g.mod.add_func(type_idx, [], c.bytes)
}

// new_local allocates a fresh local beyond the params and records its type.
fn (mut g Gen) new_local(name string, w WType, unsigned bool, width int) int {
	idx := g.nparams + g.local_types.len
	g.local_types << wt_valtype(w)
	g.var_index[name] = idx
	g.var_wtype[name] = w
	g.var_unsigned[name] = unsigned
	g.var_widths[name] = width
	return idx
}

// alloc_temp allocates an anonymous function-level local (e.g. for the shift
// over-width guard) and returns its index.
fn (mut g Gen) alloc_temp(w WType) int {
	idx := g.nparams + g.local_types.len
	g.local_types << wt_valtype(w)
	return idx
}

// snapshot_scope / restore_scope save and restore local-name bindings so inner
// declarations that shadow an outer local do not leak past their scope.
fn (g &Gen) snapshot_scope() VarScope {
	return VarScope{
		index:    g.var_index.clone()
		wtype:    g.var_wtype.clone()
		unsigned: g.var_unsigned.clone()
		widths:   g.var_widths.clone()
	}
}

fn (mut g Gen) restore_scope(s VarScope) {
	g.var_index = s.index.clone()
	g.var_wtype = s.wtype.clone()
	g.var_unsigned = s.unsigned.clone()
	g.var_widths = s.widths.clone()
}

// ---- statements ----

fn (mut g Gen) gen_stmt(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	node := g.a.nodes[int(id)]
	if node.kind == .label_stmt {
		// `outer:` marker; attach the label to the next loop. Goto-only markers
		// (outer_break/outer_continue) aren't consumed by a loop and are dropped.
		g.pending_label = node.value
		return
	}
	loop_label := g.pending_label
	g.pending_label = ''
	match node.kind {
		.fn_decl, .c_fn_decl {}
		.block {
			saved := g.snapshot_scope()
			for i in 0 .. node.children_count {
				g.gen_stmt(g.a.child(&node, i))
			}
			g.restore_scope(saved)
		}
		.expr_stmt {
			child_id := g.a.child(&node, 0)
			w := g.gen_expr(child_id)
			if w != .void {
				g.cur.drop()
			}
		}
		.decl_assign {
			g.gen_decl_assign(node)
		}
		.assign, .selector_assign {
			g.gen_assign(node)
		}
		.return_stmt {
			if node.children_count > 0 {
				g.gen_expr_as(g.a.child(&node, 0), g.cur_ret)
			}
			g.cur.ret()
		}
		.if_expr {
			g.gen_if(node)
		}
		.for_stmt {
			g.gen_for(node, loop_label)
		}
		.break_stmt {
			g.gen_branch(node, .brk)
		}
		.continue_stmt {
			g.gen_branch(node, .cont)
		}
		.assert_stmt {
			g.gen_assert(node)
		}
		else {
			g.warn('unsupported statement: ${node.kind}')
		}
	}
}

fn (mut g Gen) gen_decl_assign(node flat.Node) {
	// Multi-declaration (`x, y := a, b`): evaluate every RHS against the outer
	// scope before binding any new name, so a shadowing `x, y := x+1, x+2`
	// reads the outer x for both initializers.
	if node.children_count > 2 {
		g.gen_multi_decl(node)
		return
	}
	lhs := g.a.child_node(&node, 0)
	rhs_id := g.a.child(&node, 1)
	w := g.expr_wtype(rhs_id, node.typ)
	uns := g.decl_is_unsigned(rhs_id, node.typ)
	width := g.decl_width(rhs_id, node.typ)
	if lhs.kind == .ident {
		// Emit the initializer before binding the new name, so a shadowing
		// `x := x + 1` reads the outer x rather than the fresh zero local.
		g.gen_expr_as(rhs_id, w)
		idx := g.new_local(lhs.value, w, uns, width)
		g.narrow_for_local(lhs.value)
		g.cur.local_set(idx)
	}
}

fn (mut g Gen) gen_multi_decl(node flat.Node) {
	mut temps := []int{}
	mut names := []string{}
	mut ws := []WType{}
	mut unss := []bool{}
	mut widths := []int{}
	// Phase 1: buffer every RHS into a temporary (outer bindings still in scope).
	mut i := 0
	for i + 1 < node.children_count {
		lhs := g.a.child_node(&node, i)
		rhs_id := g.a.child(&node, i + 1)
		if lhs.kind == .ident {
			w := g.expr_wtype(rhs_id, node.typ)
			g.gen_expr_as(rhs_id, w)
			t := g.alloc_temp(w)
			g.cur.local_set(t)
			temps << t
			names << lhs.value
			ws << w
			unss << g.decl_is_unsigned(rhs_id, node.typ)
			widths << g.decl_width(rhs_id, node.typ)
		}
		i += 2
	}
	// Phase 2: bind each new local and store its buffered value.
	for k, name in names {
		idx := g.new_local(name, ws[k], unss[k], widths[k])
		g.cur.local_get(temps[k])
		g.narrow_for_local(name)
		g.cur.local_set(idx)
	}
}

fn (mut g Gen) gen_assign(node flat.Node) {
	// Multi-target assignment (`a, b = b, a`): V evaluates every RHS before any
	// store, so buffer them in temporaries first. The transformer normally
	// pre-decomposes these into temp-backed single assigns, so this path is a
	// safeguard rather than the common case.
	if node.op == .assign && node.children_count > 2 {
		g.gen_multi_assign(node)
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs := g.a.child_node(&node, i)
		rhs_id := g.a.child(&node, i + 1)
		if lhs.kind == .ident && lhs.value == '_' {
			// Blank assignment `_ = expr`: evaluate for side effects, discard.
			w := g.gen_expr(rhs_id)
			if w != .void {
				g.cur.drop()
			}
		} else if lhs.kind == .ident && lhs.value in g.var_index {
			idx := g.var_index[lhs.value]
			w := g.var_wtype[lhs.value]
			if node.op == .assign {
				g.gen_expr_as(rhs_id, w)
			} else {
				op := compound_to_op(node.op)
				signed := !g.var_unsigned[lhs.value]
				g.cur.local_get(idx)
				if op in [.left_shift, .right_shift, .right_shift_unsigned] {
					// Keep the lhs width; the count may be wider (`x <<= u64(n)`).
					if op == .right_shift_unsigned && w == .i32 {
						// `>>>=` shifts the narrow bit pattern; a narrow signed local
						// is stored sign-extended, so mask it to its width first.
						lw := g.var_widths[lhs.value]
						if lw == 8 || lw == 16 {
							g.emit_narrow(lw, true)
						}
					}
					g.emit_shift_with_count(op, w, rhs_id, signed)
				} else {
					g.gen_expr_as(rhs_id, w)
					g.emit_arith(op, w, signed)
				}
			}
			g.narrow_for_local(lhs.value)
			g.cur.local_set(idx)
		} else if lhs.kind == .ident && g.global_key(lhs.value) != '' {
			gkey := g.global_key(lhs.value)
			gidx := g.global_index[gkey]
			w := g.global_wtype[gkey]
			if node.op == .assign {
				g.gen_expr_as(rhs_id, w)
			} else {
				op := compound_to_op(node.op)
				signed := !g.global_unsigned[gkey]
				g.cur.global_get(gidx)
				if op in [.left_shift, .right_shift, .right_shift_unsigned] {
					if op == .right_shift_unsigned && w == .i32 {
						lw := g.global_widths[gkey]
						if lw == 8 || lw == 16 {
							g.emit_narrow(lw, true)
						}
					}
					g.emit_shift_with_count(op, w, rhs_id, signed)
				} else {
					g.gen_expr_as(rhs_id, w)
					g.emit_arith(op, w, signed)
				}
			}
			g.narrow_for_global(gkey)
			g.cur.global_set(gidx)
		} else {
			g.warn('unsupported assign target')
		}
		i += 2
	}
}

// gen_multi_assign evaluates all RHS values into temporaries first, then stores
// each into its target, so `a, b = b, a` swaps instead of clobbering.
fn (mut g Gen) gen_multi_assign(node flat.Node) {
	mut temps := []int{}
	mut targets := []string{} // local name, or global key when is_global
	mut is_global := []bool{}
	mut j := 0
	for j + 1 < node.children_count {
		lhs := g.a.child_node(&node, j)
		rhs_id := g.a.child(&node, j + 1)
		if lhs.kind == .ident && lhs.value != '_' && lhs.value in g.var_index {
			w := g.var_wtype[lhs.value]
			g.gen_expr_as(rhs_id, w)
			t := g.alloc_temp(w)
			g.cur.local_set(t)
			temps << t
			targets << lhs.value
			is_global << false
		} else if lhs.kind == .ident && lhs.value != '_' && g.global_key(lhs.value) != '' {
			gkey := g.global_key(lhs.value)
			w := g.global_wtype[gkey]
			g.gen_expr_as(rhs_id, w)
			t := g.alloc_temp(w)
			g.cur.local_set(t)
			temps << t
			targets << gkey
			is_global << true
		} else {
			// blank or unsupported target: evaluate for side effects, discard
			rw := g.gen_expr(rhs_id)
			if rw != .void {
				g.cur.drop()
			}
		}
		j += 2
	}
	for k, name in targets {
		g.cur.local_get(temps[k])
		if is_global[k] {
			g.narrow_for_global(name)
			g.cur.global_set(g.global_index[name])
		} else {
			g.narrow_for_local(name)
			g.cur.local_set(g.var_index[name])
		}
	}
}

// narrow_for_local masks/sign-extends the value on the stack to the local's
// declared sub-32-bit width before it is stored.
fn (mut g Gen) narrow_for_local(name string) {
	width := g.var_widths[name]
	if width == 8 || width == 16 {
		g.emit_narrow(width, g.var_unsigned[name])
	}
}

fn (mut g Gen) narrow_for_global(key string) {
	width := g.global_widths[key]
	if width == 8 || width == 16 {
		g.emit_narrow(width, g.global_unsigned[key])
	}
}

// global_key resolves a `__global` referenced by bare name from the current
// function to its metadata key, preferring a same-module global and falling
// back to a main-module one, mirroring resolve_call_keys. Globals are keyed by
// module-qualified name, so two modules declaring the same name stay distinct.
// Returns '' when no such global exists.
fn (g &Gen) global_key(name string) string {
	if g.cur_fn_module != '' && g.cur_fn_module != 'main' {
		k := '${g.cur_fn_module}.${name}'
		if k in g.global_index {
			return k
		}
	}
	if name in g.global_index {
		return name
	}
	return ''
}

// decl_width resolves the declared sub-32-bit width (8/16) of an initializer.
fn (mut g Gen) decl_width(rhs_id flat.NodeId, fallback_typ string) int {
	w := narrow_width(g.tc.resolve_type(rhs_id))
	if w != 32 {
		return w
	}
	if fallback_typ.len > 0 {
		return narrow_width(g.tc.parse_type(fallback_typ))
	}
	return 32
}

fn (mut g Gen) gen_if(node flat.Node) {
	cond_id := g.a.child(&node, 0)
	g.gen_expr_as_bool(cond_id)
	g.cur.if_void()
	g.frames << Frame{
		tag: .plain
	}
	then_block := g.a.child_node(&node, 1)
	saved_then := g.snapshot_scope()
	for i in 0 .. then_block.children_count {
		g.gen_stmt(g.a.child(then_block, i))
	}
	g.restore_scope(saved_then)
	if node.children_count > 2 {
		else_id := g.a.child(&node, 2)
		if int(else_id) >= 0 {
			else_node := g.a.nodes[int(else_id)]
			g.cur.else_()
			saved_else := g.snapshot_scope()
			if else_node.kind == .if_expr {
				g.gen_if(else_node)
			} else if else_node.kind == .block {
				for i in 0 .. else_node.children_count {
					g.gen_stmt(g.a.child(&else_node, i))
				}
			}
			g.restore_scope(saved_else)
		}
	}
	g.cur.end()
	g.frames.pop()
}

fn (mut g Gen) gen_for(node flat.Node, label string) {
	// The initializer and body are scoped to the loop; restore outer bindings
	// afterwards so a shadowing `for i := ...` or body-local does not leak.
	saved := g.snapshot_scope()
	defer {
		g.restore_scope(saved)
	}
	init_node := g.a.child_node(&node, 0)
	cond_id := g.a.child(&node, 1)
	cond_node := g.a.nodes[int(cond_id)]
	post_node := g.a.child_node(&node, 2)

	if init_node.kind != .empty {
		g.gen_stmt(g.a.child(&node, 0))
	}
	// Bindings visible to the condition and post: the loop variable from the
	// initializer, but no body-local shadows.
	header_scope := g.snapshot_scope()
	g.cur.block_void() // break target
	g.frames << Frame{
		tag:   .brk
		label: label
	}
	g.cur.loop_void() // back-edge target
	g.frames << Frame{
		tag: .plain
	}

	if cond_node.kind != .empty {
		g.gen_expr_as_bool(cond_id)
		g.cur.raw(0x45) // i32.eqz
		g.cur.br_if(g.depth_to(.brk))
	}

	g.cur.block_void() // continue target
	g.frames << Frame{
		tag:   .cont
		label: label
	}
	for i in 3 .. node.children_count {
		g.gen_stmt(g.a.child(&node, i))
	}
	g.cur.end()
	g.frames.pop()

	// Drop body-local shadows so the post statement rebinds to the loop var.
	g.restore_scope(header_scope)
	if post_node.kind != .empty {
		g.gen_stmt(g.a.child(&node, 2))
	}
	g.cur.br(g.depth_to_loop())
	g.cur.end() // loop
	g.frames.pop()
	g.cur.end() // block
	g.frames.pop()
}

fn (mut g Gen) gen_branch(node flat.Node, tag FrameTag) {
	if node.value.len > 0 {
		// `break label` / `continue label`: target the labeled loop's frame.
		g.cur.br(g.depth_to_labeled(tag, node.value))
	} else {
		g.cur.br(g.depth_to(tag))
	}
}

fn (mut g Gen) gen_assert(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	g.gen_expr_as_bool(g.a.child(&node, 0))
	g.cur.raw(0x45) // i32.eqz
	g.cur.if_void()
	g.cur.raw(0x00) // unreachable -> trap
	g.cur.end()
}

// depth_to returns the relative br depth to the nearest enclosing frame tagged
// `tag`, counted from the current (innermost) frame.
fn (g &Gen) depth_to(tag FrameTag) int {
	for i := g.frames.len - 1; i >= 0; i-- {
		if g.frames[i].tag == tag {
			return (g.frames.len - 1) - i
		}
	}
	return 0
}

// depth_to_labeled finds the nearest frame with the given tag and loop label
// (for `break label` / `continue label`), falling back to the nearest tag.
fn (g &Gen) depth_to_labeled(tag FrameTag, label string) int {
	for i := g.frames.len - 1; i >= 0; i-- {
		if g.frames[i].tag == tag && g.frames[i].label == label {
			return (g.frames.len - 1) - i
		}
	}
	return g.depth_to(tag)
}

// depth_to_loop returns the depth of the nearest `.plain` loop frame. Loop and
// if frames are both `.plain`; the back-edge target is the most recent one that
// is not the continue block, which by construction is the loop directly below
// the continue frame. We resolve it as the nearest `.plain` frame.
fn (g &Gen) depth_to_loop() int {
	for i := g.frames.len - 1; i >= 0; i-- {
		if g.frames[i].tag == .plain {
			return (g.frames.len - 1) - i
		}
	}
	return 0
}

// ---- expressions ----

// gen_expr emits code that pushes the value of `id` and returns its WType
// (`.void` if nothing was pushed).
fn (mut g Gen) gen_expr(id flat.NodeId) WType {
	if int(id) < 0 {
		g.cur.i32_const(0)
		return .i32
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			val := parse_int_literal(node.value)
			mut w := g.expr_wtype(id, '')
			// Promote to i64 when the literal does not fit in i32: the checker
			// types untyped literals as plain `int`, which would truncate.
			if w == .i32 && (val > 2147483647 || val < -2147483648) {
				w = .i64
			}
			if w == .i64 {
				g.cur.i64_const(val)
				return .i64
			}
			g.cur.i32_const(val)
			return .i32
		}
		.bool_literal {
			g.cur.i32_const(if node.value == 'true' { 1 } else { 0 })
			return .i32
		}
		.char_literal {
			// `A` etc. is a scalar (u8/rune); emit its code value.
			g.cur.i32_const(char_literal_value(node.value))
			return .i32
		}
		.float_literal {
			w := g.expr_wtype(id, '')
			fv := node.value.replace('_', '').f64()
			if w == .f32 {
				g.cur.f32_const(f32(fv))
				return .f32
			}
			g.cur.f64_const(fv)
			return .f64
		}
		.ident {
			if node.value in g.var_index {
				g.cur.local_get(g.var_index[node.value])
				return g.var_wtype[node.value]
			}
			gkey := g.global_key(node.value)
			if gkey != '' {
				g.cur.global_get(g.global_index[gkey])
				return g.global_wtype[gkey]
			}
			// Top-level consts are inlined from their recorded value expression.
			if cid := g.const_value_node(node.value) {
				return g.gen_expr(cid)
			}
			g.warn('unknown ident: ${node.value}')
			g.cur.i32_const(0)
			return .i32
		}
		.paren {
			return g.gen_expr(g.a.child(&node, 0))
		}
		.cast_expr {
			return g.gen_cast(node)
		}
		.infix {
			return g.gen_infix(id, node)
		}
		.prefix {
			return g.gen_prefix(id, node)
		}
		.postfix {
			g.gen_postfix(node)
			return .void
		}
		.call {
			return g.gen_call(node)
		}
		.if_expr {
			return g.gen_if_value(id, node)
		}
		.selector {
			// An imported-module const is accessed as `mod.name`; inline it.
			if cid := g.selector_const_node(node) {
				return g.gen_expr(cid)
			}
			g.warn('unsupported selector: ${node.value}')
			g.cur.i32_const(0)
			return .i32
		}
		else {
			g.warn('unsupported expr: ${node.kind}')
			g.cur.i32_const(0)
			return .i32
		}
	}
}

// selector_const_node resolves a `module.name` selector to a top-level const's
// value expression. v3 keys module consts by the module declaration name, so we
// try the literal base, the resolved import path and its last component, then
// the bare field name as an unambiguous suffix.
fn (g &Gen) selector_const_node(node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	base := g.a.child_node(&node, 0)
	if base.kind != .ident {
		return none
	}
	field := node.value
	real := g.resolve_module_alias(g.cur_fn_file, base.value)
	for modname in [base.value, real, real.all_after_last('.')] {
		if cid := g.const_value_node('${modname}.${field}') {
			return cid
		}
	}
	return g.const_value_node(field)
}

// const_value_node resolves an identifier to the value expression of a
// top-level const, if any (numeric/bool consts are inlined at the use site).
fn (g &Gen) const_value_node(name string) ?flat.NodeId {
	if cid := g.tc.const_exprs[name] {
		return cid
	}
	if key := g.tc.const_suffixes[name] {
		if key.len > 0 {
			if cid := g.tc.const_exprs[key] {
				return cid
			}
		}
	}
	return none
}

// gen_if_value emits an `if` used as an expression, leaving the selected
// branch's value on the stack (e.g. `x := if c { 1 } else { 2 }`).
fn (mut g Gen) gen_if_value(id flat.NodeId, node flat.Node) WType {
	result_w := g.expr_wtype(id, '')
	g.gen_if_value_typed(node, result_w)
	return result_w
}

fn (mut g Gen) gen_if_value_typed(node flat.Node, result_w WType) {
	g.gen_expr_as_bool(g.a.child(&node, 0))
	g.cur.raw(0x04) // if
	g.cur.raw(wt_valtype(result_w)) // result type
	g.frames << Frame{
		tag: .plain
	}
	g.gen_block_value(g.a.child_node(&node, 1), result_w)
	g.cur.else_()
	if node.children_count > 2 {
		else_id := g.a.child(&node, 2)
		else_node := g.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			g.gen_if_value_typed(else_node, result_w)
		} else if else_node.kind == .block {
			g.gen_block_value(else_node, result_w)
		} else {
			g.push_zero(result_w)
		}
	} else {
		// A value `if` requires an else; keep the stack typed otherwise.
		g.push_zero(result_w)
	}
	g.cur.end()
	g.frames.pop()
}

// gen_block_value emits a block's statements, leaving the value of its trailing
// expression on the stack, coerced to result_w.
fn (mut g Gen) gen_block_value(block flat.Node, result_w WType) {
	saved := g.snapshot_scope()
	n := int(block.children_count)
	for i in 0 .. n - 1 {
		g.gen_stmt(g.a.child(&block, i))
	}
	if n > 0 {
		last_id := g.a.child(&block, n - 1)
		last := g.a.nodes[int(last_id)]
		if last.kind == .expr_stmt {
			g.gen_expr_as(g.a.child(&last, 0), result_w)
		} else if last.kind == .if_expr {
			g.gen_if_value_typed(last, result_w)
		} else {
			g.gen_stmt(last_id)
			g.push_zero(result_w)
		}
	} else {
		g.push_zero(result_w)
	}
	g.restore_scope(saved)
}

fn (mut g Gen) push_zero(w WType) {
	match w {
		.i64 { g.cur.i64_const(0) }
		.f64 { g.cur.f64_const(0) }
		.f32 { g.cur.f32_const(0) }
		else { g.cur.i32_const(0) }
	}
}

// gen_expr_as emits `id` coerced to the target WType.
fn (mut g Gen) gen_expr_as(id flat.NodeId, target WType) {
	w := g.gen_expr(id)
	if target == .void || w == .void {
		return
	}
	g.coerce(w, target, g.is_signed(id))
}

// gen_expr_as_bool emits `id` leaving an i32 truth value on the stack.
fn (mut g Gen) gen_expr_as_bool(id flat.NodeId) {
	w := g.gen_expr(id)
	if w == .i64 {
		g.cur.i64_const(0)
		g.cur.raw(0x52) // i64.ne -> i32
	} else if w == .f64 {
		g.cur.f64_const(0)
		g.cur.raw(0x62) // f64.ne
	} else if w == .f32 {
		g.cur.f32_const(0)
		g.cur.raw(0x5c) // f32.ne
	}
	// i32 already a truth value
}

fn (mut g Gen) gen_infix(id flat.NodeId, node flat.Node) WType {
	lhs_id := g.a.child(&node, 0)
	rhs_id := g.a.child(&node, 1)
	op := node.op
	if op == .logical_and || op == .logical_or {
		return g.gen_logical(node)
	}
	if op in [.eq, .ne, .lt, .gt, .le, .ge] {
		ow := g.operand_wtype(lhs_id, rhs_id)
		// Mixed signed/unsigned integer comparisons compare mathematical values,
		// not bit patterns, so a negative signed operand needs special handling.
		if ow in [WType.i32, WType.i64] && g.is_unsigned(lhs_id) != g.is_unsigned(rhs_id) {
			g.gen_mixed_cmp(lhs_id, rhs_id, op, ow)
			return .i32
		}
		signed := !g.is_unsigned(lhs_id) && !g.is_unsigned(rhs_id)
		g.gen_expr_as(lhs_id, ow)
		g.gen_expr_as(rhs_id, ow)
		g.cur.raw(cmp_op(op, ow, signed))
		return .i32
	}
	if op in [.left_shift, .right_shift, .right_shift_unsigned] {
		// V keeps the result type/width from the left operand and permits a
		// wider shift count, so the width comes from the lhs, not both operands.
		mut value_w := g.expr_wtype(lhs_id, '')
		if value_w !in [WType.i32, WType.i64] {
			value_w = .i32
		}
		signed := !g.is_unsigned(lhs_id)
		g.gen_expr_as(lhs_id, value_w)
		if op == .right_shift_unsigned && value_w == .i32 {
			// `>>>` operates on the narrow bit pattern; a narrow signed lhs is
			// stored sign-extended, so mask it to its width first.
			lw := narrow_width(g.tc.resolve_type(lhs_id))
			if lw != 32 {
				g.emit_narrow(lw, true)
			}
		}
		g.emit_shift_with_count(op, value_w, rhs_id, signed)
		if value_w == .i32 {
			g.narrow_result(id)
		}
		return value_w
	}
	// arithmetic / bitwise: in V both operands share the result type, so combine
	// the node's own type with the operand-derived width (the latter recovers
	// i64/float when the node type is missing).
	ow := combine_wtype(g.expr_wtype(id, node.typ), g.operand_wtype(lhs_id, rhs_id))
	// div/rem opcode signedness must follow the promoted result, so an unsigned
	// operand (e.g. `4000000000 / u32(2)`) selects div_u/rem_u, not the lhs.
	signed := !g.is_unsigned(id) && !g.is_unsigned(lhs_id) && !g.is_unsigned(rhs_id)
	g.gen_expr_as(lhs_id, ow)
	g.gen_expr_as(rhs_id, ow)
	g.emit_arith(op, ow, signed)
	// Sub-32-bit results (e.g. u8 + u8) wrap to their declared width in V.
	if ow == .i32 {
		g.narrow_result(id)
	}
	return ow
}

// narrow_result masks/sign-extends the i32 on the stack to the resolved type's
// sub-32-bit width (a no-op for 32/64-bit types).
fn (mut g Gen) narrow_result(id flat.NodeId) {
	rt := g.tc.resolve_type(id)
	g.emit_narrow(narrow_width(rt), type_is_unsigned(rt))
}

fn (mut g Gen) gen_logical(node flat.Node) WType {
	lhs_id := g.a.child(&node, 0)
	rhs_id := g.a.child(&node, 1)
	g.gen_expr_as_bool(lhs_id)
	g.cur.raw(0x04) // if
	g.cur.raw(valtype_i32) // -> i32 result
	g.frames << Frame{
		tag: .plain
	}
	if node.op == .logical_and {
		g.gen_expr_as_bool(rhs_id)
		g.cur.else_()
		g.cur.i32_const(0)
	} else {
		g.cur.i32_const(1)
		g.cur.else_()
		g.gen_expr_as_bool(rhs_id)
	}
	g.cur.end()
	g.frames.pop()
	return .i32
}

// gen_mixed_cmp emits a comparison of a signed and an unsigned integer operand
// by mathematical value. The two operands are buffered, then: if the signed
// operand is negative it is smaller than the (non-negative) unsigned operand,
// otherwise both are non-negative and an unsigned comparison is exact.
fn (mut g Gen) gen_mixed_cmp(lhs_id flat.NodeId, rhs_id flat.NodeId, op flat.Op, ow WType) {
	lhs_signed := !g.is_unsigned(lhs_id)
	lhs_tmp := g.alloc_temp(ow)
	rhs_tmp := g.alloc_temp(ow)
	g.gen_expr_as(lhs_id, ow)
	g.cur.local_set(lhs_tmp)
	g.gen_expr_as(rhs_id, ow)
	g.cur.local_set(rhs_tmp)
	// is the signed operand negative?
	g.cur.local_get(if lhs_signed { lhs_tmp } else { rhs_tmp })
	if ow == .i64 {
		g.cur.i64_const(0)
		g.cur.raw(0x53) // i64.lt_s
	} else {
		g.cur.i32_const(0)
		g.cur.raw(0x48) // i32.lt_s
	}
	g.cur.raw(0x04) // if
	g.cur.raw(valtype_i32) // -> i32
	g.frames << Frame{
		tag: .plain
	}
	g.cur.i32_const(neg_signed_cmp_result(op, lhs_signed))
	g.cur.else_()
	g.cur.local_get(lhs_tmp)
	g.cur.local_get(rhs_tmp)
	g.cur.raw(cmp_op(op, ow, false)) // both non-negative: unsigned comparison
	g.cur.end()
	g.frames.pop()
}

// neg_signed_cmp_result is the comparison result when the signed operand is
// negative and the other operand is unsigned (hence non-negative), so the
// signed operand is strictly the smaller value.
fn neg_signed_cmp_result(op flat.Op, signed_is_lhs bool) int {
	return match op {
		.lt, .le {
			if signed_is_lhs {
				1
			} else {
				0
			}
		}
		.gt, .ge {
			if signed_is_lhs {
				0
			} else {
				1
			}
		}
		.eq {
			0
		}
		.ne {
			1
		}
		else {
			0
		}
	}
}

fn (mut g Gen) gen_prefix(id flat.NodeId, node flat.Node) WType {
	child_id := g.a.child(&node, 0)
	match node.op {
		.minus {
			child := g.a.nodes[int(child_id)]
			if child.kind == .int_literal {
				// Fold `-literal` directly: the checker may type a large literal
				// as plain int, which would wrap the magnitude to 32 bits before
				// negating (e.g. i64(-9223372036854775808)).
				mag := u64(parse_int_literal(child.value))
				if mag > 2147483648 {
					g.cur.i64_const(i64(u64(0) - mag))
					return .i64
				}
				g.cur.i32_const(-i64(mag))
				g.narrow_result(id)
				return .i32
			}
			w := g.expr_wtype(child_id, '')
			match w {
				.f64 {
					g.gen_expr(child_id)
					g.cur.raw(0x9a) // f64.neg
					return .f64
				}
				.f32 {
					g.gen_expr(child_id)
					g.cur.raw(0x8c) // f32.neg
					return .f32
				}
				.i64 {
					g.cur.i64_const(0)
					g.gen_expr_as(child_id, .i64)
					g.cur.raw(0x7d) // i64.sub
					return .i64
				}
				else {
					g.cur.i32_const(0)
					g.gen_expr_as(child_id, .i32)
					g.cur.raw(0x6b) // i32.sub
					g.narrow_result(id) // negation of a narrow type wraps to its width
					return .i32
				}
			}
		}
		.not {
			g.gen_expr_as_bool(child_id)
			g.cur.raw(0x45) // i32.eqz
			return .i32
		}
		.bit_not {
			w := g.gen_expr(child_id)
			if w == .i64 {
				g.cur.i64_const(-1)
				g.cur.raw(0x85) // i64.xor
				return .i64
			}
			g.cur.i32_const(-1)
			g.cur.raw(0x73) // i32.xor
			g.narrow_result(id) // ~ of a narrow type keeps its width (e.g. ~u8(0)=255)
			return .i32
		}
		else {
			g.warn('unsupported prefix op: ${node.op}')
			return g.gen_expr(child_id)
		}
	}
}

fn (mut g Gen) gen_postfix(node flat.Node) {
	target := g.a.child_node(&node, 0)
	inc := node.op == .inc
	if target.kind == .ident && target.value in g.var_index {
		idx := g.var_index[target.value]
		g.cur.local_get(idx)
		g.emit_inc(g.var_wtype[target.value], inc)
		g.narrow_for_local(target.value)
		g.cur.local_set(idx)
		return
	}
	if target.kind == .ident {
		gkey := g.global_key(target.value)
		if gkey != '' {
			gidx := g.global_index[gkey]
			g.cur.global_get(gidx)
			g.emit_inc(g.global_wtype[gkey], inc)
			g.narrow_for_global(gkey)
			g.cur.global_set(gidx)
			return
		}
	}
	g.warn('unsupported postfix target')
}

// emit_inc pushes 1 of the given type and adds (inc) or subtracts it from the
// value already on the stack, used by `x++`/`x--` for locals and globals.
fn (mut g Gen) emit_inc(w WType, inc bool) {
	match w {
		.i64 {
			g.cur.i64_const(1)
			g.cur.raw(if inc { u8(0x7c) } else { u8(0x7d) }) // i64.add/sub
		}
		.f64 {
			g.cur.f64_const(1)
			g.cur.raw(if inc { u8(0xa0) } else { u8(0xa1) }) // f64.add/sub
		}
		.f32 {
			g.cur.f32_const(1)
			g.cur.raw(if inc { u8(0x92) } else { u8(0x93) }) // f32.add/sub
		}
		else {
			g.cur.i32_const(1)
			g.cur.raw(if inc { u8(0x6a) } else { u8(0x6b) }) // i32.add/sub
		}
	}
}

fn (mut g Gen) gen_cast(node flat.Node) WType {
	child_id := g.a.child(&node, 0)
	tt := g.tc.parse_type(node.value)
	target := prim_wtype(tt) or { WType.i32 }
	child := g.a.nodes[int(child_id)]
	if target in [WType.f32, WType.f64] && child.kind == .int_literal {
		// Materialize the literal directly for the float target: an integer
		// path would wrap large non-negative values (e.g.
		// f64(9223372036854775808)) before the conversion and flip the sign.
		uval := u64(parse_int_literal(child.value))
		if target == .f32 {
			g.cur.f32_const(f32(uval))
		} else {
			g.cur.f64_const(f64(uval))
		}
		return target
	}
	src := g.gen_expr(child_id)
	if src in [WType.f32, WType.f64] && target in [WType.i32, WType.i64] {
		// float -> int: signedness comes from the target, not the source.
		g.emit_float_to_int(src, target, !type_is_unsigned(tt))
	} else {
		g.coerce(src, target, g.is_signed(child_id))
	}
	// Casting to a sub-32-bit type wraps/sign-extends to that width.
	g.emit_narrow(narrow_width(tt), type_is_unsigned(tt))
	return target
}

fn (mut g Gen) gen_call(node flat.Node) WType {
	callee := g.a.child_node(&node, 0)
	if callee.kind == .ident && callee.value in ['println', 'print', 'eprintln', 'eprint'] {
		g.gen_print_call(node, callee.value)
		return .void
	}
	for key in g.resolve_call_keys(callee, g.cur_fn_module, g.cur_fn_file) {
		if key in g.fn_index {
			params := g.fn_params[key]
			for i in 1 .. node.children_count {
				pw := if i - 1 < params.len { params[i - 1] } else { WType.i32 }
				g.gen_expr_as(g.a.child(&node, i), pw)
			}
			g.cur.call(g.fn_index[key])
			return g.fn_ret[key]
		}
	}
	if callee.kind == .ident {
		g.warn('unsupported call: ${callee.value}')
	} else {
		g.warn('unsupported call target')
	}
	// keep the stack balanced for value contexts
	g.cur.i32_const(0)
	return .i32
}

// collect_imports records every import path and, per source file, the
// selector/alias -> full import path mapping. Candidates are keyed by the full
// import path (see module_path_for_file), so two nested modules with the same
// leaf (`import foo.util as fu`, `import bar.util as bu`) stay distinct.
fn (mut g Gen) collect_imports() {
	// Pass 1: import paths + per-file aliases.
	mut cur_file := ''
	for node in g.a.nodes {
		if node.kind == .file {
			cur_file = node.value
		} else if node.kind == .import_decl && node.value.len > 0 {
			g.import_paths << node.value
			if node.typ.len > 0 {
				if cur_file !in g.file_aliases {
					g.file_aliases[cur_file] = map[string]string{}
				}
				g.file_aliases[cur_file][node.typ] = node.value
			}
		}
	}
	// Pass 2: the module import graph (module_path_for_file now works), keyed by
	// the importing file's module path ('' for main), used to order init calls.
	cur_file = ''
	mut cur_mod := ''
	for node in g.a.nodes {
		if node.kind == .file {
			cur_file = node.value
			cur_mod = ''
		} else if node.kind == .module_decl {
			cur_mod = if node.value != '' && node.value != 'main' {
				g.module_path_for_file(cur_file)
			} else {
				''
			}
		} else if node.kind == .import_decl && node.value.len > 0 {
			g.module_imports[cur_mod] << node.value
		}
	}
}

// collect_globals lowers primitive `__global` declarations to mutable wasm
// globals, recording each name's index/type and emitting a constant initializer.
fn (mut g Gen) collect_globals() {
	mut cur_module := ''
	mut cur_file := ''
	mut cur_module_path := ''
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		if node.kind == .file {
			cur_module = ''
			cur_file = node.value
			cur_module_path = ''
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			cur_module_path = if cur_module != '' && cur_module != 'main' {
				g.module_path_for_file(cur_file)
			} else {
				''
			}
			continue
		}
		if node.kind != .global_decl || i < g.a.user_code_start {
			continue
		}
		// Qualify the global by its declaring module so a name declared in two
		// modules maps to two distinct wasm globals (see global_key for lookup).
		mod_key := if cur_module_path != '' {
			cur_module_path
		} else if cur_module != '' && cur_module != 'main' {
			cur_module
		} else {
			''
		}
		for fi in 0 .. node.children_count {
			f := g.a.child_node(&node, fi)
			if f.kind != .field_decl || f.value.len == 0 || f.value.starts_with('C.') {
				continue
			}
			// Type from the declaration, or inferred from the initializer.
			mut t := types.Type(types.Void{})
			if f.typ.len > 0 {
				t = g.tc.parse_type(f.typ)
			} else if f.children_count > 0 {
				t = g.tc.resolve_type(g.a.child(f, 0))
			}
			w := prim_wtype(t) or { continue } // only numeric/bool globals
			width := narrow_width(t)
			unsigned := type_is_unsigned(t)
			mut init := Code{}
			if f.children_count > 0 {
				g.emit_global_init(g.a.child(f, 0), w, width, unsigned, mut init)
			} else {
				init_push_zero(w, mut init)
			}
			init.end()
			gidx := g.mod.add_global(wt_valtype(w), init.bytes)
			key := qualified_fn_key(mod_key, f.value)
			g.global_index[key] = gidx
			g.global_wtype[key] = w
			g.global_unsigned[key] = unsigned
			g.global_widths[key] = width
		}
	}
}

// emit_global_init writes a constant initializer expression for a global. Only
// constant literals (optionally negated/cast) are folded; others default to 0.
fn (g &Gen) emit_global_init(init_id flat.NodeId, w WType, width int, unsigned bool, mut c Code) {
	if w in [WType.f32, WType.f64] {
		fv := g.fold_const_float(init_id)
		if w == .f32 {
			c.f32_const(f32(fv))
		} else {
			c.f64_const(fv)
		}
		return
	}
	iv := g.fold_const_int(init_id)
	if w == .i64 {
		c.i64_const(iv)
	} else {
		// Apply the global's declared sub-32-bit width so an out-of-range cast
		// initializer (e.g. `u8(300)` -> 44, `i8(128)` -> -128) matches what a
		// later store would narrow it to via narrow_for_global.
		c.i32_const(narrow_const(iv, width, unsigned))
	}
}

// narrow_const wraps/sign-extends a constant to a declared 8/16-bit width; the
// compile-time counterpart of emit_narrow.
fn narrow_const(v i64, width int, unsigned bool) i64 {
	return match width {
		8 {
			if unsigned {
				i64(u8(v))
			} else {
				i64(i8(v))
			}
		}
		16 {
			if unsigned {
				i64(u16(v))
			} else {
				i64(i16(v))
			}
		}
		else {
			v
		}
	}
}

fn (g &Gen) fold_const_int(id flat.NodeId) i64 {
	if int(id) < 0 {
		return 0
	}
	n := g.a.nodes[int(id)]
	match n.kind {
		.int_literal {
			return parse_int_literal(n.value)
		}
		.char_literal {
			return char_literal_value(n.value)
		}
		.bool_literal {
			return if n.value == 'true' { i64(1) } else { i64(0) }
		}
		.ident {
			// A constant referenced by name folds to its own constant value.
			if cid := g.const_value_node(n.value) {
				return g.fold_const_int(cid)
			}
		}
		.prefix {
			if n.children_count > 0 {
				match n.op {
					.minus { return -g.fold_const_int(g.a.child(&n, 0)) }
					.bit_not { return ~g.fold_const_int(g.a.child(&n, 0)) }
					else {}
				}
			}
		}
		.infix {
			if n.children_count >= 2 {
				// Narrow to the infix's own resolved type so any wrapping happens
				// before a wider outer cast can observe it, matching normal codegen
				// (`int(u8(250) + u8(10))` -> 4, not 260).
				return narrow_to_type(g.fold_const_infix(n), g.tc.resolve_type(id))
			}
		}
		.paren {
			if n.children_count > 0 {
				return g.fold_const_int(g.a.child(&n, 0))
			}
		}
		.cast_expr {
			if n.children_count > 0 {
				cv := g.fold_const_int(g.a.child(&n, 0))
				// Apply the cast target's own width/signedness so a nested narrow
				// cast keeps its value even when the enclosing global is wider
				// (e.g. `int(u8(300))` -> 44, not 300).
				return if n.value.len > 0 { g.fold_cast_int(cv, n.value) } else { cv }
			}
		}
		else {}
	}

	return 0
}

// fold_const_infix folds a binary constant expression (`1 + 2`, `base << 3`, ...)
// over its already-folded integer operands. Division, remainder, right shift and
// comparisons use unsigned semantics when either operand resolves to an unsigned
// type, since folded operands are stored as i64 bit patterns (u64(max) -> -1) and
// signed ops would otherwise mis-fold, e.g. `u64(max) / u64(2)`.
fn (g &Gen) fold_const_infix(n flat.Node) i64 {
	la := g.a.child(&n, 0)
	lb := g.a.child(&n, 1)
	a := g.fold_const_int(la)
	b := g.fold_const_int(lb)
	unsigned := type_is_unsigned(g.tc.resolve_type(la)) || type_is_unsigned(g.tc.resolve_type(lb))
	return match n.op {
		.plus {
			a + b
		}
		.minus {
			a - b
		}
		.mul {
			a * b
		}
		.div {
			if b == 0 {
				i64(0)
			} else if unsigned {
				i64(u64(a) / u64(b))
			} else {
				a / b
			}
		}
		.mod {
			if b == 0 {
				i64(0)
			} else if unsigned {
				i64(u64(a) % u64(b))
			} else {
				a % b
			}
		}
		.amp {
			a & b
		}
		.pipe {
			a | b
		}
		.xor {
			a ^ b
		}
		.left_shift {
			i64(u64(a) << b)
		}
		.right_shift {
			if unsigned {
				i64(u64(a) >> b)
			} else {
				a >> b
			}
		}
		.right_shift_unsigned {
			i64(u64(a) >> b)
		}
		.eq {
			bool_to_i64(a == b)
		}
		.ne {
			bool_to_i64(a != b)
		}
		.lt {
			if unsigned {
				bool_to_i64(u64(a) < u64(b))
			} else {
				bool_to_i64(a < b)
			}
		}
		.gt {
			if unsigned {
				bool_to_i64(u64(a) > u64(b))
			} else {
				bool_to_i64(a > b)
			}
		}
		.le {
			if unsigned {
				bool_to_i64(u64(a) <= u64(b))
			} else {
				bool_to_i64(a <= b)
			}
		}
		.ge {
			if unsigned {
				bool_to_i64(u64(a) >= u64(b))
			} else {
				bool_to_i64(a >= b)
			}
		}
		else {
			i64(0)
		}
	}
}

fn bool_to_i64(b bool) i64 {
	return if b { i64(1) } else { i64(0) }
}

// fold_cast_int wraps a folded constant to a numeric cast target's width and
// signedness, the compile-time form of gen_cast's narrowing.
fn (g &Gen) fold_cast_int(v i64, type_name string) i64 {
	return narrow_to_type(v, g.tc.parse_type(type_name))
}

// narrow_to_type wraps a folded constant to an integer type's width and
// signedness (i64 targets keep the full value; i32-family targets wrap to
// 8/16/32 bits; non-integer targets are left unchanged).
fn narrow_to_type(v i64, t types.Type) i64 {
	w := prim_wtype(t) or { return v }
	if w != .i32 {
		return v
	}
	unsigned := type_is_unsigned(t)
	return match narrow_width(t) {
		8 {
			if unsigned {
				i64(u8(v))
			} else {
				i64(i8(v))
			}
		}
		16 {
			if unsigned {
				i64(u16(v))
			} else {
				i64(i16(v))
			}
		}
		else {
			if unsigned {
				i64(u32(v))
			} else {
				i64(i32(v))
			}
		}
	}
}

fn (g &Gen) fold_const_float(id flat.NodeId) f64 {
	if int(id) < 0 {
		return 0
	}
	n := g.a.nodes[int(id)]
	match n.kind {
		.float_literal {
			return n.value.replace('_', '').f64()
		}
		.int_literal {
			return f64(parse_int_literal(n.value))
		}
		.ident {
			if cid := g.const_value_node(n.value) {
				return g.fold_const_float(cid)
			}
		}
		.prefix {
			if n.op == .minus && n.children_count > 0 {
				return -g.fold_const_float(g.a.child(&n, 0))
			}
		}
		.infix {
			if n.children_count >= 2 {
				a := g.fold_const_float(g.a.child(&n, 0))
				b := g.fold_const_float(g.a.child(&n, 1))
				return match n.op {
					.plus {
						a + b
					}
					.minus {
						a - b
					}
					.mul {
						a * b
					}
					.div {
						if b != 0 {
							a / b
						} else {
							f64(0)
						}
					}
					else {
						f64(0)
					}
				}
			}
		}
		.paren {
			if n.children_count > 0 {
				return g.fold_const_float(g.a.child(&n, 0))
			}
		}
		.cast_expr {
			if n.children_count > 0 {
				cid := g.a.child(&n, 0)
				tw := prim_wtype(g.tc.parse_type(n.value)) or { WType.f64 }
				if tw !in [WType.f32, WType.f64] {
					// Cast to an integer type inside a float context: fold and
					// narrow as an int, then widen to float.
					return f64(g.fold_cast_int(g.fold_const_int(cid), n.value))
				}
				child := g.a.nodes[int(cid)]
				mut fv := if child.kind == .int_literal {
					// Match gen_cast: reinterpret the literal's wrapped i64 bits as
					// u64 so a large non-negative literal (e.g.
					// f64(9223372036854775808)) stays positive instead of flipping
					// sign through parse_int_literal.
					f64(u64(parse_int_literal(child.value)))
				} else {
					g.fold_const_float(cid)
				}
				if tw == .f32 {
					fv = f64(f32(fv)) // round to f32 precision like an f32 cast
				}
				return fv
			}
		}
		else {}
	}

	return 0
}

fn init_push_zero(w WType, mut c Code) {
	match w {
		.i64 { c.i64_const(0) }
		.f64 { c.f64_const(0) }
		.f32 { c.f32_const(0) }
		else { c.i32_const(0) }
	}
}

// order_module_inits appends init function indices in dependency order via a
// post-order walk of the import graph: a module's imports' inits run first, and
// since main ('') is the root, its own init is appended last.
fn (mut g Gen) order_module_inits(mod string, module_init map[string]string, mut visited map[string]bool) {
	if mod in visited {
		return
	}
	visited[mod] = true
	for dep in g.module_imports[mod] {
		g.order_module_inits(dep, module_init, mut visited)
	}
	if init_key := module_init[mod] {
		if idx := g.fn_index[init_key] {
			g.init_fns << idx
		}
	}
}

// module_path_for_file returns the full import path whose directory form is a
// suffix of the file's directory (longest match), or '' for main/local code.
fn (g &Gen) module_path_for_file(file string) string {
	if file.len == 0 {
		return ''
	}
	dir := os.dir(file)
	mut best := ''
	for p in g.import_paths {
		if p.len <= best.len {
			continue
		}
		df := p.replace('.', os.path_separator)
		if dir == df || dir.ends_with(os.path_separator + df) {
			best = p
		}
	}
	return best
}

// resolve_call_keys returns the candidate fn_index keys for a callee, in
// priority order. A bare ident inside an imported module resolves to the
// same-module function first (`mod.name`) and falls back to a main-module name;
// a `module.fn()` selector resolves to that module's qualified name, with any
// import alias mapped back to the real module.
fn (g &Gen) resolve_call_keys(callee &flat.Node, cur_mod string, cur_file string) []string {
	if callee.kind == .ident {
		if cur_mod != '' && cur_mod != 'main' {
			return ['${cur_mod}.${callee.value}', callee.value]
		}
		return [callee.value]
	}
	if callee.kind == .selector && callee.children_count > 0 {
		base := g.a.child_node(callee, 0)
		if base.kind == .ident {
			real := g.resolve_module_alias(cur_file, base.value)
			return ['${real}.${callee.value}']
		}
	}
	return []
}

// resolve_module_alias maps an import alias to its real module using the
// aliases declared in the call site's source file, or returns the name as-is.
fn (g &Gen) resolve_module_alias(cur_file string, name string) string {
	if aliases := g.file_aliases[cur_file] {
		if real := aliases[name] {
			return real
		}
	}
	return name
}

// ---- print intrinsics ----

fn (mut g Gen) gen_print_call(node flat.Node, name string) {
	fd := if name.starts_with('e') { 2 } else { 1 }
	newline := name.ends_with('ln')
	if node.children_count < 2 {
		if newline {
			g.emit_write_const(nl_ptr, 1, fd)
		}
		return
	}
	arg_id := g.a.child(&node, 1)
	arg := g.a.nodes[int(arg_id)]
	if arg.kind == .string_literal {
		// arg.value is already unescaped by the parser (strip_quotes ->
		// unescape_string); use the bytes as-is, no second escape pass.
		bytes := arg.value.bytes()
		off := g.intern_data(bytes)
		g.emit_write_const(off, bytes.len, fd)
		if newline {
			g.emit_write_const(nl_ptr, 1, fd)
		}
		return
	}
	if arg.kind == .call {
		inner := g.a.child_node(&arg, 0)
		if inner.kind == .ident && arg.children_count >= 2 {
			if inner.value in signed_int_format_fns {
				g.emit_print_int(g.a.child(&arg, 1), fd, newline, true)
				return
			}
			if inner.value in unsigned_int_format_fns {
				g.emit_print_int(g.a.child(&arg, 1), fd, newline, false)
				return
			}
			if inner.value in bool_format_fns {
				g.gen_print_bool(g.a.child(&arg, 1), fd, newline)
				return
			}
		}
	}
	// integer expression passed directly (e.g. print(x))
	w := g.expr_wtype(arg_id, '')
	if w in [WType.i32, WType.i64] {
		g.emit_print_int(arg_id, fd, newline, !g.is_unsigned(arg_id))
		return
	}
	g.warn('unsupported ${name} argument: ${arg.kind}')
}

// gen_print_bool prints "true"/"false" based on the i32 truth value of arg.
fn (mut g Gen) gen_print_bool(arg_id flat.NodeId, fd int, newline bool) {
	true_off := g.intern_data('true'.bytes())
	false_off := g.intern_data('false'.bytes())
	g.gen_expr_as_bool(arg_id)
	g.cur.if_void()
	g.frames << Frame{
		tag: .plain
	}
	g.emit_write_const(true_off, 4, fd)
	g.cur.else_()
	g.emit_write_const(false_off, 5, fd)
	g.cur.end()
	g.frames.pop()
	if newline {
		g.emit_write_const(nl_ptr, 1, fd)
	}
}

// emit_print_int evaluates an integer expression and prints it via the runtime
// helper, choosing signed or unsigned decimal conversion.
fn (mut g Gen) emit_print_int(int_arg_id flat.NodeId, fd int, newline bool, signed bool) {
	w := g.gen_expr(int_arg_id)
	// Widen to i64 using the formatter's signedness (an unsigned formatter must
	// zero-extend, e.g. `1 + u64(x)` or a u32 result with the high bit set).
	g.coerce(w, .i64, signed)
	g.cur.i32_const(fd)
	g.cur.i32_const(if newline { 1 } else { 0 })
	g.cur.i32_const(if signed { 1 } else { 0 })
	g.cur.call(g.print_int_idx)
}

fn (mut g Gen) emit_write_const(ptr int, len int, fd int) {
	g.cur.i32_const(ptr)
	g.cur.i32_const(len)
	g.cur.i32_const(fd)
	g.cur.call(g.write_idx)
}

// emit_write_helper builds `__v_write(ptr, len, fd)` -> calls WASI fd_write.
fn (mut g Gen) emit_write_helper(fd_write_idx int) {
	mut c := Code{}
	// iov[0] = ptr  (mem @ iov_ptr)
	c.i32_const(iov_ptr)
	c.local_get(0)
	c.i32_store(0)
	// iov[1] = len  (mem @ iov_ptr+4)
	c.i32_const(iov_ptr)
	c.local_get(1)
	c.i32_store(4)
	// fd_write(fd, iov_ptr, 1, nwritten_ptr)
	c.local_get(2)
	c.i32_const(iov_ptr)
	c.i32_const(1)
	c.i32_const(nwritten_ptr)
	c.call(fd_write_idx)
	c.drop()
	c.end()
	ti := g.mod.add_type([valtype_i32, valtype_i32, valtype_i32], [])
	g.mod.add_func(ti, [], c.bytes)
}

// emit_print_int_helper builds `__v_print_int(val: i64, fd: i32, nl: i32,
// signed: i32)`. The digit loop uses unsigned div/rem, so passing signed=0
// prints the full u64 range; signed=1 adds two's-complement sign handling.
fn (mut g Gen) emit_print_int_helper() {
	// params: val(0,i64) fd(1,i32) nl(2,i32) signed(3,i32)
	// locals: p(4,i32) v(5,i64) neg(6,i32) digit(7,i64)
	signed := 3
	p := 4
	v := 5
	neg := 6
	digit := 7
	mut c := Code{}
	// neg = signed && val < 0
	c.local_get(0)
	c.i64_const(0)
	c.raw(0x53) // i64.lt_s
	c.local_get(signed)
	c.raw(0x71) // i32.and
	c.local_set(neg)
	// v = val
	c.local_get(0)
	c.local_set(v)
	// if neg { v = 0 - v }
	c.local_get(neg)
	c.if_void()
	c.i64_const(0)
	c.local_get(v)
	c.raw(0x7d) // i64.sub
	c.local_set(v)
	c.end()
	// p = buf_end
	c.i32_const(buf_end)
	c.local_set(p)
	// do { ... } while v != 0
	c.loop_void()
	c.local_get(p)
	c.i32_const(1)
	c.raw(0x6b) // i32.sub
	c.local_set(p)
	// digit = v % 10
	c.local_get(v)
	c.i64_const(10)
	c.raw(0x82) // i64.rem_u
	c.local_set(digit)
	// mem8[p] = '0' + digit
	c.local_get(p)
	c.local_get(digit)
	c.raw(0xa7) // i32.wrap_i64
	c.i32_const(48)
	c.raw(0x6a) // i32.add
	c.i32_store8(0)
	// v = v / 10
	c.local_get(v)
	c.i64_const(10)
	c.raw(0x80) // i64.div_u
	c.local_set(v)
	// continue if v != 0
	c.local_get(v)
	c.i64_const(0)
	c.raw(0x52) // i64.ne
	c.br_if(0)
	c.end() // loop
	// if neg { p--; mem8[p]='-' }
	c.local_get(neg)
	c.if_void()
	c.local_get(p)
	c.i32_const(1)
	c.raw(0x6b)
	c.local_set(p)
	c.local_get(p)
	c.i32_const(45) // '-'
	c.i32_store8(0)
	c.end()
	// __v_write(p, buf_end - p, fd)
	c.local_get(p)
	c.i32_const(buf_end)
	c.local_get(p)
	c.raw(0x6b) // i32.sub
	c.local_get(1) // fd
	c.call(g.write_idx)
	// if nl { __v_write(nl_ptr, 1, fd) }
	c.local_get(2)
	c.if_void()
	c.i32_const(nl_ptr)
	c.i32_const(1)
	c.local_get(1)
	c.call(g.write_idx)
	c.end()
	c.end() // function
	locals := [valtype_i32, valtype_i64, valtype_i32, valtype_i64]
	ti := g.mod.add_type([valtype_i64, valtype_i32, valtype_i32, valtype_i32], [])
	g.mod.add_func(ti, locals, c.bytes)
}

fn (mut g Gen) intern_data(bytes []u8) int {
	key := bytes.bytestr()
	if key in g.data_off {
		return g.data_off[key]
	}
	off := data_base + g.data_pool.len
	g.data_pool << bytes
	g.data_off[key] = off
	return off
}

// ---- type helpers ----

fn (mut g Gen) expr_wtype(id flat.NodeId, fallback_typ string) WType {
	if int(id) >= 0 {
		node := g.a.nodes[int(id)]
		// Locals are authoritative: our own table is more reliable than the
		// shared checker scope, which the backend does not repopulate.
		if node.kind == .ident && node.value in g.var_wtype {
			return g.var_wtype[node.value]
		}
		if node.kind == .ident {
			gkey := g.global_key(node.value)
			if gkey != '' {
				return g.global_wtype[gkey]
			}
		}
		if node.kind == .paren && node.children_count > 0 {
			return g.expr_wtype(g.a.child(&node, 0), fallback_typ)
		}
		if node.kind == .cast_expr && node.value.len > 0 {
			if w := prim_wtype(g.tc.parse_type(node.value)) {
				return w
			}
		}
		if node.kind == .infix && node.children_count >= 2 {
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
				return .i32
			}
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned] {
				// Shift result width follows the lhs, not the (possibly wider) count.
				return g.expr_wtype(g.a.child(&node, 0), '')
			}
			return combine_wtype(g.expr_wtype(g.a.child(&node, 0), ''), g.expr_wtype(g.a.child(&node,
				1), ''))
		}
		if node.kind == .prefix && node.children_count >= 1 {
			if node.op == .not {
				return .i32
			}
			return g.expr_wtype(g.a.child(&node, 0), fallback_typ)
		}
	}
	t := g.tc.resolve_type(id)
	if w := prim_wtype(t) {
		if t !is types.Void {
			return w
		}
	}
	if fallback_typ.len > 0 && fallback_typ != 'void' {
		if w := prim_wtype(g.tc.parse_type(fallback_typ)) {
			return w
		}
	}
	return .i32
}

fn (mut g Gen) operand_wtype(lhs_id flat.NodeId, rhs_id flat.NodeId) WType {
	return combine_wtype(g.expr_wtype(lhs_id, ''), g.expr_wtype(rhs_id, ''))
}

// combine_wtype picks the widest of two value types (float beats int, 64 beats
// 32), which is the shared operand/result width for a binary operation.
fn combine_wtype(a WType, b WType) WType {
	if a == .f64 || b == .f64 {
		return .f64
	}
	if a == .f32 || b == .f32 {
		return .f32
	}
	if a == .i64 || b == .i64 {
		return .i64
	}
	return .i32
}

fn (mut g Gen) is_unsigned(id flat.NodeId) bool {
	if int(id) >= 0 {
		node := g.a.nodes[int(id)]
		if node.kind == .ident && node.value in g.var_unsigned {
			return g.var_unsigned[node.value]
		}
		if node.kind == .ident {
			gkey := g.global_key(node.value)
			if gkey != '' {
				return g.global_unsigned[gkey]
			}
		}
		if node.kind == .paren && node.children_count > 0 {
			return g.is_unsigned(g.a.child(&node, 0))
		}
		if node.kind == .cast_expr && node.value.len > 0 {
			return type_is_unsigned(g.tc.parse_type(node.value))
		}
	}
	return type_is_unsigned(g.tc.resolve_type(id))
}

// decl_is_unsigned resolves the signedness of a declared variable's initializer.
fn (mut g Gen) decl_is_unsigned(rhs_id flat.NodeId, fallback_typ string) bool {
	if g.is_unsigned(rhs_id) {
		return true
	}
	if fallback_typ.len > 0 {
		return type_is_unsigned(g.tc.parse_type(fallback_typ))
	}
	return false
}

fn type_is_unsigned(t_ types.Type) bool {
	t := unalias(t_)
	if t is types.Primitive {
		return t.props.has(.unsigned)
	}
	return t is types.USize
}

// narrow_width returns 8 or 16 for sub-32-bit integer types (which the backend
// stores in an i32 and must mask/sign-extend), or 32 otherwise.
fn narrow_width(t_ types.Type) int {
	t := unalias(t_)
	if t is types.Primitive && t.props.has(.integer) {
		if t.size == 8 {
			return 8
		}
		if t.size == 16 {
			return 16
		}
	}
	return 32
}

fn (mut g Gen) is_signed(id flat.NodeId) bool {
	return !g.is_unsigned(id)
}

fn (mut g Gen) coerce(from WType, to WType, signed bool) {
	if from == to || from == .void || to == .void {
		return
	}
	match to {
		.i64 {
			if from == .i32 {
				g.cur.raw(if signed { u8(0xac) } else { u8(0xad) }) // i64.extend_i32_s/u
			} else if from == .f32 || from == .f64 {
				g.emit_float_to_int(from, .i64, signed)
			}
		}
		.i32 {
			if from == .i64 {
				g.cur.raw(0xa7) // i32.wrap_i64
			} else if from == .f32 || from == .f64 {
				g.emit_float_to_int(from, .i32, signed)
			}
		}
		.f64 {
			match from {
				.i32 { g.cur.raw(if signed { u8(0xb7) } else { u8(0xb8) }) }
				.i64 { g.cur.raw(if signed { u8(0xb9) } else { u8(0xba) }) }
				.f32 { g.cur.raw(0xbb) } // f64.promote_f32
				else {}
			}
		}
		.f32 {
			match from {
				.i32 { g.cur.raw(if signed { u8(0xb2) } else { u8(0xb3) }) }
				.i64 { g.cur.raw(if signed { u8(0xb4) } else { u8(0xb5) }) }
				.f64 { g.cur.raw(0xb6) } // f32.demote_f64
				else {}
			}
		}
		else {}
	}
}

fn (mut g Gen) emit_arith(op flat.Op, w WType, signed bool) {
	if op in [.left_shift, .right_shift, .right_shift_unsigned] {
		g.emit_shift_op(op, w, w, signed)
		return
	}
	g.cur.raw(arith_op(op, w, signed))
}

// emit_shift_with_count generates the (already-on-stack value's) shift count at
// its natural width and emits the shift. The result keeps the value's width
// while V permits a wider shift count, so value and count widths may differ.
fn (mut g Gen) emit_shift_with_count(op flat.Op, value_w WType, rhs_id flat.NodeId, signed bool) {
	count_w := if g.expr_wtype(rhs_id, '') == .i64 { WType.i64 } else { WType.i32 }
	g.gen_expr_as(rhs_id, count_w)
	g.emit_shift_op(op, value_w, count_w, signed)
}

// emit_shift_op emits a shift with V's over-width semantics: WASM masks the
// count modulo the value width, but V yields 0 once the (full-width) count
// reaches the value width. Stack on entry: [value(value_w), count(count_w)];
// on exit: [result(value_w)].
fn (mut g Gen) emit_shift_op(op flat.Op, value_w WType, count_w WType, signed bool) {
	tmp := g.alloc_temp(count_w)
	g.cur.local_tee(tmp) // keep the original count, also store it
	// The WASM shift opcode needs the count at the value width.
	if count_w == .i64 && value_w == .i32 {
		g.cur.raw(0xa7) // i32.wrap_i64
	} else if count_w == .i32 && value_w == .i64 {
		g.cur.raw(0xad) // i64.extend_i32_u
	}
	g.cur.raw(arith_op(op, value_w, signed)) // value << (count mod value width)
	// result = (count < value_width) ? shifted : 0, comparing the full count.
	// V promotes narrow types to int for shifts, so the threshold is the
	// computation (WASM) width, not the declared 8/16 (matches v1: i8(-1) >> 8
	// stays -1, while u32 << 32 and u64 << 64 become 0).
	width := if value_w == .i64 { 64 } else { 32 }
	if value_w == .i64 {
		g.cur.i64_const(0)
	} else {
		g.cur.i32_const(0)
	}
	g.cur.local_get(tmp)
	if count_w == .i64 {
		g.cur.i64_const(width)
		g.cur.raw(0x54) // i64.lt_u
	} else {
		g.cur.i32_const(width)
		g.cur.raw(0x49) // i32.lt_u
	}
	g.cur.raw(0x1b) // select
}

// emit_float_to_int truncates a float to an integer with V's saturating cast
// semantics (NaN -> 0, out-of-range saturates to the target min/max), using the
// target type's signedness rather than the source's.
fn (mut g Gen) emit_float_to_int(from WType, to WType, signed bool) {
	g.cur.raw(0xfc) // saturating-truncation prefix
	op2 := if to == .i64 {
		if from == .f32 {
			if signed { u8(0x04) } else { u8(0x05) }
		} else {
			if signed { u8(0x06) } else { u8(0x07) }
		}
	} else {
		if from == .f32 {
			if signed { u8(0x00) } else { u8(0x01) }
		} else {
			if signed { u8(0x02) } else { u8(0x03) }
		}
	}
	g.cur.raw(op2)
}

// emit_narrow masks (unsigned) or sign-extends (signed) the i32 on the stack to
// a sub-32-bit width; a no-op for 32/64-bit values.
fn (mut g Gen) emit_narrow(width int, unsigned bool) {
	match width {
		8 {
			if unsigned {
				g.cur.i32_const(0xff)
				g.cur.raw(0x71) // i32.and
			} else {
				g.cur.raw(0xc0) // i32.extend8_s
			}
		}
		16 {
			if unsigned {
				g.cur.i32_const(0xffff)
				g.cur.raw(0x71) // i32.and
			} else {
				g.cur.raw(0xc1) // i32.extend16_s
			}
		}
		else {}
	}
}

fn (mut g Gen) warn(msg string) {
	g.warnings << msg
}

// ---- free helpers ----

fn wt_valtype(w WType) u8 {
	return match w {
		.i32, .void { valtype_i32 }
		.i64 { valtype_i64 }
		.f32 { valtype_f32 }
		.f64 { valtype_f64 }
	}
}

// qualified_fn_key keys fn_index/fn_ret/fn_params: a bare name for the main
// module, `module.name` for imported modules (matching call-site selectors).
fn qualified_fn_key(mod string, name string) string {
	if mod == '' || mod == 'main' {
		return name
	}
	return '${mod}.${name}'
}

// export_fn_name is the wasm export name for a function (`.` is replaced so the
// name is convenient to reference from a host like JS).
fn export_fn_name(mod string, name string) string {
	if mod == '' || mod == 'main' {
		return name
	}
	return '${mod.replace('.', '__')}__${name}'
}

// unalias resolves a (possibly chained) numeric type alias to its base type so
// scalar aliases like `type Byte = u8` classify as their underlying type.
fn unalias(t types.Type) types.Type {
	mut cur := t
	for {
		if cur is types.Alias {
			cur = cur.base_type
		} else {
			break
		}
	}
	return cur
}

// prim_wtype maps a primitive/numeric/bool V type to a WASM value type, or none
// for aggregate/reference types the backend cannot yet represent inline.
fn prim_wtype(t_ types.Type) ?WType {
	t := unalias(t_)
	if t is types.Primitive {
		if t.props.has(.float) {
			return if t.size == 32 { WType.f32 } else { WType.f64 }
		}
		if t.props.has(.integer) {
			return if t.size == 64 { WType.i64 } else { WType.i32 }
		}
		if t.props.has(.boolean) {
			return WType.i32
		}
		return WType.i32
	}
	if t is types.Char || t is types.Rune || t is types.ISize || t is types.USize {
		return WType.i32
	}
	return none
}

const signed_int_format_fns = ['strconv__format_int', 'int_str', 'i64_str', 'i8_str', 'i16_str',
	'i32_str', 'isize_str']

const unsigned_int_format_fns = ['strconv__format_uint', 'u8_str', 'u16_str', 'u32_str', 'u64_str',
	'usize_str']

const bool_format_fns = ['bool.str', 'bool__str', 'bool_str']

fn compound_to_op(op flat.Op) flat.Op {
	return match op {
		.plus_assign { flat.Op.plus }
		.minus_assign { flat.Op.minus }
		.mul_assign { flat.Op.mul }
		.div_assign { flat.Op.div }
		.mod_assign { flat.Op.mod }
		.amp_assign { flat.Op.amp }
		.pipe_assign { flat.Op.pipe }
		.xor_assign { flat.Op.xor }
		.left_shift_assign { flat.Op.left_shift }
		.right_shift_assign { flat.Op.right_shift }
		.right_shift_unsigned_assign { flat.Op.right_shift_unsigned }
		else { flat.Op.plus }
	}
}

fn arith_op(op flat.Op, w WType, signed bool) u8 {
	match w {
		.i64 {
			return match op {
				.plus {
					0x7c
				}
				.minus {
					0x7d
				}
				.mul {
					0x7e
				}
				.div {
					if signed {
						u8(0x7f)
					} else {
						u8(0x80)
					}
				}
				.mod {
					if signed {
						u8(0x81)
					} else {
						u8(0x82)
					}
				}
				.amp {
					0x83
				}
				.pipe {
					0x84
				}
				.xor {
					0x85
				}
				.left_shift {
					0x86
				}
				.right_shift {
					0x87
				}
				.right_shift_unsigned {
					0x88
				}
				else {
					0x7c
				}
			}
		}
		.f64 {
			return match op {
				.plus { 0xa0 }
				.minus { 0xa1 }
				.mul { 0xa2 }
				.div { 0xa3 }
				else { 0xa0 }
			}
		}
		.f32 {
			return match op {
				.plus { 0x92 }
				.minus { 0x93 }
				.mul { 0x94 }
				.div { 0x95 }
				else { 0x92 }
			}
		}
		else {
			return match op {
				.plus {
					0x6a
				}
				.minus {
					0x6b
				}
				.mul {
					0x6c
				}
				.div {
					if signed {
						u8(0x6d)
					} else {
						u8(0x6e)
					}
				}
				.mod {
					if signed {
						u8(0x6f)
					} else {
						u8(0x70)
					}
				}
				.amp {
					0x71
				}
				.pipe {
					0x72
				}
				.xor {
					0x73
				}
				.left_shift {
					0x74
				}
				.right_shift {
					if signed {
						u8(0x75)
					} else {
						u8(0x76)
					}
				}
				.right_shift_unsigned {
					0x76
				}
				else {
					0x6a
				}
			}
		}
	}
}

fn cmp_op(op flat.Op, w WType, signed bool) u8 {
	match w {
		.i64 {
			return match op {
				.eq {
					0x51
				}
				.ne {
					0x52
				}
				.lt {
					if signed {
						u8(0x53)
					} else {
						u8(0x54)
					}
				}
				.gt {
					if signed {
						u8(0x55)
					} else {
						u8(0x56)
					}
				}
				.le {
					if signed {
						u8(0x57)
					} else {
						u8(0x58)
					}
				}
				.ge {
					if signed {
						u8(0x59)
					} else {
						u8(0x5a)
					}
				}
				else {
					0x51
				}
			}
		}
		.f64 {
			return match op {
				.eq { 0x61 }
				.ne { 0x62 }
				.lt { 0x63 }
				.gt { 0x64 }
				.le { 0x65 }
				.ge { 0x66 }
				else { 0x61 }
			}
		}
		.f32 {
			return match op {
				.eq { 0x5b }
				.ne { 0x5c }
				.lt { 0x5d }
				.gt { 0x5e }
				.le { 0x5f }
				.ge { 0x60 }
				else { 0x5b }
			}
		}
		else {
			return match op {
				.eq {
					0x46
				}
				.ne {
					0x47
				}
				.lt {
					if signed {
						u8(0x48)
					} else {
						u8(0x49)
					}
				}
				.gt {
					if signed {
						u8(0x4a)
					} else {
						u8(0x4b)
					}
				}
				.le {
					if signed {
						u8(0x4c)
					} else {
						u8(0x4d)
					}
				}
				.ge {
					if signed {
						u8(0x4e)
					} else {
						u8(0x4f)
					}
				}
				else {
					0x46
				}
			}
		}
	}
}

fn parse_int_literal(s_ string) i64 {
	mut s := s_.replace('_', '')
	mut neg := false
	if s.starts_with('-') {
		neg = true
		s = s[1..]
	}
	mut val := i64(0)
	if s.starts_with('0x') || s.starts_with('0X') {
		val = i64(s[2..].parse_uint(16, 64) or { 0 })
	} else if s.starts_with('0o') || s.starts_with('0O') {
		val = i64(s[2..].parse_uint(8, 64) or { 0 })
	} else if s.starts_with('0b') || s.starts_with('0B') {
		val = i64(s[2..].parse_uint(2, 64) or { 0 })
	} else {
		// parse_uint preserves the full u64 bit pattern; string.i64() would
		// saturate values above i64 max (e.g. u64(18446744073709551615)).
		val = i64(s.parse_uint(10, 64) or { 0 })
	}
	return if neg { -val } else { val }
}

// char_literal_value decodes a char-literal node value to its code point.
fn char_literal_value(s string) i64 {
	if s.len == 0 {
		return 0
	}
	if s[0] == `\\` && s.len >= 2 {
		return match s[1] {
			`n` { i64(10) }
			`t` { i64(9) }
			`r` { i64(13) }
			`\\` { i64(92) }
			`'` { i64(39) }
			`"` { i64(34) }
			`0` { i64(0) }
			`a` { i64(7) }
			`b` { i64(8) }
			`f` { i64(12) }
			`v` { i64(11) }
			else { i64(s[1]) }
		}
	}
	if s.len == 1 {
		return i64(s[0])
	}
	// multi-byte UTF-8 rune: use the first code point
	runes := s.runes()
	return if runes.len > 0 { i64(runes[0]) } else { i64(s[0]) }
}
