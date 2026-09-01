module flat

import v3.token
import v3.workers

// NodeId aliases node id values used by flat.
pub type NodeId = int

// TextId is the stable identity of one canonical AST text value.
pub type TextId = u32

pub const empty_node = NodeId(-1)
pub const method_value_borrow_receiver_marker = '__v3_method_value_borrow_receiver'
pub const method_value_clone_receiver_marker_prefix = '__v3_method_value_clone_receiver:'

const empty_node_value = Node{}

// NodeKind lists node kind values used by flat.
pub enum NodeKind as u8 {
	empty
	// expressions
	int_literal
	float_literal
	bool_literal
	char_literal
	string_literal
	string_interp
	ident
	infix
	prefix
	postfix
	paren
	call
	selector
	index
	if_expr
	struct_init
	field_init
	array_literal
	array_init
	map_init
	fn_literal
	or_expr
	cast_expr
	as_expr
	enum_val
	assoc
	range
	nil_literal
	none_expr
	spawn_expr
	lock_expr
	lambda_expr
	sizeof_expr
	typeof_expr
	dump_expr
	offsetof_expr
	is_expr
	in_expr
	// statements
	expr_stmt
	assign
	decl_assign
	selector_assign
	index_assign
	return_stmt
	block
	for_stmt
	for_in_stmt
	break_stmt
	continue_stmt
	match_stmt
	match_branch
	defer_stmt
	assert_stmt
	goto_stmt
	label_stmt
	select_stmt
	select_branch
	comptime_if
	comptime_for
	asm_stmt
	// declarations
	fn_decl
	struct_decl
	field_decl
	global_decl
	const_decl
	const_field
	enum_decl
	enum_field
	type_decl
	interface_decl
	interface_field
	import_decl
	module_decl
	directive
	param
	c_fn_decl
	// top-level
	file
	sql_expr
	// A `$veb.html(...)` / `$tmpl(...)` comptime-template placeholder. It is produced
	// by parse_comptime_expr and fully expanded (into builder statements) by
	// parse_block_body at parse time, so it never reaches later phases.
	veb_template
	// A `$res()` / `$res(index)` expression. This must remain distinct from
	// `.ident` so user-spellable names cannot be reinterpreted as defer results.
	defer_result
	// A `$dbg;` statement. Keep new node kinds at the end because the hot phase
	// dispatchers use stable numeric ids for the older kinds.
	debugger_stmt
}

// Op lists op values used by flat.
pub enum Op as u8 {
	none
	plus
	minus
	mul
	div
	mod
	eq
	ne
	lt
	gt
	le
	ge
	amp
	pipe
	xor
	left_shift
	right_shift
	right_shift_unsigned
	logical_and
	logical_or
	not
	bit_not
	assign
	plus_assign
	minus_assign
	mul_assign
	div_assign
	mod_assign
	amp_assign
	pipe_assign
	xor_assign
	left_shift_assign
	right_shift_assign
	right_shift_unsigned_assign
	inc
	dec
	dot
	arrow
	gated_index
	power
	power_assign
}

// NodePayload holds the uncommon managed fields used only by declarations and
// a handful of lowering markers. Keeping it out of Node makes the hot flat
// header substantially smaller without changing string-facing phase APIs.
@[heap]
pub struct NodePayload {
pub:
	generic_params []string
}

// Comment retains source comments for syntax-preserving tools such as vfmt.
pub struct Comment {
pub:
	text string
	pos  token.Pos
}

// node_payload creates an uncommon node payload, or nil for an empty list.
pub fn node_payload(generic_params []string) &NodePayload {
	if generic_params.len == 0 {
		return &NodePayload(unsafe { nil })
	}
	return &NodePayload{
		generic_params: generic_params
	}
}

// Node represents node data used by flat.
pub struct Node {
pub mut:
	value                string
	typ                  string
	payload              &NodePayload = unsafe { nil }
	children_start       i32
	is_mut               bool
	kind                 NodeKind
	op                   Op
	skip_ownership_drops bool
	children_count       i32
	pos                  token.Pos
}

// type_text_id returns the compact canonical identity carried in this node's
// otherwise-unused source-position metadata.
@[inline]
pub fn (n &Node) type_text_id() u16 {
	return n.pos.type_text_id()
}

// set_type_text_id updates the compact type identity without disturbing a
// diagnostic reported-column override.
@[inline]
pub fn (mut n Node) set_type_text_id(id u16) {
	n.pos = n.pos.with_type_text_id(id)
}

// generic_params returns this node's uncommon generic/attribute metadata.
@[inline]
pub fn (n &Node) generic_params() []string {
	if isnil(n.payload) {
		return []string{}
	}
	return n.payload.generic_params
}

// set_generic_params replaces this node's uncommon managed payload.
@[inline]
pub fn (mut n Node) set_generic_params(params []string) {
	n.payload = node_payload(params)
}

// FlatAst represents flat ast data used by flat.
@[heap]
pub struct FlatAst {
pub mut:
	nodes           []Node
	children        []NodeId
	user_code_start int
	disabled_fns    map[string]bool
	export_fn_names map[string]string
	noreturn_fns    map[string]bool
	source_files    map[int]&token.File
	comments        []Comment
	// formatter_sources retains exact source spans or prefixes for constructs whose
	// source syntax is intentionally opaque to compiler backends.
	formatter_sources      map[int]string
	formatter_file_sources map[int]string
	// formatter_node_ends retains the full source end for nodes whose compiler-facing
	// position deliberately covers only their name or another diagnostic token.
	formatter_node_ends map[int]int
	// formatter_expanded_calls records calls whose arguments started on the next line
	// and ended with a trailing comma.
	formatter_expanded_calls map[int]bool
	// formatter_assignment_ops retains compound operator spellings that share one flat op.
	formatter_assignment_ops map[int]string
	// formatter_param_list_end retains the closing-parenthesis offset for parameter lists.
	formatter_param_list_end map[int]int
	// formatter_for_in_mut retains which for-in binder carried `mut`:
	// bit 0 is the first binder and bit 1 is the second binder.
	formatter_for_in_mut map[int]u8
	// formatter_local_sels records selectors whose direct receiver is a lexical binding.
	formatter_local_sels    map[int]bool
	formatter_migrate_json2 bool
	// Template-generated nodes keep their original template source location while
	// retaining the comptime call site used for v1-compatible diagnostic stacks.
	template_call_sites map[int]token.Pos
	template_actions    map[int]string
	// missing_imports retains source import paths for unresolved import nodes.
	missing_imports map[int]string
	// file_node_ids records every .file node the parser creates, in creation
	// order: (marker, trailing) pairs per source file. The trailing node's
	// children are the file's top-level declarations, letting collect build
	// its top-level index without a full node scan. Stages that renumber
	// nodes clear the list; consumers fall back to scanning when it is empty
	// or file_index_incomplete is set (a source file failed to read).
	file_node_ids         []int
	file_index_incomplete bool
	// source_buffers owns the storage behind zero-copy scanner strings retained
	// by AST nodes. Keeping the buffers on the AST makes the lifetime boundary
	// explicit and lets parser workers transfer ownership with their nodes.
	source_buffers []string
	// text_values/text_ids own one canonical copy of every non-empty string
	// stored in a node payload. Nodes keep string compatibility views while
	// semantic/compiler caches can use compact TextId identities.
	text_values []string
	text_ids    map[string]TextId
	worker_pool &workers.Pool = unsafe { nil }
	// specialized_fn_nodes identifies program-specific monomorphized function
	// declarations appended after parsing. Module-cache cgen keeps them with main.
	specialized_fn_nodes   map[int]bool
	specialized_fn_modules map[int]string
	specialized_fn_files   map[int]string
}

// close_workers stops the compilation-owned persistent worker pool.
pub fn (mut a FlatAst) close_workers() {
	if !isnil(a.worker_pool) {
		mut pool := a.worker_pool
		a.worker_pool = unsafe { nil }
		pool.close()
	}
}

// ensure_workers creates the compilation-owned persistent worker pool when needed.
pub fn (mut a FlatAst) ensure_workers(count int) {
	if isnil(a.worker_pool) {
		a.worker_pool = workers.new(count)
	}
}

// worker_count reports the number of persistent compiler helper threads.
pub fn (a &FlatAst) worker_count() int {
	if isnil(a.worker_pool) {
		return 0
	}
	return a.worker_pool.size()
}

// worker_tasks_run reports completed callbacks across all parallel phases.
pub fn (a &FlatAst) worker_tasks_run() u64 {
	if isnil(a.worker_pool) {
		return 0
	}
	return a.worker_pool.tasks_run()
}

// worker_stats reports scheduling and utilization across compiler phases.
pub fn (a &FlatAst) worker_stats() workers.Stats {
	if isnil(a.worker_pool) {
		return workers.Stats{}
	}
	return a.worker_pool.stats()
}

// set_node_is_mut updates a node's mut declaration marker in place.
pub fn (mut a FlatAst) set_node_is_mut(id NodeId, is_mut bool) {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return
	}
	// Mutate the array slot in place; copying flat.Node would duplicate managed fields.
	unsafe {
		mut node := &a.nodes[int(id)]
		node.is_mut = is_mut
	}
}

// new creates a FlatAst value for flat.
pub fn FlatAst.new() FlatAst {
	return FlatAst{
		nodes:                    []Node{cap: 256}
		children:                 []NodeId{cap: 512}
		disabled_fns:             map[string]bool{}
		export_fn_names:          map[string]string{}
		noreturn_fns:             map[string]bool{}
		source_files:             map[int]&token.File{}
		template_call_sites:      map[int]token.Pos{}
		template_actions:         map[int]string{}
		missing_imports:          map[int]string{}
		formatter_sources:        map[int]string{}
		formatter_file_sources:   map[int]string{}
		formatter_node_ends:      map[int]int{}
		formatter_expanded_calls: map[int]bool{}
		formatter_assignment_ops: map[int]string{}
		formatter_param_list_end: map[int]int{}
		formatter_for_in_mut:     map[int]u8{}
		formatter_local_sels:     map[int]bool{}
		text_ids:                 map[string]TextId{}
		specialized_fn_nodes:     map[int]bool{}
		specialized_fn_modules:   map[int]string{}
		specialized_fn_files:     map[int]string{}
	}
}

// intern_text returns the canonical AST-owned copy and stable identity of text.
pub fn (mut a FlatAst) intern_text(value string) (TextId, string) {
	if value.len == 0 {
		return TextId(0), ''
	}
	if id := a.text_ids[value] {
		return id, a.text_values[int(id) - 1]
	}
	canonical := value.clone()
	id := TextId(a.text_values.len + 1)
	a.text_values << canonical
	a.text_ids[canonical] = id
	return id, a.text_values.last()
}

// intern_texts_from replays the source AST's compact text table into a.
// The source table is already ordered by first occurrence, so this preserves
// deterministic text identities while avoiding a second walk over every node.
pub fn (mut a FlatAst) intern_texts_from(source &FlatAst) {
	for value in source.text_values {
		a.intern_text(value)
	}
}

// reserve_transform_texts keeps canonical text-table backing in the
// compilation arena before a disposable transform scope starts.
pub fn (mut a FlatAst) reserve_transform_texts(headroom int) {
	if headroom <= 0 {
		return
	}
	unsafe { a.text_values.grow_cap(headroom) }
	a.text_ids.reserve(u32(a.text_ids.len + headroom))
}

// promote_transform_texts_from moves canonical strings inserted by a scoped
// transform into the current arena and rebuilds table backing only if it grew
// into the disposable scope.
pub fn (mut a FlatAst) promote_transform_texts_from(start int, scope voidptr) {
	first := if start < 0 { 0 } else { start }
	mut rebuild := scoped_text_storage_owned(scope, a.text_values.data)
	$if prealloc {
		// Any string interned inside the disposable scope may have placed its
		// bytes — or grown the lookup map's internal backing — into that scope.
		// There is no supported API to inspect a `map`'s backing pointers, so
		// rather than mirror the private builtin.map/DenseArray ABI we rebuild
		// the whole text table into the current arena whenever the scope interned
		// anything. This is a superset of the cases where storage was actually
		// scoped, so it is always safe.
		if a.text_values.len > first {
			rebuild = true
		}
	}
	if rebuild {
		mut values, mut ids := a.clone_text_table_owned()
		a.text_values = values
		a.text_ids = ids.move()
	}
}

fn scoped_text_storage_owned(scope voidptr, ptr voidptr) bool {
	$if prealloc {
		return unsafe { prealloc_scope_owns(scope, ptr) }
	}
	return false
}

// clone_text_table_owned copies the canonical text table and rebuilds its
// lookup map with storage owned by the current allocation arena.
pub fn (a &FlatAst) clone_text_table_owned() ([]string, map[string]TextId) {
	mut values := []string{cap: a.text_values.len}
	mut ids := map[string]TextId{}
	ids.reserve(u32(a.text_values.len))
	for value in a.text_values {
		canonical := value.clone()
		values << canonical
		ids[canonical] = TextId(values.len)
	}
	return values, ids
}

// text resolves a stable AST text identity.
pub fn (a &FlatAst) text(id TextId) string {
	idx := int(id) - 1
	if idx < 0 || idx >= a.text_values.len {
		return ''
	}
	return a.text_values[idx]
}

// text_count returns the number of unique non-empty AST text values.
pub fn (a &FlatAst) text_count() int {
	return a.text_values.len
}

// type_text_id returns the compact canonical identity for value, or 0 when it
// has not been interned or the compilation's text table exceeds u16 range.
pub fn (a &FlatAst) type_text_id(value string) u16 {
	if value.len == 0 {
		return 0
	}
	if id := a.text_ids[value] {
		if id <= 65535 {
			return u16(id)
		}
	}
	return 0
}

// node_type_text_id preserves an already-matching identity without a map
// lookup. A changed or newly synthesized spelling is assigned during the
// ordered canonicalization pass after worker merge.
@[inline]
pub fn (a &FlatAst) node_type_text_id(value string, current u16) u16 {
	if current != 0 {
		canonical := a.text(TextId(current))
		if canonical.len == value.len && canonical.str == value.str {
			return current
		}
	}
	return 0
}

// intern_node_texts_from canonicalizes managed payloads in nodes[start..].
// This runs serially after parse/transform worker merges, so the text table
// itself requires no synchronization and cannot retain worker-arena storage.
pub fn (mut a FlatAst) intern_node_texts_from(start int) {
	a.intern_node_texts_range(start, a.nodes.len)
}

// intern_node_texts_range canonicalizes managed payloads in nodes[start..end).
// The bounded form serves the parallel parse merge, where later chunks are
// already present in the node array but must be interned in chunk order.
pub fn (mut a FlatAst) intern_node_texts_range(start int, end int) {
	first := if start < 0 { 0 } else { start }
	if first >= end {
		return
	}
	// Node texts repeat heavily by exact string instance (already-interned
	// strings share `.str`), so a direct-mapped pointer probe skips the
	// content-hash table lookup for the overwhelming majority of nodes.
	// Nothing is freed during this pass, so pointer keys stay valid.
	mut cache_ptrs := unsafe { []voidptr{len: 4096} }
	mut cache_vals := []string{len: 4096}
	mut type_cache_ptrs := unsafe { []voidptr{len: 4096} }
	mut type_cache_vals := []string{len: 4096}
	mut type_cache_ids := []u16{len: 4096}
	for idx in first .. end {
		a.intern_node_texts_one(idx, mut cache_ptrs, mut cache_vals, mut type_cache_ptrs, mut
			type_cache_vals, mut type_cache_ids)
	}
}

// intern_node_texts_at canonicalizes only the listed node indexes (in list
// order). The parallel parse merge pre-rebinds table hits on the copy threads
// and records the remaining nodes here, so canonical-table insertion order
// still matches the serial merge exactly.
pub fn (mut a FlatAst) intern_node_texts_at(indexes []int) {
	if indexes.len == 0 {
		return
	}
	mut cache_ptrs := unsafe { []voidptr{len: 4096} }
	mut cache_vals := []string{len: 4096}
	mut type_cache_ptrs := unsafe { []voidptr{len: 4096} }
	mut type_cache_vals := []string{len: 4096}
	mut type_cache_ids := []u16{len: 4096}
	for idx in indexes {
		a.intern_node_texts_one(idx, mut cache_ptrs, mut cache_vals, mut type_cache_ptrs, mut
			type_cache_vals, mut type_cache_ids)
	}
}

fn (mut a FlatAst) intern_node_texts_one(idx int, mut cache_ptrs []voidptr, mut cache_vals []string, mut type_cache_ptrs []voidptr, mut type_cache_vals []string, mut type_cache_ids []u16) {
	a.nodes[idx].value = a.intern_text_ptr_cached(a.nodes[idx].value, mut cache_ptrs, mut
		cache_vals)
	type_id, canonical_type := a.intern_type_text_ptr_cached(a.nodes[idx].typ, mut type_cache_ptrs, mut
		type_cache_vals, mut type_cache_ids)
	a.nodes[idx].typ = canonical_type
	a.nodes[idx].set_type_text_id(type_id)
	params := a.nodes[idx].generic_params()
	if params.len > 0 {
		// Always rebuild the payload: the params array itself may live in a
		// stage arena even when every param string is already canonical.
		mut canonical_params := []string{cap: params.len}
		for item in params {
			canonical_params << a.intern_text_ptr_cached(item, mut cache_ptrs, mut cache_vals)
		}
		a.nodes[idx].set_generic_params(canonical_params)
	}
}

// intern_type_text_ptr_cached canonicalizes a node type spelling and returns
// its compact identity. Programs with more than 65535 texts use 0 and retain
// the existing string-keyed fallback.
pub fn (mut a FlatAst) intern_type_text_ptr_cached(value string, mut cache_ptrs []voidptr, mut cache_vals []string, mut cache_ids []u16) (u16, string) {
	if value.len == 0 {
		return 0, ''
	}
	slot := int((u64(voidptr(value.str)) >> 4) & 4095)
	if cache_ptrs[slot] == voidptr(value.str) && cache_vals[slot].len == value.len {
		return cache_ids[slot], cache_vals[slot]
	}
	id, canonical := a.intern_text(value)
	compact_id := if id <= 65535 { u16(id) } else { u16(0) }
	cache_ptrs[slot] = voidptr(value.str)
	cache_vals[slot] = canonical
	cache_ids[slot] = compact_id
	return compact_id, canonical
}

// probe_text_ptr_cached is the read-only twin of intern_text_ptr_cached: it
// rebinds `value` to its canonical copy when the text table already holds the
// content and reports a miss otherwise, never mutating the table. Safe on
// worker threads only while no thread inserts into the table.
pub fn (a &FlatAst) probe_text_ptr_cached(value string, mut cache_ptrs []voidptr, mut cache_vals []string) (string, bool) {
	if value.len == 0 {
		return '', true
	}
	slot := int((u64(voidptr(value.str)) >> 4) & 4095)
	if cache_ptrs[slot] == voidptr(value.str) && cache_vals[slot].len == value.len {
		return cache_vals[slot], true
	}
	if id := a.text_ids[value] {
		canonical := a.text_values[int(id) - 1]
		cache_ptrs[slot] = voidptr(value.str)
		cache_vals[slot] = canonical
		return canonical, true
	}
	return value, false
}

// probe_type_text_ptr_cached is the compact-id form used while parallel parse
// workers probe the frozen master text table.
pub fn (a &FlatAst) probe_type_text_ptr_cached(value string, mut cache_ptrs []voidptr, mut cache_vals []string, mut cache_ids []u16) (u16, string, bool) {
	if value.len == 0 {
		return 0, '', true
	}
	slot := int((u64(voidptr(value.str)) >> 4) & 4095)
	if cache_ptrs[slot] == voidptr(value.str) && cache_vals[slot].len == value.len {
		return cache_ids[slot], cache_vals[slot], true
	}
	if id := a.text_ids[value] {
		canonical := a.text_values[int(id) - 1]
		compact_id := if id <= 65535 { u16(id) } else { u16(0) }
		cache_ptrs[slot] = voidptr(value.str)
		cache_vals[slot] = canonical
		cache_ids[slot] = compact_id
		return compact_id, canonical, true
	}
	return 0, value, false
}

pub fn (mut a FlatAst) intern_text_ptr_cached(value string, mut cache_ptrs []voidptr, mut cache_vals []string) string {
	if value.len == 0 {
		return ''
	}
	slot := int((u64(voidptr(value.str)) >> 4) & 4095)
	// The length check guards against distinct strings sharing one base pointer
	// (unsafe zero-copy slices); same pointer + same length means same content.
	if cache_ptrs[slot] == voidptr(value.str) && cache_vals[slot].len == value.len {
		return cache_vals[slot]
	}
	_, canonical := a.intern_text(value)
	cache_ptrs[slot] = voidptr(value.str)
	cache_vals[slot] = canonical
	return canonical
}

// intern_metadata_texts canonicalizes all source-derived FlatAst map keys and
// values. Once this and intern_node_texts_from have run, source buffers are no
// longer part of the AST representation and can be released.
pub fn (mut a FlatAst) intern_metadata_texts() {
	mut disabled_fns := map[string]bool{}
	for name, disabled in a.disabled_fns {
		_, canonical := a.intern_text(name)
		disabled_fns[canonical] = disabled
	}
	a.disabled_fns = disabled_fns.move()
	mut export_fn_names := map[string]string{}
	for name, value in a.export_fn_names {
		_, canonical_name := a.intern_text(name)
		_, canonical_value := a.intern_text(value)
		export_fn_names[canonical_name] = canonical_value
	}
	a.export_fn_names = export_fn_names.move()
	mut noreturn_fns := map[string]bool{}
	for name, is_noreturn in a.noreturn_fns {
		_, canonical := a.intern_text(name)
		noreturn_fns[canonical] = is_noreturn
	}
	a.noreturn_fns = noreturn_fns.move()
}

// source_position resolves an AST source position to file/line/column metadata.
pub fn (a &FlatAst) source_position(pos token.Pos) ?token.Position {
	if !pos.is_valid() {
		return none
	}
	file := a.source_files[pos.id] or { return none }
	if pos.offset < 0 || pos.offset > file.size {
		return none
	}
	return file.position_at(pos.offset)
}

// add updates add state for FlatAst.
pub fn (mut a FlatAst) add(kind NodeKind) NodeId {
	id := NodeId(a.nodes.len)
	a.nodes << Node{
		kind: kind
	}
	return id
}

// add_id updates add id state for FlatAst.
pub fn (mut a FlatAst) add_id(kind_id int) NodeId {
	id := NodeId(a.nodes.len)
	a.nodes << Node{
		kind: node_kind_from_id(kind_id)
	}
	return id
}

// node_kind_from_id converts node kind from id data for flat.
@[inline]
pub fn node_kind_from_id(id int) NodeKind {
	return unsafe { NodeKind(id) }
}

// add_val updates add val state for FlatAst.
pub fn (mut a FlatAst) add_val(kind NodeKind, value string) NodeId {
	id := NodeId(a.nodes.len)
	a.nodes << Node{
		kind:  kind
		value: value
	}
	return id
}

// add_val_id updates add val id state for FlatAst.
pub fn (mut a FlatAst) add_val_id(kind_id int, value string) NodeId {
	id := NodeId(a.nodes.len)
	a.nodes << Node{
		kind:  node_kind_from_id(kind_id)
		value: value
	}
	return id
}

// with_shifted_children returns a copy of the node whose children_start is moved
// by `shift`. children_start lives in the immutable section of Node, so callers
// that relocate a node's children block (e.g. the parallel-transform merge) build
// a fresh node instead of mutating in place.
pub fn (n Node) with_shifted_children(shift i32) Node {
	return Node{
		value:                n.value
		typ:                  n.typ
		payload:              n.payload
		pos:                  n.pos
		children_start:       n.children_start + shift
		children_count:       n.children_count
		kind:                 n.kind
		op:                   n.op
		is_mut:               n.is_mut
		skip_ownership_drops: n.skip_ownership_drops
	}
}

// with_pos returns a copy of the node with source position `pos`.
pub fn (n Node) with_pos(pos token.Pos) Node {
	return Node{
		value:                n.value
		typ:                  n.typ
		payload:              n.payload
		pos:                  pos.with_type_text_id(n.type_text_id())
		children_start:       n.children_start
		children_count:       n.children_count
		kind:                 n.kind
		op:                   n.op
		is_mut:               n.is_mut
		skip_ownership_drops: n.skip_ownership_drops
	}
}

// clone_owned returns a deep copy whose managed text and payload survive scoped arenas.
pub fn (n Node) clone_owned() Node {
	mut params := []string{cap: n.generic_params().len}
	for param in n.generic_params() {
		params << param.clone()
	}
	return Node{
		value:                n.value.clone()
		typ:                  n.typ.clone()
		payload:              node_payload(params)
		pos:                  n.pos
		children_start:       n.children_start
		children_count:       n.children_count
		kind:                 n.kind
		op:                   n.op
		is_mut:               n.is_mut
		skip_ownership_drops: n.skip_ownership_drops
	}
}

// add_node updates add node state for FlatAst.
pub fn (mut a FlatAst) add_node(node Node) NodeId {
	id := NodeId(a.nodes.len)
	mut stored := node
	stored.set_type_text_id(a.node_type_text_id(stored.typ, stored.type_text_id()))
	if a.nodes.len < a.nodes.cap {
		unsafe {
			mut slot := &Node(&u8(a.nodes.data) + usize(a.nodes.len) * sizeof(Node))
			*slot = stored
			a.nodes.len++
		}
		return id
	}
	a.nodes << stored
	return id
}

// child_count converts a dynamic child count to Node's compact storage type.
pub fn child_count(count int) i32 {
	if count < 0 {
		panic('flat node cannot have a negative child count')
	}
	return i32(count)
}

// begin_children supports begin children handling for FlatAst.
pub fn (mut a FlatAst) begin_children() int {
	return a.children.len
}

// add_child updates add child state for FlatAst.
pub fn (mut a FlatAst) add_child(id NodeId) {
	if a.children.len < a.children.cap {
		unsafe {
			mut slot := &NodeId(&u8(a.children.data) + usize(a.children.len) * sizeof(NodeId))
			*slot = id
			a.children.len++
		}
		return
	}
	a.children << id
}

// child supports child handling for FlatAst.
@[inline]
pub fn (a &FlatAst) child(node &Node, index int) NodeId {
	child_index := node.children_start + index
	if index < 0 || index >= node.children_count || child_index < 0 || child_index >= a.children.len {
		return empty_node
	}
	// Bounds are checked above; avoid a second array bounds check on this hot path.
	return unsafe { a.children[child_index] }
}

// child_node supports child node handling for FlatAst.
@[inline]
pub fn (a &FlatAst) child_node(node &Node, index int) &Node {
	id := a.child(node, index)
	if int(id) < 0 || int(id) >= a.nodes.len {
		return &empty_node_value
	}
	// Bounds are checked above; avoid a second array bounds check on this hot path.
	return unsafe { &a.nodes[int(id)] }
}

// node supports node handling for FlatAst.
@[inline]
pub fn (a &FlatAst) node(id NodeId) &Node {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return &empty_node_value
	}
	// Bounds are checked above; avoid a second array bounds check on this hot path.
	return unsafe { &a.nodes[int(id)] }
}

// children_of supports children of handling for FlatAst.
pub fn (a &FlatAst) children_of(node &Node) []NodeId {
	if node.children_count == 0 {
		return []
	}
	if node.children_start < 0 || node.children_start >= a.children.len {
		return []
	}
	if node.children_start + node.children_count > a.children.len {
		return a.children[node.children_start..]
	}
	return a.children[node.children_start..node.children_start + node.children_count]
}

// print_tree updates print tree state for FlatAst.
pub fn (a &FlatAst) print_tree(id NodeId, indent int) {
	node := a.nodes[int(id)]
	mut prefix := ''
	for _ in 0 .. indent {
		prefix += '  '
	}
	mut info := '${node.kind}'
	if node.value.len > 0 {
		info += ' "${node.value}"'
	}
	if node.op != .none {
		info += ' op=${node.op}'
	}
	if node.typ.len > 0 {
		info += ' typ=${node.typ}'
	}
	println('${prefix}${info}')
	for i in 0 .. node.children_count {
		child_id := a.children[node.children_start + i]
		if int(child_id) >= 0 {
			a.print_tree(child_id, indent + 1)
		}
	}
}
