module flat

import v3.token

// NodeId aliases node id values used by flat.
pub type NodeId = int

pub const empty_node = NodeId(-1)

const empty_node_value = Node{}

// NodeKind lists node kind values used by flat.
pub enum NodeKind {
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
}

// Op lists op values used by flat.
pub enum Op {
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
}

// Node represents node data used by flat.
pub struct Node {
pub mut:
	value          string
	typ            string
	generic_params []string
	kind_id        int
	is_mut         bool
pub:
	pos            token.Pos
	children_start i32
	children_count i16
	kind           NodeKind
	op             Op
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
	// specialized_fn_nodes identifies program-specific monomorphized function
	// declarations appended after parsing. Module-cache cgen keeps them with main.
	specialized_fn_nodes map[int]bool
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
		nodes:                []Node{cap: 256}
		children:             []NodeId{cap: 512}
		disabled_fns:         map[string]bool{}
		export_fn_names:      map[string]string{}
		noreturn_fns:         map[string]bool{}
		specialized_fn_nodes: map[int]bool{}
	}
}

// add updates add state for FlatAst.
pub fn (mut a FlatAst) add(kind NodeKind) NodeId {
	id := NodeId(a.nodes.len)
	a.nodes << Node{
		kind:    kind
		kind_id: int(kind)
	}
	return id
}

// add_id updates add id state for FlatAst.
pub fn (mut a FlatAst) add_id(kind_id int) NodeId {
	id := NodeId(a.nodes.len)
	a.nodes << Node{
		kind:    node_kind_from_id(kind_id)
		kind_id: kind_id
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
		kind:    kind
		kind_id: int(kind)
		value:   value
	}
	return id
}

// add_val_id updates add val id state for FlatAst.
pub fn (mut a FlatAst) add_val_id(kind_id int, value string) NodeId {
	id := NodeId(a.nodes.len)
	a.nodes << Node{
		kind:    node_kind_from_id(kind_id)
		kind_id: kind_id
		value:   value
	}
	return id
}

// with_shifted_children returns a copy of the node whose children_start is moved
// by `shift`. children_start lives in the immutable section of Node, so callers
// that relocate a node's children block (e.g. the parallel-transform merge) build
// a fresh node instead of mutating in place.
pub fn (n Node) with_shifted_children(shift i32) Node {
	return Node{
		value:          n.value
		typ:            n.typ
		generic_params: n.generic_params
		kind_id:        n.kind_id
		pos:            n.pos
		children_start: n.children_start + shift
		children_count: n.children_count
		kind:           n.kind
		op:             n.op
		is_mut:         n.is_mut
	}
}

// add_node updates add node state for FlatAst.
pub fn (mut a FlatAst) add_node(node Node) NodeId {
	id := NodeId(a.nodes.len)
	mut n := node
	if n.kind_id == 0 && int(n.kind) != 0 {
		n.kind_id = int(n.kind)
	}
	a.nodes << n
	return id
}

// child_count converts a dynamic child count to Node's compact storage type.
pub fn child_count(count int) i16 {
	if count > 32767 {
		panic('flat node has too many children')
	}
	return i16(count)
}

// begin_children supports begin children handling for FlatAst.
pub fn (mut a FlatAst) begin_children() int {
	return a.children.len
}

// add_child updates add child state for FlatAst.
pub fn (mut a FlatAst) add_child(id NodeId) {
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
