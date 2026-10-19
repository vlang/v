module flat

// discard_unused_capacity releases resident pages beyond the live AST after
// parallel workers have joined and their append regions have been compacted.
// The virtual reservation stays writable, so later appends retain their capacity.
// No surviving view may read or write beyond this AST's live array lengths.
@[unsafe]
pub fn (a &FlatAst) discard_unused_capacity() {
	$if prealloc {
		// Only full pages inside unused array capacity are discarded. Live nodes,
		// array headers, and neighboring arena allocations remain untouched.
		if a.nodes.cap > a.nodes.len {
			unsafe {
				prealloc_discard_pages(&u8(a.nodes.data) + usize(a.nodes.len) * sizeof(Node), usize(a.nodes.cap - a.nodes.len) * sizeof(Node))
			}
		}
		if a.children.cap > a.children.len {
			unsafe {
				prealloc_discard_pages(&u8(a.children.data) + usize(a.children.len) * sizeof(NodeId), usize(a.children.cap - a.children.len) * sizeof(NodeId))
			}
		}
	}
}

// discard_replaced_storage releases the resident pages of node and children
// buffers that were replaced by larger copies, for example when the transform
// stage reserves its append regions. Nothing reads a replaced buffer: every
// stage reaches the AST through `nodes`/`children`, which point to the copy.
// A buffer that is still current is left alone.
@[unsafe]
pub fn (a &FlatAst) discard_replaced_storage(old_nodes voidptr, old_nodes_cap int, old_children voidptr, old_children_cap int) {
	$if prealloc {
		if old_nodes != nil && old_nodes != a.nodes.data && old_nodes_cap > 0 {
			unsafe { prealloc_discard_pages(old_nodes, usize(old_nodes_cap) * sizeof(Node)) }
		}
		if old_children != nil && old_children != a.children.data && old_children_cap > 0 {
			unsafe { prealloc_discard_pages(old_children, usize(old_children_cap) * sizeof(NodeId)) }
		}
	}
}
