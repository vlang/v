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
