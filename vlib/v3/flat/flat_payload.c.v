module flat

// C interop behind the process-wide node payload table (see node_payload in
// flat.v): the insert spin lock and the raw chunk allocations. The table and
// its chunks are plain C allocations so they outlive every prealloc arena.

fn C.v_prealloc_atomic_cas_i32(ptr &i32, expected int, desired int) int

fn C.v_prealloc_atomic_store_i32(ptr &i32, val int) int

// node_payload_lock takes the payload table's insert lock.
fn node_payload_lock() {
	for C.v_prealloc_atomic_cas_i32(&g_node_payload_lock, 0, 1) == 0 {
	}
}

// node_payload_unlock releases the payload table's insert lock.
fn node_payload_unlock() {
	C.v_prealloc_atomic_store_i32(&g_node_payload_lock, 0)
}

// node_payload_new_table allocates the zeroed payload table, or nil.
fn node_payload_new_table() &NodePayloadTable {
	return unsafe { &NodePayloadTable(C.calloc(1, sizeof(NodePayloadTable))) }
}

// node_payload_new_chunk allocates one zeroed chunk of payload pointers, or nil.
fn node_payload_new_chunk() voidptr {
	return unsafe { C.calloc(node_payload_chunk_size, sizeof(voidptr)) }
}
