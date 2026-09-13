module flat

// C interop behind the process-wide node payload table (see node_payload in
// flat.v): the insert spin lock and the raw chunk allocations. The table and
// its chunks must outlive every prealloc arena, and under a tracing GC they
// hold the only durable pointers to GC-managed payloads, so they are scanned
// uncollectable GC memory there (see node_payload_alloc_zeroed).

// The C11 atomics header ships with every build (sync.stdatomic); the prealloc
// atomics used elsewhere are declared only in -prealloc builds, so a plain-GC
// consumer of v3.flat could not link them.
fn C.atomic_compare_exchange_strong_u32(voidptr, voidptr, u32) bool

fn C.atomic_store_u32(voidptr, u32)

// node_payload_lock takes the payload table's insert lock.
fn node_payload_lock() {
	for {
		mut expected := u32(0)
		if C.atomic_compare_exchange_strong_u32(&g_node_payload_lock, &expected, 1) {
			break
		}
	}
}

// node_payload_unlock releases the payload table's insert lock.
fn node_payload_unlock() {
	C.atomic_store_u32(&g_node_payload_lock, 0)
}

// node_payload_alloc_zeroed allocates zeroed storage for pointers to node
// payloads. With -prealloc the payloads live in arenas, so plain C memory that
// no arena can release is right; otherwise the storage comes from
// malloc_uncollectable, which a tracing GC scans but never reclaims, so the
// payloads (and their generic_params arrays) stay reachable through it.
fn node_payload_alloc_zeroed(n usize) voidptr {
	$if prealloc {
		return unsafe { C.calloc(1, n) }
	} $else {
		ptr := unsafe { malloc_uncollectable(isize(n)) }
		if !isnil(ptr) {
			unsafe { vmemset(ptr, 0, isize(n)) }
		}
		return ptr
	}
}

// node_payload_new_table allocates the zeroed payload table, or nil.
fn node_payload_new_table() &NodePayloadTable {
	return unsafe { &NodePayloadTable(node_payload_alloc_zeroed(sizeof(NodePayloadTable))) }
}

// node_payload_new_chunk allocates one zeroed chunk of payload pointers, or nil.
fn node_payload_new_chunk() voidptr {
	return node_payload_alloc_zeroed(node_payload_chunk_size * sizeof(voidptr))
}
