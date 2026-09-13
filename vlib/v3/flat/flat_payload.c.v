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

fn C.atomic_load_u32(voidptr) u32

fn C.atomic_store_u64(voidptr, u64)

fn C.atomic_load_u64(voidptr) u64

// node_payload_table_load reads the published table pointer, or nil. The
// pointer goes through the integer atomics of its width, like sync.stdatomic:
// the header's atomic_load_ptr is a bare C11 generic on some compilers.
fn node_payload_table_load() &NodePayloadTable {
	$if x32 {
		return unsafe { &NodePayloadTable(voidptr(C.atomic_load_u32(&g_node_payload_table))) }
	} $else {
		return unsafe { &NodePayloadTable(voidptr(C.atomic_load_u64(&g_node_payload_table))) }
	}
}

// node_payload_table_publish makes the freshly allocated table visible to
// lock-free lookups on other threads.
fn node_payload_table_publish(table &NodePayloadTable) {
	$if x32 {
		C.atomic_store_u32(&g_node_payload_table, u32(voidptr(table)))
	} $else {
		C.atomic_store_u64(&g_node_payload_table, u64(voidptr(table)))
	}
}

// node_payload_count_publish makes ids below `count` visible to lookups: the
// atomic store orders the chunk pointer and entry writes before it.
fn node_payload_count_publish(mut table NodePayloadTable, count u32) {
	C.atomic_store_u32(&table.count, count)
}

// node_payload_count_load reads the published id count; entries below it,
// and the chunk pointers that hold them, are visible after this load.
fn node_payload_count_load(table &NodePayloadTable) u32 {
	return C.atomic_load_u32(&table.count)
}

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
