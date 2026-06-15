// cm_stress.v — concurrent-mark soundness stress (self-contained, pure V).
//
// g_churn is NOT a sufficient gate for concurrent mark: it allocates-and-drops,
// so it passes even with the write barrier disabled. This repro specifically
// drives the hazard concurrent mark introduces — "hide a white behind a black":
//
//   * A long-lived global array `slots` of `&Node` pointers is built ONCE before
//     any worker starts, so it is marked BLACK early in every GC cycle.
//   * N worker threads then, for tens of thousands of iterations, allocate a
//     FRESH Node (+ a child object) and store it into one of `slots` — replacing
//     (and dropping) the previous occupant. The fresh node is therefore reachable
//     ONLY through the already-black `slots` buffer: the exact black->white edge a
//     concurrent collector must catch via the write barrier. Each worker also
//     allocates throwaway garbage every iteration to drive collections so a
//     concurrent mark overlaps the stores.
//   * Each Node carries a checksum derived from its id, and a child carrying a
//     magic word. If the barrier misses the store, the fresh node (or its child)
//     is swept while live and its memory reused -> the checksum / magic read back
//     wrong (or the program faults). After the workers join, main verifies every
//     slot. PASS = 0 mismatches.
//
// Run under -gc none (oracle: never collects), -gc e (STW), and
// -gc e -d vgc_concurrent (concurrent). All three must print the SAME output and
// 0 mismatches. The harness has teeth: stub out vgc_rescan_dirty_spans (or the
// barrier) and the concurrent run reports mismatches > 0.
//
//   ./v2 -gc none -prod -cc cc -o /tmp/cm_stress_none cm_stress.v
//   ./v2 -gc e    -prod -cc cc -o /tmp/cm_stress_e    cm_stress.v
//   ./v2 -gc e -d vgc_concurrent -prod -cc cc -o /tmp/cm_stress_cm cm_stress.v
//   diff <(/tmp/cm_stress_none) <(/tmp/cm_stress_e) && diff <(/tmp/cm_stress_none) <(/tmp/cm_stress_cm)

const magic = u64(0xC0FFEE_D00D_F00D)

struct Child {
mut:
	id    u64
	check u64
	magic u64
	pad   []u8 // a heap field, so the child has its own scannable allocation
}

struct Node {
mut:
	id    u64
	check u64
	child &Child = unsafe { nil }
}

@[direct_array_access]
fn checksum(id u64) u64 {
	return (id ^ u64(0x9E3779B97F4A7C15)) * u64(0x2545F4914F6CDD1D)
}

fn make_child(id u64) &Child {
	return &Child{
		id:    id
		check: checksum(id)
		magic: magic
		pad:   []u8{len: 24, init: u8(id & 0xff)}
	}
}

fn make_node(id u64) &Node {
	c := &Child{
		id:    id
		check: checksum(id)
		magic: magic
		pad:   []u8{len: 24, init: u8(id & 0xff)}
	}
	return &Node{
		id:    id
		check: checksum(id)
		child: c
	}
}

__global (
	slots []&Node
)

const nslots_per_worker = 25000 // large long-lived set => the concurrent mark takes
const iters = 400 // real time, opening a genuine concurrency window

fn worker(wid int) {
	base := wid * nslots_per_worker
	mut fresh_id := u64(wid) << 48
	for _ in 0 .. iters {
		// For each pair of this worker's nodes (a, b): take b's child C, hide it under
		// node a, then overwrite b's child with a FRESH child. This genuinely DROPS
		// C's only non-`a` reference (b no longer points at it; a's old child becomes
		// garbage). C lives one level BELOW the root-reachable `slots` buffer, so:
		//   * alloc-black does NOT cover C (C is an OLD object, pre-allocated);
		//   * the termination ROOT re-scan does NOT cover C (node a is an
		//     already-marked black heap object, not a root, so it is not re-traversed
		//     and its `.child` is not re-read);
		// hence if node a was already scanned (black) when `a.child = C` ran, C is
		// reachable ONLY through a black object — the hide-white-behind-black hazard.
		// ONLY the dirty-span write barrier on the `node.child = ...` store (which
		// dirties node a's span -> re-scanned at mark-termination) keeps C alive.
		for k in 0 .. nslots_per_worker / 2 {
			a := base + 2 * k
			b := base + 2 * k + 1
			c := slots[b].child // candidate victim (white during a concurrent mark)
			slots[a].child = c // hide C behind node a (may be black) <-- BARRIER here
			fresh_id++
			slots[b].child = make_child(fresh_id) // drop C's path through b; a's old child orphaned
		}
		// Drive collections so a concurrent mark overlaps the mutations above.
		mut garbage := []&Child{}
		for _ in 0 .. 64 {
			garbage << &Child{
				id:    0
				check: checksum(0)
				magic: magic
				pad:   []u8{len: 2048}
			}
		}
		if garbage.len == 99999 {
			println('unreachable') // keep `garbage` live across the loop body
		}
	}
}

fn main() {
	nworkers := 4
	// Pre-fill (init: runs per element) so the buffer + initial nodes are long-lived
	// (marked black) from the first cycle.
	slots = []&Node{len: nworkers * nslots_per_worker, init: make_node(u64(index))}

	mut threads := []thread{}
	for w in 0 .. nworkers {
		threads << spawn worker(w)
	}
	for t in threads {
		t.wait()
	}

	// Verify every slot's final node + child survived intact.
	mut mismatches := 0
	for i in 0 .. slots.len {
		n := slots[i]
		if n == unsafe { nil } {
			mismatches++
			continue
		}
		if n.check != checksum(n.id) {
			mismatches++
			continue
		}
		c := n.child
		if c == unsafe { nil } || c.magic != magic || c.check != checksum(c.id) {
			mismatches++
			continue
		}
	}
	println('cm_stress: workers=${nworkers} slots=${slots.len} iters=${iters} mismatches=${mismatches}')
	if mismatches == 0 {
		println('cm_stress PASS')
	} else {
		println('cm_stress FAIL: ${mismatches} live nodes/children reclaimed or corrupted')
	}
}
