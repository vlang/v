module types

import math.bits

// The dynamic dispatch of a scoped parallel check hands the cheapest bodies out
// last, in chunks dynamic_check_small_div times smaller than the others: a worker
// that finds the queue empty has at most such a chunk left, where a whole batch
// used to leave most of the pool idle for a tenth of the phase. They hold about
// dynamic_check_small_pct percent of the work. A worker still checks a batch's
// worth of them on one checker fork (see check_dynamic_batches).
const dynamic_check_small_pct = 40
const dynamic_check_small_div = 6

// split_dynamic_check_items splits `items` into about `n` chunks of equal cost
// for the bodies that cost most, followed by `small_div` times smaller chunks of
// the cheapest ones, which hold up to `small_pct` percent of the cost. The items
// of a chunk stay in their order.
fn split_dynamic_check_items(items []CheckWorkItem, n int, small_pct int, small_div int) [][]CheckWorkItem {
	if n < 2 || small_pct <= 0 || small_div < 2 || items.len < n * small_div {
		return split_check_items(items, n)
	}
	// The cheapest bodies are those below a power of two of cost.
	mut by_size := [65]i64{}
	mut total := i64(0)
	for it in items {
		cost := i64(it.cost) + 1
		total += cost
		by_size[bits.len_64(u64(cost))] += cost
	}
	mut small_bits := 0
	mut small_cost := i64(0)
	for size in 0 .. by_size.len {
		if (small_cost + by_size[size]) * 100 > total * small_pct {
			break
		}
		small_cost += by_size[size]
		small_bits = size + 1
	}
	if small_cost == 0 || small_cost == total {
		return split_check_items(items, n)
	}
	mut big := []int{cap: items.len}
	mut small := []int{cap: items.len}
	for i, it in items {
		if bits.len_64(u64(i64(it.cost) + 1)) < small_bits {
			small << i
		} else {
			big << i
		}
	}
	n_big := int_max(1, int_min(big.len, int(i64(n) * (total - small_cost) / total)))
	n_small := int_max(1, int_min(small.len, int(i64(n) * small_cost * small_div / total)))
	mut chunks := [][]CheckWorkItem{cap: n_big + n_small}
	append_check_item_buckets(items, big, n_big, mut chunks)
	append_check_item_buckets(items, small, n_small, mut chunks)
	return chunks
}

// append_check_item_buckets appends `n` chunks of about equal cost with the
// items at `selected`, assigned heaviest first as split_check_items assigns
// them. The items of a chunk stay in the order of `selected`.
@[direct_array_access]
fn append_check_item_buckets(items []CheckWorkItem, selected []int, n int, mut chunks [][]CheckWorkItem) {
	mut loads := []i64{len: n}
	mut ranked := []CheckItemRank{cap: selected.len}
	for pos, i in selected {
		ranked << CheckItemRank{
			rank: items[i].rank
			idx:  pos
		}
	}
	ranked.sort(a.rank > b.rank)
	mut least_loaded := []int{len: n, init: index}
	mut bucket_of := []int{len: selected.len}
	for r in ranked {
		best := least_loaded[0]
		bucket_of[r.idx] = best
		loads[best] += i64(items[selected[r.idx]].cost) + 1
		restore_check_load_heap(mut least_loaded, loads)
	}
	base := chunks.len
	for _ in 0 .. n {
		chunks << []CheckWorkItem{}
	}
	for pos, i in selected {
		chunks[base + bucket_of[pos]] << items[i]
	}
}

// check_dynamic_batches checks chunks of bodies from the queue of `a` until it
// is empty, as check_scoped_batches checks one chunk as one batch, with a batch
// made of as many chunks as it takes to reach the cost `quota`: the receiver is
// the worker's accumulator, and every batch uses a fresh fork in the batch
// arena. The bodies of a batch are promoted to `master` once they are merged.
fn (mut tc TypeChecker) check_dynamic_batches(mut a CheckChunkArgs, chunks &[][]CheckWorkItem, quota i64, mut master TypeChecker) {
	// Both lists are filled while the worker's own arena is current, and the
	// chunks of a batch are noted without allocating.
	mut pulled := []int{cap: chunks.len}
	mut batch_items := []CheckWorkItem{cap: 1024}
	for {
		first := <-a.chunk_queue or { break }
		pulled.clear()
		scratch_scope := tc.begin_check_batch_scope()
		mut batch := tc.fork_for_parallel_check()
		mut cost := i64(0)
		mut chunk_idx := first
		for {
			chunk := unsafe { chunks[chunk_idx] }
			batch.check_fn_items_serial(chunk)
			pulled << chunk_idx
			for item in chunk {
				cost += i64(item.cost) + 1
			}
			// A chunk that is a batch on its own is not joined by another one.
			if cost * 4 >= quota * 3 {
				break
			}
			chunk_idx = <-a.chunk_queue or { break }
		}
		check_worker_scope_leave(scratch_scope)
		batch_items.clear()
		for pulled_idx in pulled {
			batch_items << unsafe { chunks[pulled_idx] }
		}
		tc.clone_parallel_worker_node_caches(batch_items)
		tc.merge_parallel_check_worker_scoped(batch, true)
		tc.end_check_batch_scope(scratch_scope)
		a.checked += batch_items.len
		master.promote_check_chunk(batch_items, mut a.promotion, a.scope)
	}
}

// selected_fn_scan_chunks_per_job is the number of chunks of about equal cost
// that the scan of a reachability walk queues for each of its jobs.
const selected_fn_scan_chunks_per_job = 16

// selected_fn_scan_chunks orders the declarations of a reachability walk for
// its scan: `order[bounds[c] .. bounds[c + 1]]` are the declarations of chunk
// `c`. A body that costs a chunk's worth is a chunk of its own, and those come
// first, so that the scan does not end on one of them while the pool idles.
fn selected_fn_scan_chunks(decls &SelectedFnDecls, n_jobs int) ([]i32, []i32) {
	mut total := i64(0)
	for decl in decls.list {
		total += i64(decl.cost)
	}
	target := int_max(1, int(total / i64(int_max(1, n_jobs) * selected_fn_scan_chunks_per_job)))
	mut order := []i32{cap: decls.list.len}
	mut bounds := []i32{cap: n_jobs * selected_fn_scan_chunks_per_job * 2 + 2}
	for i, decl in decls.list {
		if decl.cost >= target {
			bounds << i32(order.len)
			order << i32(i)
		}
	}
	mut cost := i64(0)
	mut is_open := false
	for i, decl in decls.list {
		if decl.cost >= target {
			continue
		}
		if !is_open || cost >= target {
			bounds << i32(order.len)
			cost = 0
			is_open = true
		}
		order << i32(i)
		cost += i64(decl.cost)
	}
	bounds << i32(order.len)
	return order, bounds
}
