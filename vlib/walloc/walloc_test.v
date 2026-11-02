module walloc

fn fill_pattern(p &u8, size int, seed u8) {
	unsafe {
		for i in 0 .. size {
			p[i] = seed ^ u8(i & 0xff)
		}
	}
}

fn check_pattern(p &u8, size int, seed u8) {
	unsafe {
		for i in 0 .. size {
			assert p[i] == seed ^ u8(i & 0xff)
		}
	}
}

fn test_small_alloc_sizes() {
	mut w := new_walloc()
	sizes := [1, 2, 3, 5, 8, 9, 15, 17, 24, 33, 40, 48, 64, 80, 100, 128, 200, 248, 255, 256]
	mut ptrs := []voidptr{cap: sizes.len}
	unsafe {
		for s in sizes {
			p := w.malloc(uint(s))
			assert p != nil
			assert uint(p) & 7 == 0
			fill_pattern(&u8(p), s, 0x5a)
			ptrs << p
		}
		for i, s in sizes {
			check_pattern(&u8(ptrs[i]), s, 0x5a)
		}
		for p in ptrs {
			w.free_(p)
		}
		// freed small objects are reused
		q := w.malloc(8)
		assert q != nil
		w.free_(q)
	}
}

fn test_small_freelist_reuse_many() {
	mut w := new_walloc()
	mut ptrs := []voidptr{cap: 1000}
	unsafe {
		for _ in 0 .. 1000 {
			p := w.malloc(24)
			assert p != nil
			ptrs << p
		}
		for p in ptrs {
			w.free_(p)
		}
		// allocate again; all must succeed and be usable
		for i in 0 .. 1000 {
			p := w.malloc(24)
			assert p != nil
			fill_pattern(&u8(p), 24, u8(i & 0xff))
			ptrs[i] = p
		}
		for i in 0 .. 1000 {
			check_pattern(&u8(ptrs[i]), 24, u8(i & 0xff))
		}
		for p in ptrs {
			w.free_(p)
		}
	}
}

fn test_zero_size_and_nil_free() {
	mut w := new_walloc()
	unsafe {
		p := w.malloc(0)
		assert p != nil
		w.free_(p)
		w.free_(nil)
		w.free_(unsafe { nil })
	}
}

fn test_large_alloc() {
	mut w := new_walloc()
	sizes := [257, 300, 1000, 4096, 70000, 200000]
	mut ptrs := []voidptr{cap: sizes.len}
	unsafe {
		for s in sizes {
			p := w.malloc(uint(s))
			assert p != nil
			assert uint(p) & 7 == 0
			fill_pattern(&u8(p), s, 0xa5)
			ptrs << p
		}
		for i, s in sizes {
			check_pattern(&u8(ptrs[i]), s, 0xa5)
		}
		for p in ptrs {
			w.free_(p)
		}
	}
}

fn test_calloc_zeroed() {
	mut w := new_walloc()
	unsafe {
		for s in [1, 16, 300, 5000] {
			p := w.calloc(uint(s))
			assert p != nil
			b := &u8(p)
			for i in 0 .. s {
				assert b[i] == 0
			}
			fill_pattern(b, s, 0x11)
			w.free_(p)
			// reallocating the same class must be zeroed again
			q := w.calloc(uint(s))
			assert q != nil
			c := &u8(q)
			for i in 0 .. s {
				assert c[i] == 0
			}
			w.free_(q)
		}
	}
}

fn test_large_best_fit_and_merge() {
	mut w := new_walloc()
	unsafe {
		a := w.malloc(1000)
		b := w.malloc(2000)
		c := w.malloc(3000)
		assert a != nil && b != nil && c != nil
		fill_pattern(&u8(a), 1000, 1)
		fill_pattern(&u8(b), 2000, 2)
		fill_pattern(&u8(c), 3000, 3)
		// free the middle block, then ask for slightly less: best fit reuse
		w.free_(b)
		d := w.malloc(1900)
		assert d != nil
		fill_pattern(&u8(d), 1900, 4)
		check_pattern(&u8(d), 1900, 4)
		// neighbours must be intact
		check_pattern(&u8(a), 1000, 1)
		check_pattern(&u8(c), 3000, 3)
		// free everything; the whole heap must merge back into one region
		w.free_(a)
		w.free_(d)
		w.free_(c)
		// one big allocation must succeed after the merge
		big := w.malloc(6000)
		assert big != nil
		fill_pattern(&u8(big), 6000, 9)
		check_pattern(&u8(big), 6000, 9)
		w.free_(big)
	}
}

fn test_split_repurpose_to_small_freelist() {
	mut w := new_walloc()
	unsafe {
		a := w.malloc(1000)
		assert a != nil
		// a second block, so freeing `a` leaves non-adjacent free objects
		// (adjacent ones would just be merged back together)
		bmid := w.malloc(1000)
		assert bmid != nil
		w.free_(a)
		assert w.large_objects != unsafe { nil }
		avail := w.large_objects.size
		// request `avail - 400` bytes: the 400 byte remainder rounds down to
		// a single 256 byte tail chunk, which must be repurposed to the
		// 32-granule small object freelist instead of staying large.
		b := w.malloc(avail - 400)
		assert b != nil
		assert w.small_freelists[9] != unsafe { nil }
		fill_pattern(&u8(b), int(avail - 400), 7)
		check_pattern(&u8(b), int(avail - 400), 7)
		w.free_(b)
		w.free_(bmid)
	}
}

fn test_head_split_across_page_boundary() {
	mut w := new_walloc()
	unsafe {
		a := w.malloc(1000)
		assert a != nil
		// claim three fresh pages; the request is page-sized so the round-up
		// arm consumes the whole reservation without splitting
		m := w.malloc(150000)
		assert m != nil
		w.free_(m)
		// T1 (rest of page 1) is too small, so this splits the 3-page object
		// same-page and leaves a tail starting mid-page 3 that reaches two
		// pages further
		b := w.malloc(64500)
		assert b != nil
		// locate that spanning tail and the biggest other free object
		mut span := &LargeObject(unsafe { nil })
		mut other_max := uint(0)
		mut walk := w.large_objects
		for walk != unsafe { nil } {
			if walk.size > 100000 {
				span = walk
			} else if walk.size > other_max {
				other_max = walk.size
			}
			walk = walk.next
		}
		assert span != unsafe { nil }
		s_off := uint(large_payload(span)) & uint(page_mask)
		f := span.size
		// search a request best-fit must serve from span (too big for the
		// rest), itself sub-page, but whose kept region crosses the page
		// boundary, forcing the head-split arm
		limit := uint(page_size) - uint(sizeof(LargeObject)) - uint(chunk_size)
		mut r := uint(0)
		for cand in other_max + 1 .. limit {
			tail := (f - cand) & ~uint(chunk_mask)
			if tail != 0 && s_off + (f - tail) >= uint(page_size) + 1 {
				r = cand
				break
			}
		}
		assert r != 0
		h := w.malloc(r)
		assert h != nil
		// head-split re-bases the object at chunk 1 of the next page; a
		// same-page split would have returned span's own mid-page start
		assert (uint(h) & uint(page_mask)) == uint(chunk_size) + uint(sizeof(LargeObject))
		fill_pattern(&u8(h), int(r), 0x33)
		check_pattern(&u8(h), int(r), 0x33)
		w.free_(h)
		w.free_(b)
		w.free_(a)
	}
}

fn test_grow_and_oom() {
	mut w := new_walloc()
	unsafe {
		// force several linear-memory grows (16MiB reservation)
		mut ptrs := []voidptr{}
		for _ in 0 .. 20 {
			p := w.malloc(300000)
			assert p != nil
			ptrs << p
		}
		for p in ptrs {
			w.free_(p)
		}
		// more than the reservation must fail cleanly with nil
		huge := w.malloc(64 * 1024 * 1024)
		assert huge == nil
		// the allocator still works after a failed allocation
		p := w.malloc(16)
		assert p != nil
		w.free_(p)
	}
}

fn test_stress_mixed() {
	mut w := new_walloc()
	mut state := u64(0x853c49e6748fea9b)
	mut live := []voidptr{}
	mut live_sizes := []int{}
	mut live_seeds := []u8{}
	unsafe {
		for step in 0 .. 5000 {
			state = state * u64(6364136223846793005) + u64(1442695040888963407)
			op := (state >> 33) & 3
			if op == 0 && live.len < 200 {
				size := int((state >> 11) % 4000) + 1
				p := w.malloc(uint(size))
				assert p != nil
				seed := u8(step & 0xff)
				fill_pattern(&u8(p), size, seed)
				live << p
				live_sizes << size
				live_seeds << seed
			} else if live.len > 0 {
				idx := int((state >> 17) % u64(live.len))
				check_pattern(&u8(live[idx]), live_sizes[idx], live_seeds[idx])
				w.free_(live[idx])
				live.delete(idx)
				live_sizes.delete(idx)
				live_seeds.delete(idx)
			}
		}
		for i in 0 .. live.len {
			check_pattern(&u8(live[i]), live_sizes[i], live_seeds[i])
			w.free_(live[i])
		}
	}
}
