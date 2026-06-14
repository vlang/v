module closure

// for https://github.com/vlang/v/issues/27445
// count-based checks are boehm-only (`$if gcboehm ?`); correctness runs on every gc mode.

type IntFn = fn (int) int

fn test_captured_context_survives_gc() {
	data := [10, 20, 30, 40]
	f := fn [data] (i int) int {
		return data[i]
	}
	$if gcboehm ? {
		C.GC_gcollect()
	}
	for _ in 0 .. 2000 {
		unsafe {
			p := malloc(64)
			vmemset(p, 0x5a, 64)
		}
	}
	$if gcboehm ? {
		C.GC_gcollect()
	}
	assert f(0) == 10
	assert f(3) == 40
}

fn test_try_destroy_idempotent() {
	$if gcboehm ? {
		before := live_count()
		x := 7
		f := fn [x] (y int) int {
			return x + y
		}
		assert f(1) == 8 // use it before destroying
		assert live_count() == before + 1
		try_destroy(voidptr(f))
		assert live_count() == before
		// double-destroy and nil must be safe no-ops, never going below `before`
		try_destroy(voidptr(f))
		try_destroy(voidptr(f))
		try_destroy(unsafe { nil })
		assert live_count() == before
	}
}

fn test_setup_closures_not_reclaimed() {
	base := 100
	setup := fn [base] (y int) int {
		return base + y
	}
	// created outside any begin/end_frame_build window -> sentinel frame
	$if gcboehm ? {
		for _ in 0 .. 64 {
			begin_frame_build()
			end_frame_build()
			reclaim_frames(1)
		}
	}
	assert setup(5) == 105
}

fn test_reclaim_frames_bounds_live_count() {
	$if gcboehm ? {
		mut sink := 0
		per_frame := 8
		mut m1 := 0
		for frame in 0 .. 1000 {
			begin_frame_build()
			mut fns := []IntFn{}
			for k in 0 .. per_frame {
				cap := frame * 100 + k
				fns << fn [cap] (y int) int {
					return cap + y
				}
			}
			for f in fns {
				sink += f(1)
			}
			end_frame_build()
			reclaim_frames(2) // keep current + previous frame only
			if frame == 99 {
				m1 = live_count()
			}
		}
		// after 9x more frames the live count must not have grown materially
		growth := live_count() - m1
		assert growth <= per_frame * 3, 'closure live_count grew by ${growth} over 900 extra frames (leak)'
		assert sink > 0
	}
}
