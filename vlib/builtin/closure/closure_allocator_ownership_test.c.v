// vtest build: (linux || macos)
@[has_globals]
module closure

// Scoped prealloc is a separate allocator and cannot import arena.
$if !prealloc {
	import arena
}

$if !prealloc {
	fn closure_allocator_require(ok bool, message string) {
		if !ok { panic(message) }
	}

	fn closure_allocator_callback() int {
		return *unsafe { &int(g_closure.closure_get_data()) }
	}

	fn test_closure_global_metadata_does_not_belong_to_current_arena() {
		mut scoped := arena.new(chunk_size: 4096)
		mut value := 37
		mut callbacks := unsafe { [2048]voidptr{} }
		scoped.push()
		for i in 0 .. callbacks.len {
			callbacks[i] = closure_create_with_data(voidptr(closure_allocator_callback), &value, false)
		}
		scoped.pop()
		used := scoped.used()
		println('closure global metadata arena bytes=${used}')
		// Keep all storage valid on the red path; do not reproduce a freed-memory crash.
		closure_allocator_require(used == 0, 'process-global closure metadata was allocated in the scoped arena')
		for callback in callbacks { closure_try_destroy(callback) }
		scoped.free()
	}

	fn test_closure_lifetime_state_is_independent_of_creation_arena() {
		mut scoped := arena.new(chunk_size: 4096)
		scoped.push()
		mut lifetime := new_lifetime()
		owned := scoped.owns(lifetime.state)
		scoped.pop()
		println('closure pooled state belongs to arena=${owned}')
		closure_allocator_require(!owned, 'pooled closure lifetime state was allocated in the scoped arena')
		lifetime.dispose() or { panic(err) }
		scoped.free()
	}

	fn test_closure_frame_bookkeeping_does_not_use_the_current_arena() {
		mut lifetime := new_lifetime()
		mut scoped := arena.new(chunk_size: 4096)
		mut value := 41
		mut callbacks := unsafe { [64]voidptr{} }
		scoped.push()
		token := lifetime.begin_frame() or { panic(err) }
		for i in 0 .. callbacks.len {
			callbacks[i] = closure_create_with_data(voidptr(closure_allocator_callback), &value, false)
		}
		lifetime.end_frame(token) or { panic(err) }
		current := scoped.is_current()
		scoped.pop()
		used := scoped.used()
		println('closure frame metadata arena bytes=${used}')
		closure_allocator_require(current, 'closure bookkeeping changed caller allocator selection')
		closure_allocator_require(used == 0, 'closure frame bookkeeping was allocated in the scoped arena')
		lifetime.dispose() or { panic(err) }
		scoped.free()
	}

	// Tokens contain only the previous selection; an arena remains active while
	// suspended and can neither be reset nor freed by another thread.
	fn test_closure_allocator_nested_suspensions_restore_the_original_arena() {
		mut outer := arena.new()
		mut inner := arena.new()
		outer.push()
		inner.push()
		original := v_arena_current()
		first := unsafe { v_arena_suspend() }
		closure_allocator_require(first == original && isnil(v_arena_current()), 'outer suspension did not select ordinary allocation')
		second := unsafe { v_arena_suspend() }
		buffer := vcalloc(64)
		closure_allocator_require(isnil(second) && !outer.owns(buffer) && !inner.owns(buffer), 'nested suspension allocated into an arena')
		unsafe { v_arena_resume(second) }
		closure_allocator_require(isnil(v_arena_current()), 'nested resume restored too much state')
		unsafe { v_arena_resume(first) }
		closure_allocator_require(inner.is_current(), 'outer resume lost the original innermost arena')
		inner.pop()
		closure_allocator_require(outer.is_current(), 'suspension changed the ancestor stack')
		outer.pop()
		unsafe { free(buffer) }
		inner.free()
		outer.free()
	}

	fn test_closure_lifetime_errors_preserve_the_caller_allocator() {
		mut region := arena.new()
		region.push()
		mut lifetime := new_lifetime()
		token := lifetime.begin_frame() or { panic(err) }
		if _ := lifetime.begin_frame() { panic('nested frame accepted') }
		closure_allocator_require(region.is_current(), 'failed frame entry changed allocator')
		if _ := lifetime.end_frame(FrameToken{}) { panic('invalid frame token accepted') }
		closure_allocator_require(region.is_current(), 'failed frame completion changed allocator')
		lifetime.end_frame(token) or { panic(err) }
		closure_allocator_require(region.is_current(), 'successful frame completion changed allocator')
		lifetime.dispose() or { panic(err) }
		closure_allocator_require(region.is_current(), 'lifetime disposal changed allocator')
		region.pop()
		region.free()
	}

	@[heap]
	struct ClosureAllocatorDropProbe {
	mut:
		drops    int
		restored bool
		nested   int = 19
	}

	struct ClosureAllocatorDropData {
		probe  &ClosureAllocatorDropProbe
		region &arena.Arena
	}

	type ClosureAllocatorCallback = fn () int

	fn closure_allocator_drop(raw voidptr) {
		data := unsafe { &ClosureAllocatorDropData(raw) }
		mut probe := data.probe
		probe.drops++
		buffer := []u8{len: 64}
		probe.restored = data.region.is_current() && data.region.owns(buffer.data)
		nested := closure_create_with_data(voidptr(closure_allocator_callback), &probe.nested, false)
		callback := unsafe { ClosureAllocatorCallback(nested) }
		closure_allocator_require(callback() == 19, 'drop callback reentry failed')
		closure_try_destroy(nested)
	}

	fn test_closure_drop_runs_once_with_restored_allocator_and_can_reenter() {
		mut region := arena.new()
		mut probe := &ClosureAllocatorDropProbe{}
		data := unsafe { &ClosureAllocatorDropData(vcalloc_heap(int(sizeof(ClosureAllocatorDropData)))) }
		unsafe { *data = ClosureAllocatorDropData{probe, &region} }
		region.push()
		callback := closure_create_with_data_and_drop(voidptr(closure_allocator_callback), data, true, voidptr(closure_allocator_drop))
		closure_try_destroy(callback)
		closure_try_destroy(callback)
		closure_allocator_require(probe.drops == 1 && probe.restored, 'drop was duplicated or observed suspended allocation')
		closure_allocator_require(region.is_current(), 'drop reentry changed allocator')
		region.pop()
		region.free()
	}

	fn closure_allocator_scope_worker(ready chan int, release chan int) bool {
		mut region := arena.new()
		region.push()
		previous := unsafe { v_arena_suspend() }
		ready <- 1
		_ := <-release
		heap := vcalloc(32)
		selected_heap := isnil(v_arena_current()) && !region.owns(heap)
		unsafe { v_arena_resume(previous) }
		restored := region.is_current()
		local := vcalloc(32)
		selected_arena := region.owns(local)
		region.pop()
		region.free()
		unsafe { free(heap) }
		return selected_heap && restored && selected_arena
	}

	fn test_closure_allocator_suspension_is_thread_local() {
		ready := chan int{cap: 2}
		release := chan int{cap: 2}
		first := spawn closure_allocator_scope_worker(ready, release)
		second := spawn closure_allocator_scope_worker(ready, release)
		_ := <-ready
		_ := <-ready
		closure_allocator_require(isnil(v_arena_current()), 'worker suspension changed main thread')
		release <- 1
		release <- 1
		closure_allocator_require(first.wait() && second.wait(), 'concurrent suspensions lost a thread allocator')
	}

	$if gcboehm ?&& !gcboehm_leak ? {
		fn C.GC_register_finalizer_no_order(voidptr, fn (voidptr, voidptr), voidptr, voidptr, voidptr)
		fn C.GC_set_finalize_on_demand(i32)
		fn C.GC_get_finalize_on_demand() i32
		fn C.GC_invoke_finalizers() i32
		__global closure_allocator_finalized = false
		fn closure_allocator_finalizer(_object voidptr, _data voidptr) {
			closure_allocator_finalized = true
		}
		fn closure_allocator_gc_producer() voidptr {
			mut region := arena.new()
			region.push()
			data := unsafe { &int(vcalloc_heap(int(sizeof(int)))) }
			unsafe { *data = 73 }
			C.GC_register_finalizer_no_order(data, closure_allocator_finalizer, unsafe { nil }, unsafe { nil }, unsafe { nil })
			callback := closure_create_with_data(voidptr(closure_allocator_callback), data, true)
			region.pop()
			region.free()
			return callback
		}
	}

	fn test_closure_live_metadata_keeps_heap_contexts_traced_after_arena_release() {
		$if gcboehm ?&& !gcboehm_leak ? {
			prior := C.GC_get_finalize_on_demand()
			C.GC_set_finalize_on_demand(1)
			defer { C.GC_set_finalize_on_demand(prior) }
			producer := spawn closure_allocator_gc_producer()
			raw := producer.wait()
			for _ in 0 .. 10 {
				gc_collect()
				C.GC_invoke_finalizers()
			}
			// A registered closure must strongly retain its context. This is a live
			// survivor check; no physical collection deadline is asserted after release.
			closure_allocator_require(!closure_allocator_finalized, 'live closure context lost its GC root')
			callback := unsafe { ClosureAllocatorCallback(raw) }
			closure_allocator_require(callback() == 73, 'traced heap context changed')
			closure_try_destroy(raw)
		}
	}

	fn closure_allocator_panic_in_scope() {
		previous := closure_allocator_begin()
		defer { closure_allocator_end(previous) }
		panic('closure bookkeeping scope probe')
	}

	fn closure_allocator_recover_scope(mut probe ClosureAllocatorDropProbe) {
		defer {
			if message := recover() {
				probe.restored = message == 'closure bookkeeping scope probe'
			}
		}
		closure_allocator_panic_in_scope()
	}

	fn test_closure_allocator_restores_before_outer_panic_recovery() {
		mut region := arena.new()
		region.push()
		mut probe := ClosureAllocatorDropProbe{}
		closure_allocator_recover_scope(mut probe)
		closure_allocator_require(probe.restored && region.is_current(), 'panic recovery lost caller allocator')
		region.pop()
		region.free()
	}
}

// This scope deliberately leaves the separate prealloc allocator selected.
$if prealloc {
	fn test_closure_allocator_scope_preserves_the_existing_prealloc_policy() {
		previous := closure_allocator_begin()
		if !isnil(previous) { panic('arena suspension was enabled for scoped prealloc') }
		closure_allocator_end(previous)
	}
}
