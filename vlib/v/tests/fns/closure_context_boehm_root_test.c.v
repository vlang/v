struct CapturedValueReceiver {
	value int
}

fn (r CapturedValueReceiver) get() int {
	return r.value
}

@[heap]
struct CapturedPointerReceiver {
	value int
}

fn (r &CapturedPointerReceiver) get() int {
	return r.value
}

fn stored_anon_closure() fn () int {
	value := 27445
	return fn [value] () int {
		return value
	}
}

fn stored_value_method_closure() fn () int {
	receiver := CapturedValueReceiver{
		value: 27445
	}
	return receiver.get
}

fn stored_pointer_method_closure() fn () int {
	receiver := &CapturedPointerReceiver{
		value: 27445
	}
	return receiver.get
}

fn collect_and_churn() {
	$if gcboehm ? {
		C.GC_gcollect()
	}
	for _ in 0 .. 1000 {
		unsafe {
			p := malloc(16)
			vmemset(p, 0x33, 16)
		}
	}
	$if gcboehm ? {
		C.GC_gcollect()
	}
}

fn test_stored_closure_contexts_survive_boehm_collection() {
	anon_cb := stored_anon_closure()
	value_method_cb := stored_value_method_closure()
	pointer_method_cb := stored_pointer_method_closure()
	collect_and_churn()
	assert anon_cb() == 27445
	assert value_method_cb() == 27445
	assert pointer_method_cb() == 27445
}

fn consume_with_return(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	if n >= 0 {
		return h(n % 200)
	}
	return 0
}

fn consume_with_call_then_return(n int) int {
	big := []int{len: 200, init: index + n}
	h := fn [big] (x int) int {
		return big[x % big.len]
	}
	_ = h(n % 200)
	return 0
}

fn consume_with_labeled_break(n int) int {
	mut value := 0
	inner: for {
		big := []int{len: 200, init: index + n}
		h := fn [big] (x int) int {
			return big[x % big.len]
		}
		value = h(n % 200)
		break inner
	}
	return value
}

fn consume_with_labeled_continue(n int) int {
	mut value := 0
	outer: for _ in 0 .. 1 {
		big := []int{len: 200, init: index + n}
		h := fn [big] (x int) int {
			return big[x % big.len]
		}
		value = h(n % 200)
		continue outer
	}
	return value
}

fn test_issue_27445_local_closure_contexts_do_not_accumulate() {
	$if gcboehm ? {
		gc_collect()
		start_mb := gc_memory_use() / 1024 / 1024
		for n in 0 .. 80_000 {
			big := []int{len: 200, init: index + n}
			h := fn [big] (x int) int {
				return big[x % big.len]
			}
			_ = h(n % 200)
			if n % 20_000 == 0 {
				gc_collect()
			}
		}
		gc_collect()
		end_mb := gc_memory_use() / 1024 / 1024
		assert end_mb <= start_mb + 24
	}
}

fn test_issue_27445_return_paths_with_local_closure_do_not_accumulate() {
	$if gcboehm ? {
		gc_collect()
		start_mb := gc_memory_use() / 1024 / 1024
		for n in 0 .. 80_000 {
			_ = consume_with_return(n)
			_ = consume_with_call_then_return(n)
			if n % 20_000 == 0 {
				gc_collect()
			}
		}
		gc_collect()
		end_mb := gc_memory_use() / 1024 / 1024
		assert end_mb <= start_mb + 24
	}
}

fn test_issue_27445_labeled_branches_with_local_closure_do_not_accumulate() {
	$if gcboehm ? {
		gc_collect()
		start_mb := gc_memory_use() / 1024 / 1024
		for n in 0 .. 80_000 {
			_ = consume_with_labeled_break(n)
			_ = consume_with_labeled_continue(n)
			if n % 20_000 == 0 {
				gc_collect()
			}
		}
		gc_collect()
		end_mb := gc_memory_use() / 1024 / 1024
		assert end_mb <= start_mb + 24
	}
}
