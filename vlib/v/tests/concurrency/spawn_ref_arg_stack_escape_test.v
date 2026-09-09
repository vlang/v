struct SpawnRefArgStackValue {
	conn int
}

struct SpawnRefReceiverStackValue {
	conn int
}

fn read_spawned_value(v &SpawnRefArgStackValue, expected int, start chan int) bool {
	_ := <-start or { return false }
	return v.conn == expected
}

fn (v &SpawnRefReceiverStackValue) read_after_start(expected int, start chan int) bool {
	_ := <-start or { return false }
	return v.conn == expected
}

fn test_spawn_ref_arg_from_stack_is_auto_heap_promoted() {
	thread_count := 64
	start := chan int{cap: thread_count}
	mut threads := []thread bool{cap: thread_count}
	for i in 0 .. thread_count {
		value := SpawnRefArgStackValue{
			conn: i
		}
		threads << spawn read_spawned_value(&value, i, start)
	}
	for _ in 0 .. thread_count {
		start <- 1
	}
	for i, th in threads {
		assert th.wait(), 'spawned thread ${i} observed a reused stack value'
	}
}

fn test_spawn_ref_receiver_from_stack_is_auto_heap_promoted() {
	thread_count := 64
	start := chan int{cap: thread_count}
	mut threads := []thread bool{cap: thread_count}
	for i in 0 .. thread_count {
		value := SpawnRefReceiverStackValue{
			conn: i
		}
		threads << spawn (&value).read_after_start(i, start)
	}
	for _ in 0 .. thread_count {
		start <- 1
	}
	for i, th in threads {
		assert th.wait(), 'spawned method receiver ${i} observed a reused stack value'
	}
}
