module sync

import time

fn wait_select_once(ch &Channel, done chan string, label string) {
	mut channels := [ch]
	directions := [Direction.pop]
	mut value := 0
	mut objs := [voidptr(&value)]
	idx := channel_select(mut channels, directions, mut objs, time.infinite)
	match idx {
		0 {
			done <- '${label}:${value}'
		}
		-2 {
			done <- '${label}:closed'
		}
		else {
			done <- '${label}:idx=${idx}'
		}
	}
}

fn pop_subscriber_count(ch &Channel) int {
	ch.read_sub_mtx.lock()
	defer {
		ch.read_sub_mtx.unlock()
	}
	mut count := 0
	mut node := ch.read_subscriber
	for unsafe { node != 0 } {
		count++
		node = node.nxt
	}
	return count
}

fn wait_for_pop_subscribers(ch &Channel, want int) {
	for _ in 0 .. 200 {
		if pop_subscriber_count(ch) == want {
			return
		}
		time.sleep(5 * time.millisecond)
	}
	assert false, 'timed out waiting for ${want} read select subscriber(s)'
}

fn test_select_waiters_are_fifo() {
	mut ch := new_channel[int](0)
	done := chan string{cap: 2}
	spawn wait_select_once(ch, done, 'first')
	wait_for_pop_subscribers(ch, 1)
	spawn wait_select_once(ch, done, 'second')
	wait_for_pop_subscribers(ch, 2)
	value := 999
	ch.push(&value)
	assert <-done == 'first:999'
	ch.close()
	assert <-done == 'second:closed'
}
