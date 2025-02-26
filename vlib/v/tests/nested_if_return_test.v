struct Message {
	prio bool
}

struct Throttler[T] {
	max_wait int
mut:
	wait_counter       int
	low_prio_requests  []T
	high_prio_requests []T
}

fn new_throttler[T](max_wait int) Throttler[T] {
	return Throttler[T]{max_wait, 0, []T{len: 1, init: T{}}, []T{len: 1, init: T{}}}
}

fn pop[T](mut t Throttler[T]) ?T {
	if t.wait_counter <= t.max_wait {
		return if t.high_prio_requests.len > 10 {
			t.high_prio_requests.pop()
		} else {
			t.wait_counter = 0
			if t.low_prio_requests.len > 0 {
				t.low_prio_requests.pop()
			} else {
				none
			}
		}
	} else {
		return none
	}
}

fn test_main() {
	c := chan Message{}
	mut throttler := new_throttler[Message](10)
	msg := pop(mut throttler) or { return }
	assert msg.str() == 'Message{
    prio: false
}'
}
