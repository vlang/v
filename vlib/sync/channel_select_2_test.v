import sync
import time

fn do_rec_i64(ch chan i64) {
	mut sum := i64(0)
	for _ in 0 .. 300 {
		sum += <-ch
	}
	assert sum == 300 * (300 - 1) / 2
}

fn do_send_int(ch chan int) {
	for i in 0 .. 300 {
		ch <- i
	}
}

fn do_send_byte(ch chan byte) {
	for i in 0 .. 300 {
		ch <- byte(i)
	}
}

fn do_send_i64(ch chan i64) {
	for i in 0 .. 300 {
		ch <- i
	}
}

fn test_select() {
	chi := chan int{}
	chl := chan i64{cap: 1}
	chb := chan byte{cap: 10}
	recch := chan i64{cap: 0}
	go do_rec_i64(recch)
	go do_send_int(chi)
	go do_send_byte(chb)
	go do_send_i64(chl)
	mut channels := [&sync.Channel(chi), &sync.Channel(recch), &sync.Channel(chl), &sync.Channel(chb)]
	directions := [sync.Direction.pop, .push, .pop, .pop]
	mut sum := i64(0)
	mut rl := i64(0)
	mut ri := int(0)
	mut rb := byte(0)
	mut sl := i64(0)
	mut objs := [voidptr(&ri), &sl, &rl, &rb]
	for _ in 0 .. 1200 {
		idx := sync.channel_select(mut channels, directions, mut objs, -1)
		match idx {
			0 {
				sum += ri
			}
			1 {
				sl++
			}
			2 {
				sum += rl
			}
			3 {
				sum += rb
			}
			else {
				println('got $idx (timeout)')
			}
		}
	}
	// Use Gauß' formula for the first 2 contributions
	expected_sum :=  2 * (300 * (300 - 1) / 2) +
		// the 3rd contribution is `byte` and must be seen modulo 256
		256 * (256 - 1) / 2 +
		44 * (44 - 1) / 2
	assert sum == expected_sum
	time.sleep_ms(20) // to give assert in coroutine enough time
}
