import sync
import time

fn do_rec_i64(mut ch sync.Channel) {
	mut sum := i64(0)
	for _ in 0 .. 30 {
		mut a := i64(0)
		ch.pop(&a)
		sum += a
	}
	assert sum == 30 * (30 - 1) / 2
}

fn do_send_int(mut ch sync.Channel) {
	for i in 0 .. 30 {
		ch.push(&i)
	}
}

fn do_send_byte(mut ch sync.Channel) {
	for i in 0 .. 30 {
		ii := byte(i)
		ch.push(&ii)
	}
}

fn do_send_i64(mut ch sync.Channel) {
	for i in 0 .. 30 {
		ii := i64(i)
		ch.push(&ii)
	}
}

fn test_select() {
	mut chi := sync.new_channel<int>(0)
	mut chl := sync.new_channel<i64>(1)
	mut chb := sync.new_channel<byte>(10)
	mut recch := sync.new_channel<i64>(0)
	go do_rec_i64(mut recch)
	go do_send_int(mut chi)
	go do_send_byte(mut chb)
	go do_send_i64(mut chl)
	mut channels := [chi, recch, chl, chb]
	directions := [false, true, false, false]
	mut sum := i64(0)
	mut rl := i64(0)
	mut ri := int(0)
	mut rb := byte(0)
	mut sl := i64(0)
	mut objs := [voidptr(&ri), &sl, &rl, &rb]
	for _ in 0 .. 120 {
		idx := sync.channel_select(mut channels, directions, mut objs, 0)
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
	assert sum == 3 * (30 * (30 - 1) / 2)
}
