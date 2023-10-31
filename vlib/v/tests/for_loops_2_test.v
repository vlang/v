fn test_for_match() {
	mut a := 2
	mut b := 0
	for {
		match a {
			2 {
				println('a == 2')
				a = 0
				continue
			}
			0 {
				println('a == 0')
				a = 5
				b++
				break
			}
			else {
				println('unexpected branch')
				break
			}
		}
	}
	assert a == 5
	assert b == 1
}

fn test_for_select() {
	ch1 := chan int{}
	ch2 := chan f64{}
	spawn do_send(ch1, ch2)
	mut a := 0
	mut b := 0
	for select {
		x := <-ch1 {
			a += x
		}
		y := <-ch2 {
			a += int(y)
		}
	} {
		// count number of receive events
		b++
		println('${b}. event')
	}
	assert a == 10
	assert b == 3
}

fn do_send(ch1 chan int, ch2 chan f64) {
	ch1 <- 3
	ch2 <- 5.0
	ch2.close()
	ch1 <- 2
	ch1.close()
}
