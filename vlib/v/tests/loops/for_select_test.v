fn test_main() {
	ch := chan int{}
	sender := go do_send(ch)
	mut a := 0
	for select {
		x := <-ch {
			a += x
			check(x)
		}
	} {
	}
	assert a == 45
	sender.wait()
}

fn do_send(ch chan int) {
	for i in 0 .. 10 {
		ch <- i
	}
	ch.close()
}

fn check(a int) {
	println(a)
}
