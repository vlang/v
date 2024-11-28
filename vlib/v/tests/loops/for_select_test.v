import time

fn test_main() {
	ch := chan int{}
	go do_send(ch)
	mut a := 0
	for select {
		x := <-ch {
			a += x
			check(x)
		}
	} {
	}
	assert a == 45
	time.sleep(500 * time.millisecond)
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
