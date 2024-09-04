import time

struct Struct {
	ch chan int
}

fn listen(ch chan int) {
	for {
		t := <-ch or { break }
		println(t)
	}
}

fn sp_array() {
	mut array := [3]chan int{}
	for a in 0 .. 3 {
		spawn listen(array[a])
	}
	for i, a in array {
		a <- i
	}
	time.sleep(1000 * time.millisecond)
	for c in array {
		c.close()
	}
}

fn test_main() {
	sp_array()
	assert true
}
