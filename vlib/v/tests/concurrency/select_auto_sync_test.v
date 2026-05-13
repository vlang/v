import time

fn test_main() {
	select {
		1 * time.second {}
	}
	assert true
}

fn test_select_accepts_duration_timeout() {
	v := chan int{}
	select {
		_ := <-v {}
		time.millisecond {}
	}
	assert true
}
