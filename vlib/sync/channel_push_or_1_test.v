const n = 1000
const c = 100

fn f(ch chan int) {
	for _ in 0 .. n {
		_ := <-ch
	}
	ch.close()
}

fn test_push_or_unbuffered() {
	ch := chan int{}
	go f(ch)
	mut j := 0
	for {
		ch <- j or {
			break
		}
		j++
	}
	assert j == n
}

fn test_push_or_buffered() {
	ch := chan int{cap: c}
	go f(ch)
	mut j := 0
	for {
		ch <- j or {
			break
		}
		j++
	}
	// we don't know how many elements are in the buffer when the channel
	// is closed, so check j against an interval
	assert j >= n
	assert j <= n + c
}

fn g(ch chan int, res chan int) {
	mut j := 0
	for {
		ch <- j or {
			break
		}
		j++
	}
	println('done $j')
	res <- j
}

fn test_many_senders() {
	ch := chan int{}
	res := chan int{}
	go g(ch, res)
	go g(ch, res)
	go g(ch, res)
	mut k := 0
	for _ in 0 .. 3 * n {
		k = <-ch
	}
	ch.close()
	mut sum := <-res
	sum += <-res
	sum += <-res
	assert sum == 3 * n
}
