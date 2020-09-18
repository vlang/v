import time
import sync

struct St {
	a int
}

fn f1(ch1 chan int, ch2 chan St, ch3 chan int, sem sync.Semaphore) {
	mut a := 5
	st := St{}
	select {
		a = <-ch3 {
			a = 0
		}
		b := <-ch2 {
			a = b.a
		}
		ch2 <- st {
			a = 2
		}
		> 300 * time.millisecond {
			a = 3
		}
	}
	assert a == 3
	sem.post()
}

fn f2(ch1 chan St, ch2 chan int, sem sync.Semaphore) {
	mut r := 23
	for i in 0 .. 2 {
		select {
			b := <- ch1 {
				r = b.a
			}
			ch2 <- r {
				r = 17
			}
		}
		if i == 0 {
			assert r == 17
		} else {
			assert r == 13
		}
	}
	sem.post()
} 

fn test_select_blocks() {
	ch1 := chan int{cap: 1}
	ch2 := chan St{}
	ch3 := chan int{}
	sem := sync.new_semaphore()
	mut r := false
	t := select {
		b := <- ch1 {
			println(b)
		}
		else {
			// no channel ready
			r = true
		}
	}
	assert r == true
	assert t == true
	go f2(ch2, ch3, sem)
	n := <-ch3
	assert n == 23
	ch2 <- St{a: 13}
	sem.wait()
	stopwatch := time.new_stopwatch({})
	go f1(ch1, ch2, ch3, sem)
	sem.wait()
	elapsed_ms := f64(stopwatch.elapsed()) / time.millisecond
	assert elapsed_ms >= 295.0

	ch1.close()
	ch2.close()
	mut h := 7
	u := select {
		b := <- ch2 {
			h = 0
		}
		ch1 <- h {
			h = 1
		}
		else {
			h = 2
		}
	}
	// no branch should have run
	assert h == 7
	// since all channels are closed `select` should return `false`
	assert u == false
}

			
