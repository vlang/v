import time
import sync

struct St {
	a int
}

fn getint() int {
	return 8
}

fn f1(ch1 chan int, ch2 chan St, ch3 chan int, ch4 chan int, ch5 chan int, mut sem sync.Semaphore) {
	mut a := 5
	select {
		a = <-ch3 {
			a = 0
		}
		b := <-ch2 {
			a = b.a
		}
		ch3 <- 5 {
			a = 1
		}
		ch2 <- St{
			a: 37
		} {
			a = 2
		}
		ch4 <- (6 + 7 * 9) {
			a = 8
		}
		ch5 <- getint() {
			a = 9
		}
		300 * time.millisecond {
			a = 3
		}
	}
	assert a == 3
	sem.post()
}

fn f2(ch1 chan St, ch2 chan int, mut sem sync.Semaphore) {
	mut r := 23
	for i in 0 .. 2 {
		select {
			b := <-ch1 {
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
	ch4 := chan int{}
	ch5 := chan int{}
	mut sem := sync.new_semaphore()
	mut r := false
	t := select {
		b := <-ch1 {
			println(b)
		}
		else {
			// no channel ready
			r = true
		}
	}
	assert r == true
	assert t == true
	spawn f2(ch2, ch3, mut sem)
	n := <-ch3
	assert n == 23
	ch2 <- St{
		a: 13
	}
	sem.wait()
	stopwatch := time.new_stopwatch()
	spawn f1(ch1, ch2, ch3, ch4, ch5, mut sem)
	sem.wait()
	elapsed_ms := f64(stopwatch.elapsed()) / time.millisecond
	// https://docs.microsoft.com/en-us/windows-hardware/drivers/kernel/high-resolution-timers
	// > For example, for Windows running on an x86 processor, the default interval between
	// > system clock ticks is typically about 15 milliseconds, and the minimum interval
	// > between system clock ticks is about 1 millisecond.
	assert elapsed_ms >= 280.0 // 300 - (15ms + 5ms just in case)

	ch1.close()
	ch2.close()
	mut h := 7
	mut is_open := true
	if select {
		_ := <-ch2 {
			h = 0
		}
		ch1 <- h {
			h = 1
		}
		else {
			h = 2
		}
	} {
		panic('channel is still open')
	} else {
		is_open = false
	}
	// no branch should have run
	assert h == 7
	// since all channels are closed `select` should return `false`
	assert is_open == false
}
