module time

// Ticker holds a channel that delivers ticks at a specified interval.
pub struct Ticker {
pub:
	c chan Time
mut:
	stop_ch chan bool
	done    bool
}

// new_ticker returns a Ticker that ticks every `d`. The caller must call
// `stop()` when done to release the background goroutine.
pub fn new_ticker(d Duration) &Ticker {
	mut t := &Ticker{
		c:       chan Time{cap: 1}
		stop_ch: chan bool{cap: 1}
	}
	spawn ticker_loop(mut t, d)
	return t
}

fn ticker_loop(mut t Ticker, d Duration) {
	for {
		time.sleep(d)
		select {
			_ := <-t.stop_ch {
				t.done = true
				return
			}
			else {
				t.c <- time.now()
			}
		}
	}
}

// stop stops the ticker.
pub fn (mut t Ticker) stop() {
	if !t.done {
		t.stop_ch <- true
	}
}
