import context
import time
import x.async as xasync

fn main() {
	ctx, cancel := xasync.with_cancel()
	ticks := chan int{cap: 4}
	done := chan string{cap: 1}

	worker := spawn fn [ctx, ticks, done] () {
		xasync.every(ctx, 10 * time.millisecond, fn [ticks] (mut ctx context.Context) ! {
			_ = ctx
			ticks <- 1
		}) or {
			done <- err.msg()
			return
		}
		done <- 'completed'
	}()

	if !wait_tick(ticks) || !wait_tick(ticks) {
		eprintln('periodic job did not tick')
		exit(1)
	}
	cancel()

	select {
		msg := <-done {
			println('periodic stopped: ${msg}')
		}
		1 * time.second {
			eprintln('periodic job did not stop')
			exit(1)
		}
	}
	worker.wait()
}

fn wait_tick(ticks chan int) bool {
	select {
		_ := <-ticks {
			return true
		}
		1 * time.second {
			return false
		}
	}
	return false
}
