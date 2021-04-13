import context
import time

const (
	// a reasonable duration to block in an example
	short_duration = 1 * time.millisecond
)

fn after(dur time.Duration) chan int {
	dst := chan int{}
	go fn (dur time.Duration, dst chan int) {
		time.sleep(dur)
		dst <- 0
	}(dur, dst)
	return dst
}

// This example passes a context with an arbitrary deadline to tell a blocking
// function that it should abandon its work as soon as it gets to it.
fn test_with_deadline() {
	dur := time.now().add(short_duration)
	ctx := context.with_deadline(context.background(), dur)

	defer {
		// Even though ctx will be expired, it is good practice to call its
		// cancellation function in any case. Failure to do so may keep the
		// context and its parent alive longer than necessary.
		context.cancel(ctx)
	}

	after_ch := after(1 * time.second)
	ctx_ch := ctx.done()
	select {
		_ := <-after_ch {
			assert false
		}
		_ := <-ctx_ch {
			assert true
		}
	}
}

// This example passes a context with a timeout to tell a blocking function that
// it should abandon its work after the timeout elapses.
fn test_with_timeout() {
	// Pass a context with a timeout to tell a blocking function that it
	// should abandon its work after the timeout elapses.
	ctx := context.with_timeout(context.background(), short_duration)
	defer {
		context.cancel(ctx)
	}

	after_ch := after(1 * time.second)
	ctx_ch := ctx.done()
	select {
		_ := <-after_ch {
			assert false
		}
		_ := <-ctx_ch {
			assert true
		}
	}
}
