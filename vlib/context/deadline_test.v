// vtest build: false // TODO: it sometimes blocks, investigate why
import context
import time

// a reasonable duration to block in an example
const short_duration = 1 * time.millisecond

// This example passes a context with an arbitrary deadline to tell a blocking
// function that it should abandon its work as soon as it gets to it.
fn test_with_deadline() {
	dur := time.now().add(short_duration)
	mut background := context.background()
	mut ctx, cancel := context.with_deadline(mut background, dur)

	defer {
		// Even though ctx will be expired, it is good practice to call its
		// cancellation function in any case. Failure to do so may keep the
		// context and its parent alive longer than necessary.
		cancel()
	}

	ctx_ch := ctx.done()
	select {
		_ := <-ctx_ch {}
		1 * time.second {
			panic('This should not happen')
		}
	}
}

// This example passes a context with a timeout to tell a blocking function that
// it should abandon its work after the timeout elapses.
fn test_with_timeout() {
	// Pass a context with a timeout to tell a blocking function that it
	// should abandon its work after the timeout elapses.
	mut background := context.background()
	mut ctx, cancel := context.with_timeout(mut background, short_duration)
	defer {
		cancel()
	}

	ctx_ch := ctx.done()
	select {
		_ := <-ctx_ch {}
		1 * time.second {
			panic('This should not happen')
		}
	}
}
