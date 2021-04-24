import context
import time

const (
	// a reasonable duration to block in an example
	short_duration = 1 * time.millisecond
)

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

	ctx_ch := ctx.done()
	select {
		_ := <-ctx_ch {}
		> 1 * time.second {
			panic('This should not happen')
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

	ctx_ch := ctx.done()
	select {
		_ := <-ctx_ch {}
		> 1 * time.second {
			panic('This should not happen')
		}
	}
}
