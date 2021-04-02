module context

import time

const (
	// a reasonable duration to block in an example
	short_duration = 1 * time.millisecond
)

type FavContextKey = string

fn after(dur time.Duration) chan int {
	dst := chan int{}
	go fn (dur time.Duration, dst chan int) {
		time.sleep(dur)
		dst <- 0
	}(dur, dst)
	return dst
}

// This example demonstrates the use of a cancelable context to prevent a
// routine leak. By the end of the example function, the routine started
// by gen will return without leaking.
fn test_with_cancel() {
	// gen generates integers in a separate routine and
	// sends them to the returned channel.
	// The callers of gen need to cancel the context once
	// they are done consuming generated integers not to leak
	// the internal routine started by gen.
	gen := fn (ctx Context) chan int {
		dst := chan int{}
		go fn (ctx Context, dst chan int) {
			for {
				ch := ctx.done()
				select {
					_ := <-ch {
						// returning not to leak the routine
						return
					}
					dst <- 0 {}
				}
			}
		}(ctx, dst)
		return dst
	}

	cancel_ctx, cancel := with_cancel(background())
	defer {
		cancel(cancel_ctx)
	}

	ch := gen(cancel_ctx)
	for _ in 0 .. 5 {
		assert 0 == <-ch
	}
}

// This example passes a context with an arbitrary deadline to tell a blocking
// function that it should abandon its work as soon as it gets to it.
fn test_with_deadline() {
	dur := time.now().add(context.short_duration)
	cancel_ctx, cancel := with_deadline(background(), dur)
	defer {
		cancel(cancel_ctx)
	}
	after_ch := after(1 * time.second)
	ctx_ch := cancel_ctx.done()
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
	cancel_ctx, cancel := with_timeout(background(), context.short_duration)
	defer {
		cancel(cancel_ctx)
	}
	after_ch := after(1 * time.second)
	ctx_ch := cancel_ctx.done()
	select {
		_ := <-after_ch {
			assert false
		}
		_ := <-ctx_ch {
			assert true
		}
	}
}

// This example demonstrates how a value can be passed to the context
// and also how to retrieve it if it exists.
fn test_with_value() {
	f := fn (ctx Context, key FavContextKey) string {
		value := ctx.value(&key)
		if !isnil(value) {
			return *(&string(value))
		}
		return 'key not found'
	}

	key := FavContextKey('language')
	value := 'VAL'
	ctx := with_value(background(), &key, &value)

	assert value == f(ctx, key)
	assert 'key not found' == f(ctx, FavContextKey('color'))
}
