# Context

This module defines the Context type, which carries deadlines, cancellation signals,
and other request-scoped values across API boundaries and between processes.

Incoming requests to a server should create a Context, and outgoing calls to servers
should accept a Context. The chain of function calls between them must propagate the
Context, optionally replacing it with a derived Context created using with_cancel,
with_deadline, with_timeout, or with_value. When a Context is canceled, all Contexts
derived from it are also canceled.

The with_cancel, with_deadline, and with_timeout functions take a Context (the parent)
and return a derived Context (the child) and a CancelFunc. Calling the CancelFunc
cancels the child and its children, removes the parent's reference to the child,
and stops any associated timers. Failing to call the CancelFunc leaks the child
and its children until the parent is canceled or the timer fires.

Programs that use Contexts should follow these rules to keep interfaces consistent
across different modules.

Do not store Contexts inside a struct type; instead, pass a Context explicitly
to each function that needs it. The Context should be the first parameter,
typically named ctx, just to make it more consistent.

## Examples

In this section you can see some usage examples for this module

### Context With Cancellation

```v
import context

// This example demonstrates the use of a cancelable context to prevent a
// routine leak. By the end of the example function, the routine started
// by gen will return without leaking.
fn example_with_cancel() {
	// gen generates integers in a separate routine and
	// sends them to the returned channel.
	// The callers of gen need to cancel the context once
	// they are done consuming generated integers not to leak
	// the internal routine started by gen.
	gen := fn (ctx context.Context) chan int {
		dst := chan int{}
		go fn (ctx context.Context, dst chan int) {
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

	cancel_ctx, cancel := context.with_cancel(context.background())
	defer {
		cancel(cancel_ctx)
	}

	ch := gen(cancel_ctx)
	for _ in 0 .. 5 {
		assert 0 == <-ch
	}
}
```

### Context With Deadline

```v
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
fn example_with_deadline() {
	dur := time.now().add(short_duration)
	cancel_ctx, cancel := context.with_deadline(context.background(), dur)
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
```

### Context With Timeout

```v
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

// This example passes a context with a timeout to tell a blocking function that
// it should abandon its work after the timeout elapses.
fn example_with_timeout() {
	// Pass a context with a timeout to tell a blocking function that it
	// should abandon its work after the timeout elapses.
	cancel_ctx, cancel := context.with_timeout(context.background(), short_duration)
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
```

### Context With Value

```v
import context

type FavContextKey = string

// This example demonstrates how a value can be passed to the context
// and also how to retrieve it if it exists.
fn example_with_value() {
	f := fn (ctx context.Context, key FavContextKey) string {
		value := ctx.value(&key)
		if !isnil(value) {
			return *(&string(value))
		}
		return 'key not found'
	}

	key := FavContextKey('language')
	value := 'VAL'
	ctx := context.with_value(context.background(), &key, &value)

	assert value == f(ctx, key)
	assert 'key not found' == f(ctx, FavContextKey('color'))
}
```
