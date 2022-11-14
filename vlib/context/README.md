# Context

This module defines the Context type, which carries deadlines, cancellation signals,
and other request-scoped values across API boundaries and between processes.

Incoming requests to a server should create a Context, and outgoing calls to servers
should accept a Context. The chain of function calls between them must propagate the
Context, optionally replacing it with a derived Context created using with_cancel,
with_deadline, with_timeout, or with_value. When a Context is canceled, all Contexts
derived from it are also canceled.

The with_cancel, with_deadline, and with_timeout functions take a Context (the parent)
and return a derived Context (the child). Calling the cancel function
cancels the child and its children, removes the parent's reference to the child,
and stops any associated timers.

Programs that use Contexts should follow these rules to keep interfaces consistent
across different modules.

Do not store Contexts inside a struct type; instead, pass a Context explicitly
to each function that needs it. The Context should be the first parameter,
typically named ctx, just to make it more consistent.

## Examples

In this section you can see some usage examples for this module

### Context With Cancellation

```v
import time
import context

// This example demonstrates the use of a cancelable context to prevent a
// routine leak. By the end of the example function, the routine started
// by gen will return without leaking.
fn main() {
	// gen generates integers in a separate routine and
	// sends them to the returned channel.
	// The callers of gen need to cancel the context once
	// they are done consuming generated integers not to leak
	// the internal routine started by gen.
	gen := fn (mut ctx context.Context) chan int {
		dst := chan int{}
		spawn fn (mut ctx context.Context, dst chan int) {
			mut v := 0
			ch := ctx.done()
			for {
				select {
					_ := <-ch {
						// returning not to leak the routine
						eprintln('> go thread returns because ctx was canceled/done')
						return
					}
					dst <- v {
						v++
					}
				}
			}
		}(mut ctx, dst)
		return dst
	}

	mut background := context.background()
	mut ctx, cancel := context.with_cancel(mut &background)
	defer {
		cancel()
		time.sleep(2 * time.millisecond) // give a small time window, in which the go thread routine has a chance to return
	}

	mut mut_ctx := ctx
	mut ctx2 := &mut_ctx
	ch := gen(mut ctx2)
	for i in 0 .. 5 {
		v := <-ch
		println('> received value: ${v}')
		assert i == v
	}
	println('> main is finished here')
}
```

### Context With Deadline

```v
import time
import context

const short_duration = 2 * time.millisecond // a reasonable duration to block in an example

// This example passes a context with an arbitrary deadline to tell a blocking
// function that it should abandon its work as soon as it gets to it.
fn main() {
	dur := time.now().add(short_duration)
	mut background := context.background()
	mut ctx, cancel := context.with_deadline(mut &background, dur)

	defer {
		// Even though ctx will be expired, it is good practice to call its
		// cancellation function in any case. Failure to do so may keep the
		// context and its parent alive longer than necessary.
		cancel()
		time.sleep(short_duration) // give a small time window, in which the go thread routine has a chance to return
		eprintln('> defer block finishes')
	}

	ctx_ch := ctx.done()
	select {
		_ := <-ctx_ch {
			println('>> read from ctx_ch succeeded')
		}
		1 * time.second {
			panic('This should not happen')
		}
	}
	eprintln('> main finishes')
}
```

### Context With Timeout

```v
import time
import context

const short_duration = 2 * time.millisecond // a reasonable duration to block in an example

// This example passes a context with a timeout to tell a blocking function that
// it should abandon its work after the timeout elapses.
fn main() {
	// Pass a context with a timeout to tell a blocking function that it
	// should abandon its work after the timeout elapses.
	mut background := context.background()
	mut ctx, cancel := context.with_timeout(mut &background, short_duration)
	defer {
		cancel()
		eprintln('> defer finishes')
	}

	ctx_ch := ctx.done()
	select {
		_ := <-ctx_ch {
			eprintln('> reading from ctx_ch succeeded')
		}
		1 * time.second {
			panic('This should not happen')
		}
	}
	eprintln('> main finishes')
}
```

### Context With Value

```v
import context

const not_found_value = &Value{
	val: 'key not found'
}

struct Value {
	val string
}

// This example demonstrates how a value can be passed to the context
// and also how to retrieve it if it exists.
fn main() {
	f := fn (ctx context.Context, key context.Key) &Value {
		if value := ctx.value(key) {
			match value {
				Value {
					return value
				}
				else {}
			}
		}
		return not_found_value
	}

	key := 'language'
	value := &Value{
		val: 'VAL'
	}
	ctx := context.with_value(context.background(), key, value)

	assert value == dump(f(ctx, key))
	assert not_found_value == dump(f(ctx, 'color'))
}
```
