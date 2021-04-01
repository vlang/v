module context

fn test_cancel() {
	// gen generates integers in a separate goroutine and
	// sends them to the returned channel.
	// The callers of gen need to cancel the context once
	// they are done consuming generated integers not to leak
	// the internal goroutine started by gen.
	gen := fn(ctx Context) chan int {
		dst := chan int{}
		go fn(ctx Context, dst chan int) {
			for {
				ch := ctx.done()
				select {
					_ := <- ch {
						return // returning not to leak the goroutine
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

	ch := gen(Context(cancel_ctx))
	for _ in 0 .. 5 {
		assert 0 == <-ch
	}
}
