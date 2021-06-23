import context

// This example demonstrates the use of a cancelable context to prevent a
// routine leak. By the end of the example function, the routine started
// by gen will return without leaking.
fn test_with_cancel() {
	// gen generates integers in a separate routine and
	// sends them to the returned channel.
	// The callers of gen need to cancel the context once
	// they are done consuming generated integers not to leak
	// the internal routine started by gen.
	gen := fn (ctx context.Context) chan int {
		dst := chan int{}
		go fn (ctx context.Context, dst chan int) {
			mut v := 0
			ch := ctx.done()
			for {
				select {
					_ := <-ch {
						// returning not to leak the routine
						return
					}
					dst <- v {
						v++
					}
				}
			}
		}(ctx, dst)
		return dst
	}

	ctx := context.with_cancel(context.background())
	defer {
		context.cancel(ctx)
	}

	ch := gen(ctx)
	for i in 0 .. 5 {
		v := <-ch
		assert i == v
	}
}
