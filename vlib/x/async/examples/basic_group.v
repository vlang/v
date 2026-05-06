import context
import time
import x.async as xasync

fn main() {
	mut group := xasync.new_group(context.background())

	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		time.sleep(20 * time.millisecond)
		return error('stop group')
	})!

	group.go(fn (mut ctx context.Context) ! {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			1 * time.second {
				return error('worker did not observe cancellation')
			}
		}
	})!

	group.wait() or {
		println('group failed: ${err.msg()}')
		return
	}
}
