import context
import time
import x.async as xasync

fn main() {
	xasync.with_timeout(20 * time.millisecond, fn (mut ctx context.Context) ! {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			1 * time.second {
				return error('work unexpectedly finished')
			}
		}
	}) or {
		println('timeout result: ${err.msg()}')
		return
	}

	eprintln('expected timeout error')
	exit(1)
}
