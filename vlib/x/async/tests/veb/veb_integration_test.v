import context
import time
import veb
import x.async as xasync

fn test_veb_context_response_inside_timeout() {
	xasync.with_timeout(1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
		mut web_ctx := veb.Context{}
		_ := web_ctx.text('hello from veb')
		if web_ctx.res.status_code != 200 {
			return error('unexpected veb status')
		}
		if web_ctx.res.body != 'hello from veb' {
			return error('unexpected veb body')
		}
	})!
}
