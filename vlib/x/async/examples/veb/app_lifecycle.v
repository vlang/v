import context
import veb
import x.async as xasync

fn render_health_response() !string {
	mut web_ctx := veb.Context{}
	_ := web_ctx.text('veb ok')
	if web_ctx.res.status_code != 200 {
		return error('unexpected veb status')
	}
	return web_ctx.res.body
}

fn main() {
	mut task := xasync.run[string](fn (mut ctx context.Context) !string {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			else {}
		}
		return render_health_response()!
	})!

	println(task.wait()!)
}
