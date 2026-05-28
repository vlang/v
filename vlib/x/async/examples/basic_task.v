import context
import x.async as xasync

fn main() {
	mut task := xasync.run[int](fn (mut ctx context.Context) !int {
		_ = ctx
		return 42
	})!

	value := task.wait()!
	println('task result: ${value}')
}
