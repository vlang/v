import context
import x.async as xasync
import x.executor

fn test_executor_and_async_job_aliases_can_be_used_together() ! {
	values := chan int{cap: 2}
	mut ex := executor.new(queue_size: 2)!
	job := executor.JobFn(fn [values] () ! {
		values <- 1
	})
	ex.try_post(job)!
	assert ex.run_one()!
	assert <-values == 1
	ex.stop()
	assert !ex.run_one()!
	ex.wait()!

	async_job := xasync.JobFn(fn [values] (mut ctx context.Context) ! {
		assert ctx.err() is none
		values <- 2
	})
	mut group := xasync.new_group(xasync.background())
	group.go(async_job)!
	group.wait()!
	assert <-values == 2
}
