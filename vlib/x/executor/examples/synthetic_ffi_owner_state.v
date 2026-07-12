import x.executor

struct FakeHandle {
mut:
	calls int
}

fn main() {
	mut owner := executor.new(queue_size: 4)!
	result := chan int{cap: 1}
	mut handle := FakeHandle{}

	owner.try_post(fn [mut handle, result] () ! {
		handle.calls++
		handle.calls++
		result <- handle.calls
	})!

	if !owner.run_one()! {
		eprintln('owner job did not run')
		exit(1)
	}
	calls := <-result
	if calls != 2 {
		eprintln('unexpected handle call count: ${calls}')
		exit(1)
	}

	println('synthetic FFI handle calls=${calls}')
	owner.stop()
	if owner.run_one()! {
		eprintln('unexpected owner job after stop')
		exit(1)
	}
	owner.wait()!
}
