import sync

struct Counter {
pub mut:
	i int
}

fn write_10000(mut co Counter, mut mx sync.Mutex) {
	mx.lock()
	co.i = 10000
	mx.unlock()
}

fn test_mutex() {
	mut co := &Counter{10086}
	mut mx := sync.new_mutex()
	mx.lock()
	co.i = 888
	th := spawn write_10000(mut co, mut mx)
	mx.unlock() // after mx unlock, thread write_10000 can continue
	th.wait()
	mx.destroy()
	assert co.i == 10000
}

fn test_try_lock_mutex() {
	// In Windows, try_lock only avalible after Windows 7
	$if windows {
		$if !windows_7 ? {
			return
		}
	}
	mut mx := sync.new_mutex()
	mx.lock()
	try_fail := mx.try_lock()
	assert try_fail == false
	mx.unlock()
	try_success := mx.try_lock()
	assert try_success == true
	mx.unlock() // you must unlock it, after try_lock success
	mx.destroy()
}
