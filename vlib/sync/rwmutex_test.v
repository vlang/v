import sync
import time

struct Counter {
pub mut:
	i int
}

fn write_10000(mut co Counter, mut mx sync.RwMutex) {
	mx.lock()
	co.i = 10000
	mx.unlock()
}

fn test_rwmutex() {
	mut co := &Counter{10086}
	mut mx := sync.new_rwmutex()
	mx.lock()
	co.i = 888
	th1 := spawn write_10000(mut co, mut mx)
	mx.unlock() // after mx unlock, thread write_10000 can continue
	th1.wait()
	assert co.i == 10000

	mx.rlock()
	th2 := spawn write_10000(mut co, mut mx) // write_10000 will be blocked
	co.i = 999 // for demo purpose, don't modify data in rlock!
	time.sleep(1 * time.millisecond)
	assert co.i == 999
	mx.runlock() // after rlock released, write_10000 can continue
	th2.wait()
	assert co.i == 10000
	mx.destroy()
}

fn test_try_lock_rwmutex() {
	// In Windows, try_lock only avalible after Windows 7
	$if windows {
		$if !windows_7 ? {
			return
		}
	}
	mut mx := sync.new_rwmutex()

	// try_rlock will always fail when mx locked
	mx.lock()
	try_fail_reading1 := mx.try_rlock()
	try_fail_writing1 := mx.try_wlock()
	assert try_fail_reading1 == false
	assert try_fail_writing1 == false

	mx.unlock()

	// try_rlock will always succeed when mx unlocked,
	// multiple try_rlock can apply to the same mx
	try_success_reading2 := mx.try_rlock()
	try_success_reading3 := mx.try_rlock()
	assert try_success_reading2 == true
	assert try_success_reading3 == true

	// if mx is rlocked, then the try_wlock will fail
	try_fail_writing2 := mx.try_wlock()
	assert try_fail_writing2 == false

	mx.runlock()
	mx.runlock() // you must release rlock multiple times, as it was rlocked multiple times

	// after mx release all rlock, try_wlock will succeed
	try_success_writing3 := mx.try_wlock()
	assert try_success_writing3 == true

	mx.unlock() // you must unlock it, after try_wlock success
	mx.destroy()
}
