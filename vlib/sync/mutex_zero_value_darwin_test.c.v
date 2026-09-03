// vtest build: macos
import sync

struct MutexHolder {
mut:
	mutex sync.Mutex
}

fn test_zero_value_mutex_is_initialised_on_first_use() {
	mut holder := &MutexHolder{}
	assert holder.mutex.try_lock()
	assert !holder.mutex.try_lock()
	holder.mutex.unlock()
	holder.mutex.destroy()
}
