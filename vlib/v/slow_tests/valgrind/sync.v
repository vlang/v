import sync

fn main() {
	mut mutex := sync.new_mutex()
	mutex.lock()
	mutex.unlock()
	mutex.destroy()

	mut rwmutex := sync.new_rwmutex()
	rwmutex.rlock()
	rwmutex.unlock()
	rwmutex.lock()
	rwmutex.unlock()
	rwmutex.destroy()

	mut sem := sync.new_semaphore()
	sem.post()
	sem.wait()
	sem.destroy()
}
