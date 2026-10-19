module sync

fn test_freebsd_mutex_stores_native_typedef() {
	$if freebsd {
		assert sizeof(Mutex) == sizeof(C.pthread_mutex_t)
		mut mx := new_mutex()
		mx.lock()
		mx.unlock()
		mx.destroy()
		mut rw := new_rwmutex()
		rw.rlock()
		rw.runlock()
		rw.lock()
		rw.unlock()
		rw.destroy()
	}
}
