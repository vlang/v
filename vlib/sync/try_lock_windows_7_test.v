// vtest build: windows
// vtest vflags: -d windows_7
// With `-d windows_7`, the try_* methods call TryAcquireSRWLockExclusive and
// TryAcquireSRWLockShared. TinyCC's kernel32.def has neither, so under TinyCC
// sync_windows_tcc.h looks them up at run time. This test has to link and pass
// with every C compiler.
import sync

fn test_mutex_try_lock() {
	mut m := sync.new_mutex()
	assert m.try_lock()
	assert !m.try_lock()
	m.unlock()
	assert m.try_lock()
	m.unlock()
	m.destroy()
}

fn test_rwmutex_try_rlock_and_try_wlock() {
	mut rw := sync.new_rwmutex()
	assert rw.try_rlock()
	assert rw.try_rlock()
	assert !rw.try_wlock()
	rw.runlock()
	rw.runlock()
	assert rw.try_wlock()
	assert !rw.try_rlock()
	assert !rw.try_wlock()
	rw.unlock()
	assert rw.try_rlock()
	rw.runlock()
	rw.destroy()
}
