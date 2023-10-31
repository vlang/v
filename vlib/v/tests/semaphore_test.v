import sync

const (
	signals_per_thread = 100000
)

fn send_signals(mut sem sync.Semaphore, mut sem_end sync.Semaphore) {
	for _ in 0 .. signals_per_thread {
		sem.post()
	}
	sem_end.post()
}

fn test_semaphores() {
	mut sem := sync.new_semaphore()
	mut sem_end := sync.new_semaphore()
	spawn send_signals(mut sem, mut sem_end)
	spawn send_signals(mut sem, mut sem_end)
	for _ in 0 .. 2 * signals_per_thread {
		sem.wait()
	}
	sem_end.wait()
	sem_end.wait()
	assert true
}
