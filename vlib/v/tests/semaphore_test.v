import sync

const (
	signals_per_thread = 100000
)

fn send_signals(sem sync.Semaphore, sem_end sync.Semaphore) {
	for _ in 0 .. signals_per_thread {
		sem.post()
	}
	sem_end.post()
}

fn test_semaphores() {
	sem := sync.new_semaphore()
	sem_end := sync.new_semaphore()
	go send_signals(sem, sem_end)
	go send_signals(sem, sem_end)
	for _ in 0 .. 2 * signals_per_thread {
		sem.wait()
	}
	sem_end.wait()
	sem_end.wait()
	assert true
}
