module sync

import time

#flag -lpthread
#include <semaphore.h>

// MacOS has no function `sem_timedwait()` so this is Linux/Windows only

pub fn (s Semaphore) timed_wait(timeout time.Duration) ? {
	t_spec := timeout.timespec()
	if C.sem_timedwait(&s.sem.sem, &t_spec) == 0 {
		return
	}
	code := C.errno
	if code == C.ETIMEDOUT {
		return error('timeout')
	} else {
		return error('error waiting for semaphore: 0x${code:08x}')
	}
}
