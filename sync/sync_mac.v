module sync

#include <pthread.h>
struct Mutex {
	mutex C.pthread_mutex_t
}

fn (m Mutex) lock() {
	C.pthread_mutex_lock(&m.mutex)
}

fn (m Mutex) unlock() {
	C.pthread_mutex_unlock(&m.mutex)
}

