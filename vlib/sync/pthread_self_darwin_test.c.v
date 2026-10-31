// vtest build: macos
import os
import sync

#include <pthread.h>

fn C.pthread_self() voidptr

fn pthread_identity() u64 {
	assert !os.is_main_thread()
	assert sync.thread_id() == u64(C.pthread_self())
	return sync.thread_id()
}

fn test_pthread_self_pointer_declaration_matches_standard_modules() {
	assert os.is_main_thread()
	main_id := sync.thread_id()
	assert main_id == u64(C.pthread_self())
	worker := spawn pthread_identity()
	assert worker.wait() != main_id
}
