@[has_globals]
module driver

import os

// A build releases the arenas of finished stages, and joins the worker pool that
// C generation stopped, on helper threads while the later stages run. Every
// `exit()` between their start and the wait at the end of the build would run
// the exit handlers while they still work: the -prealloc cleanup then frees the
// arena with the pool's job channel and thread list, and the compiler crashes
// after printing its error. The exit hook waits for them first. Fixed storage
// keeps the registry out of the stage arenas, which these threads free.
const max_release_threads = 8

__global v3_release_threads [8]thread
__global v3_release_threads_len int
__global v3_release_threads_pid int
__global v3_release_threads_hook_registered bool

// track_release_thread records a helper thread that wait_release_threads, or the
// exit hook, joins.
fn track_release_thread(release_thread thread) {
	if v3_release_threads_len >= max_release_threads {
		release_thread.wait()
		return
	}
	if !v3_release_threads_hook_registered {
		v3_release_threads_hook_registered = true
		v3_release_threads_pid = os.getpid()
		// Exit handlers run in reverse registration order, so this runs before the
		// prealloc cleanup that builtin registered at startup.
		at_exit(wait_release_threads_at_exit) or {}
	}
	v3_release_threads[v3_release_threads_len] = release_thread
	v3_release_threads_len++
}

// wait_release_threads joins every helper thread recorded by track_release_thread.
fn wait_release_threads() {
	for v3_release_threads_len > 0 {
		v3_release_threads_len--
		v3_release_threads[v3_release_threads_len].wait()
	}
}

fn wait_release_threads_at_exit() {
	// A forked child has none of the parent's threads to join.
	if os.getpid() != v3_release_threads_pid {
		return
	}
	wait_release_threads()
}
