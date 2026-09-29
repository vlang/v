@[has_globals]
module builtin

// Programs built with `v -race` link the ThreadSanitizer runtime. race_options.c provides
// its default options, and reads VRACE, V's counterpart of Go's GORACE.
#flag @VEXEROOT/vlib/builtin/race_options.c

fn C.__tsan_acquire(addr voidptr)
fn C.__tsan_release(addr voidptr)
fn C.__tsan_read1(addr voidptr)
fn C.__tsan_write1(addr voidptr)
fn C.AnnotateIgnoreReadsBegin(file &char, line int)
fn C.AnnotateIgnoreReadsEnd(file &char, line int)
fn C.AnnotateIgnoreWritesBegin(file &char, line int)
fn C.AnnotateIgnoreWritesEnd(file &char, line int)
fn C.AnnotateIgnoreSyncBegin(file &char, line int)
fn C.AnnotateIgnoreSyncEnd(file &char, line int)

// The functions below are the V counterpart of Go's internal/race package. The V runtime
// uses them, under `$if race ? {}`, where it synchronizes threads in ways that
// ThreadSanitizer does not see, or sees more of than the language guarantees: like Go's
// runtime, it hides its own synchronization and memory accesses with racedisable and
// raceenable, and states the happens-before edges and accesses that the V semantics
// define with racerelease, raceacquire, raceread and racewrite.

// raceacquire makes the calling thread happen after every racerelease on `addr`.
pub fn raceacquire(addr voidptr) {
	C.__tsan_acquire(addr)
}

// racerelease makes what the calling thread did so far happen before a later
// raceacquire on `addr`.
pub fn racerelease(addr voidptr) {
	C.__tsan_release(addr)
}

// raceread tells the race detector that the calling thread reads `addr`.
pub fn raceread(addr voidptr) {
	C.__tsan_read1(addr)
}

// racewrite tells the race detector that the calling thread writes `addr`.
pub fn racewrite(addr voidptr) {
	C.__tsan_write1(addr)
}

// race_io_sync stands for all file I/O, like `ioSync` in Go's syscall package.
__global race_io_sync u64

// racereleaseio makes a file write happen before later file reads in other threads, like
// Go's syscall.Write does.
pub fn racereleaseio() {
	C.__tsan_release(&race_io_sync)
}

// raceacquireio makes a file read happen after earlier file writes in other threads, like
// Go's syscall.Read does.
pub fn raceacquireio() {
	C.__tsan_acquire(&race_io_sync)
}

// racedisable makes the race detector ignore the memory accesses and the synchronization
// of the calling thread until the matching raceenable. Calls nest.
pub fn racedisable() {
	C.AnnotateIgnoreReadsBegin(c'', 0)
	C.AnnotateIgnoreWritesBegin(c'', 0)
	C.AnnotateIgnoreSyncBegin(c'', 0)
}

// raceenable ends the region that racedisable started.
pub fn raceenable() {
	C.AnnotateIgnoreSyncEnd(c'', 0)
	C.AnnotateIgnoreWritesEnd(c'', 0)
	C.AnnotateIgnoreReadsEnd(c'', 0)
}
