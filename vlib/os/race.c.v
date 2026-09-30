module os

// In a race build (`v -race`), writing a file happens before reading it in another thread,
// like Go's syscall package does it with its `ioSync` object. ThreadSanitizer only sees
// such an ordering for the read and write system calls, not for the C `FILE` functions
// that the `File` methods use, so every `FILE` read (`fread`, `getc`, `fgets`, `getline`)
// and write must call these.

// race_file_write marks a write to a file.
@[if race ?]
fn race_file_write() {
	$if race ? {
		racereleaseio()
	}
}

// race_file_read marks a read from a file.
@[if race ?]
fn race_file_read() {
	$if race ? {
		raceacquireio()
	}
}
