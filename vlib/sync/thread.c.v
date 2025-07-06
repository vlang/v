module sync

// ThreadLocalStorage provides thread-local storage for values of type T
@[noinit]
struct ThreadLocalStorage[T] {
mut:
	key    u64  // TLS key identifier. Note: While Linux uses unsigned int, Darwin requires unsigned long. u64 accommodates both.
	in_use bool // Allocation status flag
}
