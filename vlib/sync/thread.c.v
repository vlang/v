module sync

// ThreadLocalStorage Thread-Local Storage container for type T
@[noinit]
struct ThreadLocalStorage[T] {
	key u32
	// TLS index/key
mut:
	in_use bool
	// Allocation state flag
}
