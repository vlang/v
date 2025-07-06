module sync

// ThreadLocalStorage Thread-Local Storage container for type T
@[noinit]
struct ThreadLocalStorage[T] {
mut:
	key    u32 = u32(0xDEADBEEF) // TLS index/key
	in_use bool // Allocation state flag
}
