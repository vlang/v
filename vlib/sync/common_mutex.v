module sync

// str returns a string representation of the Mutex pointer.
pub fn (m &Mutex) str() string {
	return 'Mutex(${voidptr(m)})'
}

// str returns a string representation of the RwMutex pointer.
pub fn (m &RwMutex) str() string {
	return 'RwMutex(${voidptr(m)})'
}
