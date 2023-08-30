module sync

pub fn (m &Mutex) str() string {
	return 'Mutex(${voidptr(m)})'
}

pub fn (m &RwMutex) str() string {
	return 'RwMutex(${voidptr(m)})'
}
