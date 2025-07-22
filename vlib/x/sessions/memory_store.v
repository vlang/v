module sessions

import time

struct MemoryStoreSessions[T] {
pub mut:
	created_at time.Time
	data       T
}

// MemoryStore stores sessions in a `map` in memory only.
pub struct MemoryStore[T] {
mut:
	data map[string]MemoryStoreSessions[T]
}

// get data from all sessions.
pub fn (mut store MemoryStore[T]) all() ![]T {
	return store.data.values().map(it.data)
}

// get session for session id `sid`. The session can be `max_age` old.
// `max_age` will be ignored when set to `0`
pub fn (mut store MemoryStore[T]) get(sid string, max_age time.Duration) !T {
	if record := store.data[sid] {
		// session is expired
		if max_age != 0 && record.created_at.add(max_age) < time.now() {
			store.destroy(sid)!
			return error('session is expired')
		}

		return record.data
	} else {
		return error('session does not exist')
	}
}

// destroy data for session id `sid`.
pub fn (mut store MemoryStore[T]) destroy(sid string) ! {
	store.data.delete(sid)
}

// clear all sessions.
pub fn (mut store MemoryStore[T]) clear() ! {
	store.data.clear()
}

// set session data for session id `sid`.
pub fn (mut store MemoryStore[T]) set(sid string, val T) ! {
	if sid in store.data {
		store.data[sid].data = val
	} else {
		store.data[sid] = MemoryStoreSessions[T]{
			created_at: time.now()
			data:       val
		}
	}
}
