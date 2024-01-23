module sessions

import json
import orm
import time

pub struct DBStoreSessions {
pub:
	session_id string    @[primary]
	created_at time.Time
	data       string
}

// DBStore stores sessions in a database
@[noinit]
pub struct DBStore[T] {
pub mut:
	db orm.Connection @[required]
}

// create a new Database store with a connection to a database
pub fn DBStore.create[T](db orm.Connection) !DBStore[T] {
	sql db {
		create table DBStoreSessions
	}!

	return DBStore[T]{
		db: db
	}
}

// get data from all sessions
pub fn (mut store DBStore[T]) all() []T {
	rows := sql store.db {
		select from DBStoreSessions
	} or { []DBStoreSessions{} }

	// decode should never fail
	return rows.map(json.decode(T, it.data) or {
		eprintln('[vweb.sessions] DBStore error while decoding session data: ${err.msg()}')
		T{}
	})
}

// get session for session id `sid`. The session can be `max_age` old.
// `max_age` will be ignored when set to `0`
pub fn (mut store DBStore[T]) get(sid string, max_age time.Duration) ?T {
	rows := sql store.db {
		select from DBStoreSessions where session_id == sid
	} or {
		eprintln('[vweb.sessions] DBStore orm error: ${err.msg()}')
		return none
	}

	if rows.len == 1 {
		record := rows[0]
		// session is expired
		if max_age != 0 && record.created_at.add(max_age) < time.now() {
			store.destroy(sid)
			return none
		}

		return json.decode(T, record.data) or { none }
	} else {
		return none
	}
}

// destroy data for session id `sid`
pub fn (mut store DBStore[T]) destroy(sid string) {
	sql store.db {
		delete from DBStoreSessions where session_id == sid
	} or { eprintln('[vweb.sessions] DBStore orm error: ${err.msg()}') }
}

// clear all sessions
pub fn (mut store DBStore[T]) clear() {
	sql store.db {
		delete from DBStoreSessions where session_id != ''
	} or { eprintln('[vweb.sessions] DBStore orm error: ${err.msg()}') }
}

// set session data for session id `sid`
pub fn (mut store DBStore[T]) set(sid string, val T) {
	count := sql store.db {
		select count from DBStoreSessions where session_id == sid
	} or {
		eprintln('[vweb.sessions] DBStore orm error: ${err.msg()}')
		return
	}

	if count == 1 {
		stringified := json.encode(val)
		sql store.db {
			update DBStoreSessions set data = stringified where session_id == sid
		} or { eprintln('[vweb.sessions] DBStore orm error: ${err.msg()}') }
	} else {
		record := DBStoreSessions{
			session_id: sid
			created_at: time.now()
			data: json.encode(val)
		}

		sql store.db {
			insert record into DBStoreSessions
		} or { eprintln('[vweb.sessions] DBStore orm error: ${err.msg()}') }
	}
}
