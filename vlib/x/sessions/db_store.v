module sessions

import json
import orm
import time

// DBStoreSessions is the table that is created in your database and represents a session data record.
pub struct DBStoreSessions {
pub mut:
	session_id string @[primary]
	created_at time.Time
	data       string
}

// DBStore stores sessions in a database.
@[noinit]
pub struct DBStore[T] {
pub mut:
	db orm.Connection @[required]
}

// create a new Database store with a connection to a database.
pub fn DBStore.create[T](db orm.Connection) !DBStore[T] {
	sql db {
		create table DBStoreSessions
	}!

	return DBStore[T]{
		db: db
	}
}

// all gets the data from all sessions.
pub fn (mut store DBStore[T]) all() ![]T {
	rows := sql store.db {
		select from DBStoreSessions
	}!

	// decode should never fail
	return rows.map(json.decode(T, it.data)!)
}

// get session for session id `sid`. The session can be `max_age` old.
// `max_age` will be ignored when set to `0`
pub fn (mut store DBStore[T]) get(sid string, max_age time.Duration) !T {
	rows := sql store.db {
		select from DBStoreSessions where session_id == sid
	}!

	if rows.len == 1 {
		record := rows[0]
		// session is expired
		if max_age != 0 && record.created_at.add(max_age) < time.now() {
			store.destroy(sid)!
			return error('session is expired')
		}

		return json.decode(T, record.data)!
	} else {
		return error('session does not exist')
	}
}

// destroy data for session id `sid`.
pub fn (mut store DBStore[T]) destroy(sid string) ! {
	sql store.db {
		delete from DBStoreSessions where session_id == sid
	}!
}

// clear all sessions.
pub fn (mut store DBStore[T]) clear() ! {
	sql store.db {
		delete from DBStoreSessions where session_id != ''
	}!
}

// set session data for session id `sid`.
pub fn (mut store DBStore[T]) set(sid string, val T) ! {
	count := sql store.db {
		select count from DBStoreSessions where session_id == sid
	}!

	if count == 1 {
		stringified := json.encode(val)
		sql store.db {
			update DBStoreSessions set data = stringified where session_id == sid
		}!
	} else {
		record := DBStoreSessions{
			session_id: sid
			created_at: time.now()
			data:       json.encode(val)
		}

		sql store.db {
			insert record into DBStoreSessions
		}!
	}
}
