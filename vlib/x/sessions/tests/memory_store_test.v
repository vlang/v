import time
import x.sessions

const max_age = time.second

pub struct User {
	name string
	age  int
}

const default_user = User{
	name: 'john'
	age:  99
}

fn test_store_set() {
	mut store := sessions.MemoryStore[User]{}
	store.set('a', default_user)!

	// check if created at time is not empty
	assert store.data['a'].created_at != time.Time{}
	assert store.data['a'].data == default_user

	first_created := store.data['a'].created_at
	store.set('a', User{ age: 99 })!

	assert store.data['a'].created_at == first_created
	assert store.data['a'].data.age == 99
}

fn test_store_get() {
	mut store := sessions.MemoryStore[User]{}
	store.set('a', default_user)!

	if data := store.get('a', max_age) {
		assert data == default_user
	} else {
		assert false, 'session data should not be none'
	}
}

fn test_store_session_expired() {
	mut store := sessions.MemoryStore[User]{}
	store.set('a', default_user)!

	time.sleep(2 * max_age)

	if data := store.get('a', max_age) {
		assert false, 'session should be expired!'
	} else {
		assert err.msg() == 'session is expired'
	}
	assert store.data.len == 0
}
