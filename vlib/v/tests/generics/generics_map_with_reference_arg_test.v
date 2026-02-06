import datatypes
import time

struct Entry[V] {
mut:
	value     V
	load_time u32
}

struct Lru[T, V] {
mut:
	m      map[T]Entry[V]
	list   datatypes.DoublyLinkedList[T]
	cap    u32 = 1000
	ttl    u32 = 3
	on_del ?fn (T, V)
pub mut:
	hits u32
	miss u32
}

pub fn new[T, V](cap u32, ttl u32) &Lru[T, V] {
	return &Lru[T, V]{
		cap: cap
		ttl: ttl
	}
}

pub fn (mut l Lru[T, V]) set_on_del(on_del fn (T, V)) {
	l.on_del = on_del
}

pub fn (mut l Lru[T, V]) add(k T, v V) {
	if l.m.len >= (l.cap * 97 / 100) {
		l.remove_expired(0)
		for l.m.len > (l.cap * 90 / 100) {
			l.del(l.list.pop_back() or { return })
		}
	}
	l.m[k] = Entry[V]{v, u32(time.now().unix())}
	l.list.push_front(k)
}

pub fn (mut l Lru[T, V]) get(k T) ?V {
	l.remove_expired(0)
	if k in l.m {
		l.list.delete(l.list.index(k) or { return none })
		l.list.push_front(k)
		l.hits++
		return l.m[k].value
	}
	l.miss++
	return none
}

pub fn (mut l Lru[T, V]) remove_expired(cnt int) {
	now := u32(time.now().unix())
	iter := l.list.back_iterator()
	del_cnt := 0
	for key in iter {
		if e := l.m[key] {
			if e.load_time + l.ttl >= now || (cnt > 0 && del_cnt >= cnt) {
				break
			}
			l.del(key)
		}
	}
}

pub fn (mut l Lru[T, V]) del(k T) {
	if k in l.m {
		val := l.m[k].value
		l.m.delete(k)
		l.list.delete(l.list.index(k) or { -1 })
		on_del := l.on_del or { return }
		on_del(k, val)
	}
}

struct TT {
	age int
	dd  int
}

fn test_generic_map_with_reference_arg() {
	mut c := new[int, &TT](10, 3)
	c.add(1, &TT{2, 2})
	ret := c.get(1)?
	println(ret)
	assert ret.age == 2
	assert ret.dd == 2
}
