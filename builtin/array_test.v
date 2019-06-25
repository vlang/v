import os

const (
	q = [1, 2, 3]
	A = 8
)

fn test_ints() {
	mut a := [1, 5, 2, 3]
	assert a.len == 4
	assert a[0] == 1
	assert a[2] == 2
	assert a.last() == 3
	println(a)
	a << 4
	println(a)
	assert a.len == 5
	assert a[4] == 4
	assert a.last() == 4
	// assert a.contains(4) == true
	// assert a.contains(777) == false
	// assert a.str() == '51234'
	mut s := a.str()
	assert s == '[1, 5, 2, 3, 4]'
	// Insert
	// val := 5
	// a.insert(1, val)// 1 5 2 3 4
	s = a.str()
	assert a[1] == 5
	assert a.last() == 4
	// a.sort()
	s = a.str()
	assert s == '[1, 5, 2, 3, 4]'
	// Delete
	a.delete(0)
	assert a.str() == '[5, 2, 3, 4]'
	a.delete(1)
	assert a.str() == '[5, 3, 4]'
}

fn test_short() {
	a := [1, 2, 3]
	assert a.len == 3
	assert a.cap == 3
	assert a[0] == 1
	assert a[1] == 2
	assert a[2] == 3
}

fn test_large() {
	mut a := [0; 0]
	for i := 0; i < 10000; i++ {
		a << i
	}
	assert a.len == 10000
	assert a[234] == 234
}

struct Chunk {
	val string
}

struct K {
	q []Chunk
}

fn test_empty() {
	mut chunks := []Chunk{}
	a := Chunk{}
	assert chunks.len == 0
	chunks << a
	assert chunks.len == 1
	chunks = []Chunk{}
	assert chunks.len == 0
	chunks << a
	assert chunks.len == 1
}
fn test_push() {
	mut a := []int
	a << 1
	a << 3
	assert a[1] == 3
	assert a.str() == '[1, 3]'
}

fn test_strings() {
	s := 'hi'
	if s.contains('i') {
		// println('$s')
	}
	a := ['a', 'b', 'c']
	assert a.str() == '["a", "b", "c"]'
	// println(a)
}

fn test_repeat() {
	a := [0; 5]
	// a := [0 x 5]
	assert a.len == 5
	assert a[0] == 0 && a[1] == 0 && a[2] == 0 && a[3] == 0 && a[4] == 0
}

fn test_right() {
	a := [1, 2, 3, 4]
	b := a.right(1)
	assert b[0] == 2
	assert b[1] == 3
}

fn test_left() {
	a := [1, 2, 3]
	b := a.left(2)
	assert b[0] == 1
	assert b[1] == 2
}

fn test_slice() {
	a := [1, 2, 3, 4]
	b := a.slice(2, 4)
	for val in b {
		println(val)
	}
	assert a.slice(1, 2).len == 1
	println(a.slice(2, 4))
}

fn test_push_many() {
	mut a := [1, 2, 3]
	b := [4, 5, 6]
	a._push_many(b.data, b.len)
	assert a.len == 6
	assert a[0] == 1
	assert a[3] == 4
	assert a[5] == 6
	println(a)
}


