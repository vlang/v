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
	a << 4
	assert a.len == 5
	assert a[4] == 4
	assert a.last() == 4

	mut s := a.str()
	assert s == '[1, 5, 2, 3, 4]'
	assert a[1] == 5
	assert a.last() == 4
}

fn test_deleting() {
	mut a := [1, 5, 2, 3, 4]
	assert a.len == 5
	assert a.str() == '[1, 5, 2, 3, 4]'

	a.delete(0)
	assert a.str() == '[5, 2, 3, 4]'
	assert a.len == 4

	a.delete(1)
	assert a.str() == '[5, 3, 4]'
	assert a.len == 3
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
	a := ['a', 'b', 'c']
	assert a.str() == '["a", "b", "c"]'
}

fn test_repeat() {
	a := [0; 5]
	assert a.len == 5
	assert a[0] == 0 && a[1] == 0 && a[2] == 0 && a[3] == 0 && a[4] == 0

	b := [7; 3]
	assert b.len == 3
	assert b[0] == 7 && b[1] == 7 && b[2] == 7
	{
		mut aa := [1.1 ; 10]
		// FIXME: assert aa[0] == 1.1 will fail, need fix
		assert aa[0] == f32(1.1)
		assert aa[5] == f32(1.1)
		assert aa[9] == f32(1.1)
	}
	{
		mut aa := [f32(1.1) ; 10]
		assert aa[0] == f32(1.1)
		assert aa[5] == f32(1.1)
		assert aa[9] == f32(1.1)
	}
	{
		mut aa := [f64(1.1) ; 10]
		assert aa[0] == f64(1.1)
		assert aa[5] == f64(1.1)
		assert aa[9] == f64(1.1)
	}
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
	assert b.len == 2
	assert a.slice(1, 2).len == 1
	assert a.len == 4
}

fn test_push_many() {
	mut a := [1, 2, 3]
	b := [4, 5, 6]
	a << b 
	assert a.len == 6
	assert a[0] == 1
	assert a[3] == 4
	assert a[5] == 6
}


