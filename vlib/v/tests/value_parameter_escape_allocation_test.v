struct Req {
	buf []u8
	pos int
	len int
}

// a) `&r.buf[i]` points into the heap storage of the `[]u8`, not into `r`.
fn view(r Req) string {
	return unsafe { tos(&r.buf[r.pos], r.len) }
}

@[noinline]
fn view_local(buf []u8) string {
	r := Req{
		buf: buf
		pos: 4
		len: 11
	}
	return unsafe { tos(&r.buf[r.pos], r.len) }
}

interface Repo {
	count() !int
}

struct MemRepo {}

fn (m MemRepo) count() !int {
	return 7
}

struct UseCase {
	repo Repo
}

// b) a method called through an interface field of a by-value receiver.
fn (u UseCase) count() !int {
	return u.repo.count()
}

struct Frames {
	data []u8
}

struct Row {
	payload []u8
}

struct Iter {
	data []u8
mut:
	i int
}

fn (f &Frames) iter() Iter {
	return Iter{
		data: f.data
	}
}

fn (mut it Iter) next() ?Row {
	if it.i >= it.data.len {
		return none
	}
	it.i++
	return Row{
		payload: it.data
	}
}

fn (r Row) int4(i int) !i32 {
	if i >= r.payload.len {
		return error('no column ${i}')
	}
	return i32(r.payload[i])
}

// c) a `&T` method on a by-value parameter; only an int is returned.
fn first(f Frames) int {
	mut it := f.iter()
	row := it.next() or { panic('expected a row') }
	return int(row.int4(0) or { panic(err) })
}

const rounds = 1_000

fn test_dynamic_backing_does_not_move_its_local_header_to_the_heap() {
	buf := 'GET /index.html HTTP/1.1'.bytes()
	assert view_local(buf) == '/index.html'
	before := gc_heap_usage().total_bytes
	mut length := 0
	for _ in 0 .. rounds {
		length += view_local(buf).len
	}
	allocated := gc_heap_usage().total_bytes - before
	assert allocated == 0
	assert length == rounds * 11
}

fn test_value_parameters_with_separate_backing_do_not_allocate() {
	r := Req{
		buf: 'GET /index.html HTTP/1.1'.bytes()
		pos: 4
		len: 11
	}
	uc := UseCase{
		repo: MemRepo{}
	}
	f := Frames{
		data: [u8(42), 43]
	}
	mut sum := view(r).len + (uc.count() or { 0 }) + first(f) // warm-up

	mut before := gc_heap_usage().total_bytes
	for _ in 0 .. rounds {
		sum += view(r).len
	}
	a := gc_heap_usage().total_bytes - before

	before = gc_heap_usage().total_bytes
	for _ in 0 .. rounds {
		sum += uc.count() or { 0 }
	}
	b := gc_heap_usage().total_bytes - before

	before = gc_heap_usage().total_bytes
	for _ in 0 .. rounds {
		sum += first(f)
	}
	c := gc_heap_usage().total_bytes - before

	assert a == 0
	assert b == 0
	assert c == 0
	assert sum == (rounds + 1) * (11 + 7 + 42)
}
