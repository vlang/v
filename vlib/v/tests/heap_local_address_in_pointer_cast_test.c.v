module main

struct Parse {
mut:
	outer &Parse = unsafe { nil }
	a     int
	b     int
}

struct Db {
mut:
	parse &Parse = unsafe { nil }
}

fn address_of(address voidptr) voidptr {
	return address
}

// `parse` moves to the heap because its address is stored in `db`. The address
// taken inside the pointer cast is that heap object too (C translated by c2v
// clears a struct with `C.memset(&i8(address_of(&parse)) + offset, 0, n)`).
fn prepare(mut db Db) &i8 {
	parse := Parse{
		a: 1
		b: 2
	}
	bytes := &i8(address_of(&parse))
	unsafe { C.memset(bytes + __offsetof(Parse, a), 0, sizeof(int) * 2) }
	db.parse = &parse
	return bytes
}

fn test_address_of_a_heap_local_in_a_pointer_cast() {
	mut db := Db{}
	bytes := prepare(mut db)
	assert voidptr(bytes) == voidptr(db.parse)
	assert db.parse.a == 0
	assert db.parse.b == 0
}
