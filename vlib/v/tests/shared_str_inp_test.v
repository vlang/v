module main

struct Foo {
mut:
	int shared int
	i32 shared i32
	i64 shared i64
	u32 shared u32
	u64 shared u64
}

fn test_main() {
	mut t := Foo{}
	rlock t.int {
		assert '${t.int}' == '0'
	}

	rlock t.i32 {
		assert '${t.i32}' == '0'
	}
	rlock t.i64 {
		assert '${t.i64}' == '0'
	}
	rlock t.u32 {
		assert '${t.u32}' == '0'
	}
	rlock t.u64 {
		assert '${t.u64}' == '0'
	}

	rlock t.i32 {
		assert '${t.i32:08x}' == '00000000'
	}
}
