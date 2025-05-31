struct Foo {
pub mut:
	buf  shared []u8 = []u8{len: 20, init: 6}
	buf2 shared [20]u8
}

fn test_main() {
	mut foo := Foo{
		buf2: [20]u8{init: 5}
	}
	rlock foo.buf {
		if foo.buf.len > 0 {
			x := 10
			println('first ${x} bytes: ' + foo.buf[..x].str())
			sliced := foo.buf[..x]
			assert sliced == [u8(6), 6, 6, 6, 6, 6, 6, 6, 6, 6]
		} else {
			println('no data')
			assert false
		}
	}
	rlock foo.buf2 {
		x := 4
		println('first ${x} bytes: ' + foo.buf2[..x].str())
		assert foo.buf2[..x] == [u8(5), 5, 5, 5]
	}
}
