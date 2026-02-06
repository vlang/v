import crypto.ripemd160

struct MdTest {
	o string
	i string
}

const vectors = [
	MdTest{'9c1185a5c5e9fc54612808977ee8f548b2258d31', ''},
	MdTest{'0bdc9d2d256b3ee9daae347be6f4dc835a467ffe', 'a'},
	MdTest{'8eb208f7e05d987a9b044a8e98c6b087f15a0bfc', 'abc'},
	MdTest{'5d0689ef49d2fae572b881b123a85ffa21595f36', 'message digest'},
	MdTest{'f71c27109c692c1b56bbdceb5b9d2865b3708dbc', 'abcdefghijklmnopqrstuvwxyz'},
	MdTest{'12a053384a9c0c88e405a06c27dcf49ada62eb2b', 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'},
	MdTest{'b0e20b6e3116640286ed3a87a5713079b21f5189', 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'},
	MdTest{'9b752e45573d4b39f4dbd3323cab82bf63326bfb', '12345678901234567890123456789012345678901234567890123456789012345678901234567890'},
]

fn test_vectors() {
	for tv in vectors {
		mut md := ripemd160.new()
		for j := 0; j < 3; j++ {
			if j < 2 {
				md.write(tv.i.bytes()) or { panic(err) }
			} else {
				half := tv.i.len / 2
				md.write(tv.i.bytes()[0..tv.i.len / 2]) or { panic(err) }
				md.sum([])
				md.write(tv.i.bytes()[tv.i.len / 2..]) or { panic(err) }
			}
			assert md.sum([]).hex() == tv.o
			md.reset()
		}
	}
}

fn million_a() string {
	mut md := ripemd160.new()
	for _ in 0 .. 100000 {
		md.write('aaaaaaaaaa'.bytes()) or { panic(err) }
	}
	return md.sum([]).hex()
}

fn test_million_a() {
	out := '52783243c1697bdbe16d37f97f68f08325dc1528'
	s := million_a()
	assert s == out
}
