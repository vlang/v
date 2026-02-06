module main

struct Ipv4 {
	address [4]u8
}

struct Ipv6 {
	address [16]u8
}

type Address = Ipv4 | Ipv6

fn test_main() {
	address := Address(Ipv4{
		address: [u8(192), 168, 1, 1]!
	})

	assert address == Address(Ipv4{
		address: [u8(192), 168, 1, 1]!
	})
}
