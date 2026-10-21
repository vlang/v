// A channel is shared, never converted, so a channel is accepted where another one is
// expected only when their elements are stored the same way. Aliases are transparent: a
// `chan Byte` is a `chan u8`.
type Byte = u8
type Ints = []int

struct Pipes {
	bytes chan u8
}

fn send_byte(ch chan u8) {
	ch <- 5
}

fn send_ints(ch chan []int) {
	ch <- [1, 2]
}

fn as_bytes(ch chan Byte) chan u8 {
	return ch
}

fn test_channels_of_aliases_are_channels_of_their_base_types() {
	bytes := chan Byte{cap: 1}
	send_byte(bytes)
	assert <-bytes == 5
	rows := chan Ints{cap: 1}
	send_ints(rows)
	got := <-rows
	assert got.len == 2 && got[1] == 2
	pipes := Pipes{
		bytes: bytes
	}
	pipes.bytes <- 9
	assert <-bytes == 9
	mut plain := chan u8{}
	plain = as_bytes(bytes)
	plain <- 7
	assert <-bytes == 7
}
