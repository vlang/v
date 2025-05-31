module main

type IPv4 = [4]u8
type IPv6 = [16]u8
type IP = IPv4 | IPv6

fn test_main() {
	mut arr := []IP{}
	arr << IP(IPv4([4]u8{}))
	arr << IP(IPv6([16]u8{}))
	assert arr.str() == '[IP(IPv4([0, 0, 0, 0])), IP(IPv6([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]))]'
}
