type Addr = [4]u8

fn test_assign_option_of_fixed_array() {
	mut addr := ?Addr(none)
	addr = Addr([u8(1), 2, 3, 4]!)
	println(addr)
	assert '${addr}' == 'Option(Addr([1, 2, 3, 4]))'
}
