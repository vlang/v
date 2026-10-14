module ascon

fn test_xof_clone_preserves_partial_output_position() ! {
	msg := [u8(0)]
	md := xof128(msg, 64)!
	mut parent := new_xof128(64)
	parent.write(msg)!
	mut start := []u8{len: 3}
	parent.read(mut start)!
	mut child := parent.clone()
	mut part := []u8{len: 9}
	child.read(mut part)!
	assert part == md[3..12]
	parent.reset()
	parent.write([u8(0xff)])!
	mut rest := []u8{len: 52}
	child.read(mut rest)!
	assert rest == md[12..]
}

fn test_cxof_clone_preserves_partial_output_position() ! {
	msg := [u8(0)]
	cs := [u8(0x10), 0x11]
	md := cxof128(msg, 64, cs)!
	mut parent := new_cxof128(64, cs)!
	parent.write(msg)!
	mut start := []u8{len: 3}
	parent.read(mut start)!
	mut child := parent.clone()
	mut part := []u8{len: 9}
	child.read(mut part)!
	assert part == md[3..12]
	parent.reset()
	parent.write([u8(0xff)])!
	mut rest := []u8{len: 52}
	child.read(mut rest)!
	assert rest == md[12..]
}
