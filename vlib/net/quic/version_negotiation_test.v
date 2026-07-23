module quic

fn test_handle_version_negotiation_rejects_list_including_v1() {
	vn := QuicVersionNegotiation{
		dcid:     [u8(1), 2]
		scid:     [u8(3), 4]
		versions: [u32(0x1a2a3a4a), quic_v1] // grease + v1
	}
	handle_version_negotiation(vn) or {
		assert err.msg().contains('PROTOCOL_VIOLATION')
		return
	}
	assert false, 'expected a VN packet listing v1 to be rejected as a protocol violation'
}

fn test_handle_version_negotiation_rejects_list_without_v1() {
	vn := QuicVersionNegotiation{
		dcid:     [u8(1), 2]
		scid:     [u8(3), 4]
		versions: [u32(0x1a2a3a4a), u32(0xff00_0001)]
	}
	handle_version_negotiation(vn) or {
		assert err.msg().contains('does not support QUIC v1')
		return
	}
	assert false, 'expected a VN packet without v1 to fail the connection attempt'
}

fn test_handle_version_negotiation_rejects_empty_list() {
	vn := QuicVersionNegotiation{
		dcid:     [u8(1), 2]
		scid:     [u8(3), 4]
		versions: []u32{}
	}
	handle_version_negotiation(vn) or {
		assert err.msg().contains('does not support QUIC v1')
		return
	}
	assert false, 'expected an empty VN version list to fail the connection attempt'
}
