// vtest build: present_openssl?
module quic

fn test_handle_version_negotiation_discards_list_including_v1() {
	// RFC 9000 §6.2: "A client MUST discard a Version Negotiation packet
	// that lists the QUIC version selected by the client." Discard means
	// continue the existing connection attempt as if the packet had never
	// arrived -- NOT fail it. A previous version of this function treated
	// this exact case as a fatal PROTOCOL_VIOLATION, which is the RFC's
	// prescribed reaction to the OPPOSITE input (a VN packet that lists
	// something OTHER than a genuine version list -- see the two rejection
	// tests below, which are unaffected).
	original_dcid := [u8(5), 6]
	original_scid := [u8(1), 2]
	vn := QuicVersionNegotiation{
		dcid:     original_scid
		scid:     original_dcid
		versions: [u32(0x1a2a3a4a), quic_v1] // grease + v1
	}
	handle_version_negotiation(vn, original_dcid, original_scid, false)!
}

fn test_handle_version_negotiation_rejects_list_without_v1() {
	original_dcid := [u8(5), 6]
	original_scid := [u8(1), 2]
	vn := QuicVersionNegotiation{
		dcid:     original_scid
		scid:     original_dcid
		versions: [u32(0x1a2a3a4a), u32(0xff00_0001)]
	}
	handle_version_negotiation(vn, original_dcid, original_scid, false) or {
		assert err.msg().contains('does not support QUIC v1')
		return
	}
	assert false, 'expected a VN packet without v1 to fail the connection attempt'
}

fn test_handle_version_negotiation_rejects_empty_list() {
	original_dcid := [u8(5), 6]
	original_scid := [u8(1), 2]
	vn := QuicVersionNegotiation{
		dcid:     original_scid
		scid:     original_dcid
		versions: []u32{}
	}
	handle_version_negotiation(vn, original_dcid, original_scid, false) or {
		assert err.msg().contains('does not support QUIC v1')
		return
	}
	assert false, 'expected an empty VN version list to fail the connection attempt'
}

fn test_handle_version_negotiation_discards_mismatched_dcid() {
	// RFC 9000 §17.2.1: the server MUST echo the client's own SCID as the
	// VN packet's DCID. A VN packet whose DCID does NOT match this
	// client's original SCID cannot be a genuine response to this client's
	// own Initial -- discard it silently, BEFORE even looking at its
	// version list (which here would otherwise be treated as terminal: no
	// v1 offered).
	original_dcid := [u8(5), 6]
	original_scid := [u8(1), 2]
	vn := QuicVersionNegotiation{
		dcid:     [u8(9), 9] // does NOT match original_scid
		scid:     original_dcid
		versions: [u32(0xff00_0001)] // no v1 -- would otherwise be terminal
	}
	handle_version_negotiation(vn, original_dcid, original_scid, false)!
}

fn test_handle_version_negotiation_discards_mismatched_scid() {
	// The other half of the same RFC 9000 §17.2.1 echo requirement: the VN
	// packet's SCID must equal the client's own original DCID. A VN packet
	// with the right DCID but a wrong SCID is just as spoofable as one with
	// a wrong DCID and must be discarded the same way.
	original_dcid := [u8(5), 6]
	original_scid := [u8(1), 2]
	vn := QuicVersionNegotiation{
		dcid:     original_scid
		scid:     [u8(9), 9]         // does NOT match original_dcid
		versions: [u32(0xff00_0001)] // no v1 -- would otherwise be terminal
	}
	handle_version_negotiation(vn, original_dcid, original_scid, false)!
}

fn test_handle_version_negotiation_discards_when_other_packet_already_processed() {
	// RFC 9000 §6.2, first sentence: "A client MUST discard any Version
	// Negotiation packet if it has received and successfully processed any
	// other packet, including an earlier Version Negotiation packet." This
	// applies even to a VN packet that would otherwise be terminal (no v1
	// offered) -- once the connection attempt is past this point, no VN
	// packet can still be legitimate.
	original_dcid := [u8(5), 6]
	original_scid := [u8(1), 2]
	vn := QuicVersionNegotiation{
		dcid:     original_scid
		scid:     original_dcid
		versions: [u32(0xff00_0001)] // no v1 -- would otherwise be terminal
	}
	handle_version_negotiation(vn, original_dcid, original_scid, true)!
}
