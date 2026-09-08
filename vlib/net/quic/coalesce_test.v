module quic

import encoding.hex

// quiche_server_coalesced_datagram is the raw bytes of the second UDP
// datagram in the same real quiche handshake capture Phase 3's
// packet_protection_test.v used (testdata/tls13_vectors/
// quiche_p256_handshake.pcap, frame 2: server 172.18.0.2:4433 -> client
// 172.18.0.3:53012), extracted with the same standalone pcap/UDP parser.
// Unlike frame 1 (a single non-coalesced Client Initial), this datagram
// genuinely coalesces TWO packets -- an Initial packet and a Handshake
// packet -- followed by 342 bytes of trailing raw zero-byte UDP-level
// padding (the datagram's own byte at that offset is 0x00, which fails
// RFC 9000 §17.2/§17.3.1's Fixed Bit requirement, confirming it is not a
// third coalesced packet at all -- an earlier, less careful reading of
// this same capture had assumed it was, before the Fixed Bit check below
// existed; this is the shape a peer implementation that pads via raw
// trailing bytes rather than internal PADDING frames produces -- this
// client's own outgoing Initial packets no longer pad this way (see
// pad_initial_payload), but a received datagram must still tolerate it
// either way). This is real independent-implementation evidence that
// the exact coalescing shape this module needs to handle (Length-field
// walking across long-header packets, stopping cleanly at trailing
// non-packet padding) occurs in practice, not just in the abstract per
// RFC 9000 §12.2.
const quiche_server_coalesced_datagram = 'c50000000114d7af43d3ec88a36d745b07a58c8cd29bffa3158414edb1e5824271d5fc09713615f78c85e22e1ecbbc00407580dd985a010a10fbfc61bca1d944e520f7550b65fcd6ac7542dc079244f20ef65fdcf0d968b4eee263e9f7a32bd37c8cce61bf67eaf7770899c3cbf001fa06476637dd67481a3ef84ffa6c44bdcdb9b5d015b18eb84fbc8ff754b36ce52b81640e75de8743d0ae9e5335b9f0a7a2fd96924ba0ae86e00000000114d7af43d3ec88a36d745b07a58c8cd29bffa3158414edb1e5824271d5fc09713615f78c85e22e1ecbbc428223d861b73b4cf8764965add5be4f9314e4790ac74552052574adda4c055292950ec03480a28fd6b6a4bf675e450e86987ffe0a301eb63f87138d17a9a3e6909fc38524d848d0e8e3de74d76f85aba2e2f9991e062534acf898385e6da0239cf77770d99eed5f0526e58b1f9def8cd51437358ed1d11d6bfab6af6e988abe7dc72394176e1967399c7d1e290ca02244352efbbd7cbf0645b0d4ed129d69d92936e575408edf0efaf9dd06eb900137137b3bb473914606ca925105ee398fc427d5459f66565587be9379e0a40cd9343407a803a15fb3d76910b9b384948e27256ae3ae95dbfec565e2d14700344e00c029603a505818a03df1002331d4971a19a35a372c6ec0f53d114c261c54c872ea0039deb0ffa293ab86ad41a852fe169b28409890ee418a418f8e9241a54af96626b57e673db8c6dc2f9e457a303adf230cf1c1e3306a66956075c73b6100eca4e0a157231895008bd1929d46ef1fe76a4af12024fca28c2e5df657921c9569af4ec89f07de54de4703d70f875d415011f7d99810118f5cab1d4ba6c98a5bd4a807d1cc8480edf57c1f06194dd57bab49d4005e93307eb77aead09a65659614292ee28e65bf219d60891344474553305983eaca506e86f0c30a7048bee4600324cadaa92dea1b2278277d5ff093f4e4362a577feda9570139b3140e7c836ffbff00a7b629110c8d64eeb05ef4735fd4a91d89dbf1eee48e889d710c183ea012ed5427fe9641175a0d5bd11c9a46328fca49c4ae6c7081f533bd926f31d506a3f3fab6ddba998711c351f65c187848a313e9d09221ed8a4c2157f3386ff2ebef272bd490c6bee9dd85de9ecec77baccf622790e5a656d782f74de878efb0122b1d4fae77a200f5525fbec5a2a466cfa1151f93db000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000'

fn test_split_coalesced_datagram_real_server_capture() {
	raw := hex.decode(quiche_server_coalesced_datagram)!
	packets := split_coalesced_datagram(raw)!
	assert packets.len == 2

	assert packets[0].form == .long
	assert packets[0].bytes.len == 167
	h0, _ := parse_long_header(packets[0].bytes)!
	assert h0.typ == .initial
	assert h0.version == quic_v1

	assert packets[1].form == .long
	assert packets[1].bytes.len == 691
	h1, _ := parse_long_header(packets[1].bytes)!
	assert h1.typ == .handshake
	assert h1.version == quic_v1

	// The two real packets plus the 342 trailing padding bytes exactly
	// partition the datagram; split_coalesced_datagram stops before the
	// padding rather than reporting it as a bogus third packet.
	assert packets[0].bytes.len + packets[1].bytes.len + 342 == raw.len
	assert raw[packets[0].bytes.len + packets[1].bytes.len] == 0x00
}

fn test_split_coalesced_datagram_single_initial_packet() {
	// Slice out just the first (Initial) packet from the real coalesced
	// server datagram above and re-split THAT alone: the simplest possible
	// case, one packet exactly spanning its own declared length with
	// nothing coalesced after it. Deliberately NOT padded to the 1200-byte
	// floor -- this splitter has no size-based discard (see the NOTE in
	// coalesce.v: that floor binds servers receiving client Initials, and a
	// server's own small, non-ack-eliciting Initial is legitimate; a prior
	// round's unconditional size check broke exactly that case, Codex P2,
	// pullrequestreview-4843018164).
	raw := hex.decode(quiche_server_coalesced_datagram)!
	all_packets := split_coalesced_datagram(raw)!
	single := all_packets[0].bytes

	packets := split_coalesced_datagram(single)!
	assert packets.len == 1
	assert packets[0].form == .long
	assert packets[0].bytes.len == single.len
}

fn test_split_coalesced_datagram_stops_at_version_negotiation() {
	mut vn := []u8{}
	vn << u8(0x80) // long header form
	vn << [u8(0x00), 0x00, 0x00, 0x00] // version = 0 (Version Negotiation)
	vn << u8(8) // dcid_len
	vn << []u8{len: 8, init: 0xaa}
	vn << u8(8) // scid_len
	vn << []u8{len: 8, init: 0xbb}
	vn << [u8(0x00), 0x00, 0x00, 0x01] // one supported version

	packets := split_coalesced_datagram(vn)!
	assert packets.len == 1
	assert packets[0].bytes.len == vn.len
}

fn test_split_coalesced_datagram_stops_at_short_header() {
	mut buf := []u8{}
	buf << u8(0x40) // short header, fixed bit set, form bit clear
	buf << []u8{len: 8, init: 0xcc} // dcid
	buf << []u8{len: 30, init: 0x11} // "packet number + payload"

	packets := split_coalesced_datagram(buf)!
	assert packets.len == 1
	assert packets[0].form == .short
	assert packets[0].bytes.len == buf.len
}

fn test_split_coalesced_datagram_discards_packet_claiming_length_exceeding_buffer() {
	// RFC 9000 §14.1: a Length field that can't possibly be trusted to
	// delimit a real packet is treated as "invalid packet" padding to be
	// discarded, not a reason to fail the whole datagram -- here it's the
	// only thing in the datagram, so the result is an empty (not erroring)
	// packet list.
	h := QuicLongHeader{
		typ: .initial
		version: quic_v1
		dcid: []u8{len: 8}
		scid: []u8{len: 8}
		token: []u8{}
		length: 1000 // claims far more than actually follows
	}
	mut buf := encode_long_header(h, 0, 0)!
	buf << [u8(0x01), 0x02, 0x03] // a few bytes, nowhere near 1000

	packets := split_coalesced_datagram(buf)!
	assert packets.len == 0
}

fn test_split_coalesced_datagram_keeps_valid_leading_packet_despite_trailing_length_overrun() {
	// The actual target scenario the discard-not-fail policy exists for: a
	// real leading Initial packet followed by "invalid packet" padding
	// (here, a Length field claiming more than remains) must still yield
	// the leading packet, not lose it to a whole-datagram error.
	h := QuicLongHeader{
		typ: .initial
		version: quic_v1
		dcid: []u8{len: 8}
		scid: []u8{len: 8}
		token: []u8{}
		length: 2
	}
	mut buf := encode_long_header(h, 0, 0)!
	buf << [u8(0x00), 0x00]

	bad_h := QuicLongHeader{
		typ: .initial
		version: quic_v1
		dcid: []u8{len: 8}
		scid: []u8{len: 8}
		token: []u8{}
		length: 1000
	}
	mut bad_buf := encode_long_header(bad_h, 0, 0)!
	bad_buf << [u8(0x01), 0x02, 0x03]
	buf << bad_buf

	packets := split_coalesced_datagram(buf)!
	assert packets.len == 1
	long_header, _ := parse_long_header(packets[0].bytes)!
	assert long_header.dcid == h.dcid
}

fn test_split_coalesced_datagram_discards_non_v1_version_with_retry_shaped_type_bits() {
	// peek_long_header_type's bit mapping (bits 4-5 of byte 0) is QUIC-v1-
	// specific -- a non-v1 packet whose type bits happen to be 0b11 (v1's
	// "retry") must be discarded for its wrong version BEFORE the retry
	// branch ever inspects those bits, not silently misclassified as a
	// real Retry packet (which would consume the rest of the datagram and
	// hide any real packets coalesced after it).
	mut buf := []u8{}
	buf << u8(0x80 | 0x40 | 0x30) // long header, fixed bit set, type bits = 0b11 (retry under v1)
	buf << [u8(0xaa), 0xbb, 0xcc, 0xdd] // version: not 0 (not VN), not quic_v1

	packets := split_coalesced_datagram(buf)!
	assert packets.len == 0
}

fn test_split_coalesced_datagram_keeps_valid_leading_packet_despite_trailing_garbage() {
	// The actual target scenario the discard-not-fail policy exists for
	// (RFC 9000 §14.1: "Initial packets can even be coalesced with invalid
	// packets, which a receiver will discard"): a real leading Initial
	// packet followed by bytes that fail to parse as a legitimate next
	// packet (here, an unsupported version) must still be returned, not
	// lost to a whole-datagram error.
	h := QuicLongHeader{
		typ: .initial
		version: quic_v1
		dcid: []u8{len: 8}
		scid: []u8{len: 8}
		token: []u8{}
		length: 2
	}
	mut buf := encode_long_header(h, 0, 0)!
	buf << [u8(0x00), 0x00]
	buf << [u8(0x80 | 0x40), 0xde, 0xad, 0xbe, 0xef] // form+fixed bit set, bogus non-v1 version

	packets := split_coalesced_datagram(buf)!
	assert packets.len == 1
	long_header, _ := parse_long_header(packets[0].bytes)!
	assert long_header.typ == .initial
	assert long_header.dcid == h.dcid
}

fn test_split_coalesced_datagram_rejects_version_negotiation_after_another_packet() {
	// RFC 9000 §12.2: "there is no situation where a Retry or Version
	// Negotiation packet is coalesced with another packet." A datagram
	// containing a real (small) Initial packet followed by VN-shaped bytes
	// cannot come from a compliant sender -- reject it rather than treating
	// the tail as a genuine VN packet.
	h := QuicLongHeader{
		typ: .initial
		version: quic_v1
		dcid: []u8{len: 8}
		scid: []u8{len: 8}
		token: []u8{}
		length: 2 // 1-byte packet number + 1 byte of "payload"
	}
	mut buf := encode_long_header(h, 0, 0)!
	buf << [u8(0x00), 0x00] // pn byte + one payload byte, matching length: 2

	mut vn := []u8{}
	vn << u8(0x80)
	vn << [u8(0x00), 0x00, 0x00, 0x00] // version = 0 (Version Negotiation)
	vn << u8(8)
	vn << []u8{len: 8, init: 0xaa}
	vn << u8(8)
	vn << []u8{len: 8, init: 0xbb}
	vn << [u8(0x00), 0x00, 0x00, 0x01]
	buf << vn

	split_coalesced_datagram(buf) or {
		assert err.msg().contains('coalesced after another packet')
		return
	}
	assert false, 'expected a VN packet following another packet to be rejected'
}

fn test_split_coalesced_datagram_rejects_retry_after_another_packet() {
	// RFC 9000 §12.2's coalescing prohibition covers Retry AND VN
	// symmetrically -- a real (small) Initial packet followed by
	// Retry-shaped bytes cannot come from a compliant sender either.
	h := QuicLongHeader{
		typ: .initial
		version: quic_v1
		dcid: []u8{len: 8}
		scid: []u8{len: 8}
		token: []u8{}
		length: 2
	}
	mut buf := encode_long_header(h, 0, 0)!
	buf << [u8(0x00), 0x00]

	retry_h := QuicLongHeader{
		typ: .retry
		version: quic_v1
		dcid: []u8{len: 8}
		scid: []u8{len: 8}
		token: []u8{}
	}
	mut retry_buf := encode_long_header(retry_h, 0, 0)!
	retry_buf << 'sometoken'.bytes()
	retry_buf << []u8{len: 16} // fake integrity tag, irrelevant to this check
	buf << retry_buf

	split_coalesced_datagram(buf) or {
		assert err.msg().contains('coalesced after another packet')
		return
	}
	assert false, 'expected a Retry packet following another packet to be rejected'
}

fn test_split_coalesced_datagram_ignores_packet_with_mismatched_dcid() {
	// RFC 9000 §12.2: "Receivers SHOULD ignore any subsequent packets
	// with a different Destination Connection ID than the first packet in
	// the datagram." A second Initial packet addressed to a DIFFERENT
	// connection is excluded from the result, not treated as a datagram-
	// wide error -- and scanning still continues past it.
	h1 := QuicLongHeader{
		typ: .initial
		version: quic_v1
		dcid: [u8(1), 1, 1, 1, 1, 1, 1, 1]
		scid: []u8{len: 8}
		token: []u8{}
		length: 2
	}
	mut buf := encode_long_header(h1, 0, 0)!
	buf << [u8(0x00), 0x00]

	h2 := QuicLongHeader{
		typ: .initial
		version: quic_v1
		dcid: [u8(2), 2, 2, 2, 2, 2, 2, 2] // different connection
		scid: []u8{len: 8}
		token: []u8{}
		length: 2
	}
	mut buf2 := encode_long_header(h2, 0, 0)!
	buf2 << [u8(0x00), 0x00]
	buf << buf2

	h3 := QuicLongHeader{
		typ: .initial
		version: quic_v1
		dcid: h1.dcid // back to the first connection
		scid: []u8{len: 8}
		token: []u8{}
		length: 2
	}
	mut buf3 := encode_long_header(h3, 0, 0)!
	buf3 << [u8(0x00), 0x00]
	buf << buf3

	packets := split_coalesced_datagram(buf)!
	assert packets.len == 2 // the mismatched middle packet is excluded

	h_first, _ := parse_long_header(packets[0].bytes)!
	h_last, _ := parse_long_header(packets[1].bytes)!
	assert h_first.dcid == h1.dcid
	assert h_last.dcid == h1.dcid
}

fn test_pad_initial_payload_pads_to_minimum() {
	payload := []u8{len: 50, init: 0x42}
	padded := pad_initial_payload(payload, 20, 16) // header(20) + payload(50) + tag(16) = 86
	assert padded.len == min_initial_datagram_size - 20 - 16
	assert padded[..50] == payload
	assert padded[50] == 0

	// Already at/above the minimum once header+tag are accounted for: no-op.
	already_long := []u8{len: min_initial_datagram_size, init: 0x01}
	assert pad_initial_payload(already_long, 20, 16).len == already_long.len
}
