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
// existed; exactly the kind of trailing padding pad_datagram_for_initial
// produces and frame 1's own Client Initial datagram already
// demonstrated). This is real independent-implementation evidence that
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
	// nothing coalesced after it.
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

fn test_split_coalesced_datagram_rejects_length_exceeding_buffer() {
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    []u8{len: 8}
		scid:    []u8{len: 8}
		token:   []u8{}
		length:  1000 // claims far more than actually follows
	}
	mut buf := encode_long_header(h, 0, 0)!
	buf << [u8(0x01), 0x02, 0x03] // a few bytes, nowhere near 1000

	split_coalesced_datagram(buf) or {
		assert err.msg().contains('exceeding')
		return
	}
	assert false, 'expected an oversized Length field to be rejected'
}

fn test_pad_datagram_for_initial_pads_to_minimum() {
	short := []u8{len: 50, init: 0x42}
	padded := pad_datagram_for_initial(short)
	assert padded.len == min_initial_datagram_size
	assert padded[..50] == short
	assert padded[50] == 0

	already_long := []u8{len: min_initial_datagram_size + 10, init: 0x01}
	assert pad_datagram_for_initial(already_long).len == already_long.len
}
