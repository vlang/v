module http

// ALPN negotiation on the Windows SChannel backend (vlang/v#27383).
//
// The wire-encoding test runs anywhere on Windows. The negotiation tests are
// network-dependent: run with `-d network`, e.g.
//   v -d network test vlib/net/http/vschannel_alpn_windows_test.v

fn test_alpn_wire_encoding() {
	mut want := [u8(0x02)]
	want << 'h2'.bytes()
	want << 0x08
	want << 'http/1.1'.bytes()
	assert alpn_wire(['h2', 'http/1.1']) == want
	// Empty and over-long (>255) names are skipped.
	assert alpn_wire([]string{}) == []u8{}
	assert alpn_wire(['', 'h2']) == [u8(0x02), 0x68, 0x32] // 0x68='h', 0x32='2'
}

fn test_schannel_alpn_negotiates_h2() {
	$if !network ? {
		return
	}
	// A public HTTP/2 server must select `h2` when offered.
	selected := schannel_alpn_probe('www.google.com', 443, ['h2', 'http/1.1'], false)
	assert selected == 'h2', 'expected h2, got "${selected}"'
}

fn test_schannel_alpn_falls_back_to_http1() {
	$if !network ? {
		return
	}
	// Offer only HTTP/1.1: the server must not select h2.
	selected := schannel_alpn_probe('www.google.com', 443, ['http/1.1'], false)
	assert selected == 'http/1.1', 'expected http/1.1, got "${selected}"'
}
