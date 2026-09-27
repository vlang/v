module http

fn test_http3_reports_compile_gate_without_http3_define() {
	$if !http3 ? {
		mut transport := new_transport()
		req := &Request{
			enable_http3: true
		}
		transport.h3_round_trip(req, 'key', .get, 'example.com', 443, '/', '', new_header()) or {
			assert err.msg().contains('not compiled in')
			assert err.msg().contains('-d http3')
			return
		}
		assert false, 'HTTP/3 should be unavailable without -d http3'
	}
}
