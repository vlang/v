module http

// Fallback stubs when QUIC/ngtcp2 is not available.
import net.urllib

// negotiate_version selects the HTTP version for a request.
// Without ngtcp2, HTTP/3 is never negotiated.
fn (req &Request) negotiate_version(url urllib.URL) Version {
	if req.version != .unknown {
		return req.version
	}

	if url.scheme != 'https' {
		return .v1_1
	}

	return .v2_0
}

fn (req &Request) do_http3(url urllib.URL) !Response {
	return error('HTTP/3 requires -d use_ngtcp2')
}
