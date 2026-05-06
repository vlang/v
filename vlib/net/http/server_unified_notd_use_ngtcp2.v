module http

// Stub when QUIC/ngtcp2 is not available — HTTP/3 server is a no-op.
import net.http.common

fn maybe_start_h3(s &Server, tls_addr string, handler_fn fn (common.ServerRequest) common.ServerResponse) {
	// HTTP/3 requires -d use_ngtcp2; silently skip.
}
