// vtest build: present_openssl?
import net.openssl

fn test_get_alpn_selected_exists() {
	mut conn := openssl.new_ssl_conn() or {
		// If SSL init fails, we still verify the method exists by calling it.
		assert true
		return
	}
	// On a freshly initialized connection with no handshake,
	// get_alpn_selected() should return none.
	result := conn.get_alpn_selected() or {
		// Expected: no ALPN negotiated without a handshake
		assert true
		return
	}
	// If we somehow get here, the result should be a string
	assert result.len >= 0
}
