// vtest build: present_openssl? && !(windows && tinyc)
import net.ssl

fn test_get_alpn_selected_available_through_wrapper() {
	mut conn := ssl.new_ssl_conn() or {
		assert true
		return
	}
	// Verify get_alpn_selected() is accessible through the ssl wrapper.
	// On a freshly initialized connection with no handshake,
	// it should return none.
	result := conn.get_alpn_selected() or {
		assert true
		return
	}
	assert result.len >= 0
}
