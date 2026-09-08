module mbedtls

import net
import os

fn test_mbedtls_ssl_listener_preserves_ipv6_literal_by_default() ! {
	$if macos || linux {
		cert_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.crt')
		key_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
		mut listener := new_ssl_listener('[::1]:0', SSLConnectConfig{
			cert:     cert_path
			cert_key: key_path
		})!
		listener.shutdown()!
	}
}

fn test_mbedtls_ssl_listener_preserves_unspecified_ipv6_by_default() ! {
	$if macos || linux {
		cert_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.crt')
		key_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
		mut listener := new_ssl_listener(':0', SSLConnectConfig{
			cert:     cert_path
			cert_key: key_path
		})!
		listener.shutdown()!
	}
}

fn test_mbedtls_ssl_listener_infers_hostname_family() ! {
	$if macos || linux {
		addrs := net.resolve_addrs('localhost:0', .unspec, .tcp)!
		assert addrs.len > 0
		cert_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.crt')
		key_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
		mut listener := new_ssl_listener('localhost:0', SSLConnectConfig{
			cert:     cert_path
			cert_key: key_path
		})!
		defer {
			listener.shutdown() or {}
		}
		family := net.addr_from_socket_handle(listener.server_fd.fd).family()
		assert addrs.any(it.family() == family)
	}
}
