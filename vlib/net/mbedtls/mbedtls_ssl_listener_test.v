module main

import net.mbedtls
import os

fn test_mbedtls_ssl_listener_preserves_ipv6_literal_by_default() ! {
	$if macos || linux {
		cert_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.crt')
		key_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
		mut listener := mbedtls.new_ssl_listener('[::1]:0', mbedtls.SSLConnectConfig{
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
		mut listener := mbedtls.new_ssl_listener(':0', mbedtls.SSLConnectConfig{
			cert:     cert_path
			cert_key: key_path
		})!
		listener.shutdown()!
	}
}
