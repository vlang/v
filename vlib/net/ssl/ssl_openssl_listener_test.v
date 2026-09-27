// vtest build: present_openssl? && !(windows && tinyc)
// vtest vflags: -d use_openssl
module main

import net.ssl
import os

fn test_net_ssl_openssl_listener_preserves_ipv6_options() ! {
	$if macos || linux {
		cert_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.crt')
		key_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
		mut listener := ssl.new_ssl_listener('[::1]:0', ssl.SSLConnectConfig{
			cert:     cert_path
			cert_key: key_path
		}, ssl.SSLListenerOptions{
			family: .ip6
		})!
		listener.shutdown()!
	}
}
