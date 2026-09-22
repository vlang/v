// vtest build: windows
module http

fn C.vschannel_build_proxy_request(&u16, int, &&char, &int) int

fn schannel_proxy_request_for_test(host &u16, port int) (int, string) {
	$if no_vschannel ? {
		return 0, ''
	} $else {
		mut buffer := unsafe { &char(nil) }
		mut length := -1
		status := C.vschannel_build_proxy_request(host, port, &buffer, &length)
		if status != 0 {
			assert buffer == unsafe { nil }
			assert length == 0
			return status, ''
		}
		defer {
			C.LocalFree(buffer)
		}
		assert length > 0
		assert unsafe { buffer[length] } == 0
		return status, unsafe { (&u8(buffer)).vstring_with_len(length).clone() }
	}
}

fn test_schannel_proxy_request_preserves_hostname_and_port() {
	$if no_vschannel ? {
		return
	}
	for host in ['example.com', '127.0.0.1', 'xn--mnich-kva.example', 'münich.example'] {
		for port in [1, 443, 8443, 65535] {
			status, request := schannel_proxy_request_for_test(host.to_wide(), port)
			assert status == 0
			assert request == 'CONNECT ${host}:${port} HTTP/1.0\r\nUser-Agent: webclient\r\n\r\n'
		}
	}
}

fn test_schannel_proxy_request_accepts_long_hostname() {
	$if no_vschannel ? {
		return
	}
	host := 'a'.repeat(63) + '.' + 'b'.repeat(63) + '.' + 'c'.repeat(63) + '.' + 'd'.repeat(61)
	assert host.len == 253
	status, request := schannel_proxy_request_for_test(host.to_wide(), 443)
	assert status == 0
	assert request == 'CONNECT ${host}:443 HTTP/1.0\r\nUser-Agent: webclient\r\n\r\n'
}

fn test_schannel_proxy_request_rejects_invalid_input() {
	$if no_vschannel ? {
		return
	}
	for host in ['', 'bad host', 'host\tname', 'example.com\r\nInjected: value'] {
		status, _ := schannel_proxy_request_for_test(host.to_wide(), 443)
		assert status == 87 // ERROR_INVALID_PARAMETER
	}
	for port in [-1, 0, 65536] {
		status, _ := schannel_proxy_request_for_test('example.com'.to_wide(), port)
		assert status == 87
	}
	null_status, _ := schannel_proxy_request_for_test(unsafe { nil }, 443)
	assert null_status == 87
	invalid_utf16 := [u16(0xd800), 0]!
	status, _ := schannel_proxy_request_for_test(&invalid_utf16[0], 443)
	assert status == 1113 // ERROR_NO_UNICODE_TRANSLATION
}
