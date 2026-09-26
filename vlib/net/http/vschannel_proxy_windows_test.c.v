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
	for host in ['example.com', 'Example.COM', '127.0.0.1', '[2001:db8::1]', 'xn--mnich-kva.example',
		'example.com.'] {
		for port in [1, 443, 8443, 65535] {
			status, request := schannel_proxy_request_for_test(host.to_wide(), port)
			assert status == 0
			assert request == 'CONNECT ${host}:${port} HTTP/1.0\r\nUser-Agent: webclient\r\n\r\n'
		}
	}
}

fn test_schannel_proxy_request_encodes_internationalized_hostname() {
	$if no_vschannel ? {
		return
	}
	hosts := {
		'münich.example':       'xn--mnich-kva.example'
		'mu\u0308nich.example': 'xn--mnich-kva.example'
		'münich.example.':      'xn--mnich-kva.example.'
		'例え.テスト':          'xn--r8jz45g.xn--zckzah'
		'münich。example':      'xn--mnich-kva.example'
	}
	for host, ascii_host in hosts {
		for port in [1, 443, 8443, 65535] {
			status, request := schannel_proxy_request_for_test(host.to_wide(), port)
			assert status == 0
			assert request == 'CONNECT ${ascii_host}:${port} HTTP/1.0\r\nUser-Agent: webclient\r\n\r\n'
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
	unicode_host := ('ü'.repeat(57) + '.').repeat(3) + 'ü'.repeat(55)
	ascii_host := ('xn--td' + 'a'.repeat(57) + '.').repeat(3) + 'xn--td' + 'a'.repeat(55)
	assert ascii_host.len == 253
	idn_status, idn_request := schannel_proxy_request_for_test(unicode_host.to_wide(), 443)
	assert idn_status == 0
	assert idn_request == 'CONNECT ${ascii_host}:443 HTTP/1.0\r\nUser-Agent: webclient\r\n\r\n'
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
	assert status != 0
	for host in ['münich/.example', 'münich／example', 'ü'.repeat(58) + '.example'] {
		idn_status, _ := schannel_proxy_request_for_test(host.to_wide(), 443)
		assert idn_status == 123 // ERROR_INVALID_NAME
	}
}
