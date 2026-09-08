import net.ftp

fn check_for_network(tname string) ? {
	_ = tname
	$if !network ? {
		eprintln('> skipping ${tname:-20}, since `-d network` is not passed')
		return none
	}
}

fn run_ftp_client_test(server string) ! {
	mut zftp := ftp.new()
	defer { zftp.close() or {} }
	connect_result := zftp.connect(server)!
	assert connect_result
	println('> connected to ${server}')
	login_result := zftp.login('ftp', 'ftp')!
	assert login_result
	pwd := zftp.pwd()!
	assert pwd.len > 0
	zftp.cd('/')!
	dir_list1 := zftp.dir()!
	assert dir_list1.len > 0
}

fn test_ftp_client() {
	check_for_network(@FN) or { return }
	// This test uses a third-party FTP server. Protocol and response assertions
	// still fail, while transport outages should not make CI fail.
	server := 'ftp.sunet.se:21'
	run_ftp_client_test(server) or {
		eprintln('> skipping test_ftp_client: external FTP request to ${server} failed: ${err}')
	}
}

fn run_ftp_get_test(server string) ! {
	mut zftp := ftp.new()
	defer { zftp.close() or {} }
	connect_result := zftp.connect(server)!
	assert connect_result
	println('> connected to ${server}')
	login_result := zftp.login('ftp', 'ftp')!
	assert login_result
	pwd := zftp.pwd()!
	assert pwd.len > 0
	mut txt := zftp.get('robots.txt')!
	assert txt[0] == 35 // first byte is # char
	zftp.pwd()!
	zftp.cd('pub')!
	zftp.cd('..')!
	zftp.get('robots.txt')!
}

fn test_ftp_get() {
	check_for_network(@FN) or { return }
	server := 'ftp.sunet.se:21'
	run_ftp_get_test(server) or {
		eprintln('> skipping test_ftp_get: external FTP request to ${server} failed: ${err}')
	}
}
