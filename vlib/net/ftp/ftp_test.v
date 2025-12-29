import net.ftp

fn check_for_network(tname string) ? {
	$if !network ? {
		eprintln('> skipping ${tname:-20}, since `-d network` is not passed')
		return none
	}
}

fn test_ftp_client() {
	check_for_network(@FN) or { return }
	// Note: this function makes network calls to external servers,
	// that is why it is not a very good idea to run it in CI.
	// If you want to run it manually, use:
	// `v -d network vlib/net/ftp/ftp_test.v`
	mut zftp := ftp.new()
	defer { zftp.close() or { panic(err) } }
	server := 'ftp.furry.de:21'
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
	zftp.cd('/pub/computer/win95/games/gubble/')!
	dir_list2 := zftp.dir()!
	assert dir_list2.len > 3
	wanted_txt_file := 'GubMacDemo.txt'
	assert dir_list2.contains(wanted_txt_file)
	blob := zftp.get(wanted_txt_file)!
	assert blob.len > 0
	sblob := blob.bytestr()
	assert sblob.contains('GUBBLE is a classic arcade style action/strategy game.')
}

fn test_ftp_get() ! {
	check_for_network(@FN) or { return }
	mut zftp := ftp.new()
	defer { zftp.close() or { panic(err) } }
	server := 'ftp.sunet.se:21'
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
