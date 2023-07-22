import net.ftp

fn test_ftp_client() {
	$if !network ? {
		return
	}
	// Note: this function makes network calls to external servers,
	// that is why it is not a very good idea to run it in CI.
	// If you want to run it manually, use:
	// `v -d network vlib/net/ftp/ftp_test.v`
	ftp_client_test_inside() or { panic(err) }
}

fn ftp_client_test_inside() ! {
	mut zftp := ftp.new('ftp.redhat.com', ftp.default_port)!
	defer {
		zftp.close() or { panic(err) }
	}
	zftp.login('ftp', 'ftp')!

	pwd := zftp.pwd()!
	assert pwd.len > 0

	zftp.cd('/suse/linux/enterprise/11Server/en/SAT-TOOLS/SRPMS/')!
	dir_list2 := zftp.dir() or {
		assert err.code() == 425, err.msg() // skip test if 'Security: Bad IP connecting.'
		['python-decorator-3.4.2-4.1.sles11_4sat.src.rpm']
	}
	assert dir_list2.len > 0
	assert dir_list2.contains('python-decorator-3.4.2-4.1.sles11_4sat.src.rpm')
	
	blob := zftp.get('python-decorator-3.4.2-4.1.sles11_4sat.src.rpm') or {
		assert err.code() == 425, err.msg()
		[u8(1)]
	}
	assert blob.len > 0
}
