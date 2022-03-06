import net.ftp

fn test_ftp_cleint() {
	$if !network ? {
		return
	}
	// Note: this function makes network calls to external servers,
	// that is why it is not a very good idea to run it in CI.
	// If you want to run it manually, use:
	// `v -d network vlib/net/ftp/ftp_test.v`
	ftp_client_test_inside() or { panic(err) }
}

fn ftp_client_test_inside() ? {
	mut zftp := ftp.new()
	// eprintln(zftp)
	defer {
		zftp.close() or { panic(err) }
	}
	connect_result := zftp.connect('ftp.redhat.com') ?
	assert connect_result
	login_result := zftp.login('ftp', 'ftp') ?
	assert login_result
	pwd := zftp.pwd() ?
	assert pwd.len > 0
	zftp.cd('/') or {
		assert false
		return
	}
	dir_list1 := zftp.dir() or {
		assert false
		return
	}
	assert dir_list1.len > 0
	zftp.cd('/suse/linux/enterprise/11Server/en/SAT-TOOLS/SRPMS/') or {
		assert false
		return
	}
	dir_list2 := zftp.dir() or {
		assert false
		return
	}
	assert dir_list2.len > 0
	assert dir_list2.contains('katello-host-tools-3.3.5-8.sles11_4sat.src.rpm')
	blob := zftp.get('katello-host-tools-3.3.5-8.sles11_4sat.src.rpm') or {
		assert false
		return
	}
	assert blob.len > 0
}
