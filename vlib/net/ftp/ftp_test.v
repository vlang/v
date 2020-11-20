import net.ftp

// NB: this function makes network calls to external servers,
// that is why it is not a very good idea to run it in CI.
// If you want to run it manually, use `v -d network vlib/net/ftp/ftp_test.v`
fn ftp_client_test_inside() ? {
	$if !network ? { return }
	mut ftp := ftp.new()
	defer {
		ftp.close()
	}
	connect_result := ftp.connect('ftp.redhat.com')?
	assert connect_result
	login_result := ftp.login('ftp', 'ftp')?
	assert login_result
	pwd := ftp.pwd()?
	assert pwd.len > 0
	ftp.cd('/')
	dir_list1 := ftp.dir() or {
		assert false
		return
	}
	assert dir_list1.len > 0
	ftp.cd('/suse/linux/enterprise/11Server/en/SAT-TOOLS/SRPMS/')
	dir_list2 := ftp.dir() or {
		assert false
		return
	}
	assert dir_list2.len > 0
	blob := ftp.get('katello-host-tools-3.3.5-8.sles11_4sat.src.rpm') or {
		assert false
		return
	}
	assert blob.len > 0
}


fn test_ftp_cleint() {
	ftp_client_test_inside() or {
		panic(err)
	}
}
