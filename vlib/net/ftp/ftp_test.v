module main

import net.ftp

fn test_all() {
	mut ftp := ftp.new()
	defer {
		ftp.close()
	}

	assert ftp.connect('ftp.redhat.com')
	assert ftp.login('ftp','ftp')

	pwd := ftp.pwd()
	println('pwd: $pwd')

	ftp.cd('/')
	folder := ftp.dir() or {
		assert false
		return
	}
	for file in folder {
		println(file)
	}

	ftp.cd('/suse/linux/enterprise/11Server/en/SAT-TOOLS/SRPMS/')

	dir_list := ftp.dir() or {
		assert false
		return
	}

	assert dir_list.len > 5
	println('$dir_list.len files')
	for file in dir_list {
		println('$file')
	}

	blob := ftp.get('katello-host-tools-3.3.5-8.sles11_4sat.src.rpm') or {
		assert false
		return
	}

	assert blob.len > 1024
	println('downloaded $blob.len bytes')
}
