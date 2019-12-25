module main


import ftp

fn test_all() {
	mut ftp := ftp.new()
	ftp.debug()

	// ftp.rediris.org
	if ftp.connect('ftp.redhat.com') { 
		println("connected")

		if ftp.login('ftp','ftp') {
			println('logged-in')

			pwd := ftp.pwd()
			println('pwd: $pwd')

			ftp.cd('/')

			data := ftp.dir() or {
				println('cannot list folder')
				return
			}	
			println('$data')

			ftp.cd('/suse/linux/enterprise/11Server/en/SAT-TOOLS/SRPMS/')
				
			dir_list := ftp.dir() or {
				println('cannot list folder')
				return
			}
			println('$dir_list')

			blob := ftp.get('katello-host-tools-3.3.5-8.sles11_4sat.src.rpm') or {
				println("couldn't download it")
				return
			}

			println('downloaded $blob.len bytes')
		}

		ftp.close()
	}
}