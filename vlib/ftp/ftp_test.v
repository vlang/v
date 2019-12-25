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

			folder := ftp.dir() or {
				println('cannot list folder')
				return
			}	
			for file in folder {
				println(file)
			}

			ftp.cd('/suse/linux/enterprise/11Server/en/SAT-TOOLS/SRPMS/')
				
			dir_list := ftp.dir() or {
				println('cannot list folder')
				return
			}
			
			assert dir_list.len == 8
			println('$dir_list.len files:')
			for file in dir_list {
				println('$file')
			}

			blob := ftp.get('katello-host-tools-3.3.5-8.sles11_4sat.src.rpm') or {
				println("couldn't download it")
				return
			}

			assert blob.len == 55670

			println('downloaded $blob.len bytes')
		}

		ftp.close()
	}
}
