module main


import ftp
import os

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

			dtp := ftp.pasv() or {
				println("data channel not established")
				ftp.close()
				exit
			}

			println('DTP on host $dtp.ip port $dtp.port')

			ftp.dir()
			data := dtp.read()
			println('$data')

			ftp.cd('/suse/linux/enterprise/11Server/en/SAT-TOOLS/SRPMS/')
			dtp.close()

			dtp2 := ftp.pasv() or {
				println("data channel not established")
				ftp.close()
				exit
			}

			ftp.dir()
			dir_list := dtp2.read()
			ftp.read()
			println('$dir_list')
			dtp2.close()

 
			blob := ftp.get('katello-host-tools-3.3.5-8.sles11_4sat.src.rpm') or {
				println("couldn't download it")
				return
			}

			println('downloaded $blob.len bytes')
			os.write_file('/tmp/downloaded', blob)		
		}

		ftp.close()
	}
}