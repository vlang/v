module main

import (
	os
	filepath
)

fn main() {
	println('Updating V...')
	vroot := filepath.dir(os.getenv('VEXE'))
	os.chdir(vroot)
	s := os.exec('git -C "$vroot" pull --rebase origin master') or { panic(err) }
	println(s.output)
	$if windows {
		v_backup_file := '$vroot/v_old.exe'
		if os.exists( v_backup_file ) {
			os.rm( v_backup_file )
		}
		os.mv('$vroot/v.exe', v_backup_file)
		s2 := os.exec('"$vroot/make.bat"') or { panic(err) }
		println(s2.output)
	} $else {
		s2 := os.exec('make -C "$vroot"') or { panic(err) }
		println(s2.output)
	}
}
