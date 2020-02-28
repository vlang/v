module main

import (
	os
	filepath
	v.pref
)

fn main() {
	println('V Self Compiling...')
	vroot := filepath.dir(pref.vexe_path())
	os.chdir(vroot)

	s2 := os.exec('v -o v2 cmd/v') or {
		panic(err)
	}
	println(s2.output)

	$if windows {
		bak_file := 'v_old.exe'
		if os.exists(bak_file) {
			os.rm(bak_file)
		}
		os.mv('v.exe', bak_file)
		os.mv('v2.exe', 'v.exe')


	} $else {
		bak_file := 'v_old'
		if os.exists(bak_file) {
			os.rm(bak_file)
		}
		os.mv('v', bak_file)
		os.mv('v2', 'v')
	}
}
