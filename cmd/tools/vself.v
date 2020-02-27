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

	$if windows {
		bak_file := 'v_old.exe'
		if os.exists(bak_file) {
			os.rm(bak_file)
		}
		os.mv('v.exe', bak_file)

		s2 := os.exec('v_old -o v cmd/v') or {
			panic(err)
		}
		println(s2.output)
	} $else {
		bak_file := 'v_old'
		if os.exists(bak_file) {
			os.rm(bak_file)
		}
		os.mv('v', bak_file)

		s2 := os.exec('v_old -o v cmd/v') or {
			panic(err)
		}
		println(s2.output)
	}
}
