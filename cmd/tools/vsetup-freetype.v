module main

import os
import v.pref

fn main() {
	$if windows {
		println('Setup freetype...')
		vroot := os.dir(pref.vexe_path())
		os.chdir(vroot)

		if os.is_dir('./thirdparty/freetype') {
			println('Thirdparty "freetype" is already installed.')
		}
		else {
			s := os.exec('git clone --depth=1 https://github.com/ubawurinna/freetype-windows-binaries ./thirdparty/freetype/') or {
				panic(err)
			}
			println(s.output)
			println('Thirdparty "freetype" installed successfully.')
		}
	}
	$else {
		println('It is only for Windows to setup thirdparty "freetype".')
	}
}
