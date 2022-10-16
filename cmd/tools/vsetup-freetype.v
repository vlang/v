module main

import os
import v.pref

const freetype_repo_url = 'https://github.com/ubawurinna/freetype-windows-binaries'

const freetype_folder = os.join_path('thirdparty', 'freetype')

fn main() {
	$if windows {
		println('Setup freetype...')
		vroot := os.dir(pref.vexe_path())
		os.chdir(vroot)!
		if os.is_dir(freetype_folder) {
			println('Thirdparty "freetype" is already installed.')
		} else {
			s := os.execute('git clone --depth=1 $freetype_repo_url $freetype_folder')
			if s.exit_code != 0 {
				panic(s.output)
			}
			println(s.output)
			println('Thirdparty "freetype" installed successfully.')
		}
	} $else {
		println('It is only for Windows to setup thirdparty "freetype".')
	}
}
