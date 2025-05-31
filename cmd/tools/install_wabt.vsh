#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp

import os
import net.http

const root = @VROOT

fn main() {
	os.chdir(root)! // make sure that the workfolder is stable

	tloc := os.join_path(root, 'thirdparty')
	loc := os.join_path(tloc, 'wabt')

	if os.exists(loc) {
		eprintln('thirdparty/wabt exists, will not overwrite')
		eprintln('delete the folder, and execute again')
		exit(1)
	}
	tag := '1.0.32'
	fname := 'wabt-${tag}'
	platform := $if windows {
		'windows'
	} $else $if macos {
		'macos-14'
	} $else $if linux {
		'ubuntu'
	} $else {
		eprintln('A premade binary library is not available for your system.')
		eprintln('Build it from source, following the documentation here: https://github.com/WebAssembly/wabt/')
		exit(1)
	}
	url := 'https://github.com/WebAssembly/wabt/releases/download/${tag}/${fname}-${platform}.tar.gz'
	saveloc := os.join_path(tloc, '${fname}.tar.gz')
	if !os.exists(saveloc) {
		println('Downloading archive: ${saveloc}, from url: ${url} ...')
		http.download_file(url, saveloc)!
		// defer { os.rm(saveloc) or {}! }
	}

	println('Extracting `${tloc}/${fname}` to `${tloc}/wabt` ...')
	cmd := 'tar -xvf ${saveloc} --directory ${tloc}'
	if os.system(cmd) != 0 {
		eprintln('`${cmd}` exited with a non zero exit code')
		exit(1)
	}

	println(cmd)
	println('Moving `${tloc}/${fname}` to `${tloc}/wabt` ...')

	os.rename_dir('${tloc}/${fname}', loc)!
	println('Done. You can now use `v test vlib/wasm` .')
}
