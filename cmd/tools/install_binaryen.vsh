#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp

import net.http
import json
import os

struct JQ {
	tag_name string
}

fn main() {
	root := os.dir(os.real_path(os.getenv_opt('VEXE') or { @VEXE }))

	tloc := '${root}/thirdparty'
	loc := '${tloc}/binaryen'

	if os.exists(loc) {
		eprintln('thirdparty/binaryen exists, will not overwrite')
		eprintln('delete the folder, and execute again')
		exit(1)
	}

	jq := http.get_text('https://api.github.com/repos/WebAssembly/binaryen/releases/latest')
	tag := json.decode(JQ, jq)!.tag_name

	name := $if windows {
		'x86_64-windows'
	} $else $if macos {
		$if arm64 {
			'arm64-macos'
		} $else {
			'x86_64-macos'
		}
	} $else $if linux {
		'x86_64-linux'
	} $else {
		eprintln('a premade binary library is not available for your system')
		eprintln('build from source, documentation here: https://github.com/WebAssembly/binaryen/#building')
		exit(1)
	}

	fname := 'binaryen-${tag}'
	url := 'https://github.com/WebAssembly/binaryen/releases/download/${tag}/${fname}-${name}.tar.gz'

	mkdir_all(loc, os.MkdirParams{})!
	println(loc)

	saveloc := '${tloc}/${fname}.tar.gz'
	if !os.exists(saveloc) {
		println('downloading archive: ${saveloc}, from url: ${url}')
		http.download_file(url, saveloc)!
	}
	cmd := 'tar -xvf ${saveloc} --directory ${tloc}'
	if os.system(cmd) != 0 {
		eprintln('`${cmd}` exited with a non zero exit code')
		exit(1)
	}

	println(cmd)
	println('${tloc}/${fname} to ${tloc}/binaryen')

	os.rename_dir('${tloc}/${fname}', loc)!
}
