#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp

import os
import net.http

const github_job = os.getenv('GITHUB_JOB')

struct JQ {
	tag_name string
}

fn main() {
	root := os.real_path(os.dir(os.getenv_opt('VEXE') or { @VEXE }))
	os.chdir(root)! // make sure that the workfolder is stable

	tloc := os.join_path(root, 'thirdparty')
	loc := os.join_path(tloc, 'binaryen')

	if os.exists(loc) {
		eprintln('thirdparty/binaryen exists, will not overwrite')
		eprintln('delete the folder, and execute again')
		exit(1)
	}

	/*
	// TODO: add retries here, github requests can fail
	jq := http.get_text('https://api.github.com/repos/WebAssembly/binaryen/releases/latest')
	tag := json.decode(JQ, jq)!.tag_name
	     if github_job != '' {
	             dump(jq)
	             dump(tag)
	     }
	*/
	tag := 'version_112'

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
		eprintln('A premade binary library is not available for your system.')
		eprintln('Build it from source, following the documentation here: https://github.com/WebAssembly/binaryen/#building')
		exit(1)
	}

	fname := 'binaryen-${tag}'
	url := 'https://github.com/WebAssembly/binaryen/releases/download/${tag}/${fname}-${name}.tar.gz'

	saveloc := os.join_path(tloc, '${fname}.tar.gz')
	if !os.exists(saveloc) {
		println('Downloading archive: ${saveloc}, from url: ${url} ...')
		http.download_file(url, saveloc)!
		// defer { os.rm(saveloc) or {}! }
	}

	println('Extracting `${tloc}/${fname}` to `${tloc}/binaryen` ...')
	cmd := 'tar -xvf ${saveloc} --directory ${tloc}'
	if os.system(cmd) != 0 {
		eprintln('`${cmd}` exited with a non zero exit code')
		exit(1)
	}

	println(cmd)
	println('Moving `${tloc}/${fname}` to `${tloc}/binaryen` ...')

	os.rename_dir('${tloc}/${fname}', loc)!
	println('Done. You can now use `v -b wasm file.v` .')
}
