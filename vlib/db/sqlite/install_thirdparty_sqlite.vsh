#!/usr/bin/env v

import os
import net.http
import compress.szip
import crypto.sha3

fn should_be_ok(http_status_code int, msg string) {
	assert http_status_code == 200, '${msg}. Check your internet connection and try again later.'
}

fn main() {
	os.chdir(@VEXEROOT)!

	// The amalgamation is pinned, not taken as the newest one from sqlite.org:
	// SQLite 3.54.0 uses SRWLOCK in its Windows mutex code, and the tcc Windows
	// headers do not declare it, so `v -cc tcc` cannot build that version.
	// Move the pin forward only after tcc builds the new amalgamation.
	version := '3.53.4'
	zip_name := 'sqlite-amalgamation-3530400.zip'
	zip_url := 'https://sqlite.org/2026/${zip_name}'
	url_size := 2946650
	url_sha3 := '628a44cfe82c66aed1ccbbe85a562d2e33ebe64b3288981ed76285612227934e'
	println('> Getting SQLite amalgamation version: ${version}')
	println('>           from url: ${zip_url}')
	println('>      expected size: ${url_size}')
	println('>      expected SHA3: ${url_sha3} ...')
	amalgamation_name := zip_name.to_lower().replace('.zip', '')

	println('> Downloading from ${zip_url} ...')
	zip_content := http.get(zip_url)!
	should_be_ok(zip_content.status_code, 'The .zip file URL of SQLite is not available now.')

	assert zip_content.body.len == url_size
	println('> download size: ${zip_content.body.len} matches expected size: ${url_size} .')
	zip_shasum := sha3.sum256(zip_content.body.bytes()).hex()
	assert zip_shasum == url_sha3
	println('> download sha3: ${zip_shasum} matches too.')

	os.write_file(zip_name, zip_content.body)!
	assert os.is_file(zip_name)
	assert szip.extract_zip_to_dir(zip_name, 'thirdparty')!
	os.rmdir_all('thirdparty/sqlite') or {}
	os.mv('thirdparty/${amalgamation_name}', 'thirdparty/sqlite')!
	os.rm('thirdparty/sqlite/shell.c')!
	files := os.walk_ext('thirdparty/sqlite', '')
	for f in files {
		println('> extracted file: ${f:-40s} | size: ${os.file_size(f):8}')
	}

	println('> removing ${zip_name} ...')
	os.rm(zip_name)!
	println('> done')
}
