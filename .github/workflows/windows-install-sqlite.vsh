#!/usr/bin/env -S v run

import os
import net.http
import compress.szip
import crypto.sha3

fn should_be_ok(http_status_code int, msg string) {
	assert http_status_code == 200, '${msg}. Check your internet connection and try again later.'
}

unbuffer_stdout()
os.chdir(@VROOT)!

download_page_url := 'https://sqlite.org/download.html'
println('> Getting ${download_page_url} ...')
download_page := http.get(download_page_url)!
should_be_ok(download_page.status_code, 'The download page of SQLite is not available now.')

dlines := download_page.body.split_into_lines()
amalgamation_csv := dlines.filter(|line| line.starts_with('PRODUCT,')
	&& line.contains('amalgamation'))[0].split(',')
assert amalgamation_csv.len >= 4

version := amalgamation_csv[1]
zip_name := os.file_name(amalgamation_csv[2])
zip_url := 'https://sqlite.org/${amalgamation_csv[2]}'
url_size := amalgamation_csv[3].int()
url_sha3 := amalgamation_csv[4]
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
