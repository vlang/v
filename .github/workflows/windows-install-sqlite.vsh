#!/usr/bin/env -S v run

import os
import compress.szip

const amalgamation_name = 'sqlite-amalgamation-3510000'
const zip_name = '${amalgamation_name}.zip'
const zip_url = 'https://sqlite.org/2025/${zip_name}'

unbuffer_stdout()
os.chdir(@VROOT)!

println('> Downloading from ${zip_url} ...')
download_res := os.system('v download ${zip_url}')
assert download_res == 0
assert os.is_file(zip_name)
println('> Zip file size: ${os.file_size(zip_name)} bytes.')

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
