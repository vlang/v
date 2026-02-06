// Use this script to update thirdparty/zstd to a future version of mbedtls.
import os

fn do(cmd string) {
	println(cmd)
	res := os.system(cmd)
	if res != 0 {
		panic('failed at: `${cmd}`')
	}
}

os.chdir(os.dir(@VEXE))!

version := '1.5.7'
do('rm -rf zstd-${version}*')
do('wget https://github.com/facebook/zstd/releases/download/v${version}/zstd-${version}.tar.gz')
do('tar -xf zstd-${version}.tar.gz')
do('pushd .; cd zstd-${version}/build/single_file_libs/; ./create_single_file_library.sh; popd')
do('cp zstd-${version}/build/single_file_libs/zstd.c thirdparty/zstd/zstd.c')
do('pushd .; cd thirdparty/zstd/; patch --verbose --unified -p0 --input zstd_v.patch; popd')
do('rm -rf zstd-${version}*')
println('DONE')
