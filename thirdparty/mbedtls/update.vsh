// This script can be used to update the thirdparty/mbedtls/ folder to a future version of mbedtls.
import os

fn do(cmd string) {
	println(cmd)
	res := os.system(cmd)
	if res != 0 {
		panic('failed at: `${cmd}`')
	}
}

os.chdir(os.dir(@VEXE))!
do('rm -rf mbedtls/')
do('git clone --depth=1 --recursive --shallow-submodules --single-branch -b v3.3.0 https://github.com/Mbed-TLS/mbedtls.git mbedtls/')
do('rsync -a --delete mbedtls/include/  thirdparty/mbedtls/include/')
do('rsync -a --delete mbedtls/library/  thirdparty/mbedtls/library/')
do('rsync -a --delete mbedtls/3rdparty/ thirdparty/mbedtls/3rdparty/')
do('rsync -a          mbedtls/LICENSE   thirdparty/mbedtls/LICENSE')
do('rsync -a          mbedtls/README.md thirdparty/mbedtls/README.md')
do("find thirdparty/mbedtls/ -name '*.txt' -or -name '*.inc' -or -name '.gitignore' -or -name 'Makefile' |xargs rm -f")
do('rm -rf mbedtls/')
