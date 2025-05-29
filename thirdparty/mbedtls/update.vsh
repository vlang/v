// This script can be used to update the thirdparty/mbedtls/ folder to a future version of mbedtls.
import os

fn do(cmd string) {
	println(cmd)
	res := os.system(cmd)
	if res != 0 {
		panic('failed at: `${cmd}`')
	}
}

fn fix_config(config_file string) ! {
	mut content := os.read_file(config_file)!
	content += '\n
#if defined(__TINYC__)
#undef MBEDTLS_AESNI_C

#if defined(__arm__) || defined(__aarch64__)
#undef MBEDTLS_HAVE_ASM
#else
#define MBEDTLS_HAVE_ASM
#endif
#else // __TINYC__
#if defined(__x86_64__)
#define MBEDTLS_AESNI_C
#else
#undef MBEDTLS_AESNI_C
#endif
#endif // __TINYC__

#if ( defined(__linux__) || defined(__FreeBSD__) ) || defined (__OpenBSD__)
#define MBEDTLS_THREADING_PTHREAD
#define MBEDTLS_THREADING_C
#else
#undef MBEDTLS_THREADING_PTHREAD
#undef MBEDTLS_THREADING_C
#endif
'
	os.write_file(config_file, content)!
}

os.chdir(@VEXEROOT)!

do('rm -rf mbedtls/')
do('git clone --depth=1 --recursive --shallow-submodules --single-branch -b v3.6.3.1 https://github.com/Mbed-TLS/mbedtls.git mbedtls/')
do('rsync -a --delete mbedtls/include/  thirdparty/mbedtls/include/')
do('rsync -a --delete mbedtls/library/  thirdparty/mbedtls/library/')
do('rsync -a --delete mbedtls/3rdparty/ thirdparty/mbedtls/3rdparty/')
do('rsync -a          mbedtls/LICENSE   thirdparty/mbedtls/LICENSE')
do('rsync -a          mbedtls/README.md thirdparty/mbedtls/README.md')
do("find thirdparty/mbedtls/ -name '*.txt' -or -name '*.inc' -or -name '.gitignore' -or -name 'Makefile' |xargs rm -f")
do('rm -rf mbedtls/')
fix_config('thirdparty/mbedtls/include/mbedtls/mbedtls_config.h')!
