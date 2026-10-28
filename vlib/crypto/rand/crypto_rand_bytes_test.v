import crypto.rand
import os

fn assert_random_bytes(bytes []u8) {
	mut histogram := [256]int{}
	for b in bytes {
		histogram[b]++
	}
	for h in histogram {
		assert h < 10
	}
}

fn test_bytes() {
	bytes := rand.bytes(32)!
	assert bytes.len == 32
	assert_random_bytes(bytes)
}

fn test_bytes_rejects_negative_length() {
	if _ := rand.bytes(-1) {
		assert false, 'expected rand.bytes(-1) to fail'
	} else {
		assert err.msg() == 'can not read < 0 random bytes'
	}
}

fn test_read() {
	mut buffer := []u8{len: 32}
	rand.read(mut buffer)!
	assert buffer.len == 32
	assert_random_bytes(buffer)
}

fn test_read_large_buffer() {
	mut buffer := []u8{len: 512}
	rand.read(mut buffer)!
	assert buffer[..256] != []u8{len: 256}
	assert buffer[256..] != []u8{len: 256}
}

// Removing the Android backend must not silently select the unsupported fallback.
// Calling bionic's API-28 getrandom wrapper instead of the syscall must fail even
// on hosts without an NDK. Only the syscall boundary is substituted below.
fn test_android_read_uses_raw_getrandom_without_api28_libc() {
	root := os.join_path(os.vtmp_dir(), 'crypto_rand_android_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	c_file := os.join_path(root, 'main.c')
	os.write_file(source,
		'import crypto.rand\nfn main() {\n\tmut b := []u8{len: 513}\n\trand.read(mut b)!\n}\n')!
	generated := os.exec([@VEXE, '-nocache', '-gc', 'none', '-cross', '-os', 'android', '-arch',
		'arm64', '-o', c_file, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(c_file)!
	assert !code.contains('crypto.rand.read is not implemented on this platform'), 'Android crypto.rand.read selected the unsupported backend'
	read_body := android_random_read_definition(code)
	assert read_body.contains('v_crypto_getrandom('), 'Android random reads do not call the getrandom helper: ${read_body}'
	helper := android_random_inserted_helper(code)
	cc := os.find_abs_path_of_executable('cc') or {
		eprintln('skipping Android getrandom helper execution: no host C compiler (cc)')
		return
	}
	// Model pre-28 bionic headers, not a host's getrandom libc implementation.
	// A deliberately non-ABI syscall token catches hardcoded syscall numbers.
	os.mkdir_all(os.join_path(root, 'sys'))!
	os.write_file(os.join_path(root, 'sys', 'syscall.h'), '#define SYS_getrandom 1234567\n')!
	os.write_file(os.join_path(root, 'sys', 'random.h'),
		'#error getrandom libc wrapper requires Android API 28\n')!
	os.write_file(os.join_path(root, 'unistd.h'), 'long syscall(long number, ...);\n')!
	probe := os.join_path(root, 'probe.c')
	exe := os.join_path(root, 'probe')
	os.write_file(probe, android_random_syscall_probe + '\n' + helper +
		'
int main(void) {
	unsigned char buffer[8] = {0};
	assert(v_crypto_getrandom(buffer, sizeof(buffer)) == 3);
	assert(buffer[0] == 0x42 && buffer[1] == 0x42 && buffer[2] == 0x42);
	assert(buffer[3] == 0);
	assert(v_crypto_getrandom(buffer, sizeof(buffer)) == -1);
	assert(errno == ENOSYS);
	assert(android_syscall_count == 2);
	puts("android getrandom syscall contract ok");
	return 0;
}
')!
	compiled := os.exec([cc, '-std=c99', '-Werror=implicit-function-declaration', '-I', root, '-o',
		exe, probe])
	assert compiled.exit_code == 0, compiled.output
	executed := os.exec([exe])
	assert executed.exit_code == 0, executed.output
	assert executed.output.trim_space() == 'android getrandom syscall contract ok', executed.output
}

// Locate the read definition without depending on the generated C's type spelling.
fn android_random_read_definition(code string) string {
	mut lines := []string{}
	mut depth := 0
	for line in code.split_into_lines() {
		if lines.len == 0 {
			if !line.contains(' internal__read(') || !line.trim_space().ends_with('{') {
				continue
			}
		}
		lines << line
		depth += line.count('{') - line.count('}')
		if depth == 0 {
			return lines.join('\n')
		}
	}
	panic('Android C has no complete random read definition')
}

// Compile the helper actually inserted into Android output, including its guards.
fn android_random_inserted_helper(code string) string {
	marker := '#ifndef V_CRYPTO_RAND_GETRANDOM_LINUX_H'
	start := code.index(marker) or { panic('Android C has no inserted getrandom helper') }
	mut depth := 0
	mut lines := []string{}
	for line in code[start..].split_into_lines() {
		lines << line
		if line.starts_with('#if') {
			depth++
		} else if line.starts_with('#endif') {
			depth--
			if depth == 0 {
				return lines.join('\n')
			}
		}
	}
	panic('unterminated getrandom helper in Android C')
}

const android_random_syscall_probe = '
#include <assert.h>
#include <errno.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/syscall.h>
#ifndef __linux__
#define __linux__ 1
#endif
#define __ANDROID__ 1
#define __ANDROID_API__ 24
static int android_syscall_count;
long syscall(long number, ...) {
	va_list args;
	va_start(args, number);
	void *buffer = va_arg(args, void *);
	size_t size = va_arg(args, size_t);
	int flags = va_arg(args, int);
	va_end(args);
	assert(number == SYS_getrandom && size == 8 && flags == 0);
	if (++android_syscall_count == 1) {
		memset(buffer, 0x42, 3);
		return 3;
	}
	errno = ENOSYS;
	return -1;
}
'
