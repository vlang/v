module c

// Darwin defines jmp_buf as an int array. Keep the SDK layout while declaring
// the actual functions from V, including their control-flow attributes.
fn (mut g FlatGen) jump_buffer_declarations() {
	if g.target.os != 'macos' || g.target_libc_headers || g.output_cross_c
		|| g.c_directives_use_system_libc() {
		g.writeln('#include <setjmp.h>')
		return
	}
	// Declaration workers use another builder. C's guard belongs to the emitted
	// unit, whereas the shared native-name index can describe a different builder.
	g.writeln('#ifndef V_HEADERLESS_DARWIN_JMP_BUF')
	g.writeln('#define V_HEADERLESS_DARWIN_JMP_BUF 1')
	g.writeln('#if defined(__x86_64__)')
	g.writeln('typedef int jmp_buf[37];')
	g.writeln('#elif defined(__i386__)')
	g.writeln('typedef int jmp_buf[18];')
	g.writeln('#elif defined(__arm64__) || defined(__aarch64__) || defined(__ARM_ARCH_7K__)')
	g.writeln('typedef int jmp_buf[48];')
	g.writeln('#elif defined(__arm__)')
	g.writeln('typedef int jmp_buf[28];')
	g.writeln('#else')
	g.writeln('#error unsupported Darwin jump buffer architecture')
	g.writeln('#endif')
	g.emit_c_runtime_declarations_with_replay(['setjmp', 'longjmp', '_setjmp', '_longjmp'], true)
	g.writeln('#endif')
}

// V's libc bindings and platform preamble supply these declarations. Native
// implementations still include their own SDK headers when their objects build.
fn (g &FlatGen) headerless_vlib_include(include_arg string, source_file string) bool {
	return g.target.os == 'macos' && !g.output_cross_c && !g.target_libc_headers
		&& g.c_source_file_is_in_vlib(source_file)
		&& include_arg in ['<assert.h>', '<ctype.h>', '<errno.h>', '<fcntl.h>', '<float.h>',
			'<inttypes.h>', '<limits.h>', '<math.h>', '<pthread.h>', '<signal.h>', '<stdbool.h>',
			'<stddef.h>', '<stdint.h>', '<stdio.h>', '<stdlib.h>', '<string.h>', '<strings.h>',
			'<time.h>', '<unistd.h>', '<wchar.h>', '<arpa/inet.h>', '<netdb.h>', '<netinet/in.h>',
			'<netinet/tcp.h>', '<sys/select.h>', '<sys/socket.h>', '<sys/mman.h>', '<sys/resource.h>',
			'<sys/stat.h>', '<sys/time.h>', '<sys/types.h>', '<sys/random.h>', '<sys/uio.h>',
			'<sys/un.h>', '<sys/utsname.h>', '<sys/wait.h>', '<termios.h>', '<poll.h>', '<sys/poll.h>',
			'<sys/event.h>', '<CommonCrypto/CommonDigest.h>']
}
