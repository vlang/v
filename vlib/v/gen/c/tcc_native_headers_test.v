module c

import os
import v.cmdexec
import v.flat
import v.pref

fn test_tcc_native_epoll_headers_preserve_kernel_event_layout() ! {
	$if !linux {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'tcc_native_epoll_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	configuration := os.join_path(dir, 'features.h')
	os.write_file(configuration, '#define _GNU_SOURCE 1\n')!
	for mode in ['plain', 'preinclude', 'lifted_define', 'guarded_define'] {
		mut g := FlatGen.new()
		g.a = &flat.FlatAst{}
		g.set_target(pref.target_from('linux', 'amd64')!)
		match mode {
			'preinclude' {
				g.preinclude_directives = ['#include "${configuration}"']
			}
			'lifted_define' {
				g.add_c_directive('main', '#define _GNU_SOURCE 1', false)
			}
			'guarded_define' {
				g.add_c_directive('main', '#if 0\n#include <sys/epoll.h>\n#endif', false)
				g.add_c_directive('main', '#if 1\n#define _GNU_SOURCE 1\n#include <sys/epoll.h>\n#endif', false)
			}
			else {}
		}
		g.add_c_directive('main', '#include <sys/epoll.h>', false)
		g.add_c_directive('main', '#include <sys/eventfd.h>', false)
		g.emit_translation_unit_include_directives()
		source := os.join_path(dir, '${mode}.c')
		// Two ready events expose both data-field alignment and array stride. Keep
		// the high bits nonzero so a truncated or shifted connection ID cannot pass.
		os.write_file(source, g.sb.str() + '
#include <assert.h>
#include <stdint.h>
#include <unistd.h>

#if defined(EXPECT_GNU) && defined(__GLIBC__) && !defined(__USE_GNU)
#error libc features were selected before the configuration macros
#endif

int main(void) {
	const uint64_t keys[2] = {UINT64_C(0x0123456789abcdef), UINT64_C(0xfedcba9876543210)};
	int ep = epoll_create1(EPOLL_CLOEXEC);
	int fds[2] = {eventfd(1, EFD_CLOEXEC | EFD_NONBLOCK), eventfd(1, EFD_CLOEXEC | EFD_NONBLOCK)};
	assert(ep >= 0 && fds[0] >= 0 && fds[1] >= 0);
	for (int i = 0; i < 2; ++i) {
		struct epoll_event event = {0};
		event.events = EPOLLIN;
		event.data.u64 = keys[i];
		assert(epoll_ctl(ep, EPOLL_CTL_ADD, fds[i], &event) == 0);
	}
	struct epoll_event events[2] = {0};
	assert(epoll_wait(ep, events, 2, 0) == 2);
	unsigned int seen = 0;
	for (int i = 0; i < 2; ++i) {
		assert(events[i].events & EPOLLIN);
		if (events[i].data.u64 == keys[0]) seen |= 1;
		if (events[i].data.u64 == keys[1]) seen |= 2;
	}
	assert(seen == 3);
	close(fds[0]);
	close(fds[1]);
	close(ep);
	return 0;
}
')!
		mut args := []string{}
		if mode != 'plain' {
			args << '-DEXPECT_GNU'
		}
		args << ['-run', source]
		result := cmdexec.run(tcc, args)
		assert result.exit_code == 0, '${mode}: ${result.output}'
	}
}

fn test_tcc_target_owned_headers_do_not_require_host_libc_headers() ! {
	$if !linux {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'tcc_target_owned_headers_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	os.write_file(os.join_path(dir, 'project.h'), '#define PROJECT_VALUE 7\n')!
	mut g := FlatGen.new()
	g.a = &flat.FlatAst{}
	g.set_target(pref.target_from('linux', 'amd64')!)
	g.set_target_libc_headers(true)
	g.add_c_directive('main', '#include <project.h>', false)
	g.emit_translation_unit_include_directives()
	source := os.join_path(dir, 'project.c')
	os.write_file(source, g.sb.str() + 'int project_value = PROJECT_VALUE;\n')!
	result := cmdexec.run(tcc, ['-nostdinc', '-I', dir, '-c', source, '-o',
		os.join_path(dir, 'project.o')])
	assert result.exit_code == 0, result.output
}
