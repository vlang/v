module c

import os
import v.cmdexec

// OpenBSD's <netdb.h> orders `struct addrinfo` like glibc and musl (`ai_addr` before
// `ai_canonname`), unlike Apple, FreeBSD, NetBSD and DragonFly, and defines EAI_SYSTEM
// as -11, like glibc. With the other BSDs' layout, `net` read every resolved address
// from the canonical name pointer.
fn test_headerless_addrinfo_follows_each_platform_netdb_h() {
	$if !macos && !linux {
		return
	}
	mut g := FlatGen.new()
	g.headerless_addrinfo_struct()
	code := g.sb.str()
	addr_first := 'unsigned int ai_addrlen; void* ai_addr; char* ai_canonname;'
	canonname_first := 'unsigned int ai_addrlen; char* ai_canonname; void* ai_addr;'
	for macro, order in {
		'__OpenBSD__':   addr_first
		'__linux__':     addr_first
		'__APPLE__':     canonname_first
		'__FreeBSD__':   canonname_first
		'__NetBSD__':    canonname_first
		'__DragonFly__': canonname_first
	} {
		preprocessed := preprocess_netdb_test_code(code, macro)
		assert preprocessed.contains(order), '${macro}: ${preprocessed}'
	}
}

fn test_headerless_eai_system_follows_each_platform_netdb_h() {
	$if !macos && !linux {
		return
	}
	mut g := FlatGen.new()
	g.headerless_platform_constants()
	code := g.sb.str() + '\nint eai_system = EAI_SYSTEM;\n'
	for macro, value in {
		'__OpenBSD__':   '-11'
		'__linux__':     '-11'
		'__APPLE__':     '11'
		'__FreeBSD__':   '11'
		'__NetBSD__':    '11'
		'__DragonFly__': '11'
	} {
		preprocessed := preprocess_netdb_test_code(code, macro)
		assert preprocessed.contains('int eai_system = ${value};'), '${macro}: ${preprocessed}'
	}
}

fn preprocess_netdb_test_code(code string, macro string) string {
	root := os.join_path(os.vtmp_dir(), 'openbsd_netdb_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'netdb.c')
	os.write_file(source, code) or { panic(err) }
	preprocessed := cmdexec.run('cc', ['-E', '-P', '-undef', '-D${macro}', source])
	assert preprocessed.exit_code == 0, preprocessed.output
	return preprocessed.output
}
