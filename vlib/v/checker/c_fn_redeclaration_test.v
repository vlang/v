import os

const c_fn_redeclaration_vexe = os.quoted_path(@VEXE)

fn write_c_fn_redeclaration_project(name string, files map[string]string) !string {
	root := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	for relative_path, source in files {
		path := os.join_path(root, relative_path)
		os.mkdir_all(os.dir(path))!
		os.write_file(path, source)!
	}
	return root
}

fn test_conflicting_c_fn_redeclarations_across_modules_are_reported_at_declaration() {
	root := write_c_fn_redeclaration_project('c_fn_conflicting_redeclaration', {
		'v.mod':       "Module { name: 'c_fn_conflicting_redeclaration' }\n"
		'moda/moda.v': 'module moda\n\nfn C.getpid() int\n\npub fn pid() int {\n\treturn C.getpid()\n}\n'
		'modb/modb.v': 'module modb\n\nfn C.getpid() u64\n\npub fn pid() u64 {\n\treturn C.getpid()\n}\n'
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tprintln(moda.pid())\n\tprintln(modb.pid())\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -check ${os.quoted_path(root)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('moda/moda.v:3:') || result.output.contains('modb/modb.v:3:')
	assert result.output.contains('C function `C.getpid` was already declared with a different signature')
	assert !result.output.contains('cannot use `u64` as `int`'), result.output
}

fn test_compatible_c_fn_redeclarations_across_modules_are_accepted() {
	root := write_c_fn_redeclaration_project('c_fn_compatible_redeclaration', {
		'v.mod':       "Module { name: 'c_fn_compatible_redeclaration' }\n"
		'moda/moda.v': 'module moda\n\nstruct C.Display {}\n\nfn C.compat_probe(value int, data byteptr, display voidptr, mut state usize) int\n\npub fn touch() {\n\tmut state := usize(0)\n\tC.compat_probe(0, unsafe { nil }, unsafe { nil }, mut state)\n}\n'
		'modb/modb.v': 'module modb\n\nstruct C.Display {}\n\ntype CInt = i32\n\nfn C.compat_probe(value CInt, data &u8, display &C.Display, state &usize) i32\n\npub fn touch() {\n\tmut state := usize(0)\n\tC.compat_probe(0, unsafe { nil }, unsafe { nil }, &state)\n}\n'
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tmoda.touch()\n\tmodb.touch()\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_module_local_c_fn_signatures_are_used_during_codegen() {
	root := write_c_fn_redeclaration_project('c_fn_module_local_codegen', {
		'v.mod':          "Module { name: 'c_fn_module_local_codegen' }\n"
		'local_widget.h': '#ifndef LOCAL_WIDGET_H\n#define LOCAL_WIDGET_H\n\ntypedef struct Widget {\n\tint value;\n} Widget;\n\nstatic Widget local_widget = { 21 };\n\nstatic void *local_make(void) {\n\treturn &local_widget;\n}\n\nstatic int local_value(void *widget) {\n\treturn ((Widget *)widget)->value;\n}\n\n#endif\n'
		'moda/moda.c.v':  'module moda\n\n#include "@VMODROOT/local_widget.h"\n\nfn C.local_make() voidptr\nfn C.local_value(widget voidptr) int\n\npub fn raw_value() int {\n\treturn C.local_value(C.local_make())\n}\n'
		'modb/modb.c.v':  'module modb\n\n#include "@VMODROOT/local_widget.h"\n\nstruct C.Widget {\n\tvalue int\n}\n\nfn C.local_make() &C.Widget\nfn C.local_value(widget &C.Widget) int\n\npub fn typed_value() int {\n\treturn C.local_make().value + C.local_value(C.local_make())\n}\n'
		'main.v':         'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tassert moda.raw_value() == 21\n\tassert modb.typed_value() == 42\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	executable := os.join_path(root, 'module_local_codegen')
	build_result :=
		os.execute('${c_fn_redeclaration_vexe} -cstrict -o ${os.quoted_path(executable)} ${os.quoted_path(root)}')
	assert build_result.exit_code == 0, build_result.output
	run_result := os.execute(os.quoted_path(executable))
	assert run_result.exit_code == 0, run_result.output
}

fn test_fixed_and_variadic_c_fn_redeclarations_conflict() {
	root := write_c_fn_redeclaration_project('c_fn_fixed_variadic_redeclaration', {
		'v.mod':       "Module { name: 'c_fn_fixed_variadic_redeclaration' }\n"
		'moda/moda.v': 'module moda\n\nfn C.variadic_probe(value int, weight f32) int\n\npub fn touch() {}\n'
		'modb/modb.v': 'module modb\n\nfn C.variadic_probe(value int, ...) int\n\npub fn touch() {}\n'
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tmoda.touch()\n\tmodb.touch()\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -check ${os.quoted_path(root)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('C function `C.variadic_probe` was already declared with a different signature'), result.output
}

fn test_gg_and_clipboard_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_gg_clipboard_redeclaration', {
		'main.v': 'module main\n\nimport gg as _\nimport clipboard as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -os linux -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_clipboard_and_x11_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_clipboard_x11_redeclaration', {
		'main.v': 'module main\n\nimport clipboard as _\nimport x.x11 as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -os linux -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_ecdsa_mldsa_and_slhdsa_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_ecdsa_mldsa_slhdsa_redeclaration', {
		'main.v': 'module main\n\nimport crypto.ecdsa as _\nimport x.crypto.mldsa as _\nimport x.crypto.slhdsa as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_fasthttp_and_net_windows_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_fasthttp_net_windows_redeclaration', {
		'main.v': 'module main\n\nimport fasthttp as _\nimport net as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -os windows -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_fasthttp_and_veb_linux_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_fasthttp_veb_linux_redeclaration', {
		'main.v': 'module main\n\nimport fasthttp as _\nimport veb as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -os linux -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_fasthttp_and_veb_freebsd_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_fasthttp_veb_freebsd_redeclaration', {
		'main.v': 'module main\n\nimport fasthttp as _\nimport veb as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -os freebsd -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_os_and_posix_size_t_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_os_posix_size_t_redeclaration', {
		'main.v': 'module main\n\nimport os\n\nfn C.readlink(path &char, buf &char, size usize) int\nfn C.gethostname(name &char, size u64) int\n\nfn main() {\n\t_ = os.args\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -os vinix -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_pointer_sized_c_fn_redeclarations_respect_target_width() {
	root := write_c_fn_redeclaration_project('c_fn_pointer_sized_target_width', {
		'v.mod':       "Module { name: 'c_fn_pointer_sized_target_width' }\n"
		'moda/moda.v': 'module moda\n\nfn C.size_probe(size usize) isize\n\npub fn touch() {}\n'
		'modb/modb.v': 'module modb\n\nfn C.size_probe(size u64) i64\n\npub fn touch() {}\n'
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tmoda.touch()\n\tmodb.touch()\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result_64 := os.execute('${c_fn_redeclaration_vexe} -m64 -check ${os.quoted_path(root)}')
	assert result_64.exit_code == 0, result_64.output
	result_32 := os.execute('${c_fn_redeclaration_vexe} -m32 -check ${os.quoted_path(root)}')
	assert result_32.exit_code != 0, result_32.output
	assert result_32.output.contains('C function `C.size_probe` was already declared with a different signature'), result_32.output
}

fn test_trace_calls_and_os_pthread_self_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_trace_calls_os_redeclaration', {
		'main.v': 'module main\n\nimport os\n\nfn main() {\n\t_ := os.args\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -trace-calls -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_picoev_and_os_notify_macos_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_picoev_os_notify_macos_redeclaration', {
		'main.v': 'module main\n\nimport os.notify as _\nimport picoev as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -os macos -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}
