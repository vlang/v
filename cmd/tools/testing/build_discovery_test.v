module testing

import os
import v.pref

fn test_build_discovery_recognizes_modules_after_complete_comment_headers() {
	header := '// ${'explanation '.repeat(100)}\n'
	for source in [
		'fn main() {}',
		"println('module helper')",
		'import mymodules.submodule { value }\nfn main() {}',
		'module main\nfn main() {}',
		'module\tmain // program\nfn main() {}',
		header + 'module main\nfn main() {}',
		'// module helper is imported below\nmodule main\nfn main() {}',
		'/* module helper */\nmodule no_main\n',
		'#!/usr/bin/env v\nmodule main\nfn main() {}',
		'@[has_globals]\nmodule main\nfn main() {}',
		'@[translated]\nmodule main\nfn main() {}',
		'[has_globals]\nmodule main\nfn main() {}',
	] {
		assert build_source_is_program(source), source
	}
	for source in [
		'module helper\npub fn value() int { return 1 }',
		header + 'module helper\npub fn value() int { return 1 }',
		'/* ${'module main\n'.repeat(100)} */\nmodule helper\n',
		'module\thelper // support code\n',
		'module main_helper\n',
		'module main.helper\n',
		'#!/usr/bin/env v\nmodule helper\n',
		'@[has_globals]\nmodule helper\n',
		'@[translated]\nmodule helper\n',
		'[has_globals]\nmodule helper\n',
	] {
		assert !build_source_is_program(source), source
	}
	assert build_source_is_program('module {'), 'malformed sources must reach the compiler'
}

fn test_build_discovery_recognizes_modules_after_directive_prefixes() {
	for prefix in [
		'#flag -lm\n',
		'#include <stdio.h>\n',
		'#define VALUE 1\n',
		'#define COMMENT "module helper"\n',
		'#include "module main.h"\n',
		'#flag -lm\n#define COMMENT "module main"\n#include <stdio.h>\n',
		'#!/usr/bin/env v\n#flag -lm\n@[has_globals]\n',
	] {
		assert build_source_is_program(prefix + 'fn main() {}'), prefix
		assert build_source_is_program(prefix + 'module main\nfn main() {}'), prefix
		assert build_source_is_program(prefix + 'module no_main\n'), prefix
		assert !build_source_is_program(prefix + 'module helper\n'), prefix
		assert build_source_is_program(prefix + 'module {'), 'malformed sources must reach the compiler'
	}
}

fn test_prepare_build_session_keeps_grpc_programs_and_project_folder_selection() {
	root := os.dir(pref.vexe_path())
	project := os.real_path(os.join_path(root, 'examples', 'viewer')).replace('\\', '/')
	mut session := prepare_test_session('', 'examples', [project], 'Discovering example programs')
	defer {
		os.rmdir_all(session.vtmp_dir) or {}
	}
	for program in ['client.v', 'server.v'] {
		path := os.join_path(root, 'examples', 'grpc', program).replace('\\', '/')
		assert path in session.files, path
	}
	for program in ['log.v', 'submodule/main.v'] {
		path := os.join_path(root, 'examples', program).replace('\\', '/')
		assert path in session.files, path
	}
	for support in ['codec.v', 'codec_test.v', 'service.v'] {
		path := os.join_path(root, 'examples', 'grpc', 'kv', support).replace('\\', '/')
		assert path !in session.files, path
	}
	assert session.files.all(!it.starts_with(project + '/'))
	session.add(project)
	assert project in session.files
}
