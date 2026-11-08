module driver

import os
import time
import v.gen.c as cgen
import v.pref

fn prescan_prefs(root string) &pref.Preferences {
	return &pref.Preferences{
		vroot:               @VEXEROOT
		backend:             'c'
		target:              pref.host_target()
		module_search_paths: [root]
	}
}

fn prescan_project(name string) string {
	root := os.join_path(os.vtmp_dir(), 'native_inputs_prescan_${name}_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(os.join_path(root, 'native'), mode: 0o755) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'prescan' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'import os\nimport native\n\nfn main() {\n\tprintln(native.value() + os.args.len)\n}\n') or {
		panic(err)
	}
	return root
}

fn test_prescan_finds_a_native_header_of_an_imported_project_module() {
	root := prescan_project('header')
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native', 'native.h'), '#define NATIVE_VALUE 1\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'native', 'native.v'), 'module native\n\n#include "@VMODROOT/native/native.h"\n\npub fn value() int {\n\treturn 1\n}\n') or {
		panic(err)
	}
	assert v3_program_supplies_native_inputs(os.join_path(root, 'main.v'), []string{},
		prescan_prefs(root), []string{}, false)
	// The native bridge of a TinyCC dynamic library tracks the header itself.
	assert !v3_program_supplies_native_inputs(os.join_path(root, 'main.v'), []string{},
		prescan_prefs(root), []string{}, true)
}

fn test_prescan_keeps_the_cache_for_programs_without_native_inputs() {
	root := prescan_project('plain')
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native', 'native.v'), 'module native\n\n#include <stdio.h>\n\npub fn value() int {\n\treturn 1\n}\n') or {
		panic(err)
	}
	assert !v3_program_supplies_native_inputs(os.join_path(root, 'main.v'), []string{},
		prescan_prefs(root), []string{}, false)
	// A user search path in the C flags of the build is a native input too.
	assert v3_program_supplies_native_inputs(os.join_path(root, 'main.v'), []string{},
		prescan_prefs(root), ['-I', root], false)
}

fn test_source_user_native_input_classifies_directives_like_the_cache() {
	target := pref.host_target()
	file := '/project/main.v'
	no_values := map[string]string{}
	assert cgen.source_user_native_input(file, '#include <stdio.h>\n', @VEXEROOT, target,
		[]string{}, no_values) == none
	assert cgen.source_user_native_input(file, '#include "@VEXEROOT/thirdparty/stdatomic/nix/atomic.h"\n',
		@VEXEROOT, target, []string{}, no_values) == none
	assert cgen.source_user_native_input(file, '\t#include "native.h"\n', @VEXEROOT, target,
		[]string{}, no_values) or { '' } == '"native.h"'
	assert cgen.source_user_native_input(file, '#pkgconfig sdl2\n', @VEXEROOT, target,
		[]string{}, no_values) or { '' } == '#pkgconfig sdl2'
	assert cgen.source_user_native_input(file, '#flag -I /project/include\n', @VEXEROOT, target,
		[]string{}, no_values) or { '' } == '/project/include'
	assert cgen.source_user_native_input(file, '#flag -lm\n', @VEXEROOT, target, []string{},
		no_values) == none
	assert cgen.source_user_native_input(file, 'fn main() {\n\ts := "#include \\"x.h\\""\n}\n',
		@VEXEROOT, target, []string{}, no_values) == none
}
