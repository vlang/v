module builder

import os
import v.ast
import v.pref

fn test_ensure_imported_coroutines_runtime_downloads_photonwrapper() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_coroutines_runtime')
	os.rmdir_all(test_root) or {}
	defer {
		os.rmdir_all(test_root) or {}
	}
	fake_vexe := os.join_path(test_root, 'v')
	os.mkdir_all(test_root) or { panic(err) }
	os.write_file(fake_vexe, '#!/bin/sh
if [ "\$1" = "download" ] && [ "\$2" = "-o" ]; then
	mkdir -p "\$(dirname "\$3")"
	: > "\$3"
	exit 0
fi
exit 1
') or {
		panic(err)
	}
	os.chmod(fake_vexe, 0o700) or { panic(err) }
	old_vexe := os.getenv('VEXE')
	old_dyld_paths := os.getenv('DYLD_FALLBACK_LIBRARY_PATH')
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		if old_dyld_paths == '' {
			os.unsetenv('DYLD_FALLBACK_LIBRARY_PATH')
		} else {
			os.setenv('DYLD_FALLBACK_LIBRARY_PATH', old_dyld_paths, true)
		}
	}
	os.setenv('VEXE', fake_vexe, true)
	mut table := ast.new_table()
	table.imports << 'coroutines'
	mut b := Builder{
		pref:  &pref.Preferences{}
		table: table
	}
	b.ensure_imported_coroutines_runtime() or { panic(err) }
	assert os.exists(os.join_path(test_root, 'thirdparty', 'photon', 'photonwrapper.so'))
}
