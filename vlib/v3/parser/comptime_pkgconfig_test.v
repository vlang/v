module parser

import os
import v3.pref

fn test_comptime_pkgconfig_function_keeps_dollar_prefix() {
	$if windows {
		// The deterministic probe below uses a POSIX shell helper.
		assert true
	} $else {
		root := os.join_path(os.temp_dir(), 'v3_comptime_pkgconfig_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		old_path := os.getenv('PATH')
		defer {
			os.setenv('PATH', old_path, true)
			os.rmdir_all(root) or {}
		}
		pkgconfig := os.join_path(root, 'pkg-config')
		os.write_file(pkgconfig,
			'#!/bin/sh\nif [ "$1" = "--exists" ] && [ "$2" = "present-package" ]; then\n\texit 0\nfi\nexit 1\n')!
		os.chmod(pkgconfig, 0o700)!
		os.setenv('PATH', root, true)

		prefs := pref.new_preferences()
		p := Parser.new(prefs)
		assert p.eval_comptime_cond("\$pkgconfig('present-package')")
		assert !p.eval_comptime_cond("\$pkgconfig('missing-package')")
	}
}
