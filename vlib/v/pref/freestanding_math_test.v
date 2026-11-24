module pref

import os

fn test_freestanding_c_uses_portable_math_only_in_the_standard_library() {
	root := os.join_path(os.temp_dir(), 'v_math_source_selection')
	math_dir := os.join_path(root, 'vlib', 'math')
	portable := os.join_path(math_dir, 'exp.v')
	hosted := os.join_path(math_dir, 'exp.c.v')
	bits := os.join_path(math_dir, 'bits', 'bits.c.v')
	user := os.join_path(root, 'user', 'math', 'exp.c.v')
	files := [portable, hosted, bits, user]
	for target_os in ['linux', 'macos'] {
		for arch in ['amd64', 'arm64'] {
			if target_os == 'macos' && arch == 'arm64' { continue }
			p := Preferences{
				vroot:        root
				target:       Target{ os: target_os, arch: arch }
				backend:      'c'
				user_defines: ['freestanding']
			}
			assert p.without_excluded(files) == [portable, bits, user]
		}
	}
}

fn test_hosted_and_other_targets_keep_their_c_math_overrides() {
	root := os.join_path(os.temp_dir(), 'v_math_source_selection')
	files := [os.join_path(root, 'vlib', 'math', 'exp.v'),
		os.join_path(root, 'vlib', 'math', 'exp.c.v')]
	for target_os in ['linux', 'macos', 'windows'] {
		for arch in ['amd64', 'arm64'] {
			for backend in ['c', 'native', 'js'] {
				for defines in [[]string{}, ['freestanding']] {
					if backend == 'c' && defines.len > 0 && (target_os == 'linux' || (target_os == 'macos' && arch == 'amd64')) {
						continue
					}
					p := Preferences{ vroot: root, target: Target{ os: target_os, arch: arch }, backend: backend, user_defines: defines }
					assert p.without_excluded(files) == files
				}
			}
		}
	}
}

fn test_freestanding_math_selection_keeps_explicit_exclusions() {
	root := os.join_path(os.temp_dir(), 'v_math_source_selection')
	math_dir := os.join_path(root, 'vlib', 'math')
	portable := os.join_path(math_dir, 'exp.v')
	p := Preferences{
		vroot:        root
		target:       Target{ os: 'linux', arch: 'amd64' }
		backend:      'c'
		user_defines: ['freestanding']
		exclude:      [portable]
	}
	assert p.without_excluded([portable, os.join_path(math_dir, 'exp.c.v')]) == []string{}
}
