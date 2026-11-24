module pref

import os

fn test_freestanding_linux_c_uses_portable_math_only_in_the_standard_library() {
	root := os.join_path(os.temp_dir(), 'v_math_source_selection')
	math_dir := os.join_path(root, 'vlib', 'math')
	portable := os.join_path(math_dir, 'exp.v')
	hosted := os.join_path(math_dir, 'exp.c.v')
	bits := os.join_path(math_dir, 'bits', 'bits.c.v')
	user := os.join_path(root, 'user', 'math', 'exp.c.v')
	files := [portable, hosted, bits, user]
	p := Preferences{
		vroot:        root
		target:       Target{ os: 'linux', arch: 'amd64' }
		backend:      'c'
		user_defines: ['freestanding']
	}
	assert p.without_excluded(files) == [portable, bits, user]
}

fn test_hosted_and_macos_math_sources_still_use_their_c_overrides() {
	root := os.join_path(os.temp_dir(), 'v_math_source_selection')
	files := [os.join_path(root, 'vlib', 'math', 'exp.v'),
		os.join_path(root, 'vlib', 'math', 'exp.c.v')]
	for target_os in ['linux', 'macos', 'windows'] {
		for defines in [[]string{}, ['freestanding']] {
			if target_os == 'linux' && defines.len > 0 { continue }
			p := Preferences{
				vroot:        root
				target:       Target{ os: target_os, arch: 'amd64' }
				backend:      'c'
				user_defines: defines
			}
			assert p.without_excluded(files) == files
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
