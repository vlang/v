module c

import v.pref

fn test_bare_panic_frames_follow_target_and_reachability() {
	mut g := FlatGen.new()
	g.set_compile_defines(['freestanding'])
	for os_name in ['linux', 'macos', 'darwin'] {
		for arch in ['amd64', 'arm64'] {
			g.set_target(pref.Target{ os: os_name, arch: arch })
			g.set_program_uses_recover(false)
			assert !g.program_uses_recover()
			g.set_program_uses_recover(true)
			assert g.program_uses_recover()
		}
	}
	g.set_target(pref.Target{ os: 'linux', arch: 'i386' })
	assert !g.program_uses_recover()
	g.set_target(pref.Target{ os: 'vinix', arch: 'amd64' })
	assert !g.program_uses_recover()
}
