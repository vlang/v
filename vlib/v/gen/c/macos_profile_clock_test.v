module c

import v.pref

fn test_profile_native_declarations_keep_libsystem_only_for_hosted_or_arm64() {
	for arch in ['amd64', 'arm64'] {
		for freestanding in [false, true] {
			mut g := FlatGen.new()
			g.target = pref.target_from('macos', arch) or { panic(err) }
			g.set_profile('-', false, [])
			if freestanding {
				g.set_compile_values({
					'freestanding': '1'
				})
			}
			g.gen_profile_native_declarations()
			code := g.sb.str()
			if freestanding && arch == 'amd64' {
				assert code == ''
			} else {
				assert code.contains('mach_absolute_time(void)')
				assert code.contains('mach_timebase_info(mach_timebase_info_data_t*)')
			}
		}
	}
}
