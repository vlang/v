import os
import v.gen.native
import v.pref

fn test_macho() {
	os.chdir(os.temp_dir()) or {}
	mut g := native.macho_test_new_gen(&pref.Preferences{
		arch: .amd64
	}, 'test.bin')
	g.generate_macho_header()
	g.generate_macho_footer()
}
