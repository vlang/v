import os
import v.gen.native
import v.pref
import log

fn test_macho() {
	log.info('start')
	os.chdir(os.vtmp_dir()) or {}
	log.info('chdir')
	mut g := native.macho_test_new_gen(&pref.Preferences{
		arch: .amd64
	}, 'test.bin')
	log.info('native.macho_test_new_gen')
	g.generate_macho_header()
	log.info('g.generate_macho_header')
	g.generate_macho_footer()
	log.info('g.generate_macho_footer')
}
