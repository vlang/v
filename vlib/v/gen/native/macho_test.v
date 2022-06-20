import os
import v.gen.native
import v.pref
import v.ast

fn test_macho() {
	os.chdir(os.temp_dir()) or {}
	mut g := native.Gen{
		pref: &pref.Preferences{
			arch: .amd64
		}
		out_name: 'test.bin'
		table: ast.new_table()
		code_gen: native.Amd64{
			g: 0
		}
		labels: 0
	}
	g.generate_macho_header()
	g.generate_macho_footer()
}
