import v.gen.native
import v.pref
import v.ast

fn test_macho() {
	mut g := native.Gen{
		pref: &pref.Preferences{}
		out_name: 'test.bin'
		table: &ast.Table{
			cur_fn: 0
		}
	}
	g.generate_macho_header()
	g.generate_macho_footer()
}
