import v.gen.x64
import v.pref
import v.table

fn test_macho() {
	mut g := x64.Gen{
		pref: &pref.Preferences{}
		out_name: 'test.bin'
		table: &table.Table{}
	}
	g.generate_macho_header()
	g.generate_macho_footer()
}
