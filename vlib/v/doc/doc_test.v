import v.table
import v.doc
import v.pref

fn test_vdoc() {
	mut prefs := &pref.Preferences{}
	prefs.fill_with_defaults()
	table := table.new_table()
	println(doc.doc('net', table, prefs))
}
