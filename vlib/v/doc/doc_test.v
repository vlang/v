import v.table
import v.doc

fn test_vdoc() {
	table := table.new_table()
	println(doc.doc('net', table))
}
