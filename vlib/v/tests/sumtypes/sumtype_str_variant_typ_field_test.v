// A variant with its own `typ` field must not be mistaken for the sum type tag
// when the sum value is printed.
struct Column {
	name string
	typ  string
}

struct Unique {
	cols []string
}

type TableElement = Column | Unique

fn test_str_of_a_sum_whose_variant_has_a_typ_field() {
	column := TableElement(Column{'id', 'INTEGER'})
	unique := TableElement(Unique{['id']})
	assert column.str() == "TableElement(Column{\n    name: 'id'\n    typ: 'INTEGER'\n})"
	assert unique.str() == "TableElement(Unique{\n    cols: ['id']\n})"
	elements := [column, unique]
	assert '${elements}' == '[${column}, ${unique}]'
}
