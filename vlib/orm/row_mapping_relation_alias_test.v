module orm

import db.sqlite

struct AliasMappingChild {
	id        int @[primary]
	parent_id int
	name      string
}

type AliasMappingOne = AliasMappingChild

struct AliasMappingParent {
	id       int                @[primary]
	single   ?AliasMappingOne   @[sql: 'single_id']
	children ?[]AliasMappingOne @[fkey: parent_id]
}

fn test_row_mapping_preserves_optional_alias_relationships() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	// Singular and collection aliases retain their existing table-name resolution.
	for name in [new_query[AliasMappingOne](db).config.table.name,
		new_query[AliasMappingChild](db).config.table.name] {
		db.exec('create table if not exists ${name} (id integer primary key, parent_id integer, name text)')!
		db.exec("insert or ignore into ${name} values (2, 1, 'first'), (3, 1, 'second')")!
	}
	mut parent := new_query[AliasMappingParent](db)
	parent.config.fields = ['id', 'single_id']
	parent.relation_load_mode = .implicit
	row := parent.map_row([Primitive(1), 2])!
	single := row.single or { panic('missing singular alias relationship') }
	assert single.id == 2
	assert single.name == 'first'
	children := row.children or { panic('missing collection alias relationship') }
	assert children.len == 2
	assert children.map(it.name) == ['first', 'second']
}
