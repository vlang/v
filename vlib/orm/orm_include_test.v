// vtest retry: 3
import db.sqlite
import orm

@[table: 'orm_include_parents']
struct IncludeParent {
	id       int @[primary; sql: serial]
	name     string
	children []IncludeChild @[fkey: 'parent_id']
}

@[table: 'orm_include_children']
struct IncludeChild {
	id         int @[primary; sql: serial]
	parent_id  int
	name       string
	grandkids  []IncludeGrandkid  @[fkey: 'child_id']
	grandkids2 []IncludeGrandkid2 @[fkey: 'child_id']
}

@[table: 'orm_include_grandkids']
struct IncludeGrandkid {
	id       int @[primary; sql: serial]
	child_id int
	name     string
	toys     []IncludeToy @[fkey: 'grandkid_id']
}

@[table: 'orm_include_grandkids2']
struct IncludeGrandkid2 {
	id       int @[primary; sql: serial]
	child_id int
	name     string
}

@[table: 'orm_include_toys']
struct IncludeToy {
	id          int @[primary; sql: serial]
	grandkid_id int
	name        string
}

@[table: 'orm_include_optional_parents']
struct IncludeOptionalParent {
	id       int @[primary; sql: serial]
	name     string
	children ?[]IncludeOptionalChild @[fkey: 'parent_id']
}

@[table: 'orm_include_optional_children']
struct IncludeOptionalChild {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
}

fn new_include_database() !sqlite.DB {
	mut db := sqlite.connect(':memory:')!
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	mut grandkids := orm.new_query[IncludeGrandkid](db)
	mut grandkids2 := orm.new_query[IncludeGrandkid2](db)
	mut toys := orm.new_query[IncludeToy](db)
	parents.create()!
	children.create()!
	grandkids.create()!
	grandkids2.create()!
	toys.create()!
	parents.insert(IncludeParent{
		name: 'parent'
	})!
	parent_id := parents.last_id()
	children.insert(IncludeChild{
		parent_id: parent_id
		name:      'child'
	})!
	child_id := children.last_id()
	grandkids.insert(IncludeGrandkid{
		child_id: child_id
		name:     'grandkid'
	})!
	grandkid_id := grandkids.last_id()
	grandkids2.insert(IncludeGrandkid2{
		child_id: child_id
		name:     'grandkid2'
	})!
	toys.insert(IncludeToy{
		grandkid_id: grandkid_id
		name:        'toy'
	})!
	return db
}

fn test_function_call_does_not_load_unrequested_relationships() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.query()!
	assert rows.len == 1
	assert rows[0].children.len == 0
}

fn test_sql_like_keeps_implicit_relationship_loading() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}

	rows := sql db {
		select from IncludeParent
	}!
	assert rows.len == 1
	assert rows[0].children.len == 1
}

fn test_include_loads_direct_relationship_only() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.query()!
	assert rows[0].children.len == 1
	assert rows[0].children[0].grandkids.len == 0
	assert rows[0].children[0].grandkids2.len == 0
}

fn test_then_include_loads_nested_relationships_to_any_depth() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.then_include('grandkids')!.then_include('toys')!.query()!
	assert rows[0].children.len == 1
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids[0].toys.len == 1
	assert rows[0].children[0].grandkids2.len == 0
}

fn test_include_restarts_from_root_and_merges_sibling_paths() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows :=
		parents.include('children')!.then_include('grandkids')!.include('children')!.then_include('grandkids2')!.query()!
	assert rows[0].children.len == 1
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids2.len == 1
}

fn test_include_works_when_select_does_not_name_primary_key() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.select('name')!.include('children')!.query()!
	assert rows[0].id == 0
	assert rows[0].name == 'parent'
	assert rows[0].children.len == 1
}

fn test_where_rejects_relationship_paths() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	if _ := parents.where('children.name = ?', 'child') {
		assert false
	} else {
		assert err.msg().len > 0
	}
}

fn test_include_loads_optional_array_relationship() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	sql db {
		create table IncludeOptionalParent
	}!
	sql db {
		create table IncludeOptionalChild
	}!
	parent := IncludeOptionalParent{
		name: 'optional parent'
	}
	sql db {
		insert parent into IncludeOptionalParent
	}!
	child := IncludeOptionalChild{
		parent_id: 1
		name:      'optional child'
	}
	sql db {
		insert child into IncludeOptionalChild
	}!
	mut parents := orm.new_query[IncludeOptionalParent](db)

	rows := parents.include('children')!.query()!
	assert rows[0].children?.len == 1
}

fn test_include_rejects_non_relationship_field() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	if _ := parents.include('name') {
		assert false
	} else {
		assert err.msg().contains('not a `@[fkey]` relationship')
	}
}

fn test_then_include_rejects_an_invalid_nested_relationship() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	if _ := parents.include('children')!.then_include('name')!.query() {
		assert false
	} else {
		assert err.msg().contains('not a `@[fkey]` relationship')
	}
}

fn test_then_include_validates_nested_relationships_without_rows() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	parents.create()!
	children.create()!

	if _ := parents.include('children')!.then_include('name')!.query() {
		assert false
	} else {
		assert err.msg().contains('not a `@[fkey]` relationship')
	}
}

fn test_then_include_requires_a_previous_include() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	if _ := parents.then_include('children') {
		assert false
	} else {
		assert err.msg().contains('include')
	}
}
