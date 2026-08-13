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

@[table: 'orm_include_dual_parents']
struct IncludeDualParent {
	id     int @[primary; sql: serial]
	name   string
	lefts  []IncludeDualChild @[fkey: 'left_parent_id']
	rights []IncludeDualChild @[fkey: 'right_parent_id']
}

@[table: 'orm_include_dual_children']
struct IncludeDualChild {
	id              int @[primary; sql: serial]
	left_parent_id  int
	right_parent_id int
	name            string
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

fn test_nested_where_filters_parents_without_filtering_included_relationships() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	mut grandkids := orm.new_query[IncludeGrandkid](db)
	parents.insert(IncludeParent{
		name: 'other parent'
	})!
	children.insert(IncludeChild{
		parent_id: parents.last_id()
		name:      'other child'
	})!
	grandkids.insert(IncludeGrandkid{
		child_id: children.last_id()
		name:     'other grandkid'
	})!

	rows := parents.where('name = ? AND children.name = ? AND children.grandkids.name = ?',
		'parent', 'child', 'grandkid')!.include('children')!.then_include('grandkids')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
	assert rows[0].children.len == 1
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids[0].name == 'grandkid'
}

fn test_nested_where_does_not_hydrate_relationships_without_include() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.where('children.grandkids.name = ?', 'grandkid')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
	assert rows[0].children.len == 0
}

fn test_nested_where_supports_optional_array_relationships() {
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

	rows := parents.where('children.name = ?', 'optional child')!.include('children')!.query()!
	assert rows.len == 1
	assert rows[0].children?.len == 1
}

fn test_nested_where_rejects_non_relationship_intermediate_segment() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	if _ := parents.where('children.name.value = ?', 'x') {
		assert false
	} else {
		assert err.msg().contains('not a `@[fkey]` relationship')
	}
}

fn test_nested_where_supports_two_paths_to_the_same_table() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeDualParent](db)
	mut children := orm.new_query[IncludeDualChild](db)
	parents.create()!
	children.create()!
	parents.insert(IncludeDualParent{
		name: 'dual parent'
	})!
	parent_id := parents.last_id()
	children.insert_many([
		IncludeDualChild{
			left_parent_id: parent_id
			name:           'left'
		},
		IncludeDualChild{
			right_parent_id: parent_id
			name:            'right'
		},
	])!

	rows := parents.where('lefts.name = ? AND rights.name = ?', 'left', 'right')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'dual parent'
}

fn test_include_works_when_select_does_not_name_primary_key() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.select('name')!.include('children')!.query()!
	assert rows[0].name == 'parent'
	assert rows[0].children.len == 1
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
