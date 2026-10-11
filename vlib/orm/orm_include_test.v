// vtest retry: 3
import db.sqlite
import orm

struct IncludeQueryLog {
mut:
	tables     []string
	fail_table string
}

// Embed the real adapter so counting does not replace query execution with a stub.
struct IncludeCountingConnection {
	sqlite.DB
mut:
	log &IncludeQueryLog
}

fn (mut db IncludeCountingConnection) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	db.log.tables << config.table.name
	if config.table.name == db.log.fail_table {
		return error('include test: database unavailable')
	}
	return db.DB.select(config, data, where)
}

@[table: 'orm_include_optional_singular_roots']
struct IncludeOptionalSingularRoot {
	id    int @[primary; sql: serial]
	name  string
	child ?IncludeSingularChild @[sql: 'child_id']
}

// Both APIs use the declared relationship lookup column.
@[table: 'orm_include_singular_roots']
struct IncludeFunctionSingularRoot {
	id    int @[primary; sql: serial]
	name  string
	child IncludeSingularChild @[sql: 'child_id']
}

@[table: 'orm_include_optional_singular_roots']
struct IncludeOptionalSqlRoot {
	id    int @[primary; sql: serial]
	name  string
	child ?IncludeSingularChild @[sql: 'child_id']
}

fn test_include_singular_and_optional_relationships_are_explicit() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	db.exec('create table orm_include_singular_roots (id integer primary key, name text, child_id integer)')!
	db.exec('create table orm_include_optional_singular_roots (id integer primary key, name text, child_id integer)')!
	mut children := orm.new_query[IncludeSingularChild](db)
	mut grandkids := orm.new_query[IncludeSingularGrandkid](db)
	children.create()!
	grandkids.create()!
	children.insert(IncludeSingularChild{ name: 'child' })!
	grandkids.insert(IncludeSingularGrandkid{ child_id: 1, name: 'grandkid' })!
	db.exec("insert into orm_include_singular_roots values (1, 'root', 1)")!
	db.exec("insert into orm_include_optional_singular_roots values (1, 'present', 1), (2, 'absent', null)")!
	mut log := &IncludeQueryLog{}
	mut counted := IncludeCountingConnection{
		DB:  db
		log: log
	}
	mut roots := orm.new_query[IncludeFunctionSingularRoot](counted)
	assert roots.query()![0].child.id == 0
	assert log.tables == ['orm_include_singular_roots']
	loaded := roots.include('child')!.query()!
	assert loaded[0].child.name == 'child'
	assert loaded[0].child.grandkids.len == 0
	nested := roots.select('name')!.include('child')!.then_include('grandkids')!.query()!
	assert nested[0].id == 0
	assert nested[0].child.grandkids[0].name == 'grandkid'
	mut optional := orm.new_query[IncludeOptionalSingularRoot](counted)
	unloaded := optional.query()!
	assert unloaded[0].child == none
	rows := optional.include('child')!.then_include('grandkids')!.order(.asc, 'id')!.query()!
	child := rows[0].child or { panic('missing child') }
	assert child.grandkids.len == 1
	assert rows[1].child == none
	implicit := sql db {
		select from IncludeOptionalSqlRoot order by id
	}!
	implicit_child := implicit[0].child or { panic('missing implicit child') }
	assert implicit_child.grandkids.len == 1
	log.fail_table = 'orm_include_singular_children'
	if _ := optional.include('child')!.query() {
		assert false
	} else {
		assert err.msg() == 'include test: database unavailable'
	}
}

fn test_include_counts_queries_and_deduplicates_paths() {
	mut db := new_include_database()!
	defer { db.close() or {} }
	mut log := &IncludeQueryLog{}
	mut counted := IncludeCountingConnection{
		DB:  db
		log: log
	}
	mut parents := orm.new_query[IncludeParent](counted)
	parents.insert(IncludeParent{ name: 'childless' })!
	assert parents.query()!.len == 2
	assert log.tables == ['orm_include_parents']
	log.tables.clear()
	rows := parents.include('children')!.include('children')!.order(.asc, 'id')!.query()!
	assert rows.len == 2
	assert rows[0].children.len == 1
	assert rows[1].children.len == 0
	assert log.tables == ['orm_include_parents', 'orm_include_children', 'orm_include_children']
	log.tables.clear()
	assert parents.include('children')!.count()! == 2
	assert log.tables == ['orm_include_parents']
	log.tables.clear()
	assert parents.select('name')!.distinct()!.include('children')!.count()! == 2
	assert log.tables == ['orm_include_parents']
}

fn test_explicit_include_does_not_hide_a_missing_table() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut parents := orm.new_query[IncludeParent](db)
	parents.create()!
	parents.insert(IncludeParent{ name: 'parent' })!
	if _ := parents.include('children')!.query() {
		assert false
	} else {
		assert err.msg().contains('no such table')
	}
}

fn test_include_resolves_aliased_foreign_keys() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut parents := orm.new_query[IncludeAliasedFkeyParent](db)
	mut children := orm.new_query[IncludeAliasedFkeyChild](db)
	mut grandkids := orm.new_query[IncludeAliasedFkeyGrandkid](db)
	parents.create()!
	children.create()!
	grandkids.create()!
	parents.insert(IncludeAliasedFkeyParent{ name: 'parent' })!
	children.insert(IncludeAliasedFkeyChild{ parent_id: 1, name: 'child' })!
	grandkids.insert(IncludeAliasedFkeyGrandkid{ child_id: 1, name: 'grandkid' })!
	rows := parents.include('children')!.then_include('grandkids')!.query()!
	assert rows[0].children[0].name == 'child'
	assert rows[0].children[0].grandkids[0].name == 'grandkid'
}

fn test_include_failed_path_preserves_cursor_and_query_clears_it() {
	mut db := new_include_database()!
	defer { db.close() or {} }
	mut parents := orm.new_query[IncludeParent](db)
	parents.include('children')!
	if _ := parents.then_include('missing') {
		assert false
	}
	if _ := parents.include('missing') {
		assert false
	}
	rows := parents.then_include('grandkids')!.query()!
	assert rows[0].children[0].grandkids.len == 2
	if _ := parents.then_include('grandkids') {
		assert false
	}
	assert parents.query()![0].children.len == 0
	parents.include('children')!.reset()
	if _ := parents.then_include('grandkids') {
		assert false
	}
}

fn test_include_propagates_query_errors_and_resets() {
	mut db := new_include_database()!
	defer { db.close() or {} }
	mut log := &IncludeQueryLog{
		fail_table: 'orm_include_children'
	}
	mut counted := IncludeCountingConnection{
		DB:  db
		log: log
	}
	mut parents := orm.new_query[IncludeParent](counted)
	if _ := parents.include('children')!.query() {
		assert false
	} else {
		assert err.msg() == 'include test: database unavailable'
	}
	if _ := parents.then_include('grandkids') {
		assert false
	}
	log.tables.clear()
	assert parents.query()![0].children.len == 0
	assert log.tables == ['orm_include_parents']
}

fn test_include_three_levels_and_unselected_primary_key() {
	mut db := new_include_database()!
	defer { db.close() or {} }
	mut parents := orm.new_query[IncludeParent](db)
	rows := parents.select('name')!.include('children')!.then_include('grandkids')!
		.then_include('toys')!.query()!
	assert rows[0].id == 0
	assert rows[0].children[0].grandkids[0].toys[0].name == 'toy'
	if _ := parents.select('name')!.distinct()!.include('children')!.query() {
		assert false
	} else {
		assert err.msg().contains('requires selecting the relationship key')
	}
	assert parents.query()![0].children.len == 0
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

fn test_include_optional_collection() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut parents := orm.new_query[IncludeOptionalParent](db)
	mut children := orm.new_query[IncludeOptionalChild](db)
	parents.create()!
	children.create()!
	parents.insert(IncludeOptionalParent{ name: 'parent' })!
	children.insert(IncludeOptionalChild{ parent_id: 1, name: 'child' })!
	assert parents.query()![0].children == none
	rows := parents.include('children')!.query()!
	loaded := rows[0].children or { panic('missing included collection') }
	assert loaded.len == 1
	assert loaded[0].name == 'child'
}

@[table: 'orm_include_parents']
struct IncludeParent {
	id       int @[primary; sql: serial]
	name     string
	children []IncludeChild @[fkey: 'parent_id']
	pets     []IncludePet   @[fkey: 'parent_id']
}

@[table: 'orm_include_children']
struct IncludeChild {
	id         int @[primary; sql: serial]
	parent_id  int
	name       string
	active     bool
	grandkids  []IncludeGrandkid  @[fkey: 'child_id']
	grandkids2 []IncludeGrandkid2 @[fkey: 'child_id']
}

@[table: 'orm_include_grandkids']
struct IncludeGrandkid {
	id       int @[primary; sql: serial]
	child_id int
	name     string
	nickname ?string
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

@[table: 'orm_include_pets']
struct IncludePet {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
}

@[table: 'orm_include_singular_children']
struct IncludeSingularChild {
	id        int @[primary; sql: serial]
	name      string
	grandkids []IncludeSingularGrandkid @[fkey: 'child_id']
}

@[table: 'orm_include_singular_grandkids']
struct IncludeSingularGrandkid {
	id       int @[primary; sql: serial]
	child_id int
	name     string
}

@[table: 'orm_include_aliased_fkey_parents']
struct IncludeAliasedFkeyParent {
	id       int @[primary; sql: serial]
	name     string
	children []IncludeAliasedFkeyChild @[fkey: 'parent_id']
}

@[table: 'orm_include_aliased_fkey_children']
struct IncludeAliasedFkeyChild {
	id        int                          @[primary; sql: serial]
	parent_id int                          @[sql: 'owner_id']
	name      string                       @[sql: 'display_name']
	grandkids []IncludeAliasedFkeyGrandkid @[fkey: 'child_id']
}

@[table: 'orm_include_aliased_fkey_grandkids']
struct IncludeAliasedFkeyGrandkid {
	id       int    @[primary; sql: serial]
	child_id int    @[sql: 'owner_child_id']
	name     string @[sql: 'display_name']
}

fn new_include_database() !sqlite.DB {
	mut db := sqlite.connect(':memory:')!
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	mut grandkids := orm.new_query[IncludeGrandkid](db)
	mut grandkids2 := orm.new_query[IncludeGrandkid2](db)
	mut toys := orm.new_query[IncludeToy](db)
	mut pets := orm.new_query[IncludePet](db)
	parents.create()!
	children.create()!
	grandkids.create()!
	grandkids2.create()!
	toys.create()!
	pets.create()!
	parents.insert(IncludeParent{
		name: 'parent'
	})!
	parent_id := parents.last_id()
	children.insert(IncludeChild{
		parent_id: parent_id
		name:      'child'
		active:    true
	})!
	child_id := children.last_id()
	grandkids.insert(IncludeGrandkid{
		child_id: child_id
		name:     'grandkid'
	})!
	grandkid_id := grandkids.last_id()
	grandkids.insert(IncludeGrandkid{
		child_id: child_id
		name:     'excluded grandkid'
	})!
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
