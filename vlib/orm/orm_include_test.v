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

// Function Call metadata uses the declared column name; SQL-like synthesizes child_id.
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
	child ?IncludeSingularChild
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

fn test_include_applies_legacy_tenant_filter_to_children() {
	mut db := new_scoped_include_database()!
	defer {
		orm.clear_current_tenant_id()
		orm.set_tenant_filter_enabled(false)
		db.close() or {}
	}
	orm.configure_tenant_filter(orm.TenantFilterConfig{ enabled: true, field_name: 'tenant_id' })
	orm.set_current_tenant_id(orm.Primitive(1))
	mut parents := orm.new_query[IncludeScopedParent](db)
	rows := parents.include('children')!.then_include('grandkids')!.query()!
	assert rows[0].children.len == 2
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids[0].tenant_id == 1
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
	if _ := parents.then_include('missing') { assert false
	 }
	if _ := parents.include('missing') { assert false
	 }
	rows := parents.then_include('grandkids')!.query()!
	assert rows[0].children[0].grandkids.len == 2
	if _ := parents.then_include('grandkids') { assert false
	 }
	assert parents.query()![0].children.len == 0
	parents.include('children')!.reset()
	if _ := parents.then_include('grandkids') { assert false
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
	if _ := parents.then_include('grandkids') { assert false
	 }
	log.tables.clear()
	assert parents.query()![0].children.len == 0
	assert log.tables == ['orm_include_parents']
}

fn test_include_rejects_relationship_where_before_writes_and_keeps_valid_where() {
	mut db := new_include_database()!
	defer { db.close() or {} }
	mut parents := orm.new_query[IncludeParent](db)
	parents.where('name = ?', 'parent')!
	if _ := parents.where('name = ? && children.name = ?', 'other', 'child') { assert false
	 }
	if _ := parents.or_where('children.name = ?', 'child') { assert false
	 }
	rows := parents.include('children')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 1
	mut update_called := false
	if _ := parents.where('children.name = ?', 'child') {
		update_called = true
		parents.set('name = ?', 'changed')!.update()!
	}
	assert !update_called
	mut delete_called := false
	if _ := parents.where('children.name = ?', 'child') {
		delete_called = true
		parents.delete()!
	}
	assert !delete_called
	assert parents.query()![0].name == 'parent'
}

fn test_include_scopes_each_loaded_table() {
	mut db := new_scoped_include_database()!
	defer { db.close() or {} }
	mut scoped := orm.new_db(db, orm.DataScope{
		enabled: true
		filters: [
			orm.QueryFilter{
				field:    'tenant_id'
				mode:     .dynamic
				operator: .eq
				value:    1
			},
		]
	})
	mut parents := orm.new_query[IncludeScopedParent](scoped)
	rows := parents.include('children')!.then_include('grandkids')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 2
	for child in rows[0].children {
		assert child.tenant_id == 1
		for grandkid in child.grandkids {
			assert grandkid.tenant_id == 1
		}
	}
	assert rows[0].children[0].grandkids.len == 1
}

fn test_include_self_reference_stops_at_requested_depth() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut nodes := orm.new_query[IncludeSelfNode](db)
	nodes.create()!
	nodes.insert(IncludeSelfNode{ parent_id: 1, name: 'self' })!
	rows := nodes.include('children')!.then_include('children')!.query()!
	assert rows[0].children[0].children[0].id == 1
	assert rows[0].children[0].children[0].children.len == 0
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

@[table: 'orm_include_singular_roots']
struct IncludeSingularRoot {
	id    int @[primary; sql: serial]
	name  string
	child IncludeSingularChild @[fkey: 'id']
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

@[table: 'orm_include_alias_parents']
struct IncludeAliasParent {
	id       int @[primary; sql: serial]
	name     string
	children []IncludeAliasChild @[fkey: 'parent_id'; sql: 'offspring']
}

@[table: 'orm_include_alias_children']
struct IncludeAliasChild {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
	grandkids []IncludeAliasGrandkid @[fkey: 'child_id'; sql: 'descendants']
}

@[table: 'orm_include_alias_grandkids']
struct IncludeAliasGrandkid {
	id       int @[primary; sql: serial]
	child_id int
	name     string
}

@[table: 'orm_include_shadow_parents']
struct IncludeShadowParent {
	id    int @[primary; sql: serial]
	name  string
	alpha []IncludeShadowAlpha @[fkey: 'parent_id'; sql: 'beta']
	beta  []IncludeShadowBeta  @[fkey: 'parent_id']
}

@[table: 'orm_include_shadow_alphas']
struct IncludeShadowAlpha {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
}

@[table: 'orm_include_shadow_betas']
struct IncludeShadowBeta {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
}

@[table: 'orm_include_keyless_roots']
struct IncludeKeylessRoot {
	code     string
	children []IncludeKeylessChild @[fkey: 'root_code']
}

@[table: 'orm_include_keyless_children']
struct IncludeKeylessChild {
	id        int @[primary; sql: serial]
	root_code string
	name      string
}

@[table: 'orm_include_keyed_roots']
struct IncludeKeyedRoot {
	id       int @[primary; sql: serial]
	name     string
	children []IncludeKeylessMiddle @[fkey: 'root_id']
}

@[table: 'orm_include_keyless_middles']
struct IncludeKeylessMiddle {
	root_id   int
	label     string
	grandkids []IncludeKeylessLeaf @[fkey: 'middle_label']
}

@[table: 'orm_include_keyless_leaves']
struct IncludeKeylessLeaf {
	id           int @[primary; sql: serial]
	middle_label string
	name         string
}

@[table: 'orm_include_scoped_parents']
@[ignore_tenant_filter; unscoped]
struct IncludeScopedParent {
	id        int @[primary; sql: serial]
	tenant_id int
	name      string
	children  []IncludeScopedChild @[fkey: 'parent_id']
}

@[table: 'orm_include_scoped_children']
struct IncludeScopedChild {
	id        int @[primary; sql: serial]
	parent_id int
	tenant_id int
	name      string
	grandkids []IncludeScopedGrandkid @[fkey: 'child_id']
}

@[table: 'orm_include_scoped_grandkids']
struct IncludeScopedGrandkid {
	id        int @[primary; sql: serial]
	child_id  int
	tenant_id int
	name      string
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

@[table: 'orm_include_self_nodes']
struct IncludeSelfNode {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
	children  []IncludeSelfNode @[fkey: 'parent_id']
}

@[table: 'orm_include_embedded_parents']
struct IncludeEmbeddedParent {
	id       int @[primary; sql: serial]
	name     string
	children []IncludeEmbeddedChild @[fkey: 'parent_id']
}

@[table: 'orm_include_embedded_children']
struct IncludeEmbeddedChild {
	id            int @[primary; sql: serial]
	parent_id     int
	details_label string @[sql: 'IncludeEmbeddedDetails.label']
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

fn new_scoped_include_database() !sqlite.DB {
	mut db := sqlite.connect(':memory:')!
	mut parents := orm.new_query[IncludeScopedParent](db)
	mut children := orm.new_query[IncludeScopedChild](db)
	mut grandkids := orm.new_query[IncludeScopedGrandkid](db)
	parents.create()!
	children.create()!
	grandkids.create()!
	parents.insert(IncludeScopedParent{
		tenant_id: 1
		name:      'tenant parent'
	})!
	children.insert(IncludeScopedChild{
		parent_id: 1
		tenant_id: 1
		name:      'visible child'
	})!
	children.insert(IncludeScopedChild{
		parent_id: 1
		tenant_id: 2
		name:      'hidden child'
	})!
	children.insert(IncludeScopedChild{
		parent_id: 1
		tenant_id: 1
		name:      'shallow child'
	})!
	grandkids.insert(IncludeScopedGrandkid{
		child_id:  1
		tenant_id: 1
		name:      'visible grandkid'
	})!
	grandkids.insert(IncludeScopedGrandkid{
		child_id:  1
		tenant_id: 2
		name:      'hidden grandkid'
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

fn test_sql_like_keeps_implicit_loading_through_a_singular_relationship() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut children := orm.new_query[IncludeSingularChild](db)
	mut grandkids := orm.new_query[IncludeSingularGrandkid](db)
	db.exec('create table orm_include_singular_roots (id integer primary key, name text, child_id integer)')!
	children.create()!
	grandkids.create()!
	children.insert(IncludeSingularChild{
		name: 'child'
	})!
	child_id := children.last_id()
	grandkids.insert(IncludeSingularGrandkid{
		child_id: child_id
		name:     'grandkid'
	})!
	db.exec("insert into orm_include_singular_roots (name, child_id) values ('root', ${child_id})")!

	rows := sql db {
		select from IncludeSingularRoot
	}!
	assert rows.len == 1
	assert rows[0].child.id == child_id
	assert rows[0].child.grandkids.len == 1
	assert rows[0].child.grandkids[0].name == 'grandkid'
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
	assert rows[0].children[0].grandkids.len == 2
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
	assert rows[0].children[0].grandkids.len == 2
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

fn test_include_rejects_a_partial_distinct_selection_without_the_primary_key() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	if _ := parents.select('name')!.distinct()!.include('children')!.query() {
		assert false
	} else {
		assert err.msg().contains('distinct')
		assert err.msg().contains('relationship key')
	}
}

fn test_or_where_keeps_working_for_root_conditions_with_includes() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.where('name = ?', 'missing')!.or_where('name = ?',
		'parent')!.query()!
	assert rows.len == 1
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

	if _ := parents.include('children')!.then_include('name') {
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

	if _ := parents.include('children')!.then_include('name') {
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

fn new_include_alias_database() !sqlite.DB {
	mut db := sqlite.connect(':memory:')!
	mut parents := orm.new_query[IncludeAliasParent](db)
	mut children := orm.new_query[IncludeAliasChild](db)
	mut grandkids := orm.new_query[IncludeAliasGrandkid](db)
	parents.create()!
	children.create()!
	grandkids.create()!
	parents.insert(IncludeAliasParent{
		name: 'alias parent'
	})!
	parent_id := parents.last_id()
	children.insert(IncludeAliasChild{
		parent_id: parent_id
		name:      'kept child'
	})!
	child_id := children.last_id()
	children.insert(IncludeAliasChild{
		parent_id: parent_id
		name:      'dropped child'
	})!
	grandkids.insert(IncludeAliasGrandkid{
		child_id: child_id
		name:     'alias grandkid'
	})!
	return db
}

fn test_include_accepts_the_v_name_of_an_aliased_relationship() {
	mut db := new_include_alias_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeAliasParent](db)

	rows := parents.include('children')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 2
}

fn test_include_accepts_the_sql_alias_of_a_relationship() {
	mut db := new_include_alias_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeAliasParent](db)

	rows := parents.include('offspring')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 2
}

fn test_then_include_accepts_the_sql_alias_of_a_nested_relationship() {
	mut db := new_include_alias_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeAliasParent](db)

	rows := parents.include('offspring')!.then_include('descendants')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 2
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids[0].name == 'alias grandkid'
	assert rows[0].children[1].grandkids.len == 0
}

fn test_include_requires_a_hydration_key_on_the_root() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut roots := orm.new_query[IncludeKeylessRoot](db)
	mut children := orm.new_query[IncludeKeylessChild](db)
	roots.create()!
	children.create()!
	roots.insert(IncludeKeylessRoot{
		code: 'root'
	})!
	children.insert(IncludeKeylessChild{
		root_code: 'root'
		name:      'child'
	})!

	if _ := roots.include('children') {
		assert false
	} else {
		assert err.msg().contains('orm_include_keyless_roots')
		assert err.msg().contains('`@[primary]` or `id` field')
	}
}

fn test_then_include_requires_a_hydration_key_on_intermediate_relationships() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut roots := orm.new_query[IncludeKeyedRoot](db)
	mut middles := orm.new_query[IncludeKeylessMiddle](db)
	mut leaves := orm.new_query[IncludeKeylessLeaf](db)
	roots.create()!
	middles.create()!
	leaves.create()!
	roots.insert(IncludeKeyedRoot{
		name: 'root'
	})!
	root_id := roots.last_id()
	middles.insert(IncludeKeylessMiddle{
		root_id: root_id
		label:   'middle'
	})!
	leaves.insert(IncludeKeylessLeaf{
		middle_label: 'middle'
		name:         'leaf'
	})!

	if _ := roots.include('children')!.then_include('grandkids') {
		assert false
	} else {
		assert err.msg().contains('orm_include_keyless_middles')
		assert err.msg().contains('`@[primary]` or `id` field')
	}
}

fn test_include_prefers_the_v_field_name_over_another_fields_alias() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeShadowParent](db)
	mut alphas := orm.new_query[IncludeShadowAlpha](db)
	mut betas := orm.new_query[IncludeShadowBeta](db)
	parents.create()!
	alphas.create()!
	betas.create()!
	parents.insert(IncludeShadowParent{
		name: 'shadow parent'
	})!
	parent_id := parents.last_id()
	alphas.insert(IncludeShadowAlpha{
		parent_id: parent_id
		name:      'alpha'
	})!
	betas.insert(IncludeShadowBeta{
		parent_id: parent_id
		name:      'beta'
	})!

	shadowed := parents.include('beta')!.query()!
	assert shadowed[0].alpha.len == 0
	assert shadowed[0].beta.len == 1
	assert shadowed[0].beta[0].name == 'beta'

	aliased := parents.include('alpha')!.query()!
	assert aliased[0].alpha.len == 1
	assert aliased[0].alpha[0].name == 'alpha'
	assert aliased[0].beta.len == 0
}
