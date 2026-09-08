// vtest retry: 3
import db.sqlite
import orm

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
		assert err.msg().contains('primary key')
	}
}

fn test_where_filters_the_last_included_relationship_without_filtering_the_root() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.then_include('grandkids')!.where('name = ? && grandkids.name != ?',
		'parent', 'excluded grandkid')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 1
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids[0].name == 'grandkid'
}

fn test_where_filters_a_direct_included_relationship() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.where('children.name = ?', 'child')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 1
	assert rows[0].children[0].name == 'child'
}

fn test_where_drops_the_root_when_no_related_row_matches() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.where('children.name = ?', 'missing')!.query()!
	assert rows.len == 0
}

fn test_where_filters_the_root_without_returning_the_relationship() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.where('children.name = ?', 'child')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
	assert rows[0].children.len == 0

	empty := parents.where('children.name = ?', 'missing')!.query()!
	assert empty.len == 0
}

fn test_where_filters_the_root_through_a_deep_relationship_path() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.where('children.grandkids.toys.name = ?', 'toy')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 0

	empty := parents.where('children.grandkids.toys.name = ?', 'missing')!.query()!
	assert empty.len == 0
}

fn test_where_requires_one_related_row_to_match_every_term_of_a_call() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	// the grandkid carrying the toy is `grandkid`, so pairing it with the other name
	// cannot be satisfied by a single related row
	matched := parents.where('children.grandkids.name = ? && children.grandkids.toys.name = ?',
		'grandkid', 'toy')!.query()!
	assert matched.len == 1

	crossed := parents.where('children.grandkids.name = ? && children.grandkids.toys.name = ?',
		'excluded grandkid', 'toy')!.query()!
	assert crossed.len == 0
}

fn test_where_accepts_the_full_path_of_the_last_included_relationship() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.then_include('grandkids')!.where('children.grandkids.name = ?',
		'grandkid')!.query()!
	assert rows.len == 1
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids[0].name == 'grandkid'
}

fn test_where_accumulates_filters_on_the_same_included_relationship() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.then_include('grandkids')!.where('grandkids.name != ?',
		'excluded grandkid')!.where('grandkids.name = ?', 'grandkid')!.query()!
	assert rows.len == 1
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids[0].name == 'grandkid'
}

fn test_where_preserves_or_grouping_when_accumulating_included_relationship_filters() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.then_include('grandkids')!.where('grandkids.name = ? || grandkids.name = ?',
		'grandkid', 'excluded grandkid')!.where('grandkids.id = ?', 2)!.query()!
	assert rows.len == 1
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids[0].name == 'excluded grandkid'
}

fn test_where_filters_independent_included_paths() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.then_include('grandkids')!.where('grandkids.name = ?',
		'grandkid')!.include('children')!.then_include('grandkids2')!.where('grandkids2.name = ?',
		'grandkid2')!.query()!
	assert rows.len == 1
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids2.len == 1
}

fn test_where_combines_a_root_and_a_relationship_predicate() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.then_include('grandkids')!.where('name = ? && grandkids.name = ?',
		'parent', 'grandkid')!.query()!
	assert rows.len == 1
	assert rows[0].children[0].grandkids.len == 1

	empty := parents.include('children')!.then_include('grandkids')!.where('name = ? && grandkids.name = ?',
		'parent', 'missing')!.query()!
	assert empty.len == 0
}

fn test_where_keeps_boolean_expressions_within_an_included_relationship() {
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
	other_parent_id := parents.last_id()
	children.insert(IncludeChild{
		parent_id: other_parent_id
		name:      'other child'
	})!
	grandkids.insert(IncludeGrandkid{
		child_id: children.last_id()
		name:     'other grandkid'
	})!

	rows := parents.where('name = ?', 'parent')!.include('children')!.then_include('grandkids')!.where('grandkids.name = ? || grandkids.name = ?',
		'grandkid', 'other grandkid')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 1
	assert rows[0].children[0].grandkids.len == 1
	assert rows[0].children[0].grandkids[0].name == 'grandkid'
}

fn test_where_preserves_relationship_or_when_the_condition_also_has_a_root_term() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.include('children')!.where('name = ? && (children.name = ? || children.name = ?)',
		'parent', 'missing', 'child')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 1
	assert rows[0].children[0].name == 'child'
}

fn test_where_correlates_every_unparenthesized_relationship_or_branch() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	parents.insert(IncludeParent{
		name: 'childless parent'
	})!

	rows := parents.where('children.name = ? || children.name = ?', 'missing', 'child')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
}

fn test_or_where_preserves_the_connector_when_hydrating_a_relationship() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	children.insert(IncludeChild{
		parent_id: 1
		name:      'other child'
	})!

	rows := parents.include('children')!.where('children.name = ?', 'child')!.or_where('children.name = ?',
		'other child')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 2
}

fn test_where_keeps_a_parenthesized_relationship_or_in_one_exists() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	children.insert(IncludeChild{
		parent_id: 1
		name:      'other child'
	})!

	crossed := parents.where('(children.name = ? || children.name = ?) && children.id = ?',
		'child', 'missing', 2)!.query()!
	assert crossed.len == 0
	matched := parents.where('(children.name = ? || children.name = ?) && children.id = ?',
		'missing', 'other child', 2)!.query()!
	assert matched.len == 1
}

fn test_where_applies_trailing_constraints_to_mixed_depth_or_branches() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	children.insert(IncludeChild{
		parent_id: 1
		name:      'other child'
	})!

	crossed := parents.where('(children.name = ? || children.grandkids.name = ?) && children.id = ?',
		'child', 'missing', 2)!.query()!
	assert crossed.len == 0
	matched := parents.where('(children.name = ? || children.grandkids.name = ?) && children.id = ?',
		'missing', 'grandkid', 1)!.query()!
	assert matched.len == 1
	shallow := parents.where('children.name = ? || children.grandkids.name = ?', 'other child',
		'missing')!.query()!
	assert shallow.len == 1
}

fn test_where_applies_trailing_constraints_to_root_interleaved_or_branches() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	crossed := parents.where('(children.name = ? && name = ? || children.id = ?) && children.parent_id = ?',
		'child', 'parent', 1, 999)!.query()!
	assert crossed.len == 0
	matched := parents.where('(children.name = ? && name = ? || children.id = ?) && children.parent_id = ?',
		'child', 'parent', 1, 1)!.query()!
	assert matched.len == 1
}

fn test_or_where_preserves_the_connector_after_relationship_aliases_are_canonicalized() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeAliasParent](db)
	mut children := orm.new_query[IncludeAliasChild](db)
	parents.create()!
	children.create()!
	parents.insert(IncludeAliasParent{
		name: 'parent'
	})!
	children.insert(IncludeAliasChild{
		parent_id: 1
		name:      'first'
	})!
	children.insert(IncludeAliasChild{
		parent_id: 1
		name:      'second'
	})!

	rows := parents.include('children')!.where('children.name = ?', 'first')!.or_where('offspring.name = ?',
		'second')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 2
}

fn test_where_keeps_same_relationship_terms_together_across_a_root_term() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	children.insert(IncludeChild{
		parent_id: 1
		name:      'other child'
	})!

	crossed :=
		parents.where('children.name = ? && name = ? && children.id = ?', 'child', 'parent', 2)!.query()!
	assert crossed.len == 0
	matched :=
		parents.where('children.name = ? && name = ? && children.id = ?', 'child', 'parent', 1)!.query()!
	assert matched.len == 1
}

fn test_where_resolves_aliased_fkeys_in_relationship_predicates() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeAliasedFkeyParent](db)
	mut children := orm.new_query[IncludeAliasedFkeyChild](db)
	mut grandkids := orm.new_query[IncludeAliasedFkeyGrandkid](db)
	parents.create()!
	children.create()!
	grandkids.create()!
	parents.insert(IncludeAliasedFkeyParent{
		name: 'parent'
	})!
	children.insert(IncludeAliasedFkeyChild{
		parent_id: 1
		name:      'child'
	})!
	grandkids.insert(IncludeAliasedFkeyGrandkid{
		child_id: 1
		name:     'grandkid'
	})!

	assert parents.where('children.name = ?', 'child')!.count()! == 1
	assert parents.where('children.grandkids.name = ?', 'grandkid')!.count()! == 1
}

fn test_where_aliases_each_hop_of_a_self_referential_relationship() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut nodes := orm.new_query[IncludeSelfNode](db)
	nodes.create()!
	nodes.insert(IncludeSelfNode{
		name: 'root'
	})!
	nodes.insert(IncludeSelfNode{
		name: 'other root'
	})!
	nodes.insert(IncludeSelfNode{
		parent_id: 1
		name:      'child'
	})!
	nodes.insert(IncludeSelfNode{
		parent_id: 3
		name:      'grandchild'
	})!

	direct := nodes.where('children.name = ?', 'child')!.query()!
	assert direct.len == 1
	assert direct[0].name == 'root'
	deep := nodes.where('children.children.name = ?', 'grandchild')!.query()!
	assert deep.len == 1
	assert deep[0].name == 'root'
}

fn test_where_filters_an_optional_array_relationship() {
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

	rows := parents.include('children')!.where('children.name = ?', 'optional child')!.query()!
	assert rows.len == 1
	assert rows[0].children?.len == 1

	empty := parents.include('children')!.where('children.name = ?', 'missing')!.query()!
	assert empty.len == 0
}

fn test_where_ors_a_root_condition_with_a_relationship_condition() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	// the root branch alone keeps a parent whose relationship branch does not match
	rows := parents.where('name = ? || children.name = ?', 'parent', 'missing')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'

	empty := parents.where('name = ? || children.name = ?', 'missing', 'missing')!.query()!
	assert empty.len == 0
}

fn test_where_ors_conditions_on_sibling_relationships() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.where('children.grandkids.name = ? || children.grandkids2.name = ?', 'missing',
		'grandkid2')!.query()!
	assert rows.len == 1

	empty := parents.where('children.grandkids.name = ? || children.grandkids2.name = ?',
		'missing', 'missing')!.query()!
	assert empty.len == 0
}

fn test_where_keeps_sibling_or_branches_with_a_trailing_ancestor_constraint() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.where('(children.grandkids.name = ? || children.grandkids2.name = ?) && children.id = ?',
		'grandkid', 'missing', 1)!.query()!
	assert rows.len == 1

	empty := parents.where('(children.grandkids.name = ? || children.grandkids2.name = ?) && children.id = ?',
		'grandkid', 'missing', 999)!.query()!
	assert empty.len == 0
}

fn test_where_accepts_sibling_or_branches_separated_by_a_root_term() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.where('children.grandkids.name = ? || (name = ? && children.grandkids2.name = ?)',
		'grandkid', 'missing', 'missing')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
}

fn repeated_branch_query(parents &orm.QueryBuilder[IncludeParent], condition string) ![]IncludeParent {
	return parents.where(condition, 'child', 'missing', 2)!.query()!
}

fn repeated_branch_update(parents &orm.QueryBuilder[IncludeParent], condition string) ! {
	parents.where(condition, 'child', 'missing', 2)!.set('name = ?', 'updated')!.update()!
}

fn repeated_branch_delete(parents &orm.QueryBuilder[IncludeParent], condition string) ! {
	parents.where(condition, 'child', 'missing', 2)!.delete()!
}

fn test_where_rejects_an_anded_relationship_repeated_across_another_branch() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut children := orm.new_query[IncludeChild](db)
	children.insert(IncludeChild{
		parent_id: 1
		name:      'other child'
		active:    true
	})!
	condition := '(children.name = ? || pets.name = ?) && children.id = ?'

	mut query_parents := orm.new_query[IncludeParent](db)
	if _ := repeated_branch_query(query_parents, condition) {
		assert false
	} else {
		assert err.msg().contains('relationship `children` is repeated')
	}
	mut update_parents := orm.new_query[IncludeParent](db)
	if _ := repeated_branch_update(update_parents, condition) {
		assert false
	} else {
		assert err.msg().contains('relationship `children` is repeated')
	}
	mut delete_parents := orm.new_query[IncludeParent](db)
	if _ := repeated_branch_delete(delete_parents, condition) {
		assert false
	} else {
		assert err.msg().contains('relationship `children` is repeated')
	}

	mut parents := orm.new_query[IncludeParent](db)
	rows := parents.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
}

fn test_where_preserves_trailing_and_when_projecting_a_hydration_filter() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	children.insert(IncludeChild{
		parent_id: 1
		name:      'child'
		active:    false
	})!

	rows := parents.include('children')!.where('(children.name = ? || name = ?) && children.active = ?',
		'child', 'missing', true)!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 1
	assert rows[0].children[0].active
}

fn test_where_deep_is_null_requires_a_real_descendant() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	parents.insert(IncludeParent{
		name: 'no grandkids'
	})!
	children.insert(IncludeChild{
		parent_id: parents.last_id()
		name:      'child without grandkids'
	})!

	rows := parents.where('children.grandkids.nickname IS NULL')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
}

fn test_where_resolves_an_embedded_terminal_field_after_a_relationship_path() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeEmbeddedParent](db)
	mut children := orm.new_query[IncludeEmbeddedChild](db)
	parents.create()!
	children.create()!
	parents.insert(IncludeEmbeddedParent{
		name: 'embedded parent'
	})!
	children.insert(IncludeEmbeddedChild{
		parent_id:     parents.last_id()
		details_label: 'embedded label'
	})!

	rows := parents.include('children')!.where('children.IncludeEmbeddedDetails.label = ?',
		'embedded label')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'embedded parent'
	assert rows[0].children.len == 1
	assert rows[0].children[0].details_label == 'embedded label'
	assert parents.where('children.IncludeEmbeddedDetails.label = ?', 'missing')!.count()! == 0
}

fn test_where_rejects_and_between_sibling_relationships_in_one_call() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	if _ := parents.where('children.grandkids.name = ? && children.grandkids2.name = ?',
		'grandkid', 'grandkid2')
	{
		assert false
	} else {
		assert err.msg().contains('sibling relationships')
	}
}

fn test_or_where_accepts_a_relationship_condition() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.where('name = ?', 'missing')!.or_where('children.name = ?', 'child')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
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

fn test_update_filters_rows_by_a_relationship() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	parents.insert(IncludeParent{
		name: 'childless'
	})!

	parents.where('children.name = ?', 'child')!.set('name = ?', 'updated')!.update()!
	rows := parents.order(.asc, 'id')!.query()!
	assert rows.len == 2
	assert rows[0].name == 'updated'
	assert rows[1].name == 'childless'
}

fn test_update_leaves_every_row_when_no_relationship_matches() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	parents.where('children.name = ?', 'missing')!.set('name = ?', 'updated')!.update()!
	rows := parents.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
}

fn test_delete_filters_rows_by_a_relationship() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	parents.insert(IncludeParent{
		name: 'childless'
	})!

	parents.where('children.name = ?', 'child')!.delete()!
	rows := parents.query()!
	assert rows.len == 1
	assert rows[0].name == 'childless'
}

fn test_delete_keeps_rows_when_no_relationship_matches() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	parents.where('children.name = ?', 'missing')!.delete()!
	rows := parents.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
}

fn test_insert_rejects_relationship_filters() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	value := IncludeParent{
		name: 'not inserted'
	}

	if _ := parents.include('children')!.where('children.name = ?', 'child')!.insert(value) {
		assert false
	} else {
		assert err.msg().contains('insert()')
		assert err.msg().contains('relationship-scoped filters')
	}
	if _ := parents.include('children')!.where('children.name = ?', 'child')!.insert_many([
		value,
	])
	{
		assert false
	} else {
		assert err.msg().contains('insert_many()')
		assert err.msg().contains('relationship-scoped filters')
	}
	rows := parents.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'
}

fn test_where_rejects_an_invalid_included_relationship_field() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	if _ := parents.include('children')!.then_include('grandkids')!.where('grandkids.unknown = ?',
		'value')!.query()
	{
		assert false
	} else {
		assert err.msg().contains("has no field's name: `unknown`")
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

fn test_include_filter_accepts_the_sql_alias_of_a_relationship() {
	mut db := new_include_alias_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeAliasParent](db)

	rows := parents.include('offspring')!.where('offspring.name = ?', 'kept child')!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 1
	assert rows[0].children[0].name == 'kept child'
}

fn test_include_filters_are_merged_across_relationship_spellings() {
	mut db := new_include_alias_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeAliasParent](db)

	rows :=
		parents.include('children')!.where('children.name = ?', 'kept child')!.include('offspring')!.where('offspring.id > ?', 0)!.query()!
	assert rows.len == 1
	assert rows[0].children.len == 1
	assert rows[0].children[0].name == 'kept child'
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

	if _ := roots.include('children')!.query() {
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

	if _ := roots.include('children')!.then_include('grandkids')!.query() {
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

fn test_where_keeps_a_root_term_outside_a_parenthesized_relationship_group() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	// the root term must stay in the outer query: inside the subquery `name` would
	// resolve against the children table instead
	rows := parents.where('(children.name = ? && name = ?)', 'child', 'parent')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'parent'

	empty := parents.where('(children.name = ? && name = ?)', 'child', 'missing')!.query()!
	assert empty.len == 0
}

fn test_where_keeps_parenthesized_sibling_relationship_groups_independent() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	mut grandkids2 := orm.new_query[IncludeGrandkid2](db)
	// this parent reaches `grandkids2` but has no `grandkids` row at all, so nesting the
	// second subquery inside the first would drop it
	parents.insert(IncludeParent{
		name: 'lonely parent'
	})!
	children.insert(IncludeChild{
		parent_id: parents.last_id()
		name:      'lonely child'
	})!
	grandkids2.insert(IncludeGrandkid2{
		child_id: children.last_id()
		name:     'lonely grandkid2'
	})!

	rows := parents.where('(children.grandkids.name = ? || children.grandkids2.name = ?)',
		'missing', 'lonely grandkid2')!.query()!
	assert rows.len == 1
	assert rows[0].name == 'lonely parent'
}

fn test_where_groups_terms_inside_a_single_relationship_subquery() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)

	rows := parents.where('(children.name = ? && children.parent_id = ?)', 'child', 1)!.query()!
	assert rows.len == 1

	empty := parents.where('(children.name = ? && children.parent_id = ?)', 'child', 99)!.query()!
	assert empty.len == 0
}

fn test_count_applies_relationship_predicates() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	parents.insert(IncludeParent{
		name: 'childless'
	})!

	assert parents.count()! == 2
	assert parents.where('children.name = ?', 'child')!.count()! == 1
	assert parents.where('children.name = ?', 'missing')!.count()! == 0
}

fn test_aggregates_apply_relationship_predicates_without_multiplying_rows() {
	mut db := new_include_database()!
	defer {
		db.close() or {}
	}
	mut parents := orm.new_query[IncludeParent](db)
	mut children := orm.new_query[IncludeChild](db)
	// two children under the same parent would double a JOIN based aggregate
	children.insert(IncludeChild{
		parent_id: 1
		name:      'second child'
	})!

	total := parents.where('children.parent_id = ?', 1)!.sum('id')!
	assert total.has_value
	assert total.value as int == 1
}

fn test_data_scope_applies_to_every_table_in_relationship_predicates() {
	mut raw_db := new_scoped_include_database()!
	defer {
		raw_db.close() or {}
	}
	mut scoped_db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
				mode:  .dynamic
			},
		]
	})
	mut parents := orm.new_query[IncludeScopedParent](scoped_db)

	assert parents.where('children.name = ?', 'visible child')!.count()! == 1
	assert parents.where('children.grandkids.name = ?', 'visible grandkid')!.count()! == 1
	assert parents.where('children.name = ?', 'hidden child')!.count()! == 0
	assert parents.where('children.grandkids.name = ?', 'hidden grandkid')!.count()! == 0
	assert parents.where('children.name = ? || children.grandkids.name = ?', 'shallow child',
		'missing')!.count()! == 1

	parents.where('children.name = ?', 'hidden child')!.set('name = ?', 'updated')!.update()!
	parents.where('children.grandkids.name = ?', 'hidden grandkid')!.delete()!
	mut unscoped_parents := orm.new_query[IncludeScopedParent](raw_db)
	rows := unscoped_parents.query()!
	assert rows.len == 1
	assert rows[0].name == 'tenant parent'
}

fn test_legacy_tenant_scope_applies_to_every_table_in_relationship_predicates() {
	mut db := new_scoped_include_database()!
	defer {
		orm.clear_current_tenant_id()
		orm.set_tenant_filter_enabled(false)
		db.close() or {}
	}
	orm.configure_tenant_filter(orm.TenantFilterConfig{
		enabled:    true
		field_name: 'tenant_id'
	})
	orm.set_current_tenant_id(orm.Primitive(1))
	mut parents := orm.new_query[IncludeScopedParent](db)

	assert parents.where('children.name = ?', 'visible child')!.count()! == 1
	assert parents.where('children.grandkids.name = ?', 'visible grandkid')!.count()! == 1
	assert parents.where('children.name = ?', 'hidden child')!.count()! == 0
	assert parents.where('children.grandkids.name = ?', 'hidden grandkid')!.count()! == 0
	assert parents.where('children.name = ? || children.grandkids.name = ?', 'shallow child',
		'missing')!.count()! == 1

	parents.where('children.name = ?', 'hidden child')!.set('name = ?', 'updated')!.update()!
	parents.where('children.grandkids.name = ?', 'hidden grandkid')!.delete()!
	orm.set_tenant_filter_enabled(false)
	rows := parents.query()!
	assert rows.len == 1
	assert rows[0].name == 'tenant parent'
}
