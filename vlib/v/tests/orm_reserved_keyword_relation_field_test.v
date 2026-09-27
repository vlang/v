// vtest build: present_sqlite3?
// Regression test: an ORM relation field named after a C/C++ reserved word must be
// escaped like any other struct member. The checker stores relation names verbatim
// in `object_var`, so `write_orm_insert_with_last_ids` used to emit `parent.operator`
// / `parent.explicit` against a struct whose members are declared `__v_operator` /
// `__v_explicit`, failing during C compilation. See orm_field_access_name usage in
// vlib/v/gen/c/orm.v.
import db.sqlite

struct RKChild {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
}

struct RKItem {
	id        int @[primary; sql: serial]
	parent_id int
	label     string
}

struct RKParent {
	id       int @[primary; sql: serial]
	name     string
	operator RKChild  @[fkey: 'parent_id'] // single relation named after a reserved word
	explicit []RKItem @[fkey: 'parent_id'] // array relation named after a reserved word
}

fn test_orm_insert_with_reserved_keyword_relation_fields() {
	mut db := sqlite.connect(':memory:')!
	sql db {
		create table RKChild
		create table RKItem
		create table RKParent
	}!
	parent := RKParent{
		name:     'root'
		operator: RKChild{
			name: 'child'
		}
		explicit: [
			RKItem{
				label: 'a'
			},
			RKItem{
				label: 'b'
			},
		]
	}
	sql db {
		insert parent into RKParent
	}!
	parents := sql db {
		select from RKParent
	}!
	assert parents.len == 1
	assert parents[0].name == 'root'
	assert parents[0].operator.name == 'child'
	assert parents[0].explicit.len == 2
	assert parents[0].explicit[0].label == 'a'
	assert parents[0].explicit[1].label == 'b'
}
