// vtest retry: 3
import db.sqlite

struct Person {
	id                int @[primary; sql: serial]
	age               int
	brothers          []Brother @[fkey: 'person_id']
	sisters           []Sister  @[fkey: 'person_id']
	field_after_fkeys string
}

struct Brother {
	id        int @[primary; sql: serial]
	person_id int
	name      string
}

struct Sister {
	id        int @[primary; sql: serial]
	person_id int
	name      string
}

fn test_field_after_fkeys() {
	db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Brother
		create table Sister
		create table Person
	}!

	person := Person{
		age:               21
		brothers:          [Brother{
			name: 'aaa'
		}, Brother{
			name: 'bbb'
		}]
		sisters:           [Sister{
			name: 'ccc'
		}, Sister{
			name: 'ddd'
		}]
		field_after_fkeys: 'eee'
	}

	sql db {
		insert person into Person
	}!

	persons := sql db {
		select from Person
	}!
	assert persons[0].age == 21
	assert persons[0].field_after_fkeys == 'eee'
}

@[table: 'foos']
struct Foo {
	id       int @[primary; sql: serial]
	name     string
	children []Child @[fkey: 'parent_id']
}

struct Child {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
}

fn test_fkey_insert_as_assignment_expr() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Foo
		create table Child
	}!

	foo := Foo{
		name:     'abc'
		children: [
			Child{
				name: 'abc'
			},
			Child{
				name: 'def'
			},
		]
	}
	// use insert as an assigment expr
	_ := sql db {
		insert foo into Foo
	}!

	result := sql db {
		select from Foo where id == 1
	}!
	assert result[0].children.len == 2

	res := sql db {
		select from Child
	}!
	assert res.len == 2
}

enum ScalarFkeyKind {
	zero
	one
	two
}

struct ScalarFkeyParent {
	id                int                       @[primary]
	u32_children      []ScalarFkeyU32Child      @[fkey: 'parent_id']
	optional_children []ScalarFkeyOptionalChild @[fkey: 'parent_id']
	bool_children     []ScalarFkeyBoolChild     @[fkey: 'parent_id']
	f32_children      []ScalarFkeyF32Child      @[fkey: 'parent_id']
	enum_children     []ScalarFkeyEnumChild     @[fkey: 'parent_id']
}

struct ScalarFkeyU32Child {
	id        int @[primary]
	parent_id u32
}

struct ScalarFkeyOptionalChild {
	id        int @[primary]
	parent_id ?u16
}

struct ScalarFkeyBoolChild {
	id        int @[primary]
	parent_id bool
}

struct ScalarFkeyF32Child {
	id        int @[primary]
	parent_id f32
}

struct ScalarFkeyEnumChild {
	id        int @[primary]
	parent_id ScalarFkeyKind
}

fn test_insert_child_relations_assigns_scalar_fkey_types() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table ScalarFkeyU32Child
		create table ScalarFkeyOptionalChild
		create table ScalarFkeyBoolChild
		create table ScalarFkeyF32Child
		create table ScalarFkeyEnumChild
		create table ScalarFkeyParent
	}!

	parent := ScalarFkeyParent{
		id:                2
		u32_children:      [ScalarFkeyU32Child{
			id: 1
		}]
		optional_children: [ScalarFkeyOptionalChild{
			id: 1
		}]
		bool_children:     [ScalarFkeyBoolChild{
			id: 1
		}]
		f32_children:      [ScalarFkeyF32Child{
			id: 1
		}]
		enum_children:     [ScalarFkeyEnumChild{
			id: 1
		}]
	}

	sql db {
		insert parent into ScalarFkeyParent
	}!

	u32_children := sql db {
		select from ScalarFkeyU32Child
	}!
	assert u32_children[0].parent_id == u32(2)

	optional_children := sql db {
		select from ScalarFkeyOptionalChild
	}!
	optional_parent_id := optional_children[0].parent_id or { u16(0) }
	assert optional_parent_id == u16(2)

	bool_children := sql db {
		select from ScalarFkeyBoolChild
	}!
	assert bool_children[0].parent_id

	f32_children := sql db {
		select from ScalarFkeyF32Child
	}!
	assert f32_children[0].parent_id == f32(2.0)

	enum_children := sql db {
		select from ScalarFkeyEnumChild
	}!
	assert enum_children[0].parent_id == .two
}

struct Foo2 {
	id       int @[primary; sql: serial]
	name     string
	children []Child2 @[fkey: 'parent_id']
}

struct Child2 {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
	bar       ?Bar2 @[fkey: 'child_id']
}

struct Bar2 {
	id       int @[primary; sql: serial]
	child_id int
	name     string
}

fn test_double_fkey_insert() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Foo2
		create table Child2
		create table Bar2
	}!

	child_one := Child2{
		name: 'abc'
	}

	child_two := Child2{
		name: 'def'
	}

	bar_one := Bar2{
		id:   0
		name: 'name'
	}

	foo := Foo2{
		name:     'abc'
		children: [
			child_one,
			child_two,
		]
	}
	_ := sql db {
		insert foo into Foo2
	}!

	result := sql db {
		select from Foo2 where id == 1
	}!
	assert result[0].children.len == 2

	res := sql db {
		select from Child2
	}!
	assert res.len == 2
}
