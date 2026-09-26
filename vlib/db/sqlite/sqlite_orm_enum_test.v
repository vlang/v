// vtest build: present_sqlite3?
import db.sqlite

enum Element {
	unset
	fire
	water
	air
	earth
}

enum Rank {
	novice = 10
	adept  = 20
	master = 30
}

struct Traits {
	element Element
	rank    Rank
}

@[table: 'mobs']
struct Mob {
	id      int @[primary; sql: serial]
	name    string
	element Element
	rank    Rank
}

@[table: 'embed_mobs']
struct EmbedMob {
	Traits
	id   int @[primary; sql: serial]
	name string
}

// TextMob is mapped onto a table that was *not* created by V, and whose enum columns
// keep the textual label of the value, the way a native PostgreSQL `ENUM` type does.
@[table: 'text_mobs']
struct TextMob {
	id      int @[primary; sql: serial]
	name    string
	element Element
	rank    Rank
}

// TextEmbedMob checks the same, for the fields of an embedded struct.
@[table: 'text_embed_mobs']
struct TextEmbedMob {
	Traits
	id   int @[primary; sql: serial]
	name string
}

fn test_enum_fields_round_trip() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	sql db {
		create table Mob
	}!
	gnome := Mob{
		name:    'gnome'
		element: .air
		rank:    .adept
	}
	imp := Mob{
		name:    'imp'
		element: .fire
		rank:    .master
	}
	sql db {
		insert gnome into Mob
	}!
	sql db {
		insert imp into Mob
	}!

	rows := sql db {
		select from Mob order by id
	}!
	assert rows.len == 2
	assert rows[0].element == .air
	assert rows[0].rank == .adept
	assert rows[1].element == .fire
	assert rows[1].rank == .master

	// enums are stored as their integer value
	raw := db.exec('select element, rank from mobs order by id')!
	assert raw[0].vals == ['3', '20']
	assert raw[1].vals == ['1', '30']

	filtered := sql db {
		select from Mob where element == Element.fire
	}!
	assert filtered.len == 1
	assert filtered[0].name == 'imp'
	assert filtered[0].element == .fire
	assert filtered[0].rank == .master
}

fn test_embedded_enum_fields_round_trip() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	sql db {
		create table EmbedMob
	}!
	orc := EmbedMob{
		Traits: Traits{
			element: .earth
			rank:    .master
		}
		name:   'orc'
	}
	sql db {
		insert orc into EmbedMob
	}!

	rows := sql db {
		select from EmbedMob
	}!
	assert rows.len == 1
	assert rows[0].element == .earth
	assert rows[0].rank == .master
}

fn test_embedded_enum_fields_read_from_text_columns() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('create table text_embed_mobs (`Traits.element` text not null, `Traits.rank` text not null, id integer primary key, name text not null)')!
	db.exec("insert into text_embed_mobs values ('water', 'novice', 1, 'nixie')")!

	rows := sql db {
		select from TextEmbedMob
	}!
	assert rows.len == 1
	assert rows[0].name == 'nixie'
	assert rows[0].element == .water
	assert rows[0].rank == .novice
}

fn test_enum_fields_read_from_text_columns() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	db.exec('create table text_mobs (id integer primary key, name text not null, element text not null, rank text not null)')!
	// the label of the enum value, like a native `ENUM` column returns it
	db.exec("insert into text_mobs values (1, 'orc', 'air', 'master')")!
	// the integer value of the enum, stored in a text column
	db.exec("insert into text_mobs values (2, 'troll', '2', '10')")!
	// a label that matches no enum value keeps the field at its default
	db.exec("insert into text_mobs values (3, 'ghost', 'plasma', 'novice')")!

	rows := sql db {
		select from TextMob order by id
	}!
	assert rows.len == 3
	assert rows[0].element == .air
	assert rows[0].rank == .master
	assert rows[1].element == .water
	assert rows[1].rank == .novice
	assert rows[2].element == .unset
	assert rows[2].rank == .novice
}
