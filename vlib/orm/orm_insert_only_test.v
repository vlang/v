// vtest build: present_sqlite3?
import db.sqlite
import orm
import time

struct InsertOnlyAudit {
mut:
	creator string @[sql_insert_only; sql: 'created_by']
}

struct InsertOnlyRecord {
	InsertOnlyAudit
mut:
	id         int       @[primary; sql_insert_only]
	created_at time.Time @[sql_insert_only]
	note       ?string   @[sql_insert_only]
	status     string    @[sql_insert_only; default: '`new`']
	name       string
}

struct InsertOnlyKey {
mut:
	id    int     @[primary; sql_insert_only]
	value ?string @[sql_insert_only]
}

fn test_insert_only_fields_allow_inserts_reads_filters_and_memory_mutation() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlyRecord }!
	mut record := InsertOnlyRecord{
		InsertOnlyAudit: InsertOnlyAudit{ creator: 'alex' }
		id:              1
		created_at:      time.unix(100)
		name:            'first'
	}
	sql db { insert record into InsertOnlyRecord }!
	record.creator = 'changed in memory'
	record.note = 'local'
	assert record.creator == 'changed in memory'
	sql db {
		update InsertOnlyRecord set name = 'updated' where id == 1 && creator == 'alex'
	}!
	rows := sql db {
		select from InsertOnlyRecord where creator == 'alex' order by created_at
	}!
	assert rows.len == 1
	assert rows[0].id == 1
	assert rows[0].name == 'updated'
	assert rows[0].created_at == time.unix(100)
	assert rows[0].note == none
	assert rows[0].status == 'new'
}

fn test_dynamic_guards_and_where_can_read_insert_only_fields() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlyUpsertRecord }!
	record := InsertOnlyUpsertRecord{ id: 1, creator: 'alex', name: 'before' }
	sql db { insert record into InsertOnlyUpsertRecord }!
	creator := 'alex'
	assignments := {
		if creator != '' {
			name == creator
		}
	}
	sql db {
		dynamic update InsertOnlyUpsertRecord set assignments where creator == creator
	}!
	sql db {
		dynamic update InsertOnlyUpsertRecord set {
			if creator == 'alex' {
				name == 'after'
			}
		} where id == 1
	}!
	rows := sql db { select from InsertOnlyUpsertRecord where creator == creator }!
	assert rows[0].creator == 'alex'
	assert rows[0].name == 'after'
}

fn test_save_omits_insert_only_embedded_nullable_and_default_fields() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlyRecord }!
	record := InsertOnlyRecord{
		InsertOnlyAudit: InsertOnlyAudit{ creator: 'alex' }
		id:              1
		created_at:      time.unix(100)
		name:            'before'
	}
	sql db { insert record into InsertOnlyRecord }!
	mut changed := record
	changed.creator = 'other'
	changed.created_at = time.unix(200)
	changed.note = 'filled later'
	changed.status = 'changed'
	changed.name = 'after'
	orm.save(db, changed)!
	rows := sql db { select from InsertOnlyRecord where id == 1 }!
	assert rows[0].creator == 'alex'
	assert rows[0].created_at == time.unix(100)
	assert rows[0].note == none
	assert rows[0].status == 'new'
	assert rows[0].name == 'after'
}

fn test_bulk_insert_and_automatic_update_preserve_insert_only_fields() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlyRecord }!
	records := [
		InsertOnlyRecord{ InsertOnlyAudit: InsertOnlyAudit{ creator: 'one' }, id: 1, name: 'one' },
		InsertOnlyRecord{ InsertOnlyAudit: InsertOnlyAudit{ creator: 'two' }, id: 2, name: 'two' },
	]
	sql db { insert records into InsertOnlyRecord }!
	updates := [
		InsertOnlyRecord{ InsertOnlyAudit: InsertOnlyAudit{ creator: 'changed' }, id: 1, name: 'first' },
		InsertOnlyRecord{ InsertOnlyAudit: InsertOnlyAudit{ creator: 'changed' }, id: 2, name: 'second' },
	]
	orm.update_many(mut db, updates, 'id')!
	rows := sql db { select from InsertOnlyRecord order by id }!
	assert rows[0].creator == 'one'
	assert rows[1].creator == 'two'
	assert rows[0].name == 'first'
	assert rows[1].name == 'second'
	mut rejected_updates := updates.clone()
	rejected_updates[0].name = 'must not be written'
	rejected_updates[1].name = 'must not be written'
	for field in ['creator', 'created_by', 'InsertOnlyAudit.creator', 'note', 'id'] {
		orm.update_many(mut db, rejected_updates, 'id', 'name', field) or {
			assert err.msg().contains('insert-only field `InsertOnlyRecord.')
			continue
		}
		assert false, 'explicit protected bulk assignment must fail'
	}
	unchanged := sql db { select from InsertOnlyRecord order by id }!
	assert unchanged[0].creator == 'one'
	assert unchanged[1].creator == 'two'
	assert unchanged[0].name == 'first'
	assert unchanged[1].name == 'second'
}

fn insert_only_runtime_data(field string) orm.QueryData {
	return orm.QueryData{
		fields: ['name', field]
		data:   [orm.Primitive('must not be written'), orm.Primitive('changed')]
	}
}

fn test_runtime_query_data_rejects_protected_assignments_before_writing() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlyRecord }!
	record := InsertOnlyRecord{
		InsertOnlyAudit: InsertOnlyAudit{ creator: 'alex' }
		id:              1
		name:            'original'
	}
	sql db { insert record into InsertOnlyRecord }!
	for field in ['creator', 'created_by', 'CREATED_BY', 'InsertOnlyAudit.creator', 'note'] {
		data := insert_only_runtime_data(field)
		sql db {
			dynamic update InsertOnlyRecord set data where id == 1
		} or {
			assert err.msg().contains('insert-only field `InsertOnlyRecord.')
			continue
		}
		assert false, 'runtime protected assignment must fail'
	}
	rows := sql db { select from InsertOnlyRecord where id == 1 }!
	assert rows[0].name == 'original'
	assert rows[0].creator == 'alex'
	assert rows[0].note == none
}

fn test_function_call_update_rejects_insert_only_field() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlyRecord }!
	record := InsertOnlyRecord{ id: 1, name: 'original' }
	sql db { insert record into InsertOnlyRecord }!
	mut query := orm.new_query[InsertOnlyRecord](db)
	query.set('name=?,created_by=?', 'changed', 'other')!.where('id = ?', 1)!.update() or {
		assert err.msg().contains('insert-only field `InsertOnlyRecord.InsertOnlyAudit.creator`')
		rows := sql db { select from InsertOnlyRecord where id == 1 }!
		assert rows[0].name == 'original'
		return
	}
	assert false, 'function call update must reject protected fields'
}

fn test_upsert_inserts_protected_values_and_preserves_them_on_conflict() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlyKey }!
	first := InsertOnlyKey{ id: 1, value: 'original' }
	sql db { upsert first into InsertOnlyKey }!
	replacement := InsertOnlyKey{ id: 1, value: 'replacement' }
	sql db { upsert replacement into InsertOnlyKey }!
	rows := sql db { select from InsertOnlyKey where id == 1 }!
	assert rows.len == 1
	assert rows[0].value or { '' } == 'original'
}

struct InsertOnlyUpsertRecord {
mut:
	id      int    @[primary; sql_insert_only]
	creator string @[sql_insert_only; sql: 'created_by']
	name    string
}

fn test_upsert_updates_only_writable_columns() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlyUpsertRecord }!
	first := InsertOnlyUpsertRecord{ id: 1, creator: 'alex', name: 'before' }
	sql db { upsert first into InsertOnlyUpsertRecord }!
	replacement := InsertOnlyUpsertRecord{ id: 1, creator: 'other', name: 'after' }
	sql db { upsert replacement into InsertOnlyUpsertRecord }!
	rows := sql db { select from InsertOnlyUpsertRecord where id == 1 }!
	assert rows[0].creator == 'alex'
	assert rows[0].name == 'after'
}

fn test_empty_save_and_bulk_update_return_errors() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlyKey }!
	record := InsertOnlyKey{ id: 1 }
	sql db { insert record into InsertOnlyKey }!
	mut rejected := 0
	orm.save(db, record) or {
		assert err.msg().contains('no updatable fields')
		rejected++
	}
	orm.update_many(mut db, [record], 'id') or {
		assert err.msg().contains('no updatable fields')
		rejected++
	}
	assert rejected == 2
}

struct InsertOnlySerialRecord {
	id   int @[primary; sql: serial; sql_insert_only]
	name string
}

fn test_insert_only_serial_primary_keys_are_generated() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table InsertOnlySerialRecord }!
	record := InsertOnlySerialRecord{ name: 'original' }
	sql db { insert record into InsertOnlySerialRecord }!
	id := db.last_id()
	assert id > 0
	sql db { update InsertOnlySerialRecord set name = 'changed' where id == id }!
	rows := sql db { select from InsertOnlySerialRecord where id == id }!
	assert rows[0].id == id
	assert rows[0].name == 'changed'
}
