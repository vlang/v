import db.sqlite
import orm
import models

// Regression test for https://github.com/vlang/v/issues/28106 and
// https://github.com/vlang/v/issues/28107 : an ORM struct defined in a module must resolve to the
// same table name for `create table` (the compiler path) and for the runtime query builder used by
// inserts/selects. Previously the query builder kept the module prefix (`models.sendsmsrequest`),
// while `create table` stripped it (`sendsmsrequest`), so inserts failed with a missing relation.
fn test_module_prefixed_struct_table_name_is_consistent() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}

	// created as `sendsmsrequest` - the compiler strips the `models.` module prefix
	sql db {
		create table models.SendSMSRequest
	}!

	// the runtime query-builder path from the bug report - it must target the same table
	first := models.SendSMSRequest{
		msisdn: '123456'
		msg:    'Test'
		sender: 'sender'
	}
	mut qb := orm.new_query[models.SendSMSRequest](db)
	qb.insert(first)!

	rows := sql db {
		select from models.SendSMSRequest
	}!
	assert rows.len == 1
	assert rows[0].msisdn == '123456'
	assert rows[0].sender == 'sender'
}
