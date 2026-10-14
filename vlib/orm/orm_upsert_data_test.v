// Coverage for the `orm` helpers that turn upsert data into SQL, and for the
// two error factories the SQL upsert path reports. Everything here is pure
// string and struct work: no `Connection`, no driver, no database.
//
// Expected values were read off the running library. `prepare_upsert` is
// asserted through the shape it produces for `QueryData`, because that is the
// contract the callers consume: which fields survive into the INSERT, and which
// conflict predicate the UPDATE uses.
import orm

// upsert_table is a plain `Table` with no attributes, the shape `upsert()` is
// called with for a struct that has no `@[unique_key]`.
fn upsert_table() orm.Table {
	return orm.new_table('users', [])
}

// upsert_row builds a single-row INSERT's worth of `QueryData`.
fn upsert_row(fields []string, values []orm.Primitive) orm.QueryData {
	return orm.QueryData{
		fields: fields
		data:   values
	}
}

fn test_new_table_keeps_its_name_and_attributes() {
	attrs := [
		VAttribute{
			name: 'comment'
			arg:  'accounts of the system'
			kind: .string
		},
	]
	table := orm.new_table('users', attrs)
	assert table.name == 'users'
	assert table.attrs.len == 1
	assert table.attrs[0].name == 'comment'
	assert table.attrs[0].arg == 'accounts of the system'
}

fn test_new_table_starts_with_no_fields_or_columns() {
	table := upsert_table()
	// Measured: `fields` and `columns` are filled by the caller as it walks the
	// struct, so the constructor leaves both empty rather than guessing.
	assert table.fields.len == 0
	assert table.columns.len == 0
}

fn test_prepare_upsert_builds_an_equality_where_for_one_conflict_group() {
	up := orm.prepare_upsert(upsert_row(['name', 'age'], [orm.Primitive('alice'), orm.Primitive(30)]), [['name']])
	assert up.valid
	assert up.insert_data.fields == ['name', 'age']
	assert up.where.fields == ['name']
	assert up.where.kinds == [.eq]
	assert up.where.data == [orm.Primitive('alice')]
	assert up.where.is_and.len == 0
	assert up.where.parentheses.len == 0
}

fn test_prepare_upsert_joins_two_conflict_groups_with_or() {
	up := orm.prepare_upsert(upsert_row(['name', 'age', 'city'], [
		orm.Primitive('alice'),
		orm.Primitive(30),
		orm.Primitive('rome'),
	]), [['name'], ['city']])
	assert up.valid
	assert up.where.fields == ['name', 'city']
	assert up.where.kinds == [.eq, .eq]
	assert up.where.data == [orm.Primitive('alice'), orm.Primitive('rome')]
	// Measured: a second group is OR'ed against the first, not AND'ed.
	assert up.where.is_and == [false]
	assert up.where.parentheses.len == 0
}

fn test_prepare_upsert_parenthesises_a_composite_conflict_group() {
	up := orm.prepare_upsert(upsert_row(['scope', 'key'], [orm.Primitive('web'),
		orm.Primitive('theme')]), [['scope', 'key']])
	assert up.valid
	assert up.where.fields == ['scope', 'key']
	assert up.where.data == [orm.Primitive('web'), orm.Primitive('theme')]
	// Measured: the members of one group are AND'ed and the whole group is
	// wrapped, so `scope = ? AND key = ?` cannot leak across an OR.
	assert up.where.is_and == [true]
	assert up.where.parentheses == [[0, 1]]
}

fn test_prepare_upsert_is_invalid_without_a_usable_conflict_group() {
	up := orm.prepare_upsert(upsert_row(['name'], [orm.Primitive('alice')]), [])
	assert !up.valid
	assert up.insert_data.fields == ['name']
	assert up.where.fields.len == 0
}

fn test_prepare_upsert_skips_conflict_groups_absent_from_the_data() {
	up := orm.prepare_upsert(upsert_row(['name'], [orm.Primitive('alice')]), [
		['email'],
		['name'],
	])
	// Measured: the unusable group is dropped and the surviving one still gives
	// a usable conflict predicate, so the upsert stays valid.
	assert up.valid
	assert up.where.fields == ['name']
	assert up.where.data == [orm.Primitive('alice')]
}

fn test_prepare_upsert_is_invalid_when_a_conflict_field_has_no_value() {
	up := orm.prepare_upsert(orm.QueryData{
		fields: ['name', 'age']
		data:   [orm.Primitive('alice')]
	}, [['age']])
	// Measured: the field name is present but row 0 has no value for it, so
	// `build_upsert_where` errors out and the result is reported unusable
	// rather than panicking.
	assert !up.valid
	assert up.insert_data.fields == ['name', 'age']
	assert up.where.fields.len == 0
}

fn test_prepare_upsert_ignores_an_empty_conflict_group() {
	up := orm.prepare_upsert(upsert_row(['name'], [orm.Primitive('alice')]), [[]])
	assert !up.valid
	assert up.where.fields.len == 0
}

fn test_prepare_upsert_drops_auto_fields_from_the_insert_data() {
	up := orm.prepare_upsert(orm.QueryData{
		fields:      ['id', 'name']
		data:        [orm.Primitive(0), orm.Primitive('alice')]
		auto_fields: [0]
	}, [['name']])
	// Measured: the `id` placeholder is left out so the database assigns it,
	// and the column indexes the conflict group refers to are the *prepared*
	// ones, not the caller's.
	assert up.valid
	assert up.insert_data.fields == ['name']
	assert up.insert_data.data == [orm.Primitive('alice')]
	assert up.where.fields == ['name']
	assert up.where.data == [orm.Primitive('alice')]
}

fn test_prepare_upsert_uses_the_first_row_of_a_bulk_insert() {
	up := orm.prepare_upsert(orm.QueryData{
		fields:     ['name', 'age']
		data:       [orm.Primitive('a'), orm.Primitive(1), orm.Primitive('b'), orm.Primitive(2)]
		batch_rows: 2
	}, [['name']])
	assert up.valid
	assert up.insert_data.batch_rows == 2
	assert up.insert_data.data.len == 4
	assert up.where.fields == ['name']
	assert up.where.data == [orm.Primitive('a')]
}

fn test_prepare_upsert_keeps_a_null_conflict_value() {
	up := orm.prepare_upsert(upsert_row(['name', 'note'], [orm.Primitive('alice'),
		orm.Primitive(orm.Null{})]), [['note']])
	assert up.valid
	assert up.where.fields == ['note']
	assert up.where.data.len == 1
	assert up.where.data[0] is orm.Null
}

fn test_upsert_count_reads_the_first_cell_of_the_first_row() {
	assert orm.upsert_count([[orm.Primitive(1)]]) == 1
	// Measured: the count arrives as whichever integer width the driver chose,
	// and all three spellings are converted.
	assert orm.upsert_count([[orm.Primitive(i64(7))]]) == 7
	assert orm.upsert_count([[orm.Primitive(u64(9))]]) == 9
}

fn test_upsert_count_reads_the_first_cell_not_the_first_row() {
	rows := [[orm.Primitive(3), orm.Primitive(4)], [orm.Primitive(5)]]
	assert orm.upsert_count(rows) == 3
}

fn test_upsert_count_is_zero_without_a_usable_cell() {
	assert orm.upsert_count([]) == 0
	assert orm.upsert_count([[]]) == 0
}

fn test_upsert_count_is_zero_for_a_non_integer_cell() {
	// Measured: a string, a float, a bool or a null all fall through to 0
	// rather than raising, because a driver returning one is a driver bug and
	// this is called on the cold path of an error.
	assert orm.upsert_count([[orm.Primitive('12')]]) == 0
	assert orm.upsert_count([[orm.Primitive(f64(12.0))]]) == 0
	assert orm.upsert_count([[orm.Primitive(true)]]) == 0
	assert orm.upsert_count([[orm.Primitive(orm.Null{})]]) == 0
}

fn test_upsert_missing_conflict_error_names_the_table() {
	if _ := orm.upsert_missing_conflict_error(upsert_table()) {
		assert false, 'upsert_missing_conflict_error must always fail'
	} else {
		assert err.msg() == 'upsert(): table `users` needs at least one primary or unique field with a concrete value'
	}
}

fn test_upsert_ambiguous_error_names_the_table() {
	if _ := orm.upsert_ambiguous_error(orm.new_table('accounts', [])) {
		assert false, 'upsert_ambiguous_error must always fail'
	} else {
		assert err.msg() == 'upsert(): upsert on table `accounts` matched multiple rows'
	}
}
