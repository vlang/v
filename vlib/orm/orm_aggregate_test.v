// vtest retry: 3
// vtest build: present_sqlite3? && !windows
import orm
import time
import db.sqlite

struct AggregateEntry {
	id      int @[primary; sql: serial]
	age     int
	score   f64
	label   string
	created time.Time
}

fn test_sql_orm_aggregates() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table AggregateEntry
	}!

	first_created := time.unix(1_700_000_000)
	second_created := time.unix(1_700_000_100)
	third_created := time.unix(1_700_000_200)
	entries := [
		AggregateEntry{
			age:     20
			score:   7.5
			label:   'bravo'
			created: second_created
		},
		AggregateEntry{
			age:     30
			score:   8.25
			label:   'alpha'
			created: first_created
		},
		AggregateEntry{
			age:     40
			score:   9.75
			label:   'charlie'
			created: third_created
		},
	]

	for entry in entries {
		sql db {
			insert entry into AggregateEntry
		}!
	}

	total_age := sql db {
		select sum(age) from AggregateEntry
	}!
	if value := total_age {
		assert value == 90
	} else {
		assert false
	}

	average_age := sql db {
		select avg(age) from AggregateEntry where age >= 20
	}!
	if value := average_age {
		assert value == 30.0
	} else {
		assert false
	}

	min_label := sql db {
		select min(label) from AggregateEntry
	}!
	if value := min_label {
		assert value == 'alpha'
	} else {
		assert false
	}

	max_created := sql db {
		select max(created) from AggregateEntry
	}!
	if value := max_created {
		assert value == third_created
	} else {
		assert false
	}

	empty_sum := sql db {
		select sum(age) from AggregateEntry where age > 100
	}!
	assert empty_sum == none

	empty_avg := sql db {
		select avg(score) from AggregateEntry where age > 100
	}!
	assert empty_avg == none
}

fn test_query_builder_aggregates() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	mut qb := orm.new_query[AggregateEntry](db)
	qb.create()!

	first_created := time.unix(1_800_000_000)
	second_created := time.unix(1_800_000_100)
	entries := [
		AggregateEntry{
			age:     10
			score:   1.5
			label:   'delta'
			created: second_created
		},
		AggregateEntry{
			age:     25
			score:   2.5
			label:   'beta'
			created: first_created
		},
	]

	qb.insert_many(entries)!

	assert qb.count()! == 2

	sum_age := qb.sum('age')!
	assert sum_age.has_value
	assert sum_age.as_int()? == 35

	avg_score := qb.avg('score')!
	assert avg_score.has_value
	assert avg_score.as_f64()? == 2.0

	min_label := qb.min('label')!
	assert min_label.has_value
	assert min_label.as_string()? == 'beta'

	max_created := qb.max('created')!
	assert max_created.has_value
	assert max_created.as_time()? == second_created

	empty_max := qb.where('age > ?', 100)!.max('age')!
	assert !empty_max.has_value

	if _ := qb.sum('label') {
		assert false
	} else {
		assert err.msg().contains('requires a numeric field')
	}
}
