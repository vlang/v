// vtest retry: 3
import db.sqlite
import time

struct Account {
	id     int @[primary; sql: serial]
	name   string
	year   int
	mod_at string
}

struct Holder {
	name string
}

fn (h Holder) upper() string {
	return h.name.to_upper()
}

fn make_holder(name string) Holder {
	return Holder{
		name: name
	}
}

fn holder_upper(h Holder) string {
	return h.upper()
}

fn day_text(days int) string {
	return time.unix(0).add_days(days).format_ss()
}

fn make_names() []string {
	return ['first', 'second']
}

fn make_years() []int {
	return [1999, 2000]
}

fn test_update_set_values_with_chained_calls() {
	mut db := sqlite.connect(':memory:')!
	sql db {
		create table Account
	}!
	account := Account{
		name: 'first'
	}
	sql db {
		insert account into Account
	}!

	sql db {
		update Account set mod_at = time.now().format_ss() where id == 1
	}!
	mut rows := sql db {
		select from Account where id == 1
	}!
	assert rows[0].mod_at.len == 'YYYY-MM-DD HH:mm:ss'.len

	sql db {
		update Account set mod_at = time.unix(0).add_days(1).format_ss(), year = time.unix(0).year
		where id == 1
	}!
	rows = sql db {
		select from Account where id == 1
	}!
	assert rows[0].mod_at == time.unix(0).add_days(1).format_ss()
	assert rows[0].year == 1970

	names := ['second', 'third']
	sql db {
		update Account set name = make_holder(names[0]).upper() where id == 1
	}!
	rows = sql db {
		select from Account where id == 1
	}!
	assert rows[0].name == make_holder('second').upper()

	sql db {
		update Account set name = names[1].to_upper() where id == 1
	}!
	rows = sql db {
		select from Account where id == 1
	}!
	assert rows[0].name == 'THIRD'

	sql db {
		update Account set mod_at = (time.unix(0)).format_ss(), name = (names[0]).to_upper()
		where id == 1
	}!
	rows = sql db {
		select from Account where id == 1
	}!
	assert rows[0].mod_at == time.unix(0).format_ss()
	assert rows[0].name == 'SECOND'

	sql db {
		update Account set name = make_names()[1].to_upper(), year = -1 where id == 1
	}!
	rows = sql db {
		select from Account where id == 1
	}!
	assert rows[0].name == 'SECOND'
	assert rows[0].year == -1

	sql db {
		update Account set year = 1 + make_years()[1] - -1 where id == 1
	}!
	rows = sql db {
		select from Account where id == 1
	}!
	assert rows[0].year == 2002
}

fn test_where_values_with_chained_calls() {
	mut db := sqlite.connect(':memory:')!
	sql db {
		create table Account
	}!
	first := Account{
		name:   'FIRST'
		mod_at: time.unix(0).format_ss()
	}
	second := Account{
		name:   'second'
		mod_at: time.unix(0).add_days(1).format_ss()
	}
	sql db {
		insert first into Account
		insert second into Account
	}!

	by_time := sql db {
		select from Account where mod_at == time.unix(0).format_ss()
	}!
	assert by_time.len == 1
	assert by_time[0].name == 'FIRST'

	by_call := sql db {
		select from Account where name == make_holder('first').upper()
	}!
	assert by_call.len == 1
	assert by_call[0].mod_at == first.mod_at

	by_args := sql db {
		select from Account where mod_at == day_text(1)
	}!
	assert by_args.len == 1
	assert by_args[0].name == 'second'

	by_nested_call := sql db {
		select from Account where name == holder_upper(make_holder('first'))
	}!
	assert by_nested_call.len == 1
	assert by_nested_call[0].mod_at == first.mod_at

	by_wrapped_receiver := sql db {
		select from Account where mod_at == (time.unix(0)).add_days(1).format_ss()
	}!
	assert by_wrapped_receiver.len == 1
	assert by_wrapped_receiver[0].name == 'second'

	by_indexed_call := sql db {
		select from Account where name == make_names()[1]
	}!
	assert by_indexed_call.len == 1
	assert by_indexed_call[0].mod_at == second.mod_at

	by_signed_arg := sql db {
		select from Account where mod_at > day_text(-1) && mod_at < day_text(1)
	}!
	assert by_signed_arg.len == 1
	assert by_signed_arg[0].name == 'FIRST'
}
