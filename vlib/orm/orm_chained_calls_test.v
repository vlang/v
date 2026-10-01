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

fn load_holder(name string) ?Holder {
	if name == '' {
		return none
	}
	return make_holder(name)
}

fn find_holder(name string) !Holder {
	if name == '' {
		return error('empty holder name')
	}
	return make_holder(name)
}

fn load_name(name string) ?string {
	if name == '' {
		return none
	}
	return name
}

fn find_name(name string) !string {
	if name == '' {
		return error('empty name')
	}
	return name
}

fn day_text(days int) string {
	return time.unix(0).add_days(days).format_ss()
}

fn make_names() []string {
	return ['first', 'second']
}

type HolderNames = []string

fn (names HolderNames) clone() []Holder {
	return [make_holder(names[0])]
}

type WrappedHolderNames = HolderNames

fn make_wrapped_holder_names() WrappedHolderNames {
	return WrappedHolderNames(HolderNames(['FIRST']))
}

fn HolderNames.make() WrappedHolderNames {
	return make_wrapped_holder_names()
}

fn make_holders() []Holder {
	return [make_holder('first'), make_holder('second')]
}

fn make_holder_groups() [][]Holder {
	return [make_holders()]
}

struct HolderBox {
	holder Holder
}

fn make_holder_box() HolderBox {
	return HolderBox{ holder: make_holder('first') }
}

fn load_holders() ?[]Holder {
	return none
}

struct ValueBox[T] {
	value T
}

fn (box ValueBox[T]) get() T {
	return box.value
}

fn make_value_box() ValueBox[string] {
	return ValueBox[string]{ value: 'FIRST' }
}

fn make_years() []int {
	return [1999, 2000]
}

enum Case {
	lower
	upper
}

fn cased(name string, c Case) string {
	return if c == .upper { name.to_upper() } else { name }
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
		update Account set mod_at = time.unix(0).get_fmt_time_str(.hhmm24), name = cased((make_names())[0], .upper)
		where id == 1
	}!
	rows = sql db {
		select from Account where id == 1
	}!
	assert rows[0].mod_at == '00:00'
	assert rows[0].name == 'FIRST'

	sql db {
		update Account set year = 1 + make_years()[1] - -1 where id == 1
	}!
	rows = sql db {
		select from Account where id == 1
	}!
	assert rows[0].year == 2002

	sql db {
		update Account set name = (load_holder('') or { make_holder('fourth') }).upper() where id == 1
	}!
	rows = sql db {
		select from Account where id == 1
	}!
	assert rows[0].name == 'FOURTH'
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

	by_generic_method := sql db {
		select from Account where name == make_names().last()
	}!
	assert by_generic_method.len == 1
	assert by_generic_method[0].mod_at == second.mod_at

	by_collection_method := sql db {
		select from Account where name == make_names().reverse()[0] && name == make_names().clone()[1]
	}!
	assert by_collection_method.len == 1
	assert by_collection_method[0].mod_at == second.mod_at

	by_inherited_alias_method := sql db {
		select from Account where name == make_wrapped_holder_names().clone()[0].name
	}!
	assert by_inherited_alias_method.len == 1
	assert by_inherited_alias_method[0].mod_at == first.mod_at

	wrapped_names := make_wrapped_holder_names()
	by_local_inherited_alias_method := sql db {
		select from Account where name == wrapped_names.clone()[0].name
	}!
	assert by_local_inherited_alias_method.len == 1
	assert by_local_inherited_alias_method[0].mod_at == first.mod_at

	by_grouped_inherited_alias_method := sql db {
		select from Account where name == (wrapped_names).clone()[0].name
	}!
	assert by_grouped_inherited_alias_method.len == 1
	assert by_grouped_inherited_alias_method[0].mod_at == first.mod_at

	declared_names := HolderNames(['FIRST'])
	by_declared_alias_method := sql db {
		select from Account where name == declared_names.clone()[0].name
	}!
	assert by_declared_alias_method.len == 1
	assert by_declared_alias_method[0].mod_at == first.mod_at

	by_static_alias_call := sql db {
		select from Account where name == HolderNames.make().clone()[0].name
	}!
	assert by_static_alias_call.len == 1
	assert by_static_alias_call[0].mod_at == first.mod_at

	by_grouped_option := sql db {
		select from Account where name == (load_name('FIRST')) or { 'missing' }
	}!
	assert by_grouped_option.len == 1
	assert by_grouped_option[0].mod_at == first.mod_at

	by_grouped_result := sql db {
		select from Account where name == (find_name('')) or { 'second' }
	}!
	assert by_grouped_result.len == 1
	assert by_grouped_result[0].mod_at == second.mod_at

	by_signed_arg := sql db {
		select from Account where mod_at > day_text(-1) && mod_at < day_text(1)
	}!
	assert by_signed_arg.len == 1
	assert by_signed_arg[0].name == 'FIRST'

	by_wrapped_call_receiver := sql db {
		select from Account where name == (make_holder('first')).upper()
	}!
	assert by_wrapped_call_receiver.len == 1
	assert by_wrapped_call_receiver[0].mod_at == first.mod_at

	by_wrapped_index_base := sql db {
		select from Account where name == (make_names())[1] && mod_at > day_text(0).all_before(' ')
	}!
	assert by_wrapped_index_base.len == 1
	assert by_wrapped_index_base[0].mod_at == second.mod_at

	fallback := make_holder('second')
	by_option_receiver := sql db {
		select from Account where name == (load_holder('first') or { fallback }).upper()
	}!
	assert by_option_receiver.len == 1
	assert by_option_receiver[0].mod_at == first.mod_at

	by_result_fallback := sql db {
		select from Account where name == (find_holder('') or { make_holder('first') }).upper()
	}!
	assert by_result_fallback.len == 1
	assert by_result_fallback[0].mod_at == first.mod_at

	by_indexed_holder_field := sql db {
		select from Account where name == make_holders()[0].name.to_upper()
	}!
	assert by_indexed_holder_field.len == 1
	assert by_indexed_holder_field[0].mod_at == first.mod_at

	by_nested_indexed_holder := sql db {
		select from Account where name == make_holder_groups()[0][0].upper()
	}!
	assert by_nested_indexed_holder.len == 1
	assert by_nested_indexed_holder[0].mod_at == first.mod_at

	by_nonprimitive_field_receiver := sql db {
		select from Account where name == make_holder_box().holder.upper()
	}!
	assert by_nonprimitive_field_receiver.len == 1
	assert by_nonprimitive_field_receiver[0].mod_at == first.mod_at

	fallback_holders := make_holders()
	by_option_indexed_holder := sql db {
		select from Account where name == (load_holders() or { fallback_holders })[0].upper()
	}!
	assert by_option_indexed_holder.len == 1
	assert by_option_indexed_holder[0].mod_at == first.mod_at

	by_generic_receiver := sql db {
		select from Account where name == make_value_box().get()
	}!
	assert by_generic_receiver.len == 1
	assert by_generic_receiver[0].mod_at == first.mod_at
}
