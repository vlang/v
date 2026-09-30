module ssa

fn test_option_and_result_have_distinct_fields() {
	mut m := Module.new()
	mut b := Builder{
		m:        m
		i1_type:  m.type_store.get_int(1)
		i64_type: m.type_store.get_int(64)
	}
	option := b.option_type_id('i64', false)
	result := b.option_type_id('i64', true)
	assert option != result
	assert m.type_store.types[option].field_names == ['ok', 'value']
	assert m.type_store.types[result].field_names == ['ok', 'value', 'err']
	assert b.option_type_id('i64', false) == option
	assert b.option_type_id('i64', true) == result
}
