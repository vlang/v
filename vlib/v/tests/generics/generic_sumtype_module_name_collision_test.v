import sum_name_collision

type Thing = string | bool
type OtherThing = string | bool

fn test_generic_sum_cast_keeps_callers_same_named_type() {
	value := sum_name_collision.to_sum[Thing]('direct')
	assert value is string
	if value is string {
		assert value == 'direct'
	}
	reflected := sum_name_collision.to_sum_comptime[Thing]('reflected')
	assert reflected is string
	if reflected is string {
		assert reflected == 'reflected'
	}
}

fn test_generic_sum_cast_module_and_distinct_name_controls() {
	foreign := sum_name_collision.to_sum[sum_name_collision.Thing]('foreign')
	assert foreign is string
	if foreign is string {
		assert foreign == 'foreign'
	}
	local := sum_name_collision.local_sum('local')
	assert local is string
	if local is string {
		assert local == 'local'
	}
	other := sum_name_collision.to_sum[OtherThing]('other')
	assert other is string
	if other is string {
		assert other == 'other'
	}
}
