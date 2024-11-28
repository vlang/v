import time { Time, now }

fn test_import_symbol_static_method() {
	now := now()
	now_2 := Time.new(now)
	assert now == now_2
}
