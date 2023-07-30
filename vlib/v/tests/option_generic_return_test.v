fn opt_parse[T]() ?T {
	mut cfg := T{}
	opt_set_val(mut cfg)?
	return cfg
}

fn opt_set_val[T](mut cfg T) ? {
	$for field in T.fields {
		$if field.is_array {
			mut arr := cfg.$(field.name)
			cfg.$(field.name) = opt_add_val(mut arr)?
		}
	}
}

fn opt_add_val[T](mut arr []T) ?[]T {
	return arr
}

fn res_parse[T]() !T {
	mut cfg := T{}
	res_set_val(mut cfg)!
	return cfg
}

fn res_set_val[T](mut cfg T) ! {
	$for field in T.fields {
		$if field.is_array {
			mut arr := cfg.$(field.name)
			cfg.$(field.name) = res_add_val(mut arr)!
		}
	}
}

fn res_add_val[T](mut arr []T) ![]T {
	return arr
}

struct Strings {
	val []string
}

struct Ints {
	val []int
}

fn test_option() {
	assert dump(opt_parse[Strings]()?) == Strings{}
	assert dump(opt_parse[Ints]()?) == Ints{}
}

fn test_result() {
	assert dump(res_parse[Strings]()!) == Strings{}
	assert dump(res_parse[Ints]()!) == Ints{}
}
