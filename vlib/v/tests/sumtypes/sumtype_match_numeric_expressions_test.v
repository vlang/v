struct NumericIntField {
	n int
}

struct NumericFloatField {
	n f64
}

type NumericExpressionValue = int | f64 | string

fn numeric_expression_int() int { return 5 }

fn numeric_expression_float() f64 { return 2.5 }

fn (x NumericIntField) number() int { return x.n }

fn (x NumericFloatField) number() f64 { return x.n }

fn test_sum_match_promotes_numeric_expression_arms() {
	a := NumericIntField{ n: 5 }
	b := NumericFloatField{ n: 2.5 }
	ints := [5]
	floats := [2.5]
	for k in 1 .. 3 {
		mut values := map[string]NumericExpressionValue{}
		values['selector'] = match k {
			1 { a.n }
			else { b.n }
		}
		values['call'] = match k {
			1 { numeric_expression_int() }
			else { numeric_expression_float() }
		}
		values['prefix'] = match k {
			1 { -a.n }
			else { -b.n }
		}
		values['binary'] = match k {
			1 { a.n + 1 }
			else { b.n + 0.5 }
		}
		values['index'] = match k {
			1 { ints[0] }
			else { floats[0] }
		}
		values['method'] = match k {
			1 { a.number() }
			else { b.number() }
		}
		values['local_selector'] = match k {
			1 {
				x := NumericIntField{ n: 5 }
				x.n
			}
			else {
				x := NumericFloatField{ n: 2.5 }
				x.n
			}
		}
		for key in ['selector', 'call', 'prefix', 'binary', 'index', 'method', 'local_selector'] {
			value := values[key] or { panic('missing numeric expression') }
			assert value is f64, '${k}: ${key}: ${value.type_name()}'
		}
	}
}

fn append_generic_numeric_match[T](n T, k int) []NumericExpressionValue {
	mut items := []NumericExpressionValue{}
	items << match k {
		0 { n }
		else { 2.5 }
	}
	return items
}

fn test_sum_match_promotes_specialized_generic_arms() {
	for k in 0 .. 2 {
		items := append_generic_numeric_match[int](5, k)
		value := items[0]
		assert value is f64
		if value is f64 {
			assert value == if k == 0 { 5.0 } else { 2.5 }
		}
	}
}

fn test_sum_match_promotes_nested_numeric_arms() {
	for k in 0 .. 2 {
		for j in 0 .. 2 {
			mut items := []NumericExpressionValue{}
			items << match k {
				0 {
					match j {
						0 { 1 }
						else { 2 }
					}
				}
				else { 2.5 }
			}
			value := items[0]
			assert value is f64
			assert (value as f64) == if k == 0 { f64(j + 1) } else { 2.5 }
		}
	}
}

fn append_generic_nested_numeric_match[T](n T, k int, j int, l int) []NumericExpressionValue {
	mut items := []NumericExpressionValue{}
	items << match k {
		0 {
			match j {
				0 {
					match l {
						0 { n }
						else { 6 }
					}
				}
				else { 7 }
			}
		}
		else { 2.5 }
	}
	return items
}

fn test_sum_match_promotes_deeply_nested_specialized_generic_arms() {
	for k in 0 .. 2 {
		for j in 0 .. 2 {
			for l in 0 .. 2 {
				items := append_generic_nested_numeric_match[int](5, k, j, l)
				value := items[0]
				assert value is f64
				expected := if k != 0 {
					2.5
				} else if j != 0 {
					7.0
				} else {
					f64(l + 5)
				}
				assert (value as f64) == expected
			}
		}
	}
}

fn test_sum_match_ignores_panicking_arms_during_numeric_promotion() {
	for k in 0 .. 2 {
		mut items := []NumericExpressionValue{}
		items << match k {
			0 { 5 }
			1 { 2.5 }
			else { panic('unreachable numeric arm') }
		}
		value := items[0]
		assert value is f64
		assert (value as f64) == if k == 0 { 5.0 } else { 2.5 }
	}
}

fn numeric_match_with_return(k int) []NumericExpressionValue {
	mut items := []NumericExpressionValue{}
	items << match k {
		0 { 5 }
		1 { 2.5 }
		else { return []NumericExpressionValue{} }
	}
	return items
}

fn test_sum_match_ignores_returning_arms_during_numeric_promotion() {
	for k in 0 .. 2 {
		items := numeric_match_with_return(k)
		value := items[0]
		assert value is f64
		assert (value as f64) == if k == 0 { 5.0 } else { 2.5 }
	}
	assert numeric_match_with_return(2).len == 0
}
