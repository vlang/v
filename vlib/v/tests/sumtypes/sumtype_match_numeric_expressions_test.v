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
