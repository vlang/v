fn func[T](val T) string {
	mut result := ''
	$match T {
		int {
			result += 'int'
		}
		f64 {
			result += 'f64'
		}
		string {
			result += 'string'
		}
		$else {
			result += 'unknown'
		}
	}

	$match val {
		int {
			result += ',int'
		}
		f64 {
			result += ',f64'
		}
		string {
			result += ',string'
		}
		$else {
			result += ',unknown'
		}
	}
	return result
}

fn test_comptime_match_generic_type() {
	assert func(100) == 'int,int'
	assert func(1.1) == 'f64,f64'
	assert func('1') == 'string,string'
	assert func(`a`) == 'unknown,unknown'
}
