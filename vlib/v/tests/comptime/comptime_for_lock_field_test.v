struct MyS {
	a     int
	cache shared map[u64]string
}

fn encode[T](val T) string {
	$for field in T.fields {
		$if field.is_shared {
			rlock val.$(field.name) {
				return field.name
			}
		}
	}
	return ''
}

fn test_comptime_for_lock_field() {
	assert encode(MyS{}) == 'cache'
}
