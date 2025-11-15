struct Maps {
	a map[string]int
	b map[string]bool
	c map[string]f64
}

fn decode_struct[T](t T) []string {
	mut decoded := []string{}
	$for f in T.fields {
		$if f is $map {
			k, v := map_key_value(t.$(f.name))
			decoded << '${typeof(k).name} ${typeof(v).name}'
		}
	}
	return decoded
}

fn map_key_value[K, V](m map[K]V) (K, V) {
	return K{}, V{}
}

fn test_main() {
	assert decode_struct(Maps{}) == ['string int', 'string bool', 'string f64']
}
