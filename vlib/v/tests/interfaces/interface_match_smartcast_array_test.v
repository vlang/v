interface Value {}

fn bytes_to_blr(param []u8) ([]u8, []u8) {
	return param.clone(), param
}

fn interface_match_call(v Value) []u8 {
	match v {
		[]u8 {
			blr, value := bytes_to_blr(v)
			assert value == [u8(1), 2, 3]
			return blr
		}
		else {
			return []u8{}
		}
	}
}

fn interface_match_return(v Value) []u8 {
	match v {
		[]u8 {
			matched := v
			return matched
		}
		else {
			return []u8{}
		}
	}
}

fn test_interface_match_array_smartcast_for_call_and_return() {
	input := Value([u8(1), 2, 3])
	assert interface_match_call(input) == [u8(1), 2, 3]
	assert interface_match_return(input) == [u8(1), 2, 3]
}
