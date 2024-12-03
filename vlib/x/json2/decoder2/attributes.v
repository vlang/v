module decoder2

const skip_field_attribute_handlers = {
	'skip':      skip_attribute_handler
	'json: -':   skip_attribute_handler
	'omitempty': omitempty_attribute_handler
}

fn skip_attribute_handler(mut decoder Decoder) bool {
	return true
}

fn omitempty_attribute_handler(mut decoder Decoder) bool {
	value_info := decoder.current_node.value
	match value_info.value_kind {
		.null {
			return true
		}
		.string_ {
			if value_info.length == 2 {
				return true
			}
		}
		.number {
			if value_info.length == 1 {
				if unsafe { vmemcmp(decoder.json.str + value_info.position, '0'.str, 1) == 0 } {
					return true
				} else if unsafe {
					vmemcmp(decoder.json.str + value_info.position, '0.0'.str, 3) == 0
				} {
					return true
				}
			}
		}
		else {
			return false
		}
	}

	return false
}
