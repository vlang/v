module json2

import time

fn copy_type[T](_t T) T {
	return T{}
}

fn get_decoded_sumtype_workaround[T](mut decoder Decoder, initialized_sumtype T) !T {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype !is $option {
					mut val := copy_type(initialized_sumtype)
					decode_value(mut decoder, mut val)!
					return T(val)
				} $else {
					if decoder.current_node.value.value_kind == .null {
						decoder.current_node = decoder.current_node.next
						return T(initialized_sumtype)
					} else {
						decoder.decode_error('sumtype option only support decoding null->none (for now)')!
					}
				}
			}
		}
	}
	decoder.decode_error('could not decode resolved sumtype (should not happen)')!
	return initialized_sumtype // suppress compiler error
}

fn check_element_type_valid[T](mut decoder Decoder, element T, current_node &ValueInfoNode) bool {
	if current_node == unsafe { nil } {
		$if element is $array || element is $map {
			return false
		}
		return true
	}

	$if element is $sumtype { // this will always match the first sumtype array/map
		return true
	}

	match current_node.value.value_kind {
		.string {
			$if element is string {
				return true
			} $else $if element is time.Time {
				return true
			} $else $if element is StringDecoder {
				return true
			}
		}
		.number {
			$if element is $float {
				return true
			} $else $if element is $int {
				return true
			} $else $if element is $enum {
				return true
			} $else $if element is NumberDecoder {
				return true
			}
		}
		.boolean {
			$if element is bool {
				return true
			} $else $if element is BooleanDecoder {
				return true
			}
		}
		.null {
			$if element is $option {
				return true
			} $else $if element is NullDecoder {
				return true
			}
		}
		.array {
			$if element is $array {
				return check_array_type_valid(mut decoder, element, current_node.next)
			}
		}
		.object {
			$if element is $map {
				if current_node.next != unsafe { nil } {
					return check_map_type_valid(mut decoder, element, current_node.next.next)
				} else {
					return check_map_type_valid(mut decoder, element, unsafe { nil })
				}
			} $else $if element is $struct {
				return check_struct_type_valid(mut decoder, element, current_node)
			}
		}
	}

	return false
}

fn get_array_element_type[T](_arr []T) T {
	return T{}
}

fn check_array_type_valid[T](mut decoder Decoder, arr []T, current_node &ValueInfoNode) bool {
	return check_element_type_valid(mut decoder, get_array_element_type(arr), current_node)
}

fn get_array_type_workaround[T](mut decoder Decoder, initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $array {
					return check_element_type_valid(mut decoder, initialized_sumtype,
						decoder.current_node)
				}
			}
		}
	}
	return false
}

fn get_map_element_type[U, V](_m map[U]V) V {
	return V{}
}

fn check_map_type_valid[T](mut decoder Decoder, m T, current_node &ValueInfoNode) bool {
	element := get_map_element_type(m)
	return check_element_type_valid(mut decoder, element, current_node)
}

fn check_map_empty_valid[T](mut decoder Decoder, m T) bool {
	element := get_map_element_type(m)
	return check_element_type_valid(mut decoder, element, unsafe { nil })
}

fn get_map_type_workaround[T](mut decoder Decoder, initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $map {
					val := copy_type(initialized_sumtype)
					if decoder.current_node.next != unsafe { nil } {
						return check_map_type_valid(mut decoder, val, decoder.current_node.next.next)
					} else {
						return check_map_type_valid(mut decoder, val, unsafe { nil })
					}
				}
			}
		}
	}
	return false
}

fn (decoder &Decoder) get_sumtype_type_field_node(current_node &ValueInfoNode) &ValueInfoNode {
	if current_node == unsafe { nil } {
		return unsafe { nil }
	}

	// find "_type" field in json object
	mut type_field_node := current_node.next
	map_position := current_node.value.position
	map_end := map_position + current_node.value.length

	type_field := '"_type"'

	for {
		if type_field_node == unsafe { nil } {
			break
		}

		key_info := type_field_node.value

		if key_info.position >= map_end {
			type_field_node = unsafe { nil }
			break
		}

		if unsafe {
			key_info.position + type_field.len <= decoder.json.len
				&& 0 == vmemcmp(decoder.json.str + key_info.position, type_field.str, type_field.len)
		} {
			// find type field
			return type_field_node.next
		} else {
			type_field_node = type_field_node.next
		}
	}

	return unsafe { nil }
}

fn check_struct_type_valid[T](mut decoder Decoder, s T, current_node &ValueInfoNode) bool {
	type_field_node := decoder.get_sumtype_type_field_node(current_node)
	if type_field_node == unsafe { nil } {
		return false
	}

	variant_name := typeof(s).name
	if type_field_node.value.length - 2 == variant_name.len {
		unsafe {
		}
		if unsafe {
			type_field_node.value.position + 1 + variant_name.len <= decoder.json.len
				&& 0 == vmemcmp(decoder.json.str + type_field_node.value.position + 1, variant_name.str, variant_name.len)
		} {
			return true
		}
	}

	return false
}

fn get_struct_type_workaround[T](mut decoder Decoder, initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $struct {
					val := copy_type(initialized_sumtype)
					return check_struct_type_valid(mut decoder, val, decoder.current_node)
				}
			}
		}
	}
	return false
}

fn init_sumtype_by_value_kind[T](mut decoder Decoder, mut val T, value_info ValueInfo) ! {
	mut failed_struct := false
	mut struct_variant_count := 0

	match value_info.value_kind {
		.string {
			$for v in val.variants {
				$if v.typ is string {
					val = T(v)
					return
				} $else $if v.typ is time.Time {
					val = T(v)
					return
				} $else $if v.typ is StringDecoder {
					val = T(v)
					return
				}
			}
		}
		.number {
			$for v in val.variants {
				$if v.typ is $float {
					val = T(v)
					return
				} $else $if v.typ is $int {
					val = T(v)
					return
				} $else $if v.typ is $enum {
					val = T(v)
					return
				} $else $if v.typ is NumberDecoder {
					val = T(v)
					return
				}
			}
		}
		.boolean {
			$for v in val.variants {
				$if v.typ is bool {
					val = T(v)
					return
				} $else $if v.typ is BooleanDecoder {
					val = T(v)
					return
				}
			}
		}
		.null {
			$for v in val.variants {
				$if v.typ is $option {
					val = T(v)
					return
				} $else $if v.typ is NullDecoder {
					val = T(v)
					return
				}
			}
		}
		.array {
			$for v in val.variants {
				$if v.typ is $array {
					val = T(v)

					if get_array_type_workaround(mut decoder, val) {
						return
					}
				}
			}
		}
		.object {
			$for v in val.variants {
				$if v.typ is $map {
					val = T(v)

					if get_map_type_workaround(mut decoder, val) {
						return
					}
				} $else $if v.typ is $struct {
					struct_variant_count++
					val = T(v)

					if get_struct_type_workaround(mut decoder, val) {
						return
					}

					failed_struct = true
				}
			}
		}
	}

	if failed_struct {
		// If there is only one struct variant and no explicit `_type` key,
		// the object shape is already unambiguous.
		if struct_variant_count == 1
			&& decoder.get_sumtype_type_field_node(decoder.current_node) == unsafe { nil } {
			return
		}
		decoder.decode_error('could not resolve sumtype `${T.name}`, missing "_type" field?')!
	}

	decoder.decode_error('could not resolve sumtype `${T.name}`, got ${value_info.value_kind}.')!
}

fn decode_sumtype[T](mut decoder Decoder, mut val T) ! {
	$if T is $alias {
		$if T.unaliased_typ is Any {
			mut unaliased_val := Any{}
			value_info := decoder.current_node.value

			init_sumtype_by_value_kind(mut decoder, mut unaliased_val, value_info)!

			unaliased_val = get_decoded_sumtype_workaround(mut decoder, unaliased_val)!
			val = T(unaliased_val)
		} $else {
			decoder.decode_error('Type aliased sumtypes not supported.')!
		}
	} $else {
		value_info := decoder.current_node.value

		init_sumtype_by_value_kind(mut decoder, mut val, value_info)!

		val = get_decoded_sumtype_workaround(mut decoder, val)!
	}
}
