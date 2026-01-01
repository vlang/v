module json2

import time

fn copy_type[T](t T) T {
	return T{}
}

fn (mut decoder Decoder) get_decoded_sumtype_workaround[T](initialized_sumtype T) !T {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype !is $option {
					mut val := copy_type(initialized_sumtype)
					decoder.decode_value(mut val)!
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

fn (mut decoder Decoder) check_element_type_valid[T](element T, current_node &Node[ValueInfo]) bool {
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
				return decoder.check_array_type_valid(element, current_node.next)
			}
		}
		.object {
			$if element is $map {
				if current_node.next != unsafe { nil } {
					return decoder.check_map_type_valid(element, current_node.next.next)
				} else {
					return decoder.check_map_type_valid(element, unsafe { nil })
				}
			} $else $if element is $struct {
				return decoder.check_struct_type_valid(element, current_node)
			}
		}
	}

	return false
}

fn get_array_element_type[T](arr []T) T {
	return T{}
}

fn (mut decoder Decoder) check_array_type_valid[T](arr []T, current_node &Node[ValueInfo]) bool {
	return decoder.check_element_type_valid(get_array_element_type(arr), current_node)
}

fn (mut decoder Decoder) get_array_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $array {
					return decoder.check_element_type_valid(initialized_sumtype, decoder.current_node)
				}
			}
		}
	}
	return false
}

fn get_map_element_type[U, V](m map[U]V) V {
	return V{}
}

fn (mut decoder Decoder) check_map_type_valid[T](m T, current_node &Node[ValueInfo]) bool {
	element := get_map_element_type(m)
	return decoder.check_element_type_valid(element, current_node)
}

fn (mut decoder Decoder) check_map_empty_valid[T](m T) bool {
	element := get_map_element_type(m)
	return decoder.check_element_type_valid(element, current_node)
}

fn (mut decoder Decoder) get_map_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $map {
					val := copy_type(initialized_sumtype)
					if decoder.current_node.next != unsafe { nil } {
						return decoder.check_map_type_valid(val, decoder.current_node.next.next)
					} else {
						return decoder.check_map_type_valid(val, unsafe { nil })
					}
				}
			}
		}
	}
	return false
}

fn (mut decoder Decoder) check_struct_type_valid[T](s T, current_node Node[ValueInfo]) bool {
	// find "_type" field in json object
	mut type_field_node := decoder.current_node.next
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
			type_field_node = type_field_node.next

			break
		} else {
			type_field_node = type_field_node.next
		}
	}

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

fn (mut decoder Decoder) get_struct_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $struct {
					val := copy_type(initialized_sumtype)
					return decoder.check_struct_type_valid(val, decoder.current_node)
				}
			}
		}
	}
	return false
}

fn (mut decoder Decoder) init_sumtype_by_value_kind[T](mut val T, value_info ValueInfo) ! {
	mut failed_struct := false

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

					if decoder.get_array_type_workaround(val) {
						return
					}
				}
			}
		}
		.object {
			$for v in val.variants {
				$if v.typ is $map {
					val = T(v)

					if decoder.get_map_type_workaround(val) {
						return
					}
				} $else $if v.typ is $struct {
					val = T(v)

					if decoder.get_struct_type_workaround(val) {
						return
					}

					failed_struct = true
				}
			}
		}
	}

	if failed_struct {
		decoder.decode_error('could not resolve sumtype `${T.name}`, missing "_type" field?')!
	}

	decoder.decode_error('could not resolve sumtype `${T.name}`, got ${value_info.value_kind}.')!
}

fn (mut decoder Decoder) decode_sumtype[T](mut val T) ! {
	$if T is $alias {
		decoder.decode_error('Type aliased sumtypes not supported.')!
	} $else {
		value_info := decoder.current_node.value

		decoder.init_sumtype_by_value_kind(mut val, value_info)!

		val = decoder.get_decoded_sumtype_workaround(val)!
	}
}
