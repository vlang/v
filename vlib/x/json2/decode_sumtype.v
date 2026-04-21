module json2

import time

struct SumtypeTimeValue {
	typ   string @[json: '_type'; required]
	value i64    @[required]
}

fn sumtype_variant_name(type_name string) string {
	return type_name.all_after_last('.')
}

fn copy_type[T](_t T) T {
	return T{}
}

fn (mut decoder Decoder) get_decoded_sumtype_workaround[T](initialized_sumtype T) !T {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
		resolved_sumtype := initialized_sumtype
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is time.Time {
					mut val := copy_type(initialized_sumtype)
					decoder.decode_sumtype_time(mut val)!
					return T(val)
				} $else $if initialized_sumtype !is $option {
					mut val := copy_type(initialized_sumtype)
					decoder.decode_value(mut val)!
					return T(val)
				} $else {
					if decoder.current_node.value.value_kind == .null {
						decoder.current_node = decoder.current_node.next
						return resolved_sumtype
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

fn get_array_element_type[T](_arr []T) T {
	return T{}
}

fn (mut decoder Decoder) check_array_type_valid[T](arr []T, current_node &Node[ValueInfo]) bool {
	return decoder.check_element_type_valid(get_array_element_type(arr), current_node)
}

fn (mut decoder Decoder) get_array_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is $array {
					return decoder.check_element_type_valid(initialized_sumtype,
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

fn (mut decoder Decoder) check_map_type_valid[T](m T, current_node &Node[ValueInfo]) bool {
	element := get_map_element_type(m)
	return decoder.check_element_type_valid(element, current_node)
}

fn (mut decoder Decoder) check_map_empty_valid[T](m T) bool {
	element := get_map_element_type(m)
	return decoder.check_element_type_valid(element, current_node)
}

fn (mut decoder Decoder) get_map_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
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

fn (decoder &Decoder) get_sumtype_type_field_node(current_node &Node[ValueInfo]) &Node[ValueInfo] {
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

fn (decoder &Decoder) sumtype_type_field_matches(type_field_node &Node[ValueInfo], expected string) bool {
	if type_field_node == unsafe { nil } {
		return false
	}
	if type_field_node.value.value_kind != .string {
		return false
	}
	if type_field_node.value.length - 2 != expected.len {
		return false
	}
	return unsafe {
		type_field_node.value.position + 1 + expected.len <= decoder.json.len
			&& 0 == vmemcmp(decoder.json.str + type_field_node.value.position + 1, expected.str, expected.len)
	}
}

fn (decoder &Decoder) check_sumtype_type_valid[T](value T, current_node &Node[ValueInfo]) bool {
	type_field_node := decoder.get_sumtype_type_field_node(current_node)
	return decoder.sumtype_type_field_matches(type_field_node,
		sumtype_variant_name(typeof(value).name))
}

fn (mut decoder Decoder) check_struct_type_valid[T](s T, current_node &Node[ValueInfo]) bool {
	return decoder.check_sumtype_type_valid(s, current_node)
}

fn (mut decoder Decoder) get_struct_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
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

fn (mut decoder Decoder) get_time_type_workaround[T](initialized_sumtype T) bool {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				$if initialized_sumtype is time.Time {
					val := copy_type(initialized_sumtype)
					return decoder.check_sumtype_type_valid(val, decoder.current_node)
				}
			}
		}
	}
	return false
}

fn (mut decoder Decoder) resolve_sumtype_from_type_field[T](mut val T) !bool {
	if decoder.get_sumtype_type_field_node(decoder.current_node) == unsafe { nil } {
		return false
	}
	mut has_discriminated_variant := false
	$for v in val.variants {
		$if v.typ is time.Time {
			has_discriminated_variant = true
			val = T(v)
			if decoder.get_time_type_workaround(val) {
				return true
			}
		} $else $if v.typ is $struct {
			has_discriminated_variant = true
			val = T(v)
			if decoder.get_struct_type_workaround(val) {
				return true
			}
		}
	}
	if !has_discriminated_variant {
		return false
	}
	decoder.decode_error('could not resolve sumtype `${T.name}` from "_type" field')!
	return false
}

fn (mut decoder Decoder) decode_sumtype_time(mut val time.Time) ! {
	mut wrapper := SumtypeTimeValue{
		typ:   ''
		value: 0
	}
	decoder.decode_value(mut wrapper)!
	val = time.unix(wrapper.value)
}

fn (mut decoder Decoder) init_sumtype_by_value_kind[T](mut val T, value_info ValueInfo) ! {
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

					if decoder.get_array_type_workaround(val) {
						return
					}
				}
			}
		}
		.object {
			if decoder.resolve_sumtype_from_type_field(mut val)! {
				return
			}
			$for v in val.variants {
				$if v.typ is $map {
					val = T(v)

					if decoder.get_map_type_workaround(val) {
						return
					}
				} $else $if v.typ is $struct {
					struct_variant_count++
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

fn (mut decoder Decoder) decode_sumtype[T](mut val T) ! {
	value_info := decoder.current_node.value

	decoder.init_sumtype_by_value_kind(mut val, value_info)!

	val = decoder.get_decoded_sumtype_workaround(val)!
}
