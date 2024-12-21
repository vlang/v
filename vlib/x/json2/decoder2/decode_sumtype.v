module decoder2

import time

fn (mut decoder Decoder) get_decoded_sumtype_workaround[T](initialized_sumtype T) !T {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				// workaround for auto generated function considering sumtype as array
				unsafe {
					$if initialized_sumtype is $map {
						mut val := initialized_sumtype.clone()
						decoder.decode_value(mut val)!
						return T(val)
					} $else {
						mut val := initialized_sumtype
						decoder.decode_value(mut val)!
						return T(val)
					}
				}
			}
		}
	}
	return initialized_sumtype
}

fn (mut decoder Decoder) init_sumtype_by_value_kind[T](mut val T, value_info ValueInfo) ! {
	$for v in val.variants {
		if value_info.value_kind == .string_ {
			$if v.typ is string {
				val = T(v)
				return
			} $else $if v.typ is time.Time {
				val = T(v)
				return
			}
		} else if value_info.value_kind == .number {
			$if v.typ is $float {
				val = T(v)
				return
			} $else $if v.typ is $int {
				val = T(v)
				return
			} $else $if v.typ is $enum {
				val = T(v)
				return
			}
		} else if value_info.value_kind == .boolean {
			$if v.typ is bool {
				val = T(v)
				return
			}
		} else if value_info.value_kind == .object {
			$if v.typ is $map {
				val = T(v)
				return
			} $else $if v.typ is $struct {
				// find "_type" field in json object
				mut type_field_node := decoder.current_node.next
				map_position := value_info.position
				map_end := map_position + value_info.length

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
						vmemcmp(decoder.json.str + key_info.position, type_field.str,
							type_field.len) == 0
					} {
						// find type field
						type_field_node = type_field_node.next

						break
					} else {
						type_field_node = type_field_node.next
					}
				}

				if type_field_node != unsafe { nil } {
					$for v in val.variants {
						variant_name := typeof(v.typ).name
						if type_field_node.value.length - 2 == variant_name.len {
							unsafe {
							}
							if unsafe {
								vmemcmp(decoder.json.str + type_field_node.value.position + 1,
									variant_name.str, variant_name.len) == 0
							} {
								val = T(v)
							}
						}
					}
				}

				return
			}
		} else if value_info.value_kind == .array {
			$if v.typ is $array {
				val = T(v)
				return
			}
		}
	}
}

fn (mut decoder Decoder) decode_sumtype[T](mut val T) ! {
	value_info := decoder.current_node.value

	decoder.init_sumtype_by_value_kind(mut val, value_info)!

	decoded_sumtype := decoder.get_decoded_sumtype_workaround(val)!
	unsafe {
		*val = decoded_sumtype
	}
}
