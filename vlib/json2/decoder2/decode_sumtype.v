module decoder2

import time

fn sumtype_variant_name(type_name string) string {
	return type_name.all_after_last('.')
}

fn (mut decoder Decoder) get_decoded_sumtype_workaround[T](initialized_sumtype T) !T {
	$if initialized_sumtype is $sumtype || (T is $alias && T.unaliased_typ is $sumtype) {
		$for v in T.variants {
			if initialized_sumtype is v {
				mut val := $zero(v.typ)
				decoder.decode_value(mut val)!
				return T(val)
			}
		}
	}
	return initialized_sumtype
}

fn (mut decoder Decoder) init_sumtype_by_value_kind[T](mut val T, value_info ValueInfo) ! {
	$for v in T.variants {
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
		} else if value_info.value_kind == .null {
			$if v.typ is NullDecoder {
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

				type_field := '_type'

				for {
					if type_field_node == unsafe { nil } {
						break
					}

					key_info := type_field_node.value

					if key_info.position >= map_end {
						type_field_node = unsafe { nil }
						break
					}

					mut value_node := type_field_node.next
					if value_node == unsafe { nil } {
						type_field_node = unsafe { nil }
						break
					}

					if decoder.decode_string(key_info)! == type_field {
						// find type field
						type_field_node = value_node
						break
					}

					value_end := value_node.value.position + value_node.value.length
					type_field_node = value_node.next
					for type_field_node != unsafe { nil }
						&& type_field_node.value.position < value_end {
						type_field_node = type_field_node.next
					}
				}

				if type_field_node != unsafe { nil } {
					if type_field_node.value.value_kind == .string_ {
						decoded_type := decoder.decode_string(type_field_node.value)!
						$for v in T.variants {
							variant_name := sumtype_variant_name(typeof(v.typ).name)
							if decoded_type == variant_name {
								val = T(v)
								return
							}
						}
						return error('could not resolve sumtype `${T.name}` from `_type` value `${decoded_type}`')
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
