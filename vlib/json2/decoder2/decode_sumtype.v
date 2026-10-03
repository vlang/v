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
				// type_field_idx is the index in values_info of the value of the `_type`
				// key, or -1 without such a key.
				mut type_field_idx := -1
				mut key_idx := decoder.current_idx + 1
				map_position := value_info.position
				map_end := map_position + value_info.length

				type_field := '_type'

				for key_idx < decoder.values_info.len {
					key_info := decoder.values_info[key_idx]

					if key_info.position >= map_end {
						break
					}

					value_idx := key_idx + 1
					if value_idx >= decoder.values_info.len {
						break
					}

					if decoder.decode_string(key_info)! == type_field {
						// find type field
						type_field_idx = value_idx
						break
					}

					// Skip the value, with everything nested in it.
					key_value_info := decoder.values_info[value_idx]
					value_end := key_value_info.position + key_value_info.length
					key_idx = value_idx + 1
					for key_idx < decoder.values_info.len
						&& decoder.values_info[key_idx].position < value_end {
						key_idx++
					}
				}

				if type_field_idx != -1 {
					type_field_info := decoder.values_info[type_field_idx]
					if type_field_info.value_kind == .string_ {
						decoded_type := decoder.decode_string(type_field_info)!
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
	value_info := decoder.values_info[decoder.current_idx]

	decoder.init_sumtype_by_value_kind(mut val, value_info)!

	decoded_sumtype := decoder.get_decoded_sumtype_workaround(val)!
	unsafe {
		*val = decoded_sumtype
	}
}
