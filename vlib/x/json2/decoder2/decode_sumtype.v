module decoder2

import time

fn (mut decoder Decoder) get_decoded_sumtype_workaround[T](initialized_sumtype T) !T {
	$if initialized_sumtype is $sumtype {
		$for v in initialized_sumtype.variants {
			if initialized_sumtype is v {
				mut val := initialized_sumtype
				decoder.decode_value(mut val)!
				return T(val)
			}
		}
	}
	return initialized_sumtype
}

fn init_sumtype_by_value_kind[T](mut val T, value_info ValueInfo) ! {
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
				// Will only be supported when json object has field "_type"
				error('cannot encode value with ${typeof(val).name} type')
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

	init_sumtype_by_value_kind(mut val, value_info)!

	decoded_sumtype := decoder.get_decoded_sumtype_workaround(val)!
	unsafe {
		*val = decoded_sumtype
	}
}
