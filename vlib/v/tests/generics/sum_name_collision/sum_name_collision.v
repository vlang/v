module sum_name_collision

pub type Thing = int | bool | string

// to_sum casts a string to the caller's chosen sum type.
pub fn to_sum[T](s string) T {
	return T(s)
}

// to_sum_comptime casts through the reflected variants of the caller's sum type.
pub fn to_sum_comptime[T](s string) T {
	mut result := T{}
	$for variant in T.variants {
		$if variant.typ is string {
			result = T(s)
		}
	}
	return result
}

// local_sum constructs this module's sum type.
pub fn local_sum(s string) Thing {
	return Thing(s)
}
