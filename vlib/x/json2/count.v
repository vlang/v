module json2

import time

struct Count {
mut:
	total int
}

// get_total
fn (mut count Count) get_total() int {
	return count.total
}

// reset_total
fn (mut count Count) reset_total() {
	count.total = 0
}

// count_chars count json sizen without new encode
fn (mut count Count) count_chars[T](val T) {
	$if val is $option {
		workaround := val
		if workaround != none {
			count.count_chars(val)
		}
	} $else $if T is string {
		count.chars_in_string(val)
	} $else $if T is $sumtype {
		$for v in val.variants {
			if val is v {
				count.count_chars(val)
			}
		}
	} $else $if T is $alias {
		// TODO
	} $else $if T is time.Time {
		count.total += 26 // "YYYY-MM-DDTHH:mm:ss.123Z"
	} $else $if T is $map {
		count.total++ // {
		for k, v in val {
			count.count_chars(k)
			count.total++ // :
			count.count_chars(v)
		}
		count.total++ // }
	} $else $if T is $array {
		count.total += 2 // []
		if val.len > 0 {
			for element in val {
				count.count_chars(element)
			}
			count.total += val.len - 1 // ,
		}
	} $else $if T is $struct {
		count.chars_in_struct(val)
	} $else $if T is $enum {
		count.count_chars(int(val))
	} $else $if T is $int {
		// TODO: benchmark
		mut abs_val := val
		if val < 0 {
			count.total++ // -
			abs_val = -val
		}
		for number_value := abs_val; number_value >= 1; number_value /= 10 {
			count.total++
		}
		if val == 0 {
			count.total++
		}
	} $else $if T is $float {
		// TODO
	} $else $if T is bool {
		if val {
			count.total += 4 // true
		} else {
			count.total += 5 // false
		}
	} $else {
	}
}

// chars_in_struct
fn (mut count Count) chars_in_struct[T](val T) {
	count.total += 2 // {}
	$for field in T.fields {
		// TODO: handle attributes
		count.total += field.name.len + 3 // "":
		workaround := val.$(field.name)
		count.count_chars(workaround)
	}
}

// chars_in_string
fn (mut count Count) chars_in_string(val string) {
	count.total += val.len + 2 // ""
}
