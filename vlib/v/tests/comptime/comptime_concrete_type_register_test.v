struct Encoder {}

struct StructType[T] {
mut:
	val T
}

fn encode_struct[U](val U) ! {
	$for field in U.fields {
		$if field.typ is $map {
			for _, v in val.$(field.name) {
				encode_value_with_level(v)!
			}
		}
	}
}

fn encode_value_with_level[T](val T) ! {
	$if T is $struct {
		dump(val)
		println(encode_struct(val)!)
	} $else $if T is $map {
		dump(val)
	} $else $if T is string {
		dump(val)
	}
}

fn (e &Encoder) encode_struct[U](val U) ! {
	$for field in U.fields {
		$if field.typ is $map {
			for _, v in val.$(field.name) {
				e.encode_value_with_level(v)!
			}
		}
	}
}

fn (e &Encoder) encode_value_with_level[T](val T) ! {
	$if T is $struct {
		dump(val)
		println(e.encode_struct(val)!)
	} $else $if T is $map {
		dump(val)
	} $else $if T is string {
		dump(val)
	}
}

fn test_method() ! {
	e := Encoder{}
	e.encode_struct(StructType[map[string]map[string]int]{
		val: {
			'1': {
				'val': 1
			}
		}
	})!
	e.encode_struct(StructType[map[string]string]{
		val: {
			'1': '1'
		}
	})!
}

fn test_func() ! {
	encode_struct(StructType[map[string]map[string]int]{
		val: {
			'1': {
				'val': 1
			}
		}
	})!
	encode_struct(StructType[map[string]string]{
		val: {
			'1': '1'
		}
	})!
}
