pub type Any = []Any
	| bool
	| f32
	| f64
	| i16
	| i64
	| i8
	| int
	| map[string]Any
	| string
	| u16
	| u32
	| u64
	| u8

fn test_main() {
	ana := Any([Any('')])
	name(ana)
	match ana {
		[]Any {
			for i := 0; i < ana.len; i++ {
				name(ana[i])
			}
		}
		else {
			assert false
		}
	}
	assert true
}

fn name[T](val T) {
	$for v in val.variants {
		if val is v {
			dump(val.str())
			$if val is []Any {
				assert val.str() == "[Any('')]"
			} $else {
				assert val.str() == ''
			}
			dump(val)
			println(val)
		}
	}
}
