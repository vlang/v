struct Encoder {}

struct Writer {}

struct ComptimeTypeofIdxInner {
	x int
}

struct ComptimeTypeofIdxOuter {
mut:
	inner &ComptimeTypeofIdxInner = unsafe { nil }
}

struct StructType[T] {
mut:
	val  &T
	val2 T
}

fn (e &Encoder) encode_struct[U](val U, mut wr Writer) ! {
	$for field in U.fields {
		if field.indirections == 1 {
			assert field.indirections == 1
			value := val.$(field.name)
			$if field.indirections == 1 {
				assert *value == 'ads'
			} $else {
				assert false
			}
		} else {
			assert field.name == 'val2'
		}
	}
}

fn test_indirection_checking() {
	e := Encoder{}
	mut sb := Writer{}
	mut string_pointer := 'ads'
	e.encode_struct(StructType[string]{ val: &string_pointer }, mut sb)!
}

fn allocate_comptime_typeof_idx_ptrs[T](mut s T) {
	$for f in T.fields {
		$if f.indirections == 1 {
			s.$(f.name) = &typeof(s.$(f.name)).idx{}
		}
	}
}

fn test_comptime_typeof_idx_struct_init() {
	mut outer := ComptimeTypeofIdxOuter{}
	allocate_comptime_typeof_idx_ptrs(mut outer)
	assert outer.inner != unsafe { nil }
	assert outer.inner.x == 0
}
