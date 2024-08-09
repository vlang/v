type MyInt = int

struct Abc {
	x    int
	y    int
	name string
}

enum JobTitle {
	manager
	executive
	worker
}

struct Complex {
	s        string
	i        int
	ch_i     chan int
	atomic_i atomic int
	my_alias MyInt = 144

	pointer1_i &int   = unsafe { nil }
	pointer2_i &&int  = unsafe { nil }
	pointer3_i &&&int = unsafe { nil }

	array_i          []int
	map_i            map[int]int
	my_struct        Abc
	my_struct_shared shared Abc

	o_s        ?string
	o_i        ?int
	o_ch_i     ?chan int = chan int{cap: 10}
	o_my_alias ?MyInt    = 123
	// o_atomic_i ?atomic int // TODO: cgen error, but should be probably a checker one, since option atomics do not make sense
	o_pointer1_i ?&int
	o_pointer2_i ?&&int
	o_pointer3_i ?&&&int

	o_array_i          ?[]int
	o_map_i            ?map[int]int
	o_my_struct        ?Abc
	o_my_struct_shared ?shared Abc
	jobtitle_enum      JobTitle
}

fn test_is_shared() {
	$for f in Complex.fields {
		if f.name.contains('_shared') {
			assert f.is_shared, 'Complex.${f.name} should have f.is_shared set'
		} else {
			assert !f.is_shared, 'Complex.${f.name} should NOT have f.is_shared set'
		}
	}
}

fn test_is_atomic() {
	$for f in Complex.fields {
		if f.name.contains('atomic_') {
			assert f.is_atomic, 'StructWithAtomicFields.${f.name} should have f.is_atomic set'
		} else {
			assert !f.is_atomic, 'StructWithAtomicFields.${f.name} should NOT have f.is_atomic set'
		}
	}
}

fn test_is_option() {
	$for f in Complex.fields {
		if f.name.starts_with('o_') {
			assert f.is_option, 'Complex.${f.name} should have f.is_option set'
		} else {
			assert !f.is_option, 'Complex.${f.name} should NOT have f.is_option set'
		}
	}
}

fn test_is_array() {
	$for f in Complex.fields {
		if f.name.contains('array_') {
			assert f.is_array, 'Complex.${f.name} should have f.is_array set'
		} else {
			assert !f.is_array, 'Complex.${f.name} should NOT have f.is_array set'
		}
	}
}

fn test_is_map() {
	$for f in Complex.fields {
		if f.name.contains('map_') {
			assert f.is_map, 'Complex.${f.name} should have f.is_map set'
		} else {
			assert !f.is_map, 'Complex.${f.name} should NOT have f.is_map set'
		}
	}
}

fn test_is_chan() {
	$for f in Complex.fields {
		if f.name.contains('ch_') {
			assert f.is_chan, 'Complex.${f.name} should have f.is_chan set'
		} else {
			assert !f.is_chan, 'Complex.${f.name} should NOT have f.is_chan set'
		}
	}
}

fn test_is_struct() {
	$for f in Complex.fields {
		if f.name.contains('_struct') {
			assert f.is_struct, 'Complex.${f.name} should have f.is_struct set'
		} else {
			assert !f.is_struct, 'Complex.${f.name} should NOT have f.is_struct set'
		}
	}
}

fn test_is_alias() {
	$for f in Complex.fields {
		if f.name.contains('_alias') {
			assert f.is_alias, 'Complex.${f.name} should have f.is_alias set'
		} else {
			assert !f.is_alias, 'Complex.${f.name} should NOT have f.is_alias set'
		}
	}
}

fn test_is_enum() {
	$for f in Complex.fields {
		if f.name.contains('_enum') {
			assert f.is_enum, 'Complex.${f.name} should have f.is_enum set'
		} else {
			assert !f.is_enum, 'Complex.${f.name} should NOT have f.is_enum set'
		}
	}
}

fn test_indirections() {
	$for f in Complex.fields {
		if f.name.contains('pointer') || f.name in ['my_struct_shared', 'o_my_struct_shared'] {
			assert f.indirections > 0, 'Complex.${f.name} should have f.indirections > 0'
		} else {
			assert !(f.indirections > 0), 'Complex.${f.name} should NOT have f.indirections > 0'
		}
		if f.name.contains('pointer1') {
			assert f.indirections == 1
		}
		if f.name.contains('pointer2') {
			assert f.indirections == 2
		}
		if f.name.contains('pointer3') {
			assert f.indirections == 3
		}
		if f.name.contains('my_struct_shared') {
			assert f.indirections == 1
		}
	}
}
