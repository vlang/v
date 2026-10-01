module types

fn interner_parallel_writer(mut interner TypeInterner, input []Type) {
	for n, typ in input {
		if n == 0 {
			interner.reserve(128)
		}
		id, canonical := interner.canonicalize(typ)
		assert semantic_types_equal(canonical, typ)
		assert interner.name(id) == typ.name()
	}
}

fn interner_parallel_reader(interner &TypeInterner) {
	for _ in 0 .. 12000 {
		canonical := interner.probe(Type(string_)) or {
			assert false, 'an already interned type disappeared during insertion'
			return
		}
		assert canonical is String
	}
}

fn test_interner_probe_during_parallel_growth() {
	$if prealloc {
		// Worker arenas cannot own storage shared after their threads terminate.
		return
	}
	mut interner := new_type_interner()
	interner.canonicalize(Type(string_))
	// Keep semantic payloads owned by the parent until every thread has joined.
	mut input := []Type{cap: 9000}
	for n in 0 .. 9000 {
		input << Type(ArrayFixed{
			elem_type: Type(int_)
			len:       n
		})
	}
	mut writers := []thread{}
	mut readers := []thread{}
	for lane in 0 .. 3 {
		writers << spawn interner_parallel_writer(mut interner, input[lane * 3000..(lane + 1) * 3000])
		readers << spawn interner_parallel_reader(interner)
	}
	writers.wait()
	readers.wait()
	assert interner.len() == 9001
	if _ := interner.probe(Type(bool_)) {
		assert false, 'a missing type must not be returned'
	}
	// The missing-type path must release the lock before the next insertion.
	interner.canonicalize(Type(bool_))
	canonical := interner.probe(Type(bool_)) or { panic('inserted type is missing') }
	assert semantic_types_equal(canonical, Type(bool_))
}
