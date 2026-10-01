module types

fn interner_parallel_writer(mut interner TypeInterner, input []Type, ready chan bool, start chan bool) {
	ready <- true
	_ := <-start
	for n, typ in input {
		if n == 0 {
			interner.reserve(128)
		}
		id, canonical := interner.canonicalize(typ)
		assert semantic_types_equal(canonical, typ)
		assert interner.name(id) == typ.name()
	}
}

fn interner_parallel_reader(interner &TypeInterner, ready chan bool, start chan bool, stop chan bool) {
	ready <- true
	_ := <-start
	// Keep probing until every writer has finished growing the table.
	for {
		canonical := interner.probe(Type(string_)) or {
			assert false, 'an already interned type disappeared during insertion'
			return
		}
		assert canonical is String
		select {
			_ := <-stop {
				return
			}
			else {
			}
		}
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
	lanes := 3
	ready := chan bool{cap: 2 * lanes}
	start := chan bool{cap: 2 * lanes}
	stop := chan bool{cap: lanes}
	mut writers := []thread{}
	mut readers := []thread{}
	for lane in 0 .. lanes {
		writers << spawn interner_parallel_writer(mut interner, input[lane * 3000..(lane + 1) * 3000],
			ready, start)
		readers << spawn interner_parallel_reader(interner, ready, start, stop)
	}
	// Release readers and writers together once all of them are running.
	for _ in 0 .. 2 * lanes {
		_ := <-ready
	}
	for _ in 0 .. 2 * lanes {
		start <- true
	}
	writers.wait()
	for _ in 0 .. lanes {
		stop <- true
	}
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
