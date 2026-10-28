module flat

fn C.atomic_fetch_add_u32(voidptr, u32) u32

const lookup_race_readers = u32(4)
const lookup_race_writers = 4
const lookup_race_spellings_per_writer = 4096

struct SourceTypeLookupRace {
mut:
	started u32
	done    u32
}

// forget_recent_source_type clears the recent-cache slot of `text`, so the next
// lookup probes the shared index unless another thread refills the slot first.
fn forget_recent_source_type(text string) {
	slot := source_type_recent_slot(text)
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_recent_entries[slot]), 0)
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_recent_entries[slot]), 0)
	}
}

fn intern_source_types_during_lookups(spellings []string, race &SourceTypeLookupRace) string {
	for C.atomic_load_u32(&race.started) < lookup_race_readers {
		C.cpu_relax()
	}
	for text in spellings {
		id := source_type_id(text)
		forget_recent_source_type(text)
		found := source_type_existing_id(text) or {
			return '${text}: missing right after its insertion as ${id}'
		}
		if found != id {
			return '${text}: inserted as ${id}, found as ${found}'
		}
	}
	return ''
}

fn lookup_source_types_during_growth(reader int, known []string, known_ids []u32, absent []string, growing [][]string, race &SourceTypeLookupRace) string {
	C.atomic_fetch_add_u32(&race.started, 1)
	for round := 0; true; round++ {
		// Every spelling is published once the writers are joined, so the pass
		// that starts after that point must find all of them.
		writers_done := C.atomic_load_u32(&race.done) != 0
		for i, text in known {
			if (round + reader + i) % 2 == 0 {
				forget_recent_source_type(text)
			}
			id := source_type_existing_id(text) or { return '${text}: known spelling missing' }
			if id != known_ids[i] {
				return '${text}: known as ${known_ids[i]}, found as ${id}'
			}
		}
		for text in absent {
			if id := source_type_existing_id(text) {
				return '${text}: absent spelling found as ${id}'
			}
		}
		for spellings in growing {
			// A writer inserts its spellings in order. Scanning backwards, once one
			// is found, every earlier spelling was published before this lookup.
			mut published := false
			for i := spellings.len - 1; i >= 0; i-- {
				text := spellings[i]
				forget_recent_source_type(text)
				if id := source_type_existing_id(text) {
					published = true
					name := source_type_name(id)
					if name != text {
						return '${text}: found as ${id}, named `${name}`'
					}
				} else if published || writers_done {
					return '${text}: missing after a later spelling was published'
				}
			}
		}
		if writers_done {
			break
		}
	}
	return ''
}

fn test_source_type_existing_id_while_the_shared_table_grows() {
	mut known := []string{}
	mut known_ids := []u32{}
	for i in 0 .. 256 {
		text := 'existing_concurrency.Known_${i}'
		known << text
		known_ids << source_type_id(text)
	}
	absent := []string{len: 64, init: 'existing_concurrency.Absent_${index}'}
	mut growing := [][]string{}
	for writer in 0 .. lookup_race_writers {
		growing << []string{len: lookup_race_spellings_per_writer, init: 'existing_concurrency.Grow_${writer}_${index}'}
	}
	table := source_type_table_load()
	count := C.atomic_load_u32(&table.count)
	capacity := source_type_index_load(table).capacity
	race := &SourceTypeLookupRace{}
	mut readers := []thread string{}
	for reader in 0 .. int(lookup_race_readers) {
		readers << spawn lookup_source_types_during_growth(reader, known, known_ids, absent,
			growing, race)
	}
	mut writers := []thread string{}
	for spellings in growing {
		writers << spawn intern_source_types_during_lookups(spellings, race)
	}
	mut failures := []string{}
	for handle in writers {
		failures << handle.wait()
	}
	C.atomic_store_u32(&race.done, 1)
	for handle in readers {
		failures << handle.wait()
	}
	assert failures.filter(it != '') == []
	// Lookups never intern: only the writers' spellings were added, and they
	// spread over new chunks and several index generations during the reads.
	added := u32(lookup_race_writers * lookup_race_spellings_per_writer)
	assert C.atomic_load_u32(&table.count) == count + added
	assert source_type_index_load(table).capacity >= capacity * 8
	for spellings in growing {
		for text in spellings {
			id := source_type_existing_id(text) or { panic('${text} missing') }
			assert source_type_name(id) == text
		}
	}
	for text in absent {
		if _ := source_type_existing_id(text) {
			assert false
		}
	}
}
