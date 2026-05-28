struct Wrapped[T] {
	value T
}

struct WrappedStore[T] {
mut:
	data map[string]Wrapped[T]
}

fn new_wrapped_store[T]() WrappedStore[T] {
	return WrappedStore[T]{
		data: map[string]Wrapped[T]{}
	}
}

fn new_seeded_wrapped_store[T](key string, val T) WrappedStore[T] {
	return WrappedStore[T]{
		data: {
			key: Wrapped[T]{
				value: val
			}
		}
	}
}

fn (mut store WrappedStore[T]) set(key string, val T) {
	store.data[key] = Wrapped[T]{
		value: val
	}
}

fn (mut store WrappedStore[T]) get(key string) !T {
	if record := store.data[key] {
		return record.value
	}
	return error('missing')
}

struct DirectStore[T] {
mut:
	data map[string]T
}

fn new_direct_store[T]() DirectStore[T] {
	return DirectStore[T]{
		data: map[string]T{}
	}
}

fn new_seeded_direct_store[T](key string, val T) DirectStore[T] {
	return DirectStore[T]{
		data: {
			key: val
		}
	}
}

fn (mut store DirectStore[T]) set(key string, val T) {
	store.data[key] = val
}

fn (mut store DirectStore[T]) get(key string) !T {
	if record := store.data[key] {
		return record
	}
	return error('missing')
}

struct IntKeyWrappedStore[T] {
mut:
	data map[int]Wrapped[T]
}

fn new_int_key_wrapped_store[T]() IntKeyWrappedStore[T] {
	return IntKeyWrappedStore[T]{
		data: map[int]Wrapped[T]{}
	}
}

fn new_seeded_int_key_wrapped_store[T](key int, val T) IntKeyWrappedStore[T] {
	return IntKeyWrappedStore[T]{
		data: {
			key: Wrapped[T]{
				value: val
			}
		}
	}
}

fn (mut store IntKeyWrappedStore[T]) set(key int, val T) {
	store.data[key] = Wrapped[T]{
		value: val
	}
}

fn (mut store IntKeyWrappedStore[T]) get(key int) !T {
	if record := store.data[key] {
		return record.value
	}
	return error('missing')
}

struct IntKeyDirectStore[T] {
mut:
	data map[int]T
}

fn new_int_key_direct_store[T]() IntKeyDirectStore[T] {
	return IntKeyDirectStore[T]{
		data: map[int]T{}
	}
}

fn new_seeded_int_key_direct_store[T](key int, val T) IntKeyDirectStore[T] {
	return IntKeyDirectStore[T]{
		data: {
			key: val
		}
	}
}

fn (mut store IntKeyDirectStore[T]) set(key int, val T) {
	store.data[key] = val
}

fn (mut store IntKeyDirectStore[T]) get(key int) !T {
	if record := store.data[key] {
		return record
	}
	return error('missing')
}

struct UserRecord {
	name string
}

fn test_generic_struct_method_with_generic_map_value() {
	mut string_store := new_wrapped_store[string]()
	string_store.set('alpha', 'one')
	assert string_store.get('alpha')! == 'one'

	mut record_store := new_wrapped_store[UserRecord]()
	record_store.set('alpha', UserRecord{
		name: 'two'
	})
	assert record_store.get('alpha')!.name == 'two'

	mut seeded_string_store := new_seeded_wrapped_store('seed', 'three')
	assert seeded_string_store.get('seed')! == 'three'

	mut seeded_record_store := new_seeded_wrapped_store('seed', UserRecord{
		name: 'four'
	})
	assert seeded_record_store.get('seed')!.name == 'four'
}

fn test_generic_struct_method_with_int_key_generic_map_value() {
	mut string_store := new_int_key_wrapped_store[string]()
	string_store.set(1, 'one')
	assert string_store.get(1)! == 'one'

	mut record_store := new_int_key_wrapped_store[UserRecord]()
	record_store.set(1, UserRecord{
		name: 'two'
	})
	assert record_store.get(1)!.name == 'two'

	mut seeded_string_store := new_seeded_int_key_wrapped_store(2, 'three')
	assert seeded_string_store.get(2)! == 'three'

	mut seeded_record_store := new_seeded_int_key_wrapped_store(2, UserRecord{
		name: 'four'
	})
	assert seeded_record_store.get(2)!.name == 'four'
}

fn test_generic_struct_method_with_int_key_direct_generic_map_value() {
	mut string_store := new_int_key_direct_store[string]()
	string_store.set(1, 'one')
	assert string_store.get(1)! == 'one'

	mut record_store := new_int_key_direct_store[UserRecord]()
	record_store.set(1, UserRecord{
		name: 'two'
	})
	assert record_store.get(1)!.name == 'two'

	mut seeded_string_store := new_seeded_int_key_direct_store(2, 'three')
	assert seeded_string_store.get(2)! == 'three'

	mut seeded_record_store := new_seeded_int_key_direct_store(2, UserRecord{
		name: 'four'
	})
	assert seeded_record_store.get(2)!.name == 'four'
}

fn test_generic_struct_method_with_direct_generic_map_value() {
	mut string_store := new_direct_store[string]()
	string_store.set('alpha', 'one')
	assert string_store.get('alpha')! == 'one'

	mut record_store := new_direct_store[UserRecord]()
	record_store.set('alpha', UserRecord{
		name: 'two'
	})
	assert record_store.get('alpha')!.name == 'two'

	mut seeded_string_store := new_seeded_direct_store('seed', 'three')
	assert seeded_string_store.get('seed')! == 'three'

	mut seeded_record_store := new_seeded_direct_store('seed', UserRecord{
		name: 'four'
	})
	assert seeded_record_store.get('seed')!.name == 'four'
}
