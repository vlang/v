@[translated]
module main

__global translated_total = int(0)
__global translated_saved = &TranslatedItem(unsafe { nil })
__global translated_freed = int(0)

struct TranslatedItem {
	value int
}

fn (item &TranslatedItem) free() {
	translated_freed++
}

fn translated_items() []TranslatedItem {
	return [TranslatedItem{ value: 1 }, TranslatedItem{ value: 2 }]
}

fn translated_capture(item &TranslatedItem) int {
	translated_saved = item
	return item.value
}

struct TranslatedCounter {
	count int
}

fn translated_bump(state &TranslatedCounter) {
	state.count++
	state.count--
	state.count++
}

fn translated_store(target &int, value int) {
	*target = value
	translated_total = value
}

fn test_translated_storage() {
	mut value := 0
	translated_store(&value, 17)
	assert value == 17
	assert translated_total == 17
	counter := TranslatedCounter{}
	translated_bump(&counter)
	assert counter.count == 1
}

fn test_translated_global_retains_array_map_element() {
	translated_freed = 0
	mapped := translated_items().map(translated_capture(&it))
	assert mapped == [1, 2]
	assert translated_saved.value == 2
	assert translated_freed == 0
}

fn translated_pointer_call(value &int) &int {
	return value
}

fn test_translated_assignment_through_pointer_call() {
	value := 0
	*translated_pointer_call(&value) = 42
	assert value == 42
	*translated_pointer_call(&value) += 1
	assert value == 43
}
