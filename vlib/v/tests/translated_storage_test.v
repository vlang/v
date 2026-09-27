@[translated]
module main

__global translated_total = int(0)

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
