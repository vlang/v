@[translated]
module main

__global translated_total = int(0)

fn translated_store(target &int, value int) {
	*target = value
	translated_total = value
}

fn test_translated_storage() {
	mut value := 0
	translated_store(&value, 17)
	assert value == 17
	assert translated_total == 17
}
