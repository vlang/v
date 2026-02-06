import arrays

fn test_main() {
	items := ['item1', 'item2', 'item3']
	list := arrays.map_indexed[string, string](items, |i, item| '${i}. ${item}')
	assert list == ['0. item1', '1. item2', '2. item3']
}
