fn test_array_method_using_it_in_defer() {
	arr := ['1', '1.1', '1.2', '2', '2.1', '2.2']

	defer {
		ret := arr.filter(it.contains('1'))
		println(ret)
		assert ret == ['1', '1.1', '1.2', '2.1']
	}
}
