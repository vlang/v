fn test_main() {
	unsafe {
		mut data := malloc(4)
		data[0] = c'f'
		data[1] = c'o'
		data[2] = c'o'
		data[3] = c'\0'
		assert data.vstring() == 'foo'
		data.free()
	}
}
