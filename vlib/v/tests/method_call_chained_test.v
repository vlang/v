// for issue 20395
// Phenomenon: cgen brackets are asymmetric in chained method calls.
fn test_main() {
	mut path := 'hello/file.txt'
	extension := path.split('.').last()
	assert extension == 'txt'
}
