interface Any {}

fn test_main() {
	mut a := []Any{}
	a.insert(0, Any(5))
	a.insert(1, Any(5.0))
	a.insert(2, Any(a[0]))
	a.insert(3, Any(a)) // Terminated by signal 11 (SIGSEGV)
	assert dump('${a}') == '[Any(5), Any(5.0), Any(5), <circular>]'
}
