module builtin

$if tinyc {
	// Alpine's libgc references these as weak probes. TCC errors on the
	// unresolved weak symbols when linking against musl's libgc, so provide
	// weak definitions with the same array type libgc declares.

	@[export: '__data_start']
	@[markused; weak]
	__global C.__data_start = [1]int{}

	@[export: 'data_start']
	@[markused; weak]
	__global C.data_start = [1]int{}
}
