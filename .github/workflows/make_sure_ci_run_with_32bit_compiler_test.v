fn test_ci_run_with_32bit_compiler() {
	$if x64 && tinyc {
		// TODO: uncomment that next assert when tcc32 vs tcc64 detection on windows works reliably
		// assert false
		assert true
	}
}
