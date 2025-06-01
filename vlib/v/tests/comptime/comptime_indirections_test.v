fn count_muls[T](val T) int {
	mut nr_muls := 0
	dump(T.indirections)
	dump(typeof(val).indirections)
	$if T.indirections > 0 {
		nr_muls = (int(typeof(val).idx) >> 16) & 0xff
	}
	assert nr_muls == T.indirections
	$if T.indirections != 0 {
		nr_muls = (int(typeof(val).idx) >> 16) & 0xff
	}
	assert nr_muls == T.indirections
	return nr_muls
}

fn test_main() {
	val := ''
	pval := &val
	ppval := &pval
	assert count_muls(val) == 0
	assert count_muls(pval) == 1
	assert count_muls(ppval) == 2
}
